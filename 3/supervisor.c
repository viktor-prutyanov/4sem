/**
 *  Distributed Simpson method
 *
 *  @file supervisor.c
 *  
 *  @date 04.2016
 * 
 *  @copyright GNU GPL v2.0
 * 
 *  @author Viktor Prutyanov mailto:viktor.prutyanov@phystech.edu 
 *  
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/select.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "common.h"

extern int errno;

int udp_broadcast();
inline long int get_workers_num(char *argv[]);
inline long int get_threads_num(char *argv[]);
inline double get_segm_begin(char *argv[]);
inline double get_segm_end(char *argv[]);
double serve_workers(int main_tcp_sock_fd, double begin, double end, unsigned int workers_num, unsigned int threads_num);

struct tcp_conn_t
{
    struct sockaddr_in sock_in;
    bool received;
    bool sent;
    bool accepted;
    int sock_fd;
    calc_segm_arg_t arg;
};

typedef struct tcp_conn_t tcp_conn_t;

int main(int argc, char *argv[])
{
    if (argc != 5)
    {
        printf("usage: supervisor <workers_num> <threads_num> <begin> <end>\n");
        printf("\tset threads_num = 0 to use maximum threads at each worker\n");
        return -1;
    }
    
    long int workers_num = get_workers_num(argv);
    long int threads_num = get_threads_num(argv);
    double begin = get_segm_begin(argv);
    double end = get_segm_end(argv);

    int main_tcp_sock_fd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    
    int optval = 1;
    setsockopt(main_tcp_sock_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval));

    struct sockaddr_in main_tcp_sock_in = {0};
    
    main_tcp_sock_in.sin_addr.s_addr = htonl(INADDR_ANY);
    main_tcp_sock_in.sin_port = htons(SUPERVISOR_MAIN_TCP_PORT);
    main_tcp_sock_in.sin_family = AF_INET;
    
    bind(main_tcp_sock_fd, (struct sockaddr *)&main_tcp_sock_in, sizeof(struct sockaddr_in));
    
    listen(main_tcp_sock_fd, workers_num);

    printf("Looking for workers...\n");
    errno = 0;
    if (udp_broadcast() != 0)
    {
        perror(NULL);
        return -1;
    }

    printf("Result is %lg\n", serve_workers(main_tcp_sock_fd, begin, end, workers_num, threads_num));

    close(main_tcp_sock_fd);

    return 0;
}

double serve_workers(int main_tcp_sock_fd, double begin, double end, unsigned int workers_num, unsigned int threads_num)
{
    fd_set read_fds, write_fds;
    FD_ZERO(&read_fds);
    FD_ZERO(&write_fds);
    FD_SET(main_tcp_sock_fd, &read_fds);

    double result = 0.;

    long int accepted = 0;
    long int result_received = 0;

    char str_addr[INET_ADDRSTRLEN] = {0};

    socklen_t tcp_sock_in_len = sizeof(struct sockaddr_in);
    struct tcp_conn_t *workers = (struct tcp_conn_t *)calloc(workers_num, sizeof(tcp_conn_t));
    
    while(1)
    {
        select(FD_SETSIZE, &read_fds, &write_fds, NULL, NULL);

        if (FD_ISSET(main_tcp_sock_fd, &read_fds))
        {
            if (accepted == workers_num)
            {
                printf("Excess worker tried to connect. Connection will be refused.\n");
                int sock_fd = accept(main_tcp_sock_fd, NULL, NULL);
                close(sock_fd);
            }
            else 
            {
                errno = 0;
                workers[accepted].sock_fd = accept(main_tcp_sock_fd, (struct sockaddr *)&workers[accepted].sock_in, &tcp_sock_in_len);
                if (workers[accepted].sock_fd == -1)
                {
                    perror(NULL);
                    exit(-1);
                }
                                
                inet_ntop(AF_INET, &(workers[accepted].sock_in.sin_addr), str_addr, INET_ADDRSTRLEN);
                printf("Worker at %s:%d connected.\n", str_addr, ntohs(workers[accepted].sock_in.sin_port));
                
                workers[accepted].accepted = true;
                workers[accepted].sent     = false;
                workers[accepted].received = false;
                workers[accepted].arg = (calc_segm_arg_t){ 
                    .begin = begin, 
                    .end = end, 
                    .i = accepted, 
                    .workers_num = workers_num,
                    .threads_num = threads_num
                };
                ++accepted;
            }
        }

        for (unsigned int i = 0; i < workers_num; ++i)
        {
            if (FD_ISSET(workers[i].sock_fd, &write_fds))
            {
                if (send(workers[i].sock_fd, &workers[i].arg, sizeof(calc_segm_arg_t), 0) != sizeof(calc_segm_arg_t))
                {
                    printf("Error during sending args.\n");
                    exit(-1);
                }
                workers[i].sent = true;
            }
            else if (FD_ISSET(workers[i].sock_fd, &read_fds))
            {
                double worker_result = 0.;
                if (recv(workers[i].sock_fd, &worker_result, sizeof(double), 0) != sizeof(double))
                {
                    printf("Error during receiving result.\n");
                    exit(-1);
                }
                workers[i].received = true;
                result += worker_result;
                ++result_received;
                close(workers[i].sock_fd);  
            }
        }

        if (result_received == workers_num)
            break;
        
        FD_ZERO(&write_fds);
        FD_ZERO(&read_fds);
        
        for (unsigned int i = 0; i < workers_num; ++i)
        {
            if (!workers[i].sent && workers[i].accepted)
                FD_SET(workers[i].sock_fd, &write_fds);
    
            if (!workers[i].received && workers[i].accepted)
                FD_SET(workers[i].sock_fd, &read_fds);
        }
            
        if (accepted !=  workers_num)
            FD_SET(main_tcp_sock_fd, &read_fds);
    }

    free(workers);

    return result;
}


int udp_broadcast()
{
    int sock_fd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock_fd == -1)
        return -1;

    int optval = 1;
    if (setsockopt(sock_fd, SOL_SOCKET, SO_BROADCAST, &optval, sizeof(optval)) != 0)
        return -1;
    if (setsockopt(sock_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval)) != 0)
        return -1;
    
    struct sockaddr_in sock_in = {0};
    
    sock_in.sin_addr.s_addr = htonl(INADDR_BROADCAST);
    sock_in.sin_port = htons(WORKER_UDP_PORT);
    sock_in.sin_family = AF_INET;

    char buffer[1] = "";
    if (sendto(sock_fd, buffer, 1, 0, (struct sockaddr *)&sock_in, sizeof(struct sockaddr_in)) != 1)
        return -1;

    close(sock_fd);
    
    return 0;
}

double get_segm_end(char *argv[])
{
    char *endptr;
    errno = 0;
    double end = strtod(argv[4], &endptr);
    if (errno == ERANGE)
    {
        printf("End of segment is out of range.\n");
        exit(-1);
    }
    else if (*endptr != '\0')
    {
        printf("Invalid end of segment.\n");
        exit(-1);
    }

    return end;    
}

double get_segm_begin(char *argv[])
{
    char *endptr;
    errno = 0;
    double begin = strtod(argv[3], &endptr);
    if (errno == ERANGE)
    {
        printf("Begin of segment is out of range.\n");
        exit(-1);
    }
    else if (*endptr != '\0')
    {
        printf("Invalid begin of segment.\n");
        exit(-1);
    }

    return begin;    
}

long int get_threads_num(char *argv[])
{
    char *endptr;
    errno = 0;
    long int threads_num = strtol(argv[2], &endptr, 10);
    if (errno == ERANGE)
    {
        printf("Number of threads is out of range.\n");
        exit(-1);
    }
    else if (*endptr != '\0')
    {
        printf("Invalid number of threads.\n");
        exit(-1);
    }
    else if (threads_num < 0)
    {
        printf("Negative number of threads.\n");
        exit(-1);
    }
    else if (threads_num == 0)
    {
        printf("Workers will choose theirs own maximum number of threads.\n");
    }

    return threads_num;      
}   

long int get_workers_num(char *argv[])
{
    char *endptr;
    errno = 0;
    long int workers_num = strtol(argv[1], &endptr, 10);
    if (errno == ERANGE)
    {
        printf("Number of workers is out of range.\n");
        exit(-1);
    }
    else if (*endptr != '\0')
    {
        printf("Invalid number of workers.\n");
        exit(-1);
    }
    else if (workers_num <= 0)
    {
        printf("Nonpositive number of workers.\n");
        exit(-1);
    }

    return workers_num;      
}   
