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
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <pthread.h>

#include "common.h"

extern int errno;

int workers_udp_broadcast();
long int get_workers_num(int argc, char *argv[]);

int main(int argc, char *argv[])
{
    long int workers_num = get_workers_num(argc, argv);
    if (workers_num < 0)
        return -1;

    int main_tcp_sock_fd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    
    int optval = 1;
    struct sockaddr_in main_tcp_sock_in = {0};
    setsockopt(main_tcp_sock_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval));

    main_tcp_sock_in.sin_addr.s_addr = htonl(INADDR_ANY);
    main_tcp_sock_in.sin_port = htons(SUPERVISOR_MAIN_TCP_PORT);
    main_tcp_sock_in.sin_family = AF_INET;
    
    bind(main_tcp_sock_fd, (struct sockaddr *)&main_tcp_sock_in, sizeof(struct sockaddr_in));
    
    listen(main_tcp_sock_fd, workers_num);

    printf("Looking for workers...\n");
    errno = 0;
    if (workers_udp_broadcast() != 0)
    {
        perror(NULL);
        return -1;
    }
 
    socklen_t tcp_sock_in_len;
    char str_addr[INET_ADDRSTRLEN] = {0};
    struct sockaddr_in *tcp_sock_in = (struct sockaddr_in *)calloc(workers_num, sizeof(struct sockaddr_in));
    for (unsigned int i = 0; i < workers_num; ++i)
    {
        int tcp_sock_fd = accept(main_tcp_sock_fd, (struct sockaddr *)&tcp_sock_in[i], &tcp_sock_in_len);
        inet_ntop(AF_INET, &(tcp_sock_in[i].sin_addr), str_addr, INET_ADDRSTRLEN);
        printf("Worker found at %s:%d\n", str_addr, ntohs(tcp_sock_in[i].sin_port));
        sleep(20);
        close(tcp_sock_fd);
    }

    free(tcp_sock_in);
    close(main_tcp_sock_fd);

    return 0;
}

int workers_udp_broadcast()
{
    int sock_fd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock_fd < 0)
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

    char buffer[] = "HELLO FROM SUPERVISOR";
    if (sendto(sock_fd, buffer, strlen(buffer) + 1, 0, (struct sockaddr *)&sock_in, sizeof(struct sockaddr_in)) < 0)
        return -1;

    close(sock_fd);
    
    return 0;
}

long int get_workers_num(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: supervisor [number of workers]\n");
        return -1;
    }

    char *endptr;
    long int workers_num = strtol(argv[1], &endptr, 10);
    if (errno == ERANGE)
    {
        printf("Number of workers is out of range.\n");
        return -1;
    }
    else if (*endptr != '\0')
    {
        printf("Invalid number of workers.\n");
        return -1;
    }
    else if (workers_num <= 0)
    {
        printf("Nonpositive number of workers.\n");
        return -1;
    }

    return workers_num;      
}   
