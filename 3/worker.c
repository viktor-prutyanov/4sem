/**
 *  Distributed Simpson method
 *
 *  @file worker.c
 *  
 *  @date 04.2016
 * 
 *  @copyright GNU GPL v2.0
 * 
 *  @author Viktor Prutyanov mailto:viktor.prutyanov@phystech.edu 
 *  
 */

#define _GNU_SOURCE

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
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <math.h>
#include <pthread.h>

#include "common.h"

#define FUNC(x) (sin(x))
#define DELTA (0.00001)

struct calc_subsegm_arg_t
{
    double begin;
    double end;
    double result;
    unsigned int threads_num;
    unsigned int i;
};

typedef struct calc_subsegm_arg_t calc_subsegm_arg_t;

int tcp_serve(struct sockaddr_in *sock_in);
int udp_receive(struct sockaddr_in *sock_in, socklen_t *sock_len);
inline double calc_segm(calc_segm_arg_t arg);
void *calc_subsegm(void *arguments);

int main(int argc, char *argv[])
{
    socklen_t udp_sock_in_len = sizeof(struct sockaddr_in);
    struct sockaddr_in udp_sock_in = {0};

    printf("Waiting for supervisors' requests...\n");
    errno = 0;
    if (udp_receive(&udp_sock_in, &udp_sock_in_len) != 0)
    {
        perror(NULL);
        return -1;
    }

    char str_addr[INET_ADDRSTRLEN] = {0};
    inet_ntop(AF_INET, &(udp_sock_in.sin_addr.s_addr), str_addr, INET_ADDRSTRLEN);
    printf("Received broadcast request from supervisor at %s:%d\n", str_addr, ntohs(udp_sock_in.sin_port));
    
    struct sockaddr_in tcp_sock_in = {0};

    tcp_sock_in.sin_addr.s_addr = udp_sock_in.sin_addr.s_addr;
    tcp_sock_in.sin_port = htons(SUPERVISOR_MAIN_TCP_PORT);
    tcp_sock_in.sin_family = AF_INET;

    printf("Connecting to supevisor...\n");
    errno = 0;
    if (tcp_serve(&tcp_sock_in) != 0)
    {
        perror(NULL);
        return -1;
    }
   
    return 0;
}

int tcp_serve(struct sockaddr_in *sock_in)
{
    int sock_fd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sock_fd < 0)
        return -1;

    int optval = 1;
    int keepidle = 5;
    int keepintvl = 1;
    int keepcnt = 5;
    
    setsockopt(sock_fd, SOL_SOCKET, SO_KEEPALIVE, &optval, sizeof(optval));
    setsockopt(sock_fd, SOL_TCP, TCP_KEEPIDLE, &keepidle, sizeof(keepidle));
    setsockopt(sock_fd, SOL_TCP, TCP_KEEPINTVL, &keepintvl, sizeof(keepintvl));
    setsockopt(sock_fd, SOL_TCP, TCP_KEEPCNT, &keepcnt, sizeof(keepcnt));

    if (connect(sock_fd, (struct sockaddr *)sock_in, sizeof(struct sockaddr_in)) != 0)
        return -1;
    
    calc_segm_arg_t arg;

    if (recv(sock_fd, &arg, sizeof(struct calc_segm_arg_t), 0) != sizeof(struct calc_segm_arg_t))
    {
        printf("Failed to receive args.\n");
        exit(-1);
    }

    double result = calc_segm(arg);

    if (send(sock_fd, &result, sizeof(double), 0) != sizeof(double))
        return -1;

    close(sock_fd);

    return 0;
}

int udp_receive(struct sockaddr_in *sock_in, socklen_t *sock_in_len)
{
    int sock_fd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock_fd < 0)
        return -1;
    
    int optval = 1;
    if (setsockopt(sock_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval)) != 0)
        return -1;
    
    sock_in->sin_addr.s_addr = htonl(INADDR_ANY);
    sock_in->sin_port = htons(WORKER_UDP_PORT);
    sock_in->sin_family = AF_INET;
    
    if (bind(sock_fd, (struct sockaddr *)sock_in, sizeof(struct sockaddr_in)) != 0)
        return -1;

    sock_in->sin_addr.s_addr = htonl(INADDR_ANY);
    sock_in->sin_port = htons(0);
    sock_in->sin_family = AF_INET;

    char buffer[1] = "";
    if (recvfrom(sock_fd, buffer, 1, 0, (struct sockaddr *)sock_in, sock_in_len) < 0)
        return -1;

    close(sock_fd);

    return 0;
}

inline double calc_segm(struct calc_segm_arg_t arg)
{
    double segm_len = (arg.end - arg.begin) / arg.workers_num;
    double segm_begin = arg.begin + arg.i * segm_len;
    double segm_end = segm_begin + segm_len;

    unsigned int threads_num = ((arg.threads_num > 0) ? arg.threads_num : sysconf(_SC_NPROCESSORS_ONLN));

    pthread_t *threads = (pthread_t *)malloc(threads_num * sizeof(pthread_t));
    calc_subsegm_arg_t *args = (calc_subsegm_arg_t *)malloc(threads_num * sizeof(struct calc_subsegm_arg_t));
    
    for (unsigned int i = 0; i < threads_num; ++i)
        args[i] = (calc_subsegm_arg_t){ 
            .begin = segm_begin,
            .end = segm_end,
            .i = i,
            .threads_num = threads_num
        };
    
    for (unsigned int i = 0; i < threads_num; ++i)
        pthread_create(&threads[i], NULL, calc_subsegm, (void *)(args + i)); 

    for (unsigned int i = 0; i < threads_num; ++i)
        pthread_join(threads[i], NULL);

    double total = 0.; 
    for (unsigned int i = 0; i < threads_num; ++i)
        total += args[i].result;

    free(threads);
    free(args);

    return total;
}

void *calc_subsegm(void *arguments)
{      
    calc_subsegm_arg_t *arg = (calc_subsegm_arg_t *)arguments;

    double subsegm_len = (arg->end - arg->begin) / arg->threads_num;
    unsigned long int subsubsegm_num = (unsigned long int)floor(subsegm_len / DELTA);
    double subsegm_begin = arg->begin + arg->i * subsegm_len;
    double calc_begin = subsegm_begin;
    double result = 0.;

    for (unsigned long int i = 0; i < subsubsegm_num; ++i)
    {
        result += DELTA * (FUNC(calc_begin) + 4 * FUNC(calc_begin + DELTA / 2) + FUNC(calc_begin + DELTA)) / 6;
        calc_begin += DELTA;
    }

    double last_delta = subsegm_begin + subsegm_len - calc_begin;
    arg->result = result + last_delta * (FUNC(calc_begin) + 4 * FUNC(calc_begin + last_delta / 2) + FUNC(calc_begin + last_delta)) / 6;

    return NULL;
}
