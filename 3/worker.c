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
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

#include "ports.h"

#define MAX_BUF_SIZE 256

extern int errno;

int main(int argc, char *argv[])
{
    socklen_t sock_in_len = sizeof(struct sockaddr_in);
    struct sockaddr_in sock_in;
    memset(&sock_in, 0, sock_in_len);

    int sock_fd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    
    sock_in.sin_addr.s_addr = htonl(INADDR_ANY);
    sock_in.sin_port = htons(WORKER_RCV_PORT);
    sock_in.sin_family = AF_INET;
    
   // int optval = 1;
   // errno = 0;
   // setsockopt(sock_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval));
   // perror(NULL);
    
    errno = 0;
    bind(sock_fd, (struct sockaddr *)&sock_in, sock_in_len);
    perror(NULL);

    sock_in.sin_addr.s_addr = htonl(INADDR_ANY);
    sock_in.sin_port = htons(0);
    sock_in.sin_family = AF_INET;

    char buffer[MAX_BUF_SIZE] = {0};
    errno = 0;
    recvfrom(sock_fd, buffer, MAX_BUF_SIZE, 0, (struct sockaddr *)&sock_in, &sock_in_len);
    perror(NULL);

    printf("%s", buffer);

    close(sock_fd);

    return EXIT_SUCCESS;
}
