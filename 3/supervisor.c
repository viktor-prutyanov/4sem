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

#include "ports.h"

extern int errno;

int main(int argc, char *argv[])
{
    socklen_t sock_in_len = sizeof(struct sockaddr_in);
    struct sockaddr_in sock_in;
    memset(&sock_in, 0, sock_in_len);

    int sock_fd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    
    //sock_in.sin_addr.s_addr = htonl(INADDR_ANY);
    //sock_in.sin_port = htons(0);
    //sock_in.sin_family = AF_INET;
    
    int optval = 1;
    errno = 0;
    setsockopt(sock_fd, SOL_SOCKET, SO_BROADCAST, &optval, sizeof(optval));
    perror(NULL);

    //errno = 0;
    //bind(sock_fd, (struct sockaddr *)&sock_in, sock_in_len);
    //perror(NULL);

    sock_in.sin_addr.s_addr = htonl(INADDR_BROADCAST);
    sock_in.sin_port = htons(WORKER_RCV_PORT);
    sock_in.sin_family = AF_INET;

    char buffer[] = "LOL";
    
    errno = 0;
    sendto(sock_fd, buffer, strlen(buffer) + 1, 0, (struct sockaddr *)&sock_in, sock_in_len);
    perror(NULL);

    close(sock_fd);

    return EXIT_SUCCESS;
}
