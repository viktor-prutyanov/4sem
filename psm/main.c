/**
 *  Parallel Simpson method
 *
 *  @file main.c
 *  
 *  @date 03.2016
 * 
 *  @copyright GNU GPL v2.0
 * 
 *  @author Viktor Prutyanov mailto:viktor.prutyanov@phystech.edu 
 *  
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <unistd.h>

#include "psm.h"

extern int errno;

int main(int argc, char *argv[])
{
    if (argc != 4)
    {
        printf("Usage: psm [number of threads] [begin] [end]\n");
        return 1;
    }

    char *endptr;
    
    long int threads_num = strtol(argv[1], &endptr, 10);
    if (errno == ERANGE)
    {
        printf("Number of threads is out of range.\n");
        return 1;
    }
    else if (*endptr != '\0')
    {
        printf("Invalid number of threads.\n");
        return 1;
    }
    else if (threads_num <= 0)
    {
        printf("Nonpositive number of threads.\n");
        return 1;
    }

    double begin = strtod(argv[2], &endptr);
    if (errno == ERANGE)
    {
        printf("Begin of segment is out of range.\n");
        return 1;
    }
    else if (*endptr != '\0')
    {
        printf("Invalid begin of segment.\n");
        return 1;
    }

    double end = strtod(argv[3], &endptr);
    if (errno == ERANGE)
    {
        printf("End of segment is out of range.\n");
        return 1;
    }
    else if (*endptr != '\0')
    {
        printf("Invalid end of segment.\n");
        return 1;
    }

    long int cpus_num = sysconf(_SC_NPROCESSORS_ONLN);

    printf("Segment is [%lf ; %lf], num of threads is %lu, num of CPUs is %lu ,f(x) = %s\n", 
            begin, end, threads_num, cpus_num, FUNC_STR); 

    printf("Result is %lf\n", calc_segm(begin, end, threads_num, cpus_num));

    return 0;
}
