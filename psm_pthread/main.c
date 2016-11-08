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
 
#define _GNU_SOURCE 

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <unistd.h>
#include <pthread.h>
#include <sched.h>

#define FUNC(x) (x*exp(x))
#define DELTA (0.00001)
#define SEGM_BEGIN (-2000.0)
#define SEGM_END (-1000.0)

extern int errno;

typedef struct
{
    double result;
    unsigned int threads_num;
    unsigned int i;
} calc_arg_t;

void *calc_segm_approx_inc(void *arguments);

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: psm [number of threads]\n");
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

    pthread_t *threads = (pthread_t *)malloc(threads_num * sizeof(pthread_t));
    calc_arg_t *args = (calc_arg_t *)malloc(threads_num * sizeof(calc_arg_t));

    for (unsigned int i = 0; i < threads_num; ++i)
        args[i] = (calc_arg_t){ .i = i, .threads_num = threads_num };
    
    for (unsigned int i = 0; i < threads_num; ++i)
        pthread_create(&threads[i], NULL, calc_segm_approx_inc, (void *)(args + i)); 

    for (unsigned int i = 0; i < threads_num; ++i)
        pthread_join(threads[i], NULL);

    double total = 0.; 
    for (unsigned int i = 0; i < threads_num; ++i)
        total += args[i].result;

    printf("Result is %lf\n", total);

    free(threads);
    free(args);

    return 0;
}

void *calc_segm_approx_inc(void *arguments)
{      
    calc_arg_t *arg = (calc_arg_t *)arguments;

    double subsegm_len = (SEGM_END - SEGM_BEGIN) / arg->threads_num;
    unsigned long int subsubsegm_num = (unsigned long int)floor(subsegm_len / DELTA);
    double subsegm_begin = SEGM_BEGIN + arg->i * subsegm_len;
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
