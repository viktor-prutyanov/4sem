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

#include "psm.h"

//Coroutines that calculates integral on its own subsegment
void *calc_segm_approx_inc(void *arguments)
{      
    calc_arg_t *arg = (calc_arg_t *)arguments;

    double subsegm_len = (arg->end - arg->begin) / arg->threads_num;
    unsigned long int subsubsegm_num = (unsigned long int)floor(subsegm_len / DELTA);
    double subsegm_begin = arg->begin + arg->i * subsegm_len;
    double calc_begin = subsegm_begin;

    for (unsigned long int i = 0; i < subsubsegm_num; ++i)
    {
        arg->result += DELTA * (FUNC(calc_begin) + 4 * FUNC(calc_begin + DELTA / 2) + FUNC(calc_begin + DELTA)) / 6;
        calc_begin += DELTA;
    }

    double last_delta = subsegm_begin + subsegm_len - calc_begin;
    arg->result += last_delta * (FUNC(calc_begin) + 4 * FUNC(calc_begin + last_delta / 2) + FUNC(calc_begin + last_delta)) / 6;

    return NULL;
}

double calc_segm(double begin, double end, unsigned int threads_num, long int cpus_num)
{   
    pthread_t threads[threads_num];
    calc_arg_t args[threads_num];

    for (unsigned int i = 0; i < threads_num; ++i)
        args[i] = (calc_arg_t){ .result = 0., .begin = begin, .end = end, .i = i, .threads_num = threads_num };
    
    cpu_set_t set;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    for (unsigned int i = 0; i < threads_num; ++i)
    {
        CPU_ZERO(&set);
        CPU_SET(i % cpus_num, &set); 
        pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &set); 
        pthread_create(&threads[i], &attr, calc_segm_approx_inc, (void *)(args + i)); 
    }

    for (unsigned int i = 0; i < threads_num; ++i)
        pthread_join(threads[i], NULL);

    double total = 0.; 
    for (unsigned int i = 0; i < threads_num; ++i)
        total += args[i].result;

    return total;
}   
