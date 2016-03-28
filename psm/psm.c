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

#include <sched.h>

double total = 0.;

pthread_mutex_t mutex;

void *calc_segm_approx_inc(void *arguments)
{      
    calc_arg_t *arg = (calc_arg_t *)arguments;

    unsigned long int segm_num = (unsigned long int)floor(arg->len / DELTA);

    double calc_res = 0.;
    double calc_begin = arg->begin;

    for (unsigned long int i = 0; i < segm_num; ++i)
    {
        double interm_res = DELTA * (FUNC(calc_begin) + 4 * FUNC(calc_begin + DELTA / 2) + FUNC(calc_begin + DELTA)) / 6;
        calc_res += interm_res;
        calc_begin += DELTA;
    }

    double last_delta = arg->begin + arg->len - calc_begin;
    calc_res += last_delta * (FUNC(calc_begin) + 4 * FUNC(calc_begin + last_delta / 2) + FUNC(calc_begin + last_delta)) / 6;
    
    pthread_mutex_lock(&mutex);
    total += calc_res;
    pthread_mutex_unlock(&mutex);

    return NULL;
}

double calc_segm(double begin, double end, unsigned int num)
{
    double segm_len = (end - begin) / num;

    pthread_t threads[num];
    calc_arg_t args[num];

    for (unsigned int i = 0; i < num; ++i)
    {
        args[i].begin = begin + segm_len * i;
        args[i].len = segm_len;
    }

    pthread_mutex_init(&mutex, NULL);

    //struct sched_param param = {.sched_priority = 99};

    for (unsigned int i = 0; i < num; ++i)
    {
        pthread_create(&threads[i], NULL, calc_segm_approx_inc, (void *)(args + i)); 
        //if (0 != pthread_setschedparam(threads[i], SCHED_FIFO, &param))
        //    perror(NULL);
        pthread_setschedprio(threads[i], 90);
    }

    for (unsigned int i = 0; i < num; ++i)
    {
        pthread_join(threads[i], NULL);
    }

    pthread_mutex_destroy(&mutex);

    return total;
}   
