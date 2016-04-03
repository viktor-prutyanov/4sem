/**
 *  Parallel Simpson method
 *
 *  @file psm.h
 *  
 *  @date 03.2016
 * 
 *  @copyright GNU GPL v2.0
 * 
 *  @author Viktor Prutyanov mailto:viktor.prutyanov@phystech.edu 
 *  
 */

#define _GNU_SOURCE 

#include <math.h>
#include <pthread.h>
#include <sched.h>

#define FUNC(x) (sin(x))
#define FUNC_STR "sin(x)" 
#define DELTA (0.0001)

typedef struct
{
    double begin;
    double end;
    double result;
    unsigned int threads_num;
    unsigned int i;
} calc_arg_t;

void *calc_segm_approx_inc(void *arguments);

double calc_segm(double begin, double end, unsigned int threads_num, long int cpus_num);
