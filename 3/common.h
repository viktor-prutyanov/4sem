/**
 *  Distributed Simpson method
 *
 *  @file common.h
 *  
 *  @date 04.2016
 * 
 *  @copyright GNU GPL v2.0
 * 
 *  @author Viktor Prutyanov mailto:viktor.prutyanov@phystech.edu 
 *  
 */

#define WORKER_UDP_PORT 8001
#define SUPERVISOR_MAIN_TCP_PORT 8003

typedef struct
{
    double begin;
    double end;
    unsigned int workers_num;
    unsigned int threads_num;
    unsigned int i;
} calc_segm_arg_t;

typedef struct
{
    double result;
} calc_segm_result_t;
