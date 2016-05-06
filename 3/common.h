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

struct calc_segm_arg_t
{
    double begin;
    double end;
    unsigned int workers_num;
    unsigned int threads_num;
    unsigned int i;
};

typedef struct calc_segm_arg_t calc_segm_arg_t;
