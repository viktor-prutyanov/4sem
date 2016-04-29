/**
 *  Parallel Simpson method
 *
 *  @file psm.cpp
 *  
 *  @date 04.2016
 * 
 *  @copyright GNU GPL v2.0
 * 
 *  @author Viktor Prutyanov mailto:viktor.prutyanov@phystech.edu 
 *  
 */
 
#include <iostream>
#include <thread>
#include <vector>
#include <cerrno>
#include <cstdlib>
#include <cmath>

#define FUNC(x) (sin(x))
#define DELTA (0.00001)
#define SEGM_BEGIN (-1000.0)
#define SEGM_END (1000.0)

extern int errno;

typedef struct
{
    double result;
    unsigned int threadsNum;
    unsigned int i;
} calcArg;

void calcSegmApproxInc(calcArg *arg);

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        std::cout << "Usage: psm [number of threads]\n";
        return 1;
    }

    char *endptr;
    long int threadsNum = strtol(argv[1], &endptr, 10);
    if (errno == ERANGE)
    {
        std::cout << "Number of threads is out of range.\n";
        return 1;
    }
    else if (*endptr != '\0')
    {
        std::cout << "Invalid number of threads.\n";
        return 1;
    }
    else if (threadsNum <= 0)
    {
        std::cout << "Nonpositive number of threads.\n";
        return 1;
    }

    std::vector<std::thread> threads;
    std::vector<calcArg> args;

    for (unsigned int i = 0; i < threadsNum; ++i)
    {
        calcArg arg;
        arg.i = i;
        arg.threadsNum = threadsNum;
        args.push_back(arg);
    }
    
    for (unsigned int i = 0; i < threadsNum; ++i)
        threads.push_back(std::thread(calcSegmApproxInc, &args[i]));

    for (auto& thread : threads) 
        thread.join();

    double total = 0.; 
    for (unsigned int i = 0; i < threadsNum; ++i)
        total += args[i].result;

    std::cout << "Result is " << total << std::endl;

    return 0;
}

void calcSegmApproxInc(calcArg *arg)
{      
    double subsegmLen = (SEGM_END - SEGM_BEGIN) / arg->threadsNum;
    unsigned long int subsubsegmNum = (unsigned long int)floor(subsegmLen / DELTA);
    double subsegmBegin = SEGM_BEGIN + arg->i * subsegmLen;
    double calcBegin = subsegmBegin;
    double result = 0.;

    for (unsigned long int i = 0; i < subsubsegmNum; ++i)
    {
        result += DELTA * (FUNC(calcBegin) + 4 * FUNC(calcBegin + DELTA / 2) + FUNC(calcBegin + DELTA)) / 6;
        calcBegin += DELTA;
    }

    double lastDelta = subsegmBegin + subsegmLen - calcBegin;
    arg->result = result + lastDelta * (FUNC(calcBegin) + 4 * FUNC(calcBegin + lastDelta / 2) + FUNC(calcBegin + lastDelta)) / 6;
}
