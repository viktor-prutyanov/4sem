/**
*   Priority queue
*
*   @date 02.2016
*
*   @copyright GNU GPL v2.0
*
*   @author Viktor Prutyanov mailto:viktor.prutyanov@phystech.edu 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int HeapqData_t;

typedef int HeapqKey_t;

typedef struct HeapqPair_t
{
    HeapqData_t data;
    HeapqKey_t key;    
} HeapqPair_t;

typedef struct Heapq_t
{
    HeapqPair_t *array;
    size_t size;
    size_t real_size;
    size_t capacity;
} Heapq_t;

int Heapq_print(Heapq_t *heapq);

int Heapq_ctor(Heapq_t *heapq, size_t capacity);

int Heapq_dtor(Heapq_t *heapq);

int Heapq_insert(Heapq_t *heapq, HeapqData_t data, HeapqKey_t key);

int Heapq_increase_key(Heapq_t *heapq, HeapqKey_t key, size_t i);

int Heapq_max(Heapq_t *heapq, HeapqData_t *data);

int Heapq_extract_max(Heapq_t *heapq, HeapqData_t *data);

int Heapq_resize(Heapq_t *heapq, size_t new_capacity);

int Heapq_ok(Heapq_t *heapq);

int Heapq_heapify(Heapq_t *heapq, size_t i);