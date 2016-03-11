/**
*   Priority queue
*
*   @date 02.2016
*
*   @copyright GNU GPL v2.0
*
*   @author Viktor Prutyanov mailto:viktor.prutyanov@phystech.edu 
*/

#include "heapq.h"

#define PARENT(i) ((i - 1) / 2)
#define LEFT(i) (2 * i + 1)
#define RIGHT(i) (2 * i + 2)   


int Heapq_ctor(Heapq_t *heapq, size_t capacity)
{
    if (heapq == NULL)
        return -1;
    if (capacity == 0)
        return -1;

    heapq->size = 0;
    heapq->capacity = capacity;
    
    heapq->array = (HeapqPair_t *)calloc(capacity, sizeof(HeapqPair_t));
    if (heapq->array == NULL)
        return -1;

    return 0;
}

int Heapq_dtor(Heapq_t *heapq)
{
    if (heapq == NULL)
        return -1;
    if (heapq->array == NULL)
        return -1;

    heapq->size = 0;
    heapq->capacity = 0;

    free(heapq->array);

    return 0;
}

int Heapq_ok(Heapq_t *heapq)
{
    if (heapq == NULL)
        return -1;
    if (heapq->array == NULL)
        return -1;
    if (heapq->capacity == 0)
        return -1;
    if (heapq->size > heapq->capacity)
        return -1;

    return 0;
}

int Heapq_print(Heapq_t *heapq)
{
    if (Heapq_ok(heapq) != 0)
    {
        printf("Heapq at 0x%p is NOT OK, size = %lu, capacity = %lu\n", heapq, heapq->size, heapq->capacity);
        return -1;
    }

    printf("array at 0x%p:\n\t[ ", heapq->array);
    for (int i = 0; i < heapq->size; ++i)
    {
        printf("{%d} ", heapq->array[i].key);
    }
    printf("| ");
    for (int i = heapq->size; i < heapq->capacity; ++i)
    {
        printf("{%d} ", heapq->array[i].key);
    }
    printf("]\n");

    printf("Heapq at 0x%p, size = %lu, capacity = %lu\n", heapq, heapq->size, heapq->capacity);
    for (size_t i = 0; i < heapq->capacity; ++i)
    {
        printf("\t[%lu] data = %d, key = %d\n", i, heapq->array[i].data, heapq->array[i].key);
        if (i == 0)
            printf("\t\t Root, left at [%lu], right at [%lu]\n", LEFT(i), RIGHT(i));
        else
            printf("\t\t Parent at [%lu], left at [%lu], right at [%lu]\n", PARENT(i), LEFT(i), RIGHT(i));
    }

    return 0;
}

int Heapq_max(Heapq_t *heapq, HeapqData_t *data)
{
    if (Heapq_ok(heapq) != 0)
        return -1;
    if (data == NULL)
        return -1;

    *data = heapq->array[0].data;

    return 0;
}

int Heapq_increase_key(Heapq_t *heapq, HeapqKey_t new_key, size_t i)
{
    if (Heapq_ok(heapq) != 0)
        return -1;
    if (i >= heapq->capacity)
        return -1;
    if (new_key < heapq->array[i].key)
        return -1;

    heapq->array[i].key = new_key;

    HeapqPair_t swap;
    size_t j = i;
    while ((j > 0) && (heapq->array[PARENT(j)].key < heapq->array[j].key))
    {
        swap = heapq->array[j];
        heapq->array[j] = heapq->array[PARENT(j)];
        heapq->array[PARENT(j)] = swap;

        j = PARENT(j);
    }
    return 0;    
}

int Heapq_insert(Heapq_t *heapq, HeapqData_t data, HeapqKey_t key)
{
    if (key == 0)
        return 1;
    if (Heapq_ok(heapq) != 0)
        return -1;

    if (heapq->size == heapq->capacity)
    {
        if (Heapq_resize(heapq, heapq->capacity * 2) != 0)
        {
            return -1;
        }
    }

    ++heapq->size;
    heapq->array[heapq->size - 1].data = data;
    heapq->array[heapq->size - 1].key = 0;

    return Heapq_increase_key(heapq, key, heapq->size - 1);
}

int Heapq_resize(Heapq_t *heapq, size_t new_capacity)
{
    if (Heapq_ok(heapq) != 0)
        return -1;
    if (new_capacity <= heapq->capacity)
        return 1;

    HeapqPair_t *new_array = (HeapqPair_t *)realloc(heapq->array, new_capacity * sizeof(HeapqPair_t));

    if (new_array == NULL)
        return 1;
    else
    {
        memset(new_array + heapq->capacity, 0, new_capacity - heapq->capacity);
        heapq->array = new_array;
        heapq->capacity = new_capacity; 
        return 0;
    }
}

int Heapq_heapify(Heapq_t *heapq, size_t i)
{
    if (Heapq_ok(heapq) != 0)
        return -1;

    size_t largest = 0;
    if ((LEFT(i) < heapq->size) && (heapq->array[LEFT(i)].key > heapq->array[i].key))
        largest = LEFT(i);
    else
        largest = i;
    if ((RIGHT(i) < heapq->size) && (heapq->array[RIGHT(i)].key > heapq->array[largest].key))
        largest = RIGHT(i); 
    if (largest != i)
    {
        HeapqPair_t swap = heapq->array[i];
        heapq->array[i] = heapq->array[largest];
        heapq->array[largest] = swap;
        if (Heapq_heapify(heapq, largest) != 0)
            return -1;
    }

    return 0;
}

int Heapq_extract_max(Heapq_t *heapq, HeapqData_t *data)
{
    if (Heapq_max(heapq, data) != 0)
        return -1;
    
    heapq->array[0].key = 0;
    heapq->array[0].data = 0;

    if (Heapq_heapify(heapq, 0) != 0)
        return -1;
    --heapq->size;

    return 0;
}
