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
    heapq->real_size = 0;
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
        return 1;

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
    if ((heapq->size <= heapq->real_size) && (heapq->real_size <= heapq->capacity))
        return 0;
    else
        return -1;
}

int Heapq_print(Heapq_t *heapq)
{
    if (Heapq_ok(heapq) != 0)
    {
        printf("Heapq at 0x%p is NOT OK, size = %lu, real_size = %lu, capacity = %lu\n", 
            heapq, heapq->size, heapq->real_size, heapq->capacity);
        return -1;
    }

    printf("array at 0x%p:\n\t[ ", heapq->array);
    for (int i = 0; i < heapq->real_size; ++i)
    {
        printf("{%d} ", heapq->array[i].key);
    }
    printf("| ");
    for (int i = heapq->real_size; i < heapq->capacity; ++i)
    {
        printf("{%d} ", heapq->array[i].key);
    }
    printf("]\n");

    printf("Heapq at 0x%p, size = %lu, real_size = %lu, capacity = %lu\n", 
        heapq, heapq->size, heapq->real_size, heapq->capacity);
    for (size_t i = 0; i < heapq->capacity; ++i)
    {
        printf("\t[%lu] data = %d, key = %d", i, heapq->array[i].data, heapq->array[i].key);
        if (i == 0)
            printf(" is root, left at [%lu], right at [%lu]\n", LEFT(i), RIGHT(i));
        else
            printf(", parent at [%lu], left at [%lu], right at [%lu]\n", PARENT(i), LEFT(i), RIGHT(i));
    }

    return 0;
}

int Heapq_max(Heapq_t *heapq, HeapqData_t *data)
{
    if (Heapq_ok(heapq) != 0)
        return -1;
    if (data == NULL)
        return 1;

    *data = heapq->array[0].data;

    return 0;
}

int Heapq_increase_key(Heapq_t *heapq, HeapqKey_t new_key, size_t i)
{
    if (Heapq_ok(heapq) != 0)
        return -1;
    if (i >= heapq->real_size)
        return 1;
    if (new_key < heapq->array[i].key)
        return 1;

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

    if (heapq->real_size == heapq->capacity)
        if (Heapq_resize(heapq, heapq->capacity * 2) != 0)
            return -1;

    heapq->array[heapq->real_size].data = data;
    heapq->array[heapq->real_size].key = 0;
    ++heapq->real_size;
    ++heapq->size;

    return Heapq_increase_key(heapq, key, heapq->real_size - 1);
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
        memset(new_array + heapq->capacity, 0, 
            (new_capacity - heapq->capacity) * sizeof(HeapqPair_t));
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
    if ((LEFT(i) < heapq->real_size) && (heapq->array[LEFT(i)].key > heapq->array[i].key))
        largest = LEFT(i);
    else
        largest = i;
    if ((RIGHT(i) < heapq->real_size) && (heapq->array[RIGHT(i)].key > heapq->array[largest].key))
        largest = RIGHT(i); 

    if (largest != i)
    {
        HeapqPair_t swap = heapq->array[i];
        heapq->array[i] = heapq->array[largest];
        heapq->array[largest] = swap;
        if (Heapq_heapify(heapq, largest) != 0)
            return -1;
    }

    if (heapq->array[heapq->real_size - 1].key == 0)
        --heapq->real_size;

    return 0;
}

int Heapq_extract_max(Heapq_t *heapq, HeapqData_t *data)
{
    if (Heapq_max(heapq, data) != 0)
        return Heapq_max(heapq, data);
    
    heapq->array[0].key = 0;
    heapq->array[0].data = 0;

    if (Heapq_heapify(heapq, 0) != 0)
        return -1;
    --heapq->size;

    return 0;
}

int Heapq_iterator_fetch(Heapq_iterator_t *iter, Heapq_t *heapq)
{
    if (Heapq_ok(heapq) != 0)
        return -1;
    if (iter == NULL)
        return 1;

    iter->heapq = heapq;
    iter->index = 0;

    return 0;
}

int Heapq_iterator_move_next(Heapq_iterator_t *iter)
{
    if (iter == NULL)
        return 1;
    if (Heapq_ok(iter->heapq) != 0)
        return -1;

    do 
    {
        if (iter->index != iter->heapq->real_size - 1)
            ++iter->index;   
        else
            return 1;
    }
    while (iter->heapq->array[iter->index].key == 0);

    return 0;
}

int Heapq_iterator_get_data(Heapq_iterator_t iter, HeapqData_t *data)
{
    if (Heapq_ok(iter.heapq) != 0)
        return -1;
    if (data == NULL)
        return 1;

    *data = iter.heapq->array[iter.index].data; 

    return 0;
}
