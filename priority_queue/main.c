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

int main()
{
    Heapq_t *heapq = (Heapq_t *)calloc(1, sizeof(Heapq_t));
    printf("(%d)\n", Heapq_ctor(NULL, 1));
    printf("(%d)\n", Heapq_ctor(heapq, 0));
    printf("(%d)\n", Heapq_ctor(heapq, 6));
    printf("(%d)\n", Heapq_print(heapq));
    printf("(%d)\n", Heapq_insert(heapq, 10, 10));
    printf("(%d)\n", Heapq_insert(heapq, 9, 9));
    printf("(%d)\n", Heapq_insert(heapq, 8, 8));
    printf("(%d)\n", Heapq_insert(heapq, 7, 7));
    printf("(%d)\n", Heapq_insert(heapq, 6, 6));
    printf("(%d)\n", Heapq_insert(heapq, 5, 5));
    printf("(%d)\n", Heapq_insert(heapq, 6, 6));
    printf("(%d)\n", Heapq_insert(heapq, 4, 4));
    printf("(%d)\n", Heapq_insert(heapq, 3, 3));
    printf("(%d)\n", Heapq_insert(heapq, 3, 3));
    printf("(%d)\n", Heapq_insert(heapq, 2, 2));
    printf("(%d)\n", Heapq_insert(heapq, 1, 1));
    printf("(%d)\n", Heapq_insert(heapq, 10, 10));
    printf("(%d)\n", Heapq_print(heapq));

    Heapq_iterator_t it;
    HeapqData_t dt;
    printf("(%d)\n", Heapq_iterator_fetch(&it, heapq));
    do
    {
        Heapq_iterator_get_data(it, &dt);
        printf("{%d}\n", dt);
    }
    while(Heapq_iterator_move_next(&it) == 0);

    while (heapq->size != 0)
    {
        printf("(%d) ", Heapq_extract_max(heapq, &dt));
        printf("%d extracted\n", dt);
    }

    printf("(%d)\n", Heapq_iterator_get_data(it, NULL));
    
    printf("(%d)\n", Heapq_dtor(heapq));
    free(heapq);

    printf("(%d)\n", Heapq_dtor(NULL));
    heapq = (Heapq_t *)calloc(1, sizeof(Heapq_t));
    printf("(%d)\n", Heapq_ctor(heapq, 1));
    printf("(%d)\n", Heapq_iterator_fetch(NULL, heapq));
    printf("(%d)\n", Heapq_iterator_fetch(NULL, NULL));
    it.heapq = NULL;
    printf("(%d)\n", Heapq_iterator_move_next(&it));
    printf("(%d)\n", Heapq_iterator_move_next(NULL));
    printf("(%d)\n", Heapq_iterator_get_data(it, NULL));
    printf("(%d)\n", Heapq_extract_max(heapq, NULL));
    printf("(%d)\n", Heapq_insert(heapq, 7, 7));
    printf("(%d)\n", Heapq_max(heapq, NULL));
    printf("(%d)\n", Heapq_resize(NULL, 10));
    printf("(%d)\n", Heapq_resize(heapq, 0));
    printf("(%d)\n", Heapq_insert(NULL, (HeapqData_t)0, 1));
    printf("(%d)\n", Heapq_insert(heapq, (HeapqData_t)0, 0));
    printf("(%d)\n", Heapq_increase_key(NULL, 0, 0));
    printf("(%d)\n", Heapq_increase_key(heapq, 0, 100));
    printf("(%d)\n", Heapq_increase_key(heapq, 0, 0));
    printf("(%d)\n", Heapq_heapify(NULL, 0));
    printf("(%d)\n", Heapq_extract_max(NULL, NULL));
    heapq->capacity = 5;
    heapq->real_size = 7;
    printf("(%d)\n", Heapq_ok(heapq));
    heapq->capacity = 0;
    printf("(%d)\n", Heapq_ok(heapq));
    free(heapq->array);
    heapq->array = NULL;
    printf("(%d)\n", Heapq_ok(heapq));
    printf("(%d)\n", Heapq_ok(NULL));
    printf("(%d)\n", Heapq_dtor(heapq));
    printf("(%d)\n", Heapq_print(heapq));
    printf("(%d)\n", Heapq_max(NULL, &dt));
    free(heapq);

    return 0;
}
