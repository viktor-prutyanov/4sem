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
    printf("(%d)\n", Heapq_ctor(heapq, 6));
    printf("(%d)\n", Heapq_print(heapq));
    printf("(%d)\n", Heapq_insert(heapq, 10, 10));
    printf("(%d)\n", Heapq_insert(heapq, 9, 9));
    printf("(%d)\n", Heapq_insert(heapq, 8, 8));
    printf("(%d)\n", Heapq_insert(heapq, 7, 7));
    printf("(%d)\n", Heapq_insert(heapq, 6, 6));
    printf("(%d)\n", Heapq_insert(heapq, 5, 5));
    printf("(%d)\n", Heapq_insert(heapq, 4, 4));
    printf("(%d)\n", Heapq_insert(heapq, 3, 3));
    printf("(%d)\n", Heapq_insert(heapq, 2, 2));
    printf("(%d)\n", Heapq_insert(heapq, 1, 1));
    // printf("(%d)\n", Heapq_insert(heapq, 2, 2));
    // printf("(%d)\n", Heapq_insert(heapq, 2, 2));
    // printf("(%d)\n", Heapq_insert(heapq, 3, 3));
    // printf("(%d)\n", Heapq_insert(heapq, 4, 4));
    // printf("(%d)\n", Heapq_insert(heapq, 5, 5));
    // printf("(%d)\n", Heapq_insert(heapq, 6, 6));
    // printf("(%d)\n", Heapq_insert(heapq, 7, 7));
    // printf("(%d)\n", Heapq_insert(heapq, 8, 8));
    // printf("(%d)\n", Heapq_insert(heapq, 9, 9));
    // printf("(%d)\n", Heapq_insert(heapq, 10, 10));
    // printf("(%d)\n", Heapq_insert(heapq, 11, 11));
    // printf("(%d)\n", Heapq_insert(heapq, 7, 7));
    // printf("(%d)\n", Heapq_insert(heapq, 10, 10));
    // printf("(%d)\n", Heapq_insert(heapq, 12, 12));
    printf("(%d)\n", Heapq_print(heapq));
    HeapqData_t dt;
    while (heapq->size != 0)
    {
        printf("(%d)\n", Heapq_extract_max(heapq, &dt));
        printf("%d extracted\n", dt);
        printf("(%d)\n", Heapq_print(heapq));
        printf("------------------------\n");
    }
    // printf("(%d)\n", Heapq_print(heapq));
    // printf("(%d)\n", Heapq_insert(heapq, 1, 1));
    // printf("(%d)\n", Heapq_insert(heapq, 2, 2));
    // printf("(%d)\n", Heapq_insert(heapq, 3, 3));
    // printf("(%d)\n", Heapq_insert(heapq, 4, 4));
    // printf("(%d)\n", Heapq_print(heapq));
    // printf("(%d)\n", Heapq_insert(heapq, 2, 2));
    // printf("(%d)\n", Heapq_insert(heapq, 1, 1));
    // printf("(%d)\n", Heapq_print(heapq));
    // while (heapq->size != 0)
    // {
    //     printf("(%d)\n", Heapq_extract_max(heapq, &dt));
    //     printf("%d extracted\n", dt);
    //     printf("(%d)\n", Heapq_print(heapq));
    // }
    //printf("(%d)\n", Heapq_insert(heapq, 0, 0));
    printf("(%d)\n", Heapq_dtor(heapq));
    free(heapq);
    return 0;
}
