#include <mpi.h>

#define CORRECT_OR_FINALIZE(st, msg)\
    if (st)                         \
    {                               \
        if (rank == 0)              \
            fprintf(stderr, msg);   \
        MPI_Finalize();             \
        return 1;                   \
    }

typedef struct
{
    int size_sqrt;
    int rank;
    int size;
    int coords[2];
    MPI_Comm comm;
    MPI_Comm rows_comm;
    MPI_Comm cols_comm;
} grid_info_t;

long int get_matrix_size(char *argv[]);

void fox(grid_info_t *grid_info, double *sub_A, double *sub_B, double *sub_C, double *temp, int matrix_size, int sub_matrix_size);

void splitMatrix(double *matrix, int matrix_size, int sub_matrix_size, double *sub_matrix, grid_info_t *grid_info);
void joinMatrix(double *matrix, int matrix_size, int sub_matrix_size, double *sub_matrix, grid_info_t *grid_info, double *temp_matrix);

void printMatrix(double *matrix, int matrix_size); 
void multMatrix(double *matrix0, double *matrix1, double *product, int matrix_size);
void initRandomMatrix(double *matrix, int matrix_size);
