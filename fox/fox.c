#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <errno.h>

#include "fox.h"

extern int errno;

int main(int argc, char *argv[])
{
    MPI_Init(&argc, &argv);
    int size, rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);    
    int size_sqrt = (int)sqrt((double)size);

    CORRECT_OR_FINALIZE((argc != 2),
        "usage: fox <matrix_size>\n");

    int matrix_size = get_matrix_size(argv);
    int sub_matrix_size = matrix_size / size_sqrt;

    CORRECT_OR_FINALIZE(((size_sqrt * size_sqrt != size) || ((matrix_size % size_sqrt) != 0)),
        "Invalid matrix or process number.\n");

    grid_info_t grid_info = {0};
    grid_info.size = size;
    grid_info.size_sqrt = size_sqrt;
    int dims[2];
    dims[0] = dims[1] = grid_info.size_sqrt;
    const int periods[2] = {1, 1};
    MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, 1, &(grid_info.comm));
    MPI_Comm_rank(grid_info.comm, &grid_info.rank);
    MPI_Cart_coords(grid_info.comm, grid_info.rank, 2, grid_info.coords);    

    int remain_dims[2] = {0, 1};
    MPI_Cart_sub(grid_info.comm, remain_dims, &(grid_info.rows_comm));

    remain_dims[0] = 1;
    remain_dims[1] = 0;
    MPI_Cart_sub(grid_info.comm, remain_dims, &(grid_info.cols_comm));   

    // There and next C = A * B
    double *A, *B, *C;
    if (grid_info.rank == 0)
    {
        A = (double *)malloc(matrix_size * matrix_size * sizeof(double));
        B = (double *)malloc(matrix_size * matrix_size * sizeof(double));
        C = (double *)malloc(matrix_size * matrix_size * sizeof(double));
        initRandomMatrix(A, matrix_size);
        initRandomMatrix(B, matrix_size);
        printf("Matrix A is:\n");
        printMatrix(A, matrix_size);
        printf("Matrix B is:\n");
        printMatrix(B, matrix_size);
    }

    double *temp = (double *)malloc(sub_matrix_size * sub_matrix_size * sizeof(double));
    double *sub_A = (double *)malloc(sub_matrix_size * sub_matrix_size * sizeof(double));
    double *sub_B = (double *)malloc(sub_matrix_size * sub_matrix_size * sizeof(double));
    double *sub_C = (double *)malloc(sub_matrix_size * sub_matrix_size * sizeof(double));
    
    splitMatrix(A, matrix_size, sub_matrix_size, sub_A, &grid_info);
    splitMatrix(B, matrix_size, sub_matrix_size, sub_B, &grid_info);
  
    fox(&grid_info, sub_A, sub_B, sub_C, temp, matrix_size, sub_matrix_size);

    joinMatrix(C, matrix_size, sub_matrix_size, sub_C, &grid_info, temp);

    if (grid_info.rank == 0)
    {
        printf("Matrix C is:\n");
        printMatrix(C, matrix_size);
    }
    
    free(temp);
    free(sub_A);
    free(sub_B);
    free(sub_C);

    if (grid_info.rank == 0)
    {
        free(A);
        free(B);
        free(C);
    }

    MPI_Finalize();
    return 0;
}

void splitMatrix(double *matrix, int matrix_size, int sub_matrix_size, double *sub_matrix, grid_info_t *grid_info)
{
    int coords[2];
    MPI_Status status;
    
    int sub_matrix_num = grid_info->size_sqrt;
    
    if (grid_info->rank == 0) 
    {
        for (int matrix_row = 0; matrix_row < matrix_size; ++matrix_row)
        {
            int grid_row = matrix_row / sub_matrix_size;
            coords[0] = grid_row;
            for (int grid_col = 0; grid_col < sub_matrix_num; ++grid_col)
            {
                coords[1] = grid_col;
                int dest;
                MPI_Cart_rank(grid_info->comm, coords, &dest);
                if (dest == 0)
                    memcpy(sub_matrix + (matrix_row % sub_matrix_size) * sub_matrix_size, 
                        matrix + matrix_row * matrix_size + grid_col * sub_matrix_size, sub_matrix_size * sizeof(double));
                else
                {
                    MPI_Send(matrix + matrix_row * matrix_size + grid_col * sub_matrix_size,
                        sub_matrix_size, MPI_DOUBLE, dest, 0, grid_info->comm);
                }
            }
        }
    } 
    else 
        for (int sub_matrix_row = 0; sub_matrix_row < sub_matrix_size; sub_matrix_row++) 
            MPI_Recv(sub_matrix + sub_matrix_row * sub_matrix_size , sub_matrix_size, MPI_DOUBLE, 0, 0, grid_info->comm, &status);
}

void joinMatrix(double *matrix, int matrix_size, int sub_matrix_size, double *sub_matrix, grid_info_t *grid_info, double *temp_matrix)
{
    int coords[2];
    MPI_Status status;
    
    int sub_matrix_num = grid_info->size_sqrt;

    if (grid_info->rank == 0)
    {
        for (int grid_col = 0; grid_col < sub_matrix_num; ++grid_col)
        {
            coords[1] = grid_col;
            for (int grid_row = 0; grid_row < sub_matrix_num; ++grid_row)
            {
                coords[0] = grid_row;
                int src;
                MPI_Cart_rank(grid_info->comm, coords, &src);
                if (src == 0)
                {
                    for (int sub_matrix_row = 0; sub_matrix_row < sub_matrix_size; ++sub_matrix_row)
                    {
                        memcpy(matrix + grid_row * matrix_size * sub_matrix_size + grid_col * sub_matrix_size + sub_matrix_row * matrix_size, sub_matrix + sub_matrix_row * sub_matrix_size, sub_matrix_size * sizeof(double));
                    }
                }
                else
                {
                    MPI_Recv(temp_matrix, sub_matrix_size * sub_matrix_size, MPI_DOUBLE, src, 0, grid_info->comm, &status);
                    for (int sub_matrix_row = 0; sub_matrix_row < sub_matrix_size; ++sub_matrix_row)
                    {
                        memcpy(matrix + grid_row * matrix_size * sub_matrix_size + grid_col * sub_matrix_size + sub_matrix_row * matrix_size, temp_matrix + sub_matrix_row * sub_matrix_size, sub_matrix_size * sizeof(double));
                    }
                }
            }
        }
    }
    else
        MPI_Send(sub_matrix, sub_matrix_size * sub_matrix_size, MPI_DOUBLE, 0, 0, grid_info->comm);
}

void fox(grid_info_t *grid_info, double *sub_A, double *sub_B, double *sub_C, double *temp, int matrix_size, int sub_matrix_size)
{
    int sub_matrix_num = grid_info->size_sqrt;
    
    int src = (grid_info->coords[0] + 1) % sub_matrix_num;
    int dst = (grid_info->coords[0] + sub_matrix_num - 1) % sub_matrix_num;

    MPI_Status status;
    
    for (int i = 0; i < sub_matrix_num; ++i)
    {
        int bcast_root = (grid_info->coords[0] + i) % sub_matrix_num;
        if (bcast_root == grid_info->coords[1])
        {
            MPI_Bcast(sub_A, sub_matrix_size * sub_matrix_size, MPI_DOUBLE, bcast_root, grid_info->rows_comm);
            multMatrix(sub_A, sub_B, sub_C, sub_matrix_size);
        }
        else
        {
            MPI_Bcast(temp, sub_matrix_size * sub_matrix_size, MPI_DOUBLE, bcast_root, grid_info->rows_comm);
            multMatrix(temp, sub_B, sub_C, sub_matrix_size);
        }
        MPI_Sendrecv_replace(sub_B, sub_matrix_size * sub_matrix_size, MPI_DOUBLE, dst, 0, src, 0, grid_info->cols_comm, &status);
    }
}

void initRandomMatrix(double *matrix, int matrix_size)
{
    for (int i = 0; i < matrix_size; ++i)
        for (int j = 0; j < matrix_size; ++j)
            matrix[i * matrix_size + j] = rand() % 10;
}

void printMatrix(double *matrix, int matrix_size)
{
    for (int i = 0; i < matrix_size; ++i)
    {
        for (int j = 0; j < matrix_size; ++j)
            printf("%6.lf ", matrix[i * matrix_size + j]);
        printf("\n");
    } 
}

void multMatrix(double *mult0, double *mult1, double *prod, int matrix_size)
{
    for (int k = 0; k < matrix_size; ++k)
        for (int i = 0; i < matrix_size; ++i)
            for (int j = 0; j < matrix_size; ++j)
                 prod[i * matrix_size + j] += mult0[i * matrix_size + k] * mult1[k * matrix_size + j];
}

long int get_matrix_size(char *argv[])
{
    char *endptr;
    errno = 0;
    long int matrix_size = strtol(argv[1], &endptr, 10);
    if (errno == ERANGE)
    {
        printf("Matrix size is out of range.\n");
        exit(1);
    }
    else if (*endptr != '\0')
    {
        printf("Invalid matrix size.\n");
        exit(1);
    }
    else if (matrix_size <= 0)
    {
        printf("Nonpositive matrix size.\n");
        exit(1);
    }

    return matrix_size;      
}   
