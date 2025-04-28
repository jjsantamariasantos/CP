#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi/mpi.h>

int main(int argc, char *argv[])
{
    int i, done = 0, n, count,local_count;
    double PI25DT = 3.141592653589793238462643;
    double pi, x, y, z;
    int numprocs,rank;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    while (!done)
    {
        if(rank == 0){
            printf("Enter the number of points: (0 quits) \n");
            scanf("%d",&n);
            for( i = 1 ; i < numprocs ; i++){
                MPI_Send(&n,1,MPI_INT,i,0,MPI_COMM_WORLD);
            }
        }else{
            MPI_Recv(&n,1,MPI_INT,0,0,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
        }
        
        if (n == 0) break; //0 quits

        local_count = 0;  

        for (i = rank; i < n; i+=numprocs) {
            // Get the random numbers between 0 and 1
	        x = ((double) rand()) / ((double) RAND_MAX);
	        y = ((double) rand()) / ((double) RAND_MAX);

	        // Calculate the square root of the squares
	        z = sqrt((x*x)+(y*y));

	        // Check whether z is within the circle
	        if(z <= 1.0)
                local_count++;
        }
        
        if(rank == 0){
            count = local_count;
            for( i = 1; i < numprocs; i++){
                MPI_Recv(&local_count,1,MPI_INT,MPI_ANY_SOURCE,0,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
                count += local_count;
            }

            pi = ((double) count/(double) n)*4.0;
        
            printf("pi is approx. %.16f, Error is %.16f\n", pi, fabs(pi - PI25DT));
        }else{
        
            MPI_Send(&local_count,1,MPI_INT,0,0,MPI_COMM_WORLD);
        }
    
    }

    MPI_Finalize();
    return 0;
}