#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi/mpi.h>


/*
int MPI_BinomialBcast(void *buffer, int count, MPI_Datatype datatype,int root, MPI_Comm comm){

    int numprocs,rank, np;
    int *n = buffer;
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    for(int i = 1; i < numprocs; i ++){
        int ph = (rank - pow(2, i +1));
        if (rank < pow(2,i - 1)){
            np = rank + pow(2,i + 1);
            if(np < numprocs){
                MPI_Send(n,1,MPI_INT,np,0,MPI_COMM_WORLD);
            }
            
        } else if((0 <= ph) && (ph < pow(2,i - 1))  ){
            MPI_Recv(n, 1, MPI_INT, ph, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        }   
    }
}
*/

int MPI_BinomialBcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm){

    int numprocs,rank;
    MPI_Comm_size(comm,&numprocs);
    MPI_Comm_rank(comm,&rank);

    // Bucle for que pasa por todos los procesos
    for (int i = 1; i < numprocs; i*=2){
        
        if(rank < i && (rank + i < numprocs)){ // solo se puede enviar pa'lante
            MPI_Send(buffer, count, datatype, rank + i , 0, comm);
        }else if(rank >= i && rank < i << 1){ // solo se puede recibir pa'tras
            MPI_Recv(buffer, count, datatype, rank - i, 0, comm, MPI_STATUS_IGNORE);
        }
    }

    return MPI_SUCCESS;
}

int MPI_FlattreeColectiva(void *buffer,void *recvbuffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm){

    int numprocs,rank;
    int n, totalCount;
    MPI_Comm_size(comm,&numprocs);
    MPI_Comm_rank(comm,&rank);

    if(rank == root){
        totalCount = *(int *)buffer;
        for( int i = 1; i < numprocs ; i ++){
            MPI_Recv(&n, count, datatype, MPI_ANY_SOURCE, 0, comm, MPI_STATUS_IGNORE);
            totalCount += n;
        }   
        *(int*)recvbuffer = totalCount;
        
    }else{
        MPI_Send(buffer, 1, datatype, root , 0, comm);
    }
    
    return MPI_SUCCESS;
}

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
        }

        MPI_BinomialBcast(&n,1,MPI_INT,0,MPI_COMM_WORLD);        

        if (n == 0) break; //0 quits

        count = 0;  

        for (i = rank; i < n; i+=numprocs) {
            // Get the random numbers between 0 and 1
	        x = ((double) rand()) / ((double) RAND_MAX);
	        y = ((double) rand()) / ((double) RAND_MAX);

	        // Calculate the square root of the squares
	        z = sqrt((x*x)+(y*y));

	        // Check whether z is within the circle
	        if(z <= 1.0)
                count++;
        }
        
        local_count = count;
        MPI_FlattreeColectiva(&local_count,&count,1,MPI_INT,0,MPI_COMM_WORLD);

        if(rank == 0){
            pi = ((double) count/(double) n)*4.0;
        
            printf("pi is approx. %.16f, Error is %.16f\n", pi, fabs(pi - PI25DT));
        }
    }

    MPI_Finalize();
    return 0;
}