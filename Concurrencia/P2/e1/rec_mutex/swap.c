/*
 * @file swap.c
 * @brief Programa que utiliza múltiples hilos para intercambiar elementos en un búfer compartido.
 *
 * Este programa crea múltiples hilos que intercambian posiciones aleatorias dentro de un búfer
 * compartido, protegiendo el acceso con un mutex. Se controla el número de iteraciones y retardos
 * para simular concurrencia.
 */

/*
 * Librerías necesarias para la correcta implementación del código.
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "op_count.h"
#include "options.h"
#include "rec_mutex.h"

/*
 * @struct buffer
 * @brief Estructura que representa un búfer compartido.
 *
 * @param data Puntero a un arreglo de enteros que almacena los datos.
 * @param size Tamaño del búfer.
 */
struct buffer
{
    int *data;
    int size;
};

/*
 * @struct thread_info
 * @brief Estructura que contiene la información de cada hilo creado.
 *
 * @param thread_id Identificador del hilo devuelto por pthread_create().
 * @param thread_num Número de hilo definido por la aplicación.
 */
struct thread_info
{
    pthread_t thread_id; // id returned by pthread_create()
    int thread_num;      // application defined thread #
};

/*
 * @struct args
 * @brief Estructura que almacena los parámetros de cada hilo.
 *
 * @param thread_num Número de hilo definido por la aplicación.
 * @param delay Retardo entre operaciones de intercambio.
 * @param iterations Número de iteraciones que realizará el hilo.
 * @param buffer Puntero al búfer compartido.
 * @param mutex2 Arreglo de mutex para sincronizar el acceso concurrente.
 */
struct args
{
    int thread_num; // application defined thread #
    int delay;      // delay between operations
    int iterations;
    struct buffer *buffer; // Shared buffer
    rec_mutex_t *mutex2; 
};

/*
 * @brief Función que realiza intercambios aleatorios en el búfer compartido.
 *
 * @param ptr Puntero a una estructura args con los parámetros del hilo.
 * @return NULL al finalizar la ejecución del hilo.
 */
void *swap(void *ptr)
{
    struct args *args = ptr;

    while (args ->iterations > 0)
    {
        int i, j, tmp;
        i = rand() % args->buffer->size;
        j = rand() % args->buffer->size;

        if (i < j) {
            rec_mutex_lock(&args->mutex2[i]);
            rec_mutex_lock(&args->mutex2[j]);
        } else {
            rec_mutex_lock(&args->mutex2[j]);
            rec_mutex_lock(&args->mutex2[i]);
        }
    

        printf("Thread %d swapping positions %d (== %d) and %d (== %d)\n",
            args->thread_num, i, args->buffer->data[i], j, args->buffer->data[j]);

        tmp = args->buffer->data[i];
        if (args->delay)
            usleep(args->delay); // Force a context switch

        args->buffer->data[i] = args->buffer->data[j];
        if (args->delay)
            usleep(args->delay);

        args->buffer->data[j] = tmp;
        if (args->delay)
            usleep(args->delay);
        
        args->iterations--;
        inc_count();
        
        rec_mutex_unlock(&args->mutex2[j]);
        rec_mutex_unlock(&args->mutex2[i]);
    }
    
    return NULL;
}

/*
 * @brief Función de comparación utilizada por qsort.
 *
 * @param e1 Primer elemento a comparar.
 * @param e2 Segundo elemento a comparar.
 * @return -1 si e1 < e2, 1 si e1 > e2, 0 si son iguales.
 */
int cmp(int *e1, int *e2)
{
    if (*e1 == *e2)
        return 0;
    if (*e1 < *e2)
        return -1;
    return 1;
}

/*
 * @brief Imprime el contenido del búfer en pantalla.
 *
 * @param buffer Estructura del búfer a imprimir.
 */
void print_buffer(struct buffer buffer)
{
    int i;

    for (i = 0; i < buffer.size; i++)
        printf("%i ", buffer.data[i]);
    printf("\n");
}

/*
 * @brief Crea e inicia los hilos de ejecución.
 *
 * @param opt Estructura con las opciones del programa.
 */
void start_threads(struct options opt)
{
    int i;
    struct thread_info *threads;
    struct args *args;
    struct buffer buffer;

    srand(time(NULL));

    if ((buffer.data = malloc(opt.buffer_size * sizeof(int))) == NULL)
    {
        printf("Out of memory\n");
        exit(1);
    }
    buffer.size = opt.buffer_size;

    for (i = 0; i < buffer.size; i++)
        buffer.data[i] = i;

    printf("creating %d threads\n", opt.num_threads);
    threads = malloc(sizeof(struct thread_info) * opt.num_threads);
    args = malloc(sizeof(struct args) * opt.num_threads);

    rec_mutex_t *aux = malloc(sizeof(rec_mutex_t) * opt.buffer_size);
    
    for(i = 0; i < opt.buffer_size; i++) {
        if(rec_mutex_init(&aux[i]) != 0) {
            printf("Mutex initialization failed.\n");
            exit(1);
        }
    }

    if (threads == NULL || args == NULL)
    {
        printf("Not enough memory\n");
        exit(1);
    }

    printf("Buffer before: ");
    print_buffer(buffer);
    // Create num_thread threads running swap()
    for (i = 0; i < opt.num_threads; i++)
    {
        threads[i].thread_num = i;

        args[i].thread_num = i;
        args[i].buffer = &buffer;
        args[i].delay = opt.delay;
        args[i].iterations = opt.iterations;
        args[i].mutex2 = aux;

        if (0 != pthread_create(&threads[i].thread_id, NULL,
                                swap, &args[i]))
        {
            printf("Could not create thread #%d", i);
            exit(1);
        }
    }

    // Wait for the threads to finish
    for (i = 0; i < opt.num_threads; i++)
        pthread_join(threads[i].thread_id, NULL);
    
    for (i = 0; i < opt.buffer_size; i++)
        rec_mutex_destroy(&aux[i]);
    // Print the buffer
    printf("Buffer after:  ");
    // qsort(buffer.data, opt.buffer_size, sizeof(int), (int (*)(const void *, const void *)) cmp);
    print_buffer(buffer);

    printf("iterations: %d\n", get_count());

    free(aux);
    free(args);
    free(threads);
    free(buffer.data);

    pthread_exit(NULL);
}

/*
 * @brief Función principal del programa.
 *
 * @param argc Número de argumentos de línea de comandos.
 * @param argv Lista de argumentos de línea de comandos.
 * @return Código de salida del programa.
 */
int main(int argc, char **argv)
{
    struct options opt;

    // Default values for the options
    opt.num_threads = 10;
    opt.buffer_size = 10;
    opt.iterations = 100;
    opt.delay = 10;

    read_options(argc, argv, &opt);

    start_threads(opt);

    exit(0);
}
