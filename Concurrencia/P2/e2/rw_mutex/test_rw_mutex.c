#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include "rw_mutex.h"

#define NUM_READERS 10
#define NUM_WRITERS 5
#define NUM_ITERATIONS 5

int shared_resource = 0;

rw_mutex_t rw_mutex;

void manejar_error(int ret, const char *msg) {
    if (ret != 0) {
        fprintf(stderr, "Error: %s (código: %d)\n", msg, ret);
        exit(EXIT_FAILURE);
    }
}

// Función que ejecutarán los lectores en las pruebas
void *lector(void *arg) {
    long id = (long)arg;
    
    for (int i = 0; i < NUM_ITERATIONS; i++) {
        printf("Lector %ld intentando obtener lock de lectura...\n", id);
        int ret = rw_mutex_readlock(&rw_mutex);
        manejar_error(ret, "rw_mutex_readlock falló");
        
        
        printf("Lector %ld leyendo valor: %d\n", id, shared_resource);
        
        usleep(rand() % 100000);
        
        ret = rw_mutex_readunlock(&rw_mutex);
        manejar_error(ret, "rw_mutex_readunlock falló");
        
        printf("Lector %ld liberó lock de lectura\n", id);
        
        
        usleep(rand() % 200000);
    }
    
    return NULL;
}

// Función que ejecutarán los escritores durante las pruebas
void *escritor(void *arg) {
    long id = (long)arg;
    
    for (int i = 0; i < NUM_ITERATIONS; i++) {
        printf("Escritor %ld intentando obtener lock de escritura...\n", id);
        int ret = rw_mutex_writelock(&rw_mutex);
        manejar_error(ret, "rw_mutex_writelock falló");
        
        
        shared_resource++;
        printf("Escritor %ld modificó valor a: %d\n", id, shared_resource);
        
        usleep(rand() % 150000);
        
        ret = rw_mutex_writeunlock(&rw_mutex);
        manejar_error(ret, "rw_mutex_writeunlock falló");
        
        printf("Escritor %ld liberó lock de escritura\n", id);
        
        
        usleep(rand() % 300000);
    }
    
    return NULL;
}

void testInicializarDestruir() {
    printf("=== Prueba de inicialización y destrucción ===\n");
    
    rw_mutex_t test_mutex;
    
    int ret = rw_mutex_init(&test_mutex);
    manejar_error(ret, "rw_mutex_init falló");
    printf("Mutex inicializado correctamente\n");
    
    ret = rw_mutex_destroy(&test_mutex);
    manejar_error(ret, "rw_mutex_destroy falló");
    printf("Mutex destruido correctamente\n\n");
}

void testPunteroNulo() {
    printf("=== Prueba con puntero NULL ===\n");
    
    int ret = rw_mutex_init(NULL);
    if (ret == -1) {
        printf("rw_mutex_init(NULL) retornó -1 correctamente\n");
    } else {
        printf("Error: rw_mutex_init(NULL) no falló como se esperaba\n");
    }
    
    ret = rw_mutex_destroy(NULL);
    if (ret == -1) {
        printf("rw_mutex_destroy(NULL) retornó -1 correctamente\n");
    } else {
        printf("Error: rw_mutex_destroy(NULL) no falló como se esperaba\n");
    }
    
    ret = rw_mutex_readlock(NULL);
    if (ret == -1) {
        printf("rw_mutex_readlock(NULL) retornó -1 correctamente\n");
    } else {
        printf("Error: rw_mutex_readlock(NULL) no falló como se esperaba\n");
    }
    
    ret = rw_mutex_writelock(NULL);
    if (ret == -1) {
        printf("rw_mutex_writelock(NULL) retornó -1 correctamente\n");
    } else {
        printf("Error: rw_mutex_writelock(NULL) no falló como se esperaba\n");
    }
    
    ret = rw_mutex_readunlock(NULL);
    if (ret == -1) {
        printf("rw_mutex_readunlock(NULL) retornó -1 correctamente\n");
    } else {
        printf("Error: rw_mutex_readunlock(NULL) no falló como se esperaba\n");
    }
    
    ret = rw_mutex_writeunlock(NULL);
    if (ret == -1) {
        printf("rw_mutex_writeunlock(NULL) retornó -1 correctamente\n\n");
    } else {
        printf("Error: rw_mutex_writeunlock(NULL) no falló como se esperaba\n\n");
    }
}

void testConcurrencia() {
    printf("=== Prueba de concurrencia ===\n");
    
    pthread_t readers[NUM_READERS];
    pthread_t writers[NUM_WRITERS];
    
    int ret = rw_mutex_init(&rw_mutex);
    manejar_error(ret, "rw_mutex_init falló");
    
    for (long i = 0; i < NUM_READERS; i++) {
        ret = pthread_create(&readers[i], NULL, lector, (void *)i);
        if (ret != 0) {
            manejar_error(ret, "pthread_create para lector falló");
        }
    }
    
    for (long i = 0; i < NUM_WRITERS; i++) {
        ret = pthread_create(&writers[i], NULL, escritor, (void *)i);
        if (ret != 0) {
            manejar_error(ret, "pthread_create para escritor falló");
        }
    }
    
    for (int i = 0; i < NUM_READERS; i++) {
        ret = pthread_join(readers[i], NULL);
        if (ret != 0) {
            manejar_error(ret, "pthread_join para lector falló");
        }
    }
    
    for (int i = 0; i < NUM_WRITERS; i++) {
        ret = pthread_join(writers[i], NULL);
        if (ret != 0) {
            manejar_error(ret, "pthread_join para escritor falló");
        }
    }
    
    ret = rw_mutex_destroy(&rw_mutex);
    manejar_error(ret, "rw_mutex_destroy falló");
    
    printf("Valor final del recurso compartido: %d\n", shared_resource);
    printf("Prueba de concurrencia finalizada\n\n");
}

int main() {
    
    srand(time(NULL));
    
    printf("=== Programa de prueba para rw_mutex ===\n\n");
    
    testInicializarDestruir();
    testPunteroNulo();
    testConcurrencia();
    
    printf("=== Todas las pruebas completadas ===\n");
    
    return 0;
}