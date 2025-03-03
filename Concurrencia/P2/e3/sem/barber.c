#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include "options.h"
#include "sem.h"

struct threadInfo
{
    pthread_t id;
    int num;
};
struct barberArgsT
{
    int num;
    int cutTime;
    customSemT *barbersSem;
    customSemT *customersSem;
    pthread_mutex_t *mutex;
    int *waiting;
    bool *exit;
};
struct customerArgsT
{
    int num;
    customSemT *barbersSem;
    customSemT *customersSem;
    pthread_mutex_t *mutex;
    int *waiting;
    int chairs;
};

void startThreads(struct options opt);
void *barber(void *arg);
void *customer(void *arg);

int main(int argc, char **argv)
{
    struct options opt;

    // Default values for the options
    opt.barbers = 5;
    opt.customers = 1000;
    opt.cut_time = 3000;

    read_options(argc, argv, &opt);
    startThreads(opt);
    exit(0);
}

void startThreads(struct options opt)
{
    int waiting = 0;
    bool exitB = false;
    struct threadInfo *barberThreads, *customerThreads;
    struct barberArgsT *barberArgs;
    struct customerArgsT *customerArgs;
    customSemT *barbersSem, *customersSem;
    pthread_mutex_t *mutex;

    barberThreads = malloc(opt.barbers * sizeof(struct threadInfo));
    customerThreads = malloc(opt.customers * sizeof(struct threadInfo));
    customersSem = malloc(sizeof(customSemT));
    barbersSem = malloc(sizeof(customSemT));
    mutex = malloc(sizeof(pthread_mutex_t));

    if (sem_init(barbersSem, opt.barbers) != 0)
    {
        perror("Failed to initialize barbers semaphore");
        exit(1);
    }
    if (sem_init(customersSem, 0) != 0)
    {
        perror("Failed to initialize customers semaphore");
        exit(1);
    }
    pthread_mutex_init(mutex, NULL);

    barberArgs = malloc(opt.barbers * sizeof(struct barberArgsT));
    for (int i = 0; i < opt.barbers; i++)
    {
        barberThreads[i].id = i;
        barberArgs[i].num = i;
        barberArgs[i].cutTime = opt.cut_time;
        barberArgs[i].barbersSem = barbersSem;
        barberArgs[i].customersSem = customersSem;
        barberArgs[i].mutex = mutex;
        barberArgs[i].waiting = &waiting;
        barberArgs[i].exit = &exitB;

        if (pthread_create(&barberThreads[i].id, NULL, barber, &barberArgs[i]) != 0)
        {
            perror("Failed to create barber thread");
            exit(1);
        }
    }

    customerArgs = malloc(opt.customers * sizeof(struct customerArgsT));
    for (int i = 0; i < opt.customers; i++)
    {
        customerThreads[i].id = i;
        customerArgs[i].num = i;
        customerArgs[i].barbersSem = barbersSem;
        customerArgs[i].customersSem = customersSem;
        customerArgs[i].mutex = mutex;
        customerArgs[i].waiting = &waiting;
        customerArgs[i].chairs = opt.barbers * 5;

        if (pthread_create(&customerThreads[i].id, NULL, customer, &customerArgs[i]) != 0)
        {
            perror("Failed to create customer thread");
            exit(1);
        }
    }

    for (int i = 0; i < opt.customers; i++)
    {
        pthread_join(customerThreads[i].id, NULL);
    }

    pthread_mutex_lock(mutex);
    exitB = true;
    pthread_mutex_unlock(mutex);

    for (int i = 0; i < opt.barbers; i++)
    {
        sem_v(customersSem); 
    }

    for (int i = 0; i < opt.barbers; i++)
    {
        pthread_join(barberThreads[i].id, NULL);
    }

    sem_destroy(barbersSem);
    sem_destroy(customersSem);
    pthread_mutex_destroy(mutex);

    free(barberThreads);
    free(customerThreads);
    free(barberArgs);
    free(customerArgs);
    free(customersSem);
    free(barbersSem);
    free(mutex);

    pthread_exit(NULL);
}

void *barber(void *arg)
{
    struct barberArgsT *args = arg;
    printf("💈 Barber %d started working\n", args->num);

    while (1)
    {
        pthread_mutex_lock(args->mutex);
        if (*args->exit && *args->waiting == 0)
        {
            pthread_mutex_unlock(args->mutex);
            break;
        }
        pthread_mutex_unlock(args->mutex);

        sem_p(args->customersSem); 

        pthread_mutex_lock(args->mutex);
        if (*args->exit && *args->waiting == 0)
        {
            pthread_mutex_unlock(args->mutex);
            break;
        }
        (*args->waiting)--; 
        pthread_mutex_unlock(args->mutex);

        printf("💈 Barber %d is cutting hair\n", args->num);
        usleep(args->cutTime); 
        printf("💈 Barber %d finished cutting hair\n", args->num);

        sem_v(args->barbersSem); 
    }

    printf("💈 Barber %d is going home\n", args->num);
    return NULL;
}

void *customer(void *arg)
{
    struct customerArgsT *args = arg;

    printf("👤 Customer %d arrived\n", args->num);

    pthread_mutex_lock(args->mutex);
    if (*args->waiting < args->chairs)
    {
        (*args->waiting)++;
        printf("👤 Customer %d is waiting, %d customers in waiting room\n", args->num, *args->waiting);
        pthread_mutex_unlock(args->mutex);

        sem_v(args->customersSem); 
        sem_p(args->barbersSem);   

        printf("👤 Customer %d is getting a haircut\n", args->num);
        printf("👤 Customer %d finished haircut and left\n", args->num);
    }
    else
    {
        pthread_mutex_unlock(args->mutex);
        printf("👤 Customer %d left because waiting room was full\n", args->num);
    }
    return NULL;
}
