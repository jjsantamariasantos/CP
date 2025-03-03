#ifndef __SEM_H__
#define __SEM_H__

#include <pthread.h>

typedef struct customSemT
{
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    int count;
} customSemT;

int sem_init(customSemT *s, int value);
int sem_destroy(customSemT *s);

int sem_p(customSemT *s);
int sem_v(customSemT *s);
int sem_tryp(customSemT *s); // 0 on sucess, -1 if already locked

#endif