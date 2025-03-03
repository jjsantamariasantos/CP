#include "sem.h"

int sem_init(customSemT *s, int value)
{
    int ret = 0;
    if ((ret = pthread_mutex_init(&s->mutex, NULL)) != 0)
    {
        return ret;
    }
    if ((ret = pthread_cond_init(&s->cond, NULL)) != 0)
    {
        pthread_mutex_destroy(&s->mutex);
        return ret;
    }
    s->count = value;
    return ret;
}

int sem_destroy(customSemT *s)
{
    pthread_mutex_destroy(&s->mutex);
    pthread_cond_destroy(&s->cond);
    return 0;
}

int sem_p(customSemT *s)
{
    pthread_mutex_lock(&s->mutex);
    while (s->count == 0)
    {
        pthread_cond_wait(&s->cond, &s->mutex);
    }
    s->count--;
    pthread_mutex_unlock(&s->mutex);
    return 0;
}

int sem_v(customSemT *s)
{
    pthread_mutex_lock(&s->mutex);
    s->count++;
    pthread_cond_signal(&s->cond);
    pthread_mutex_unlock(&s->mutex);
    return 0;
}

int sem_tryp(customSemT *s)
{
    pthread_mutex_lock(&s->mutex);
    if (s->count == 0)
    {
        pthread_mutex_unlock(&s->mutex);
        return -1;
    }
    s->count--;
    pthread_mutex_unlock(&s->mutex);
    return 0; // 0 on sucess, -1 if already locked
}
