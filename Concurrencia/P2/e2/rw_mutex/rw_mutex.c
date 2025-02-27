
#include "rw_mutex.h"

int rw_mutex_init(rw_mutex_t *m){
    /*
    ? se podría hacer así también 
    if (m == NULL) return -1;
    
    if (pthread_mutex_init(&m->mutex, NULL) != 0)
        return -1;
    if (pthread_cond_init(&m->readersCond, NULL) != 0) {
        pthread_mutex_destroy(&m->mutex);
        return -1;
    }
    if (pthread_cond_init(&m->writersCond, NULL) != 0) {
        pthread_mutex_destroy(&m->mutex);
        pthread_cond_destroy(&m->readersCond);
        return -1;
    }

    m->activeReaders = 0;
    m->waitingReaders = 0;
    m->activeWriters = 0;
    m->waitingWriters = 0;
    
    return 0;
    */

    if(m == NULL) 
        return -1;

    int ret = 0;

    if((ret = pthread_mutex_init(&m->mutex, NULL)) != 0){
        return ret;
    }
    
    if((ret = pthread_cond_init(&m->readersCond, NULL)) != 0){  
        pthread_mutex_destroy(&m->mutex);
        return ret;
    }
    if((ret = pthread_cond_init(&m->writersCond, NULL)) != 0){  
        pthread_mutex_destroy(&m->mutex);
        pthread_cond_destroy(&m->readersCond);
        return ret;
    }
    m->activeReaders = 0;
    m->waitingReaders = 0;
    m->activeWriters = 0;
    m->waitingWriters = 0;

    return ret;
}

int rw_mutex_destroy(rw_mutex_t *m) {
    if (m == NULL) 
        return -1;

    pthread_mutex_destroy(&m->mutex);
    pthread_cond_destroy(&m->readersCond);
    pthread_cond_destroy(&m->writersCond);
    return 0;
}

int rw_mutex_readlock(rw_mutex_t *m) {
    if (m == NULL) return -1;
    
    pthread_mutex_lock(&m->mutex);

    m->waitingReaders++;

    while (m->activeWriters > 0 || m->waitingWriters > 0) 
        pthread_cond_wait(&m->readersCond, &m->mutex);
    
    
    m->waitingReaders--;

    m->activeReaders++;
    
    pthread_mutex_unlock(&m->mutex) != 0;
        
    return 0;
}

int rw_mutex_writelock(rw_mutex_t *m) {
    if (m == NULL) 
        return -1;
    
    pthread_mutex_lock(&m->mutex) != 0;

    m->waitingWriters++;
    
    while (m->activeReaders > 0 || m->activeWriters > 0)
        pthread_cond_wait(&m->writersCond, &m->mutex) != 0;
    
    
    m->waitingWriters--;
    m->activeWriters = 1;
    
    pthread_mutex_unlock(&m->mutex) != 0;  

    return 0;
}

int rw_mutex_readunlock(rw_mutex_t *m) {
    if (m == NULL) 
        return -1;
    
    pthread_mutex_lock(&m->mutex) != 0;

    m->activeReaders--;
    
    if (m->activeReaders == 0 && m->waitingWriters > 0)
        pthread_cond_signal(&m->writersCond);
    
    pthread_mutex_unlock(&m->mutex) != 0;
    return 0;
}

int rw_mutex_writeunlock(rw_mutex_t *m) {
    if (m == NULL) 
        return -1;
    
    pthread_mutex_lock(&m->mutex) != 0;

    m->activeWriters = 0;
    
    if (m->waitingWriters > 0)
        pthread_cond_signal(&m->writersCond);
        else if (m->waitingReaders > 0) {
            pthread_cond_broadcast(&m->readersCond);
        }
    
    pthread_mutex_unlock(&m->mutex);

    return 0;
}
