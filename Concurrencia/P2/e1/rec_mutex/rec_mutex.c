#include <pthread.h>
#include "rec_mutex.h"

int rec_mutex_init(rec_mutex_t *m) {
    m->count = 0;
    m->locked = 0;
    m->owner = 0;

    return pthread_mutex_init(&m->mutex, NULL);
}

int rec_mutex_destroy(rec_mutex_t *m) {
    return pthread_mutex_destroy(&m->mutex);
}

int rec_mutex_lock(rec_mutex_t *m) {
    pthread_t self = pthread_self();

    if(m->locked && pthread_equal(self, m->owner)){
        m->count++;
        return 0;
    }

    int ret = pthread_mutex_lock(&m->mutex);
    if(ret == 0){
        m->owner = self;
        m->locked = 1;
        m->count = 1;
    }

    return ret;
};

int rec_mutex_unlock(rec_mutex_t *m) {
    pthread_t self = pthread_self();

    if(!m->locked || !pthread_equal(self, m->owner))
        return 1;
    
    m->count--;
    if(m->count == 0){
        m->locked = 0;
        m->owner = 0;
        return pthread_mutex_unlock(&m->mutex);
    }

    return 0;
};

int rec_mutex_trylock(rec_mutex_t *m) {
    pthread_t self = pthread_self();

    if (m->locked && pthread_equal(m->owner, self)) {
        m->count++;
        return 0;
    }
    
    int ret = pthread_mutex_trylock(&m->mutex);
    if(ret == 0) {
        m->owner = self;
        m->locked = 1;
        m->count = 1;
    }

    return ret;
};
