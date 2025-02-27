/*
 * @file op_count.c
 * @brief Implementación de un contador seguro con mutex.
 *
 * Este archivo contiene la implementación de un contador protegido por un mutex,
 * permitiendo operaciones seguras en entornos multihilo.
 */

/*
 * Librerías necesarias para la correcta implementación del código
 */
#include <pthread.h>

/*
 * @brief Mutex para proteger el acceso a la variable count.
 */
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

/*
 * @brief Variable global que almacena el contador.
 */
int count = 0;

/*
 * @brief Incrementa el contador de manera segura.
 *
 * Protege el acceso a la variable count usando un mutex para evitar condiciones de carrera.
 */
void inc_count() {
    pthread_mutex_lock(&m);
    count ++;
    pthread_mutex_unlock(&m);
}

/*
 * @brief Obtiene el valor actual del contador de manera segura.
 *
 * Bloquea el mutex antes de leer la variable count para garantizar consistencia.
 *
 * @return El valor actual del contador.
 */
int get_count() {
    int cnt;
    pthread_mutex_lock(&m);
    cnt = count;
    pthread_mutex_unlock(&m);

    return cnt;
}