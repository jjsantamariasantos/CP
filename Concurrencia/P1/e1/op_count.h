/*
 * @file op_count.h
 * @brief Definiciones de funciones para la gestión de un contador seguro.
 *
 * Este archivo contiene las declaraciones de funciones para incrementar y obtener
 * el valor de un contador protegido por un mutex en entornos multihilo.
 */
#ifndef __OP_COUNT_H__
#define __OP_COUNT_H__

/*
 * @brief Incrementa el contador de manera segura.
 *
 * Esta función protege el acceso a la variable del contador utilizando un mutex,
 * asegurando que no haya condiciones de carrera en entornos multihilo.
 */
void inc_count();

/*
 * @brief Obtiene el valor actual del contador de manera segura.
 *
 * Bloquea el mutex antes de leer la variable del contador para garantizar consistencia.
 *
 * @return El valor actual del contador.
 */
int get_count();

#endif