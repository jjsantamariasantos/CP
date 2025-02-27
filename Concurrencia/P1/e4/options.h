/*
 * @file options.h
 * @brief Definición de la estructura de opciones y funciones para su manejo.
 *
 * Este archivo define la estructura que almacena las opciones de configuración
 * y la función para leer los argumentos de la línea de comandos.
 */

#ifndef __OPTIONS_H__
#define __OPTIONS_H__

/*
 * @struct options
 * @brief Estructura que almacena los parámetros de configuración.
 *
 * Contiene los valores necesarios para la ejecución del programa, incluyendo
 * el número de hilos, tamaño del búfer, número de iteraciones, retraso y
 * tiempo de espera entre impresiones.
 */
struct options {
	int num_threads;
	int buffer_size;
	int iterations;
	int delay;
	int print_wait;
};

/*
 * @brief Lee y procesa los argumentos de la línea de comandos.
 *
 * Parsea los argumentos pasados al programa, validando los valores y almacenándolos
 * en la estructura de opciones proporcionada.
 *
 * @param argc Número de argumentos de la línea de comandos.
 * @param argv Vector de cadenas de caracteres con los argumentos.
 * @param opt Puntero a la estructura donde se almacenarán las opciones.
 * @return 0 si el procesamiento fue exitoso, otro valor si hubo un error.
 */
int read_options(int argc, char **argv, struct options *opt);


#endif
