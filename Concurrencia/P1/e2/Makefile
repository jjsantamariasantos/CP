# * Makefile para la compilación del programa de intercambio de datos en un buffer con múltiples hilos.
# * Este Makefile gestiona la compilación y enlazado de los archivos necesarios.

# * Compilador utilizado.
CC=gcc

# * Opciones de compilación:
# * -Wall: Muestra todas las advertencias.
# * -pthread: Habilita el soporte para hilos POSIX.
# * -g: Incluye información de depuración.
CFLAGS=-Wall -pthread -g

# * Librerías adicionales (actualmente vacío).
LIBS=

# * Archivos objeto que forman el programa.
OBJS=swap.o options.o op_count.o

# * Nombre del ejecutable generado.
PROGS= swap

# * Regla principal que compila el programa.
all: $(PROGS)

# * Regla genérica para compilar archivos .c en archivos .o.
%.o : %.c
	$(CC) $(CFLAGS) -c $<

# * Regla para enlazar el ejecutable final.
swap: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

# * Regla para limpiar los archivos generados.
clean:
	rm -f $(PROGS) *.o *~

