CC = gcc
CFLAGS = -g
LDFLAGS = -lpthread

SOURCES = rw_mutex.c test_rw_mutex.c
EXECUTABLE = test_rw_mutex

OBJECTS = $(SOURCES:.c=.o)

all: $(EXECUTABLE)
	./$(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)

build: $(EXECUTABLE)


