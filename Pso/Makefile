CC=g++
CFLAGS=-c -O3 -Wall -fopenmp
FFLAGS=-c -O3 -Wall -fopenmp
LFLAGS=-Llib -lpso -lgsl -lgslcblas -lm -fopenmp

lib:	src/pso.o
	ar -cvq lib/libpso.a src/pso.o

example:	src/main.o
	$(CC) src/main.o $(LFLAGS) -o example/example

clean:
	rm -rf src/*.o lib/*.a example/example* 
