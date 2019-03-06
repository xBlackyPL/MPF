CC=gcc
FC = gfortran

CFLAGS=-c -O3 -Wall -fopenmp
FFLAGS=-c -O3 -Wall -fopenmp
LFLAGS=-Llib -lpso -lgsl -lgslcblas -lm -fopenmp


lib:	src/pso.o
	ar -cvq lib/libpso.a src/pso.o

example:	src/main.o
	$(CC) src/main.o $(LFLAGS) -o example/example

example2:	src/main2.o src/ffunc.o 
	$(CC) src/main2.o src/ffunc.o  $(LFLAGS) -o example/example2

example3:	src/main_imp.o src/cfunc.o src/ffunc.o 
	$(FC) src/main_imp.o src/cfunc.o src/ffunc.o  $(LFLAGS) -o example/example3

src/pso.o:	src/pso.c
	$(CC) $(CFLAGS) src/pso.c -o src/pso.o

src/main.o:	src/main.c
	$(CC) $(CFLAGS) src/main.c -o src/main.o

src/ffunc.o:	src/ffunc.f95
	$(FC) $(FFLAGS) src/ffunc.f95 -o src/ffunc.o

src/cfunc.o:	src/cfunc.c
	$(CC) $(CFLAGS) src/cfunc.c -o src/cfunc.o

src/main2.o:	src/main2.c
	$(CC) $(CFLAGS) src/main2.c -o src/main2.o

src/main_imp.o:	src/main_imp.f95
	$(FC) $(FFLAGS) src/main_imp.f95 -o src/main_imp.o

clean:
	rm -rf src/*.o lib/*.a example/example* 
