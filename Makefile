FC       = gfortran
CC       = gcc
FFLAGS   = -O3 -fopenmp
BLAS     = -llapack -lblas 

%.o : %.mod
%.o : %.f90
	$(FC) -c $(FFLAGS) $<

obj = constants.o overlap.o hhg-1d.o

all : hhg-1d.x

hhg-1d.x : $(obj)
	$(FC) -o $@ $^ $(FFLAGS) 

.PHONY : clean
clean :
	rm *.o *.mod hhg-1d.x

hhg-1d.o     : constants.o overlap.o
overlap.o    : constants.o
