FC       = gfortran
CC       = gcc
FFLAGS   = -O3 -fopenmp
BLAS     = -llapack -lblas 

%.o : %.mod
%.o : %.f90
	$(FC) -c $(FFLAGS) $<

generated = dipole.o kinetic.o overlap.o partial_z.o
obj = constants.o $(generated)

all : hhg-1d.x

hhg-1d.x : $(obj) hhg-1d.o
	$(FC) -o $@ $^ $(FFLAGS) 

.PHONY : clean
clean :
	rm *.o *.mod hhg-1d.x

hhg-1d.o     : $(obj)

overlap.o    : constants.o
dipole.o     : constants.o
kinetic.o    : constants.o
partial_z.o  : constants.o

overlap.f90   : gen_code_def.m gen_code_run.m
	MathKernel < gen_code_run.m
dipole.f90    : gen_code_def.m gen_code_run.m
	MathKernel < gen_code_run.m
kinetic.f90   : gen_code_def.m gen_code_run.m
	MathKernel < gen_code_run.m
partial_z.f90 : gen_code_def.m gen_code_run.m
	MathKernel < gen_code_run.m
