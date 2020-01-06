default:
	@echo "Please provide a compiler name (llvm, gnu, intel, pgi, xi, nag)"
	exit 1

gnu:
	( $(MAKE) mpas_stack \
	 "FC = gfortran" \
	 "FFLAGS = -g -Wall -fcheck=all -pedantic -std=f2003 -fbacktrace")

intel:
	( $(MAKE) mpas_stack \
	 "FC = ifort " \
	 "FFLAGS = -g -warn all -check all -traceback" )

pgi:
	( $(MAKE) mpas_stack \
	 "FC = pgfortran" \
	 "FFLAGS = -g -Mbounds -Mchkptr -traceback" )


mpas_stack:
	$(FC) -c mpas_stack.f90
	$(FC) -o stack_test mpas_stack_test.f90 mpas_stack.o

clean:
	rm -rf *.o *.mod stack_test
