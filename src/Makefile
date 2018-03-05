FC = gfortran

MODULEDIR = ../module
MODULEFLAG = -J $(MODULEDIR)

%.o : %.f90
	$(FC) $(FFLAG) $(MODULEFLAG) -c $<

.PHONEY: all

all : signal_slot_mod.o
