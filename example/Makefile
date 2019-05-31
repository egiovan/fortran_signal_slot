FC = gfortran
FFLAGS = -Wall
MODINC = -I ../module

OBJ = observable_implementation_mod.o observer_implementation_mod.o
LIB = ../src/signal_slot_mod.o

%.o : %.f90
	$(FC) $(FFLAGS) $(MODINC) -c $< 

.PHONEY: all

all : test_signal_slot.exe minimal_example.exe

test_signal_slot.exe :  test_signal_slot.f90  $(OBJ) $(LIB)
	$(FC) $(FFLAGS) $(MODINC) -o  $@ $^

minimal_example.exe : minimal_example.f90 $(OBJ) $(LIB)
	$(FC) $(FFLAGS) $(MODINC) -o  $@ $^