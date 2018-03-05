FC = gfortran
FFLAGS = -Wall

.PHONEY : all

all : 
	cd src && $(MAKE)
	cd example && $(MAKE)
