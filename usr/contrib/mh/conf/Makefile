######################################################################
#	Instructions to Make, for compilation of MH modules
######################################################################

CC      =   	cc
CFLAGS  =   	-O
LFLAGS  =   	-bhu
LDFLAGS =
LIBES   =


######################################################################
#	Here it is...
######################################################################

all:		mhconfig
lint:		l-mhconfig


######################################################################
# mhconfig
######################################################################

mhconfig:	mhconfig.o
		$(CC) $(LDFLAGS) -o $@ mhconfig.o $(LIBES)


l-mhconfig:;	lint $(LFLAGS) mhconfig.c $(LLIBS)


######################################################################
#	Miscellaneous tasks
######################################################################

distribution:	clean
		rm -f mhconfig config.sed doc/config.sed

clean:		unclean

unclean:;	rm -f *.o _* :* */_* */:* */*/_* */*/:*
