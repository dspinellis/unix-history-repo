#
#       Makefile for UTREE
#       klin, Mon Feb 24 00:02:38 1992
#
#       @(#) utree 3.03-um (klin) Feb 23 1992 Makefile
#
#       Version: Apollo Domain DN4500, DomainIX 9.5 (BSD4.2 + SYSV)

# Compiler flags, loader flags and libraries
CFLAGS= -DBSD -DAPOLLO -O
MFLAGS=
LFLAGS=
LIBS=   -ltermcap
MLIBS=
# Additional local libraries
LLIBS=

# --------------------------------------------------------------------- #

# Some needed commands to create utree
CC=     cc
CP=     cp
STRIP=  strip

# --------------------------------------------------------------------- #

# Utree executables
UTREE=  ../bin/utree
UTPRL=  ../bin/utree.prlist

# --------------------------------------------------------------------- #

# Utree header files
DEFS=   conf.h cmds.h defs.h help.h hist.h keys.h list.h term.h vars.h

# Utree sources and objects
SRCS=   main.c bind.c comm.c echo.c edit.c file.c help.c hist.c list.c \
	stat.c term.c tree.c util.c vars.c vers.c getopt.c
OBJS=   main.o bind.o comm.o echo.o edit.o file.o help.o hist.o list.o \
	stat.o term.o tree.o util.o vars.o vers.o getopt.o

# --------------------------------------------------------------------- #

all:    utree utree.prlist

utree:  $(UTREE)

strip:  $(UTREE)
	$(CP) $(UTREE) $(UTREE)+sym
	$(STRIP) $(UTREE)

utree.prlist:   $(UTPRL)

# Compile and link utree
$(UTREE):       $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) $(OBJS) -o $@ $(LIBS) $(LLIBS)

# Compile and link utree.prlist
$(UTPRL):       utree.prlist.c getopt.o
	$(CC) -s $(CFLAGS) $(MFLAGS) $(LFLAGS) $? -o $@ $(MLIBS) $(LIBS) $(LLIBS)

# Clean up
clean:
	rm -f core *.o

# --------------------------------------------------------------------- #

$(OBJS):        $(DEFS)
	$(CC) $(CFLAGS) -c $<

# --------------------------------------------------------------------- #
