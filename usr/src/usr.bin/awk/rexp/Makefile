
####################################
# This is a makefile for mawk,
# an implementation of AWK (1988).
####################################
#
#
# This builds a regular expression library
# Remove the -DMAWK and the library has general use.
#

CC = cc
CFLAGS = -O  -DMAWK
RANLIB = ranlib
AR = ar

###################
#  if you don't have strchr()
#CFLAGS = -O -DMAWK -Dstrchr=index
###########################


C=rexp.c rexp0.c rexp1.c rexp2.c rexp3.c rexpdb.c

regexp.a : $(C)
	rm -f *.o
	$(CC) -c $(CFLAGS) $?
	$(AR)  r regexp.a *.o
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ] ; then\
	      $(RANLIB) regexp.a ; else true ; fi
	rm -f *.o
