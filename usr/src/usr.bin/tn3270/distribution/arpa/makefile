#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)makefile	3.1 (Berkeley) %G%
#
# msdos versus unix defines
O	= .o
#PC_O	= .obj

X	=
#PC_X	= .exe

L	=
#PC_L	= -link

CC	= cc
#PC_CC	= cl

MV	= mv
#PC_MV	= rename

RM	= rm -f
#PC_RM= erase

LINT_ARGS =
#PC_LINT_ARGS = -DLINT_ARGS

DEBUG_FLAGS = -g
#PC_DEBUG_FLAGS = -Zi -Od

AR	= ar
AR1	= cr
AR2	=
AR3	=
#PC_AR	= lib
#PC_AR1	=
#PC_AR2	= +
#PC_AR3	= ";"

RANLIB	= ranlib
#PC_RANLIB = echo "Done with "

PRINT = lpr -p

ALLC =
ALLH = telnet.h

ALLPRINT = ${ALLH} ${ALLC}

ALLSOURCE = ${ALLPRINT} makefile makefile.mak

clean:

sccsclean:
	-sccs clean
	-sccs get makefile

action:
	${ACTION}

print:
	${PRINT} ${ALLPRINT}


sourcelist:	${ALLSOURCE}
	@for i in ${ALLSOURCE}; \
		do (echo ${DIRPATH}$$i); done

.DEFAULT:
	sccs get $<
