# This source code is a product of Sun Microsystems, Inc. and is provided
# for unrestricted use provided that this legend is included on all tape
# media and as a part of the software program in whole or part.  Users
# may copy or modify this source code without charge, but are not authorized
# to license or distribute it to anyone else except as part of a product or
# program developed by the user.
#
# THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
# SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
# OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
# EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
# ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
# NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
# INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
# FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
# 
# This source code is provided with no support and without any obligation on
# the part of Sun Microsystems, Inc. to assist in its use, correction, 
# modification or enhancement.
#
# SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
# INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
# SOURCE CODE OR ANY PART THEREOF.
#
# Sun Microsystems, Inc.
# 2550 Garcia Avenue
# Mountain View, California 94043

# Copyright (c) 1991 Sun Microsystems, Inc.

#
# @(#)Makefile 1.20 88/02/08 SMI
#
#	crt0	Normal C run time startoff
#	mcrt0	C run time start off for profiling, ``prof'' conventions
#	gcrt0	C run time start off for profiling, ``gprof'' conventions
#	f?crt0	C run time start off with sky board initialization.
#
LD= ld
CPP= /lib/cpp
CPPFLAGS= -DLOCORE
SRCS=   crt0.s
OBJS=	crt0.o
CFLAGS= -O -m68010 -fsoft
ASFLAGS= -10
INSTALL= install -m 644

all: ${OBJS}

install: ${OBJS}
	$(INSTALL)  crt0.o $(DESTDIR)/usr/lib/crt0.o

crt0.o:	crt0.s
	$(CPP) $(CPPFLAGS) crt0.s >tmp.s
	$(AS) $(ASFLAGS) -o crt0.o tmp.s && rm tmp.s
	$(LD) -x -r crt0.o
	mv a.out crt0.o

clean:
	rm -f ${OBJS}
