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
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#

# Copyright (c) 1991 Sun Microsystems, Inc.

#
# @(#)Makefile 1.103 89/11/30 SMI; from UCB 5.13 85/09/09
#
DESTDIR=
CFLAGS=	-O 
LDFLAGS=
CC=	/bin/cc
CHOWN=	/usr/etc/chown
MAKE= make CC=${CC}
RM=	/usr/bin/rm -f
ROOT=   root
SPGRP=  daemon

.KEEP_STATE:

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	ldd

# Programs built "small"
#
ldd := LDFLAGS += -n -Bdynamic

all:	${STD}

${STD} ${TTY} cp:
	${CC} ${LDFLAGS} ${CFLAGS} -o $@ $@.c

install: ${STD} ${NSTD} ${TTY} cp ${STATIC} ${SCRIPTSRC} ${ASP}
	install -s ${STD} ${DESTDIR}/usr/bin
clean:
	$(RM) a.out core *.s *.o errs
	$(RM) ${STD}

FRC:

# Files listed in ${NSTD} have explicit make lines given below.

