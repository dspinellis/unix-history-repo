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
# @(#)Makefile 1.22 88/05/12
#
all : xall
include ../MakeCommon

DIR = ..
CFLAGS  += -O -DSUN2=0 -DSUN3=1 -DSUN4=2 -Dsun3 -DDEBUG -DTARGET=$(TARGET) -I..
CPPLFAGS += -I..
LINTFLAGS = 
HDRS = dynamic.h reloc_info.h
PROGRAM = ld

xall : ${HDRS:%.h=$(DIR)/%.h} ${PROGRAM}

${PROGRAM}:	${LIBS}	${OBJS}
	${CC} -o ${PROGRAM} ${OBJS} ${LIBS}

install: ${PROGRAM}
	install -s ${PROGRAM} ${DESTDIR}/bin/${PROGRAM}

clean: masterclean

tags:
	ctags $(MID_SRCS)

lint : masterlint
