# $Header: Makefile,v 2.4 87/02/14 13:48:35 jqj Exp $
#
# $Log:	Makefile,v $
# Revision 2.4  87/02/14  13:48:35  jqj
# new directory structure
# 
# Revision 2.3  87/01/05  11:20:42  ed
# Updated to new directory structure
# 
# Revision 2.2  86/06/25  12:49:28  jqj
# use ${CC} instead of cc
# 
# Revision 2.1  85/11/21  10:39:51  jqj
# make sure lex.yy.c etc are not around!
# 
# Revision 2.0  85/11/21  07:21:26  jqj
# 4.3BSD standard release
# 
# Revision 1.6  85/10/30  07:36:03  root
# BINDIR=/usr/new by default
# 
# Revision 1.5  85/05/06  08:12:49  jqj
# Almost Beta-test version.
# 
# Revision 1.4  85/03/26  06:09:17  jqj
# Revised public alpha-test version, released 26 March 1985
# 
# Revision 1.3  85/03/11  16:38:29  jqj
# Public alpha-test version, released 11 March 1985
# 
# Revision 1.2  85/02/21  11:04:38  jqj
# alpha test version
# 
# Note: the library, including /usr/include/courier.h, must be installed first

BINDIR = ../bin
LIBDIR = ../lib
MANDIR = ../man
INCDIR = ../include
COURIERDIR = ../xnscourier

RCSREV=
RCSNAME=current
OBJS = checktype.o constants.o constantcode.o code.o lex.yy.o main.o \
	misc.o procedures.o symbols.o types.o typecode.o y.tab.o
CFLAGS = -I${INCDIR} -DDEBUG -O
LDFLAGS =

xnscourier: $(OBJS)
	${CC} $(CFLAGS) -o xnscourier $(OBJS) -ll \
			${LIBDIR}/libcourier.a $(LDFLAGS)

install: xnscourier
	install -c -s xnscourier ${BINDIR}/xnscourier

y.tab.h y.tab.c: courier.y
	-@rm -f y.tab.c y.tab.h
	@echo expect one shift/reduce conflict
	yacc -d courier.y

lex.yy.c: scanner.l
	-@rm -f lex.yy.c
	lex scanner.l

$(OBJS): compiler.h
lex.yy.o: y.tab.o

lint:	y.tab.c lex.yy.c
	lint -hx code.c main.c misc.c symbols.c types.c typecode.c \
		checktype.c constants.c constantcode.c procedures.c \
		y.tab.c lex.yy.c

tags:	code.c main.c misc.c symbols.c types.c typecode.c \
		checktype.c constants.c constantcode.c \
		procedures.c compiler.h
	ctags code.c main.c misc.c symbols.c types.c typecode.c \
		checktype.c constants.c constantcode.c \
		procedures.c compiler.h

clean:
	-rm -f $(OBJS) lex.yy.c y.tab.c y.tab.h *.BAK *.CKP xnscourier

rcs:	
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
