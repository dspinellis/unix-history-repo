#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
# Berkeley Mail
#
CFLAGS=	-O -DVMUNIX -DV7
XSTR=	/usr/ucb/xstr
DESTDIR=
CTAGS=	/usr/ucb/ctags -w
VPRINT=	/usr/ucb/print
LIBES=
AS=	-as
RM=	-rm
OBJS=	version.o aux.o cmd1.o cmd2.o cmd3.o cmdtab.o collect.o \
	config.o edit.o fio.o \
	getname.o head.o v7.local.o lex.o list.o main.o \
	names.o optim.o popen.o quit.o send.o strings.o temp.o tty.o \
	vars.o str.o
SRCS=	aux.c cmd1.c cmd2.c cmd3.c cmdtab.c collect.c config.c edit.c fio.c \
	getname.c head.c v7.local.c lex.c list.c main.c \
	names.c optim.c popen.c quit.c send.c strings.c temp.c tty.c vars.c \
	version.c
HDRS=	rcv.h configdefs.h def.h glob.h v7.local.h local.h
ALL=	Mail fmt
S = $(SRCS) $(HDRS)

#
# Special massaging of C files for sharing of strings
#
.c.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c 
	mv x.o $*.o
	@$(RM) -f x.c

all:	${ALL}

Mail:	$S $(OBJS)
	$(RM) -f Mail
	@echo Loading ...
	@$(CC) -o Mail $(OBJS) $(LIBES)
	@size Mail

install: all
	install -s Mail ${DESTDIR}/usr/ucb/Mail
	cp misc/Mail.* ${DESTDIR}/usr/lib
	install -s fmt ${DESTDIR}/usr/ucb/fmt
	cd ${DESTDIR}/usr/ucb; rm -f mail; ln Mail mail

version.o:	version.c
	$(CC) -c -R version.c

$S:
	sccs get $@;

srcs:	$S

tags:	$(SRCS)
	${CTAGS} $(SRCS);

clean:
	$(RM) -f *.o
	rm -f Mail a.out fmt x.c xs.c tags core xhead.c
	cp /dev/null strings

lint:
	lint $(CFLAGS) $(SRCS)

fmt:	fmt.o xhead.o
	$(CC) $(CFLAGS) fmt.o xhead.o -o fmt
	size fmt

fmt.o:	fmt.c
	$(CC) $(CFLAGS) -c fmt.c

xhead.c:
	ln -s head.c xhead.c

xhead.o: xhead.c
	$(CC) $(CFLAGS) -c xhead.c

str.o: strings
	$(XSTR)
	$(CC) -R -c xs.c
	mv xs.o str.o
	$(RM) xs.c

cmdtab.o: cmdtab.c
	$(CC) -R -c $(CFLAGS) cmdtab.c

print:	$S fmt.c
	@$(VPRINT) Makefile rcv.h def.h glob.h local.h v7.local.h $(SRCS) fmt.c

wc:
	@wc rcv.h def.h glob.h local.h v7.local.h $(SRCS)

sc:
	@grep -c \; rcv.h def.h glob.h local.h v7.local.h $(SRCS)
