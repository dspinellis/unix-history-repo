#
# Unix version 7.
#
# Sccs Id = "@(#)Makefile	1.4 %G%";
#

CFLAGS=-O -DVMUNIX -DV7
XSTR=/usr/ucb/xstr
DESTDIR=/usr/src
CTAGS=/usr/ucb/ctags -w
VPRINT=/usr/ucb/vprint
LIBES=-ljobs
ED=-ed
AS=-as
RM=-rm
OBJS=aux.o cmd1.o cmd2.o cmd3.o cmdtab.o collect.o edit.o fio.o \
	getname.o head.o v7.local.o lock.o optim.o names.o lex.o list.o main.o \
	quit.o send.o strings.o temp.o tty.o vars.o version.o \
	errlst.o str.o

SRCS=aux.c cmd1.c cmd2.c cmd3.c cmdtab.c collect.c edit.c fio.c \
	getname.c head.c v7.local.c lock.c optim.c names.c lex.c list.c main.c \
	quit.c send.c strings.c temp.c tty.c vars.c errlst.c

HDRS=rcv.h def.h glob.h v7.local.h local.h

S = $(SRCS) $(HDRS)

#
# Special massaging of C files for sharing of strings
#
.c.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c 
	mv x.o $*.o

Mail:	$S $(OBJS) tags
	$(RM) -f Mail
	$(CC) -n -o Mail $(OBJS) $(LIBES)
	@size Mail

install:
	install -s Mail ${DESTDIR}/usr/ucb
	cp Mail.help* ${DESTDIR}/usr/lib
	cd ${DESTDIR}/usr/ucb; rm -f mail; ln Mail mail

$S:
	sccs get $@

tags:
	${CTAGS} *.c;

clean:
	$(RM) -f *.o
	rm -f Mail a.out fmt x.c xs.c tags core
	cp /dev/null strings
	cp /dev/null version.c

lint:
	lint $(CFLAGS) $(SRCS)

fmt:	fmt.c head.c
	$(CC) fmt.c head.c -o fmt
	$(RM) fmt.o head.o
	size fmt

str.o: strings
	$(XSTR)
	$(CC) -S xs.c
	$(ED) - <:rofix xs.s
	$(AS) -o str.o xs.s
	$(RM) xs.s
	$(RM) xs.c

cmdtab.o: cmdtab.c
	$(CC) -S $(CFLAGS) cmdtab.c
	$(ED) - <:rofix cmdtab.s
	$(AS) -o cmdtab.o cmdtab.s
	$(RM) cmdtab.s

errlst.o: errlst.c
	${CC} -E ${CFLAGS} errlst.c | ${XSTR} -c -
	${CC} ${CFLAGS} -S x.c
	${ED} - <:rofix x.s
	${AS} -o errlst.o x.s
	${RM} x.s
	${RM} x.c

#
# Make the version file.  /tmp is always changing.
#
version.c: #	/tmp
	/bin/sh version > version.c

print:
	@$(VPRINT) makefile rcv.h def.h glob.h local.h v7.local.h $(SRCS) fmt.c;

wc:
	@wc rcv.h def.h glob.h local.h v7.local.h $(SRCS)

sc:
	@grep -c \; rcv.h def.h glob.h local.h v7.local.h $(SRCS)
