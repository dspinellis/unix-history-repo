#
# Computer center unix A
#
# Sccs Id = "@(#)makefile.cc	2.1 %G%";
#

CFLAGS=-O -DCC -I/usr/include/retrofit -Dsignal=Signal
XSTR=/usr/new/xstr
DESTDIR=/usr/new
CTAGS=/usr/new/ctags
ED=-ed
AS=-as
RM=-rm
OBJS=aux.o cmd1.o cmd2.o cmd3.o cmdtab.o collect.o edit.o fio.o \
	head.o cc.local.o getname.o lex.o list.o main.o names.o optim.o \
	quit.o send.o strings.o temp.o tty.o vars.o version.o \
	Signal.o errlst.o str.o

SRCS=aux.c cmd1.c cmd2.c cmd3.c cmdtab.c collect.c edit.c fio.c \
	head.c cc.local.c getname.c lex.c list.c main.c names.c optim.c \
	quit.c send.c strings.c temp.c tty.c vars.c version.c \
	Signal.c errlst.c str.c

#
# Special massaging of C files for sharing of strings
#
.c.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c 
	mv x.o $*.o

all:	Mail fmt

install:	all
	cp Mail $(DESTDIR)
	cp fmt $(DESTDIR)

Mail:	$(OBJS) tags
	$(CC) -n -o Mail $(OBJS) -lretro -lS
	@size Mail

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
	$(CC) $(CFLAGS) fmt.c head.c -o fmt -lretro -lS
	$(RM) fmt.o head.o
	size fmt

str.o: strings
	$(XSTR)
	$(CC) -S xs.c
	$(ED) - <:rofix xs.s
	$(AS) -o str.o xs.s
	$(RM) xs.s

cmdtab.o: cmdtab.c
	$(CC) -S $(CFLAGS) cmdtab.c
	$(ED) - <:rofix cmdtab.s
	$(AS) - -o cmdtab.o cmdtab.s
	$(RM) cmdtab.s

errlst.o: errlst.c
	${CC} -E ${CFLAGS} errlst.c | ${XSTR} -c -
	${CC} ${CFLAGS} -S x.c
	${ED} - <:rofix x.s
	${AS} -o errlst.o x.s
	${RM} x.s

#
# Make the version file.  /tmp is always changing.
#
version.c:	/tmp
	/bin/sh version > version.c

Signal.o:	Signal.c
	$(CC) -c -O Signal.c

print:
	@pr rcv.h def.h glob.h local.h cc.local.h $(SRCS)
	@pr fmt.c
