#
# Version 7
#
CFLAGS=-O -I.
XSTR=/usr/ucb/xstr
CTAGS=/usr/ucb/ctags
ED=-ed
AS=-as
RM=-rm
OBJS=aux.o cmd1.o cmd2.o cmd3.o cmdtab.o collect.o edit.o fio.o \
	getname.o head.o v7.local.o names.o lex.o list.o main.o quit.o send.o \
	strings.o temp.o tty.o vars.o version.o \
	errlst.o str.o

#
# Special massaging of C files for sharing of strings
#
.c.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c 
	mv x.o $*.o

all:	fmt Mail tags

Mail:	$(OBJS) tags
	$(CC) -n -o Mail $(OBJS) -s

tags:
	${CTAGS} *.c;

install: all
	cp Mail /usr/ucb/Mail
	cp fmt /usr/ucb/fmt
	cp Mail.help /usr/lib/Mail.help
	cp Mail.help.~ /usr/lib/Mail.help.~
	cat /dev/null >> /usr/lib/Mail.rc
	echo you should add a line of the form
	echo "	" "'set append'"
	echo to /usr/lib/Mail.rc if the standard system mailer
	echo adds new mail at the end of the mail file

clean:
	$(RM) -f *.o
	$(RM) -f Mail a.out strings fmt x.c xs.c tags

lint:
	lint aux.c cmd1.c cmd2.c cmd3.c cmdtab.c collect.c edit.c fio.c \
head.c getname.c names.c lex.c list.c main.c quit.c send.c strings.c \
temp.c tty.c

fmt:	fmt.c head.c
	$(CC) -I/usr/include/retrofit fmt.c head.c -o fmt
	$(RM) fmt.o head.o

print:
	pr bugs
	pr makefile
	ls -l | pr -h "RCV directory"
	sh printhem

str.o: strings
	$(XSTR)
	$(CC) -S xs.c
	$(ED) - <:rofix xs.s
	$(AS) -o str.o xs.s
	$(RM) xs.s

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

#
# Make the version file.  /tmp is always changing.
#
version.c:	/tmp
	/bin/sh version > version.c
