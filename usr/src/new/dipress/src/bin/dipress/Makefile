#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
CFLAGS = -g
DESTBIN=../../../bin
DESTINCLUDE=../../../include
DESTLIB=../../../lib

dipress: font.o parser.o signals.o main.o ipress.o bitmap.o  bitdraw.o globals.o $(DESTLIB)/libip.a  $(DESTLIB)/libgetopt.a
	$(CC) $(CFLAGS) -o dipress font.o parser.o signals.o main.o  bitdraw.o ipress.o bitmap.o  globals.o $(DESTLIB)/libip.a $(DESTLIB)/libgetopt.a -lm

ipress.o: ipress.c deviceinfo.h $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h externs.h defs.h
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c ipress.c

font.o: font.c $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h deviceinfo.h externs.h defs.h
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c font.c

bitdraw.o: defs.h externs.h bitdraw.c
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c bitdraw.c

signals.o: defs.h externs.h signals.c
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c signals.c

main.o: externs.h defs.h main.c
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c main.c

parser.o: externs.h defs.h parser.c
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c parser.c

bitmap.o: bitmap.c $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c bitmap.c

install: $(DESTBIN)/dipress

$(DESTBIN)/dipress: dipress
	install -c -s dipress $(DESTBIN)/dipress

clean:
	rm -f dipress *.o *.BAK *.CKP
	(cd Test; make clean)

lint:
	lint -ab -I$(DESTINCLUDE) parser.c signals.c main.c bitdraw.c ipress.c bitmap.c  globals.c $(DESTLIB)/llib-lip.ln $(DESTLIB)/llib-lgetopt.ln -lm -lc

print: Makefile defs.h deviceinfo.h types.h externs.h globals.c main.c signals.c parser.c bitdraw.c iptroff ipress.c font.c
	maha Makefile defs.h deviceinfo.h types.h externs.h globals.c main.c signals.c font.c bitdraw.c  parser.c ipress.c 
