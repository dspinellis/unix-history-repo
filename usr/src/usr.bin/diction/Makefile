#
#	@(#)Makefile	4.6	(Berkeley)	87/05/31
#
#	make file for STYLE and DICTION
#
#	BIN is the directory for storing the object code
#		for the 3 programs that make up style
#	DICT is the full pathname of the file dict.d, the
#		dictionary file used by DICTION
#
DESTDIR=
SRCS = \
	abbrev.c conp.h dict.c dict.d \
	diction.c diction.sh edict.c ehash.c \
	explain.d \
	end.l getopt.c names.h nhash.c \
	nwords.l outp.c part.l pscan.c \
	style.h style.sh explain.sh ydict.c \
	README

BIN = ${DESTDIR}/usr/lib
DICT= -DDICT=\"/usr/lib/dict.d\"

all: diction style

style: style1 style2 style3

install: style diction style.sh diction.sh explain.sh dict.d explain.d
	install -s style1 ${DESTDIR}/usr/lib
	install -s style2 ${DESTDIR}/usr/lib
	install -s style3 ${DESTDIR}/usr/lib
	install -s dprog ${DESTDIR}/usr/lib
	install -c style.sh ${DESTDIR}/usr/bin/style
	install -c diction.sh ${DESTDIR}/usr/bin/diction
	install -c explain.sh ${DESTDIR}/usr/bin/explain
	install -c dict.d ${DESTDIR}/usr/lib
	install -c explain.d ${DESTDIR}/usr/lib
#	ln ${DESTDIR}/usr/bin/explain ${DESTDIR}/usr/bin/suggest

clean:
	rm -f prtlex.c wdslex.c endlex.c
	rm -f *.o style1 style2 style3 dprog

depend:

style3: prtlex.o pscan.o outp.o
	cc prtlex.o pscan.o outp.o -ll  -o style3
prtlex.c: part.l
	lex part.l
	mv lex.yy.c prtlex.c
prtlex.o: names.h prtlex.c conp.h style.h names.h
	cc -c -O prtlex.c
pscan.o: names.h conp.h pscan.c
	cc -c -O pscan.c
outp.o: names.h conp.h style.h outp.c
	cc -c -O outp.c

style1: wdslex.o
	cc wdslex.o -ll  -o style1
wdslex.o: wdslex.c nhash.c dict.c ydict.c names.h abbrev.c
	cc -c wdslex.c
wdslex.c: nwords.l
	lex nwords.l
	mv lex.yy.c wdslex.c

style2: endlex.o
	cc endlex.o -ll  -o style2
endlex.o: names.h endlex.c ehash.c edict.c
	cc -c endlex.c
endlex.c: end.l
	lex end.l
	mv lex.yy.c endlex.c

diction: dprog
dprog: diction.c
	cc -O $(DICT) diction.c -o dprog

srcs:	sources

sources: $(SRCS)

$(SRCS):
	sccs get $@

