#
#	@(#)Makefile	4.2	(Berkeley)	82/11/06
#
#	make file for STYLE and DICTION
#
#	BIN is the directory for storing the object code
#		for the 3 programs that make up style
#	DICT is the full pathname of the file dict.d, the
#		dictionary file used by DICTION
#
BIN = ${DESTDIR}/usr/lib
DICT= -DDICT=\"/usr/lib/dict.d\"

all: diction style suggest deroff
style: style1 style2 style3

install: style diction
	install -s style1 ${DESTDIR}/usr/lib
	install -s style2 ${DESTDIR}/usr/lib
	install -s style3 ${DESTDIR}/usr/lib
	install -s dprog ${DESTDIR}/usr/lib
	install -s deroff ${DESTDIR}/usr/lib
	rm -f ${DESTDIR}/usr/bin/deroff
	ln ${DESTDIR}/usr/lib/deroff ${DESTDIR}/usr/bin/deroff
	install -c style ${DESTDIR}/usr/bin
	install -c diction ${DESTDIR}/usr/bin
	install -c explain ${DESTDIR}/usr/bin
	install -c dict.d ${DESTDIR}/usr/lib
	install -c explain.d ${DESTDIR}/usr/lib
#	ln ${DESTDIR}/usr/bin/explain ${DESTDIR}/usr/bin/suggest
#	ln ${DESTDIR}/usr/lib/explain.d ${DESTDIR}/usr/bin/suggest.d

clean:
	rm -f prtlex.c wdslex.c endlex.c
	rm -f *.o style1 style2 style3 dprog

style3: prtlex.o pscan.o outp.o
	cc prtlex.o pscan.o outp.o -ll  -o style3
prtlex.c: part.l
	lex part.l
	mv lex.yy.c prtlex.c
prtlex.o: names.h prtlex.c
	cc -c -O prtlex.c
pscan.o: names.h conp.h
	cc -c -O pscan.c
outp.o: names.h conp.h style.h 
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
	cc -O $(DICT) diction.c -i -o dprog

deroff:	deroff.c getopt.c
	cc -O deroff.c  getopt.c -i -o deroff
