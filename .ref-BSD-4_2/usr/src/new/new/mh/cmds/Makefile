#	@(#)Makefile	4.9	(Berkeley)	83/07/07
#
# define UCB to get #files instead of ,files
#
CFLAGS= -O -DUNIXCOMP -DUCB -I.
BINDIR= /usr/new/mh
MHDIR=  /usr/new/lib/mh
SUBS = ../subs
STRINGS = ../strings
PROGS=	comp refile folder forw inc install-mh \
	next news pick prev prompter repl rescue rmf rmm scan \
	send show unixtomh
DESTDIR=

all:	${PROGS}

subs.a: mh.h
	rm -f ${SUBS}/subs.a subs.a
	cd ${SUBS}; make
	ranlib ${SUBS}/subs.a
	ln -s ${SUBS}/subs.a subs.a

strings.a:
	rm -f ${STRINGS}/strings.a strings.a
	cd ${STRINGS}; make
	ranlib ${STRINGS}/strings.a
	ln -s ${STRINGS}/strings.a strings.a

comp:   mh.h comp.o subs.a strings.a
	${CC} -o comp comp.o subs.a strings.a

refile:   mh.h refile.o subs.a strings.a
	${CC} -o refile refile.o subs.a strings.a

folder: mh.h folder.o subs.a strings.a
	${CC} -o folder folder.o subs.a strings.a

forw:   mh.h forw.o annotate.o subs.a strings.a
	${CC} -o forw forw.o annotate.o subs.a strings.a

inc:    mh.h inc.o scansub.o subs.a strings.a
	${CC} -o inc inc.o scansub.o subs.a strings.a

install-mh: mh.h install-mh.o subs.a strings.a
	${CC} -o install-mh install-mh.o subs.a strings.a

news:   mh.h news.o subs.a strings.a
	${CC} -o news news.o subs.a strings.a

next:   mh.h nexthdr.o next.o subs.a strings.a
	${CC} -o next nexthdr.o next.o subs.a strings.a

pick:   mh.h pick.o grep.o subs.a strings.a
	${CC} -o pick pick.o grep.o subs.a strings.a

prev:   mh.h prevhdr.o next.o subs.a strings.a
	${CC} -o prev prevhdr.o next.o subs.a strings.a

prompter: mh.h prompter.o subs.a strings.a
	${CC} -o prompter prompter.o subs.a strings.a

repl:   mh.h reply.o replsubs.o annotate.o subs.a strings.a
	${CC} -o repl reply.o replsubs.o annotate.o subs.a strings.a

rescue:	rescue.o emitl.o
	${CC} -o rescue rescue.o

rmf:    mh.h rmf.o subs.a strings.a
	${CC} -o rmf rmf.o subs.a strings.a

rmm:    mh.h rmm.o subs.a strings.a
	${CC} -o rmm rmm.o subs.a strings.a

scan:   mh.h scan.o scansub.o subs.a strings.a
	${CC} -o scan scan.o scansub.o subs.a strings.a

send:   mh.h send.o subs.a strings.a
	${CC} -o send send.o subs.a strings.a

show:   mh.h show.o subs.a strings.a
	${CC} -o show show.o subs.a strings.a

unixtomh: unixtomh.o
	${CC} -o unixtomh unixtomh.o subs.a

onceonly:
	-@if [ ! -f ${DESTDIR}/usr/bin/filetype -a \
	       ! -f ${DESTDIR}/bin/filetype -a -f ${DESTDIR}/bin/file ] ; then \
		mv ${DESTDIR}/bin/file ${DESTDIR}/bin/filetype; \
		echo "${DESTDIR}/bin/file => ${DESTDIR}/bin/filetype" ; \
	 else true ; \
	 fi
	-@if [ ! -f ${DESTDIR}/bin/filetype -a \
	       ! -f ${DESTDIR}/usr/bin/filetype -a \
		 -f ${DESTDIR}/usr/bin/file ] ; then \
		mv ${DESTDIR}/usr/bin/file ${DESTDIR}/usr/bin/filetype; \
		echo "${DESTDIR}/usr/bin/file => ${DESTDIR}/usr/bin/filetype"; \
	 else true ; \
	 fi
	-@if [ ! -f ${DESTDIR}/bin/bellmail -a -f ${DESTDIR}/bin/mail ] ; then \
		mv ${DESTDIR}/bin/mail ${DESTDIR}/bin/bellmail; \
		echo "${DESTDIR}/bin/mail => ${DESTDIR}/bin/bellmail" ;\
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/comp ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/comp" ; \
	 else true ; \
	 fi
#       -@if [ -f ${DESTDIR}/${BINDIR}/dist ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/dist" ; \
	 else true ; \
	 fi
#       -@if [ -f ${DESTDIR}/${BINDIR}/file ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/file" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/folder ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/folder" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/forw ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/forw" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/inc  ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/inc"  ; \
	 else true ; \
	 fi
#       -@if [ -f ${DESTDIR}/${BINDIR}/mail ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/mail" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/news ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/news" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/next ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/next" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/pick ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/pick" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/prev ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/prev" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/prompter ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/prompter" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/repl ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/repl" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/rmf  ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/rmf"  ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/rmm  ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/rmm"  ; \
	  else true ; \
	  fi
	-@if [ -f ${DESTDIR}/${BINDIR}/scan ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/scan" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/send ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/send" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${BINDIR}/show ] ; then \
		echo "Collision on ${DESTDIR}/${BINDIR}/show" ; \
	 else true ; \
	 fi
	-@if [ -f ${DESTDIR}/${MHDIR} -o -d ${DESTDIR}/${MHDIR} ] ; then \
	     echo "${DESTDIR}/${MHDIR} already exists!"; \
	 else true ; \
	 fi

install:
	-mkdir ${DESTDIR}/${BINDIR}
	-mkdir ${DESTDIR}/${MHDIR}
	install -s -m 1755 comp ${DESTDIR}/${BINDIR}/comp;
	install -s refile ${DESTDIR}/${BINDIR}/refile;
	install -s folder ${DESTDIR}/${BINDIR}/folder;
	install -s forw ${DESTDIR}/${BINDIR}/forw;
	install -s -m 1755 inc ${DESTDIR}/${BINDIR}/inc;
	install -s install-mh ${DESTDIR}/${MHDIR}/install-mh;
	install -s -m 1755 next ${DESTDIR}/${BINDIR}/next;
	install -s news ${DESTDIR}/${BINDIR}/news;
	install -s pick ${DESTDIR}/${BINDIR}/pick;
	install -s prev ${DESTDIR}/${BINDIR}/prev;
	install -s -m 1755 prompter ${DESTDIR}/${BINDIR}/prompter;
	install -s repl ${DESTDIR}/${BINDIR}/repl;
	install -s rmf ${DESTDIR}/${BINDIR}/rmf;
	install -s rmm ${DESTDIR}/${BINDIR}/rmm;
	install -s scan ${DESTDIR}/${BINDIR}/scan;
	install -s send ${DESTDIR}/${BINDIR}/send;
	install -s -m 1755 show ${DESTDIR}/${BINDIR}/show;
	install -s unixtomh ${DESTDIR}/${MHDIR}/unixtomh;
	rm -f ${DESTDIR}/${BINDIR}/folders
	ln ${DESTDIR}/${BINDIR}/folder ${DESTDIR}/${BINDIR}/folders
	install -c ../support/components ${DESTDIR}/${MHDIR}/components;
	install -c ../support/distcomps ${DESTDIR}/${MHDIR}/distcomps;

cleanup:
	rm -f ${PROGS}

clean:
	cd ${SUBS}; make ${MFLAG} clean
	cd ${STRINGS}; make ${MFLAG} clean
	rm -f ${PROGS} subs.a strings.a *.o
