##  $Revision: 1.14 $
SHELL	= /bin/sh
MAKE	= make
##  =()<P	= @<P>@>()=
P	= 

##  =()<CC	= @<CC>@>()=
CC	= cc
##  =()<DEFS	= @<DEFS>@>()=
DEFS	= -I../include
##  =()<CFLAGS	= @<CFLAGS>@>()=
CFLAGS	= $(DEFS) -g
##  =()<LDFLAGS	= @<LDFLAGS>@>()=
LDFLAGS	= -g
##  =()<LINTFLAGS	= @<LINTFLAGS>@>()=
LINTFLAGS	= -b -h -z $(DEFS)
##  =()<LINTFILTER	= @<LINTFILTER>@>()=
LINTFILTER	= | sed -n -f ../sedf.sun
##  =()<CTAGS		= @<CTAGS>@>()=
CTAGS		= ctags -t -w
##  =()<PROF	= @<PROF>@>()=
PROF	= -pg

##  =()<NEWSBIN	= @<_PATH_NEWSBIN>@>()=
NEWSBIN	= /usr/contrib/news
OWNER	= -O bin -G bin

##  =()<LIBS	= @<LIBS>@>()=
LIBS	= 
LIBNEWS	= ../libinn.a
LINTLIB	= ../llib-linn.ln

SOURCES	=	convdate.c expire.c expireover.c fastrm.c grephistory.c \
		makeactive.c makehistory.c newsrequeue.c prunehistory.c
ALL	=	convdate expire expireover fastrm grephistory \
		makeactive makehistory newsrequeue prunehistory

all:		$(ALL)

install:	$(NEWSBIN)/convdate $(NEWSBIN)/expire $(NEWSBIN)/expireover \
		$(NEWSBIN)/fastrm $(NEWSBIN)/grephistory $(NEWSBIN)/makeactive \
		$(NEWSBIN)/makehistory $(NEWSBIN)/makehistory \
		$(NEWSBIN)/newsrequeue $(NEWSBIN)/prunehistory

clobber clean:
	rm -f *.o $(ALL)
	rm -f expirep expireoverp profiled
	rm -f all install lint

tags ctags:	$(SOURCES)
	$(CTAGS) $(SOURCES) ../lib/*.c ../include/*.h

convdate:	$(P) convdate.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ convdate.o $(LIBNEWS) $(LIBS)

expire:		$(P) expire.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ expire.o $(LIBNEWS) $(LIBS)

expireover:	$(P) expireover.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ expireover.o $(LIBNEWS) $(LIBS)

fastrm:	$(P) fastrm.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ fastrm.o $(LIBNEWS) $(LIBS)

grephistory:	$(P) grephistory.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ grephistory.o $(LIBNEWS) $(LIBS)

makeactive:	$(P) makeactive.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ makeactive.o $(LIBNEWS) $(LIBS)

makehistory:	$(P) makehistory.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ makehistory.o $(LIBNEWS) $(LIBS)

newsrequeue:	$(P) newsrequeue.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ newsrequeue.o $(LIBNEWS) $(LIBS)

prunehistory:	$(P) prunehistory.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ prunehistory.o $(LIBNEWS) $(LIBS)

lint:		$(ALL)
	@rm -f lint
	lint $(LINTFLAGS) convdate.c     $(LINTLIB) $(LINTFILTER)  >lint
	lint $(LINTFLAGS) expire.c       $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) expireover.c   $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) fastrm.c       $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) grephistory.c  $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) makeactive.c   $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) makehistory.c  $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) newsrequeue.c  $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) prunehistory.c $(LINTLIB) $(LINTFILTER) >>lint

../include/dbz.h:
	(cd ../lib ; $(MAKE) ../include/dbz.h)
$(LIBNEWS) $(LINTLIB):
	(cd ../lib ; $(MAKE) install)

##  Profiling.  The rules are a bit brute-force, but good enough.
profiled:	expirep expireoverp
	date >$@

NEWFLAGS	= CFLAGS="$(CFLAGS) $(PROF)" LDFLAGS="$(LDFLAGS) $(PROF)"

expirep:	expire.c
	rm -f expire.o
	$(MAKE) expire $(NEWFLAGS) LIBNEWS=../libinn_p.a
	mv expire expirep
	rm -f expire.o

expireoverp:	expireover.c
	rm -f expireover.o
	$(MAKE) expireover $(NEWFLAGS) LIBNEWS=../libinn_p.a
	mv expireover expireoverp
	rm -f expireover.o

##
.SUFFIXES: .c .src
.c.src:
	#load $(CFLAGS) $<

##  Low-level install actions.
$(NEWSBIN)/convdate:		convdate
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/expire:		expire
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/expireover:		expireover
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/fastrm:		fastrm
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/grephistory:		grephistory
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/makeactive:		makeactive
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/makehistory:		makehistory
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/newsrequeue:		newsrequeue
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/prunehistory:	prunehistory
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@

##  Dependencies.  Default list, below, is probably good enough.
depend:		Makefile $(SOURCES)
	makedepend $(DEFS) $(SOURCES)

# DO NOT DELETE THIS LINE -- make depend depends on it.
convdate.o:	../include/clibrary.h
convdate.o:	../include/configdata.h
convdate.o:	../include/libinn.h
convdate.o:	../include/macros.h
expire.o:	../include/clibrary.h
expire.o:	../include/configdata.h
expire.o:	../include/dbz.h
expire.o:	../include/inndcomm.h
expire.o:	../include/libinn.h
expire.o:	../include/macros.h
expire.o:	../include/paths.h
expire.o:	../include/qio.h
expireover.o:	../include/clibrary.h
expireover.o:	../include/configdata.h
expireover.o:	../include/libinn.h
expireover.o:	../include/macros.h
expireover.o:	../include/mydir.h
expireover.o:	../include/paths.h
expireover.o:	../include/qio.h
fastrm.o:	../include/clibrary.h
fastrm.o:	../include/configdata.h
fastrm.o:	../include/libinn.h
fastrm.o:	../include/macros.h
fastrm.o:	../include/mydir.h
grephistory.o:	../include/clibrary.h
grephistory.o:	../include/configdata.h
grephistory.o:	../include/dbz.h
grephistory.o:	../include/libinn.h
grephistory.o:	../include/macros.h
grephistory.o:	../include/paths.h
makeactive.o:	../include/clibrary.h
makeactive.o:	../include/configdata.h
makeactive.o:	../include/libinn.h
makeactive.o:	../include/macros.h
makeactive.o:	../include/mydir.h
makeactive.o:	../include/paths.h
makehistory.o:	../include/clibrary.h
makehistory.o:	../include/configdata.h
makehistory.o:	../include/dbz.h
makehistory.o:	../include/libinn.h
makehistory.o:	../include/macros.h
makehistory.o:	../include/paths.h
makehistory.o:	../include/qio.h
newsrequeue.o:	../include/clibrary.h
newsrequeue.o:	../include/configdata.h
newsrequeue.o:	../include/dbz.h
newsrequeue.o:	../include/libinn.h
newsrequeue.o:	../include/macros.h
newsrequeue.o:	../include/paths.h
newsrequeue.o:	../include/qio.h
prunehistory.o:	../include/clibrary.h
prunehistory.o:	../include/configdata.h
prunehistory.o:	../include/dbz.h
prunehistory.o:	../include/libinn.h
prunehistory.o:	../include/macros.h
prunehistory.o:	../include/paths.h
prunehistory.o:	../include/paths.h
