##  $Revision: 1.21 $
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

##  =()<INND	= @<_PATH_INND>@>()=
INND	= /usr/contrib/news/innd
##  =()<INNDSTART	= @<_PATH_INNDSTART>@>()=
INNDSTART	= /usr/contrib/news/inndstart
OWNER	= -O bin -G bin

##  =()<LIBS	= @<LIBS>@>()=
LIBS	= 
LIBNEWS	= ../libinn.a
LINTLIB	= ../llib-linn.ln

SOURCES	= \
	art.c cc.c chan.c his.c icd.c innd.c lc.c nc.c newsfeeds.c ng.c \
	proc.c rc.c site.c

OBJECTS	= \
	art.o cc.o chan.o his.o icd.o innd.o lc.o nc.o newsfeeds.o ng.o \
	proc.o rc.o site.o

ALL	= innd inndstart

all:		$(ALL)

install:	$(INND) $(INNDSTART)

clobber clean:
	rm -f *.o $(ALL)
	rm -f inndp profiled
	rm -f all install lint

tags ctags:	$(SOURCES)
	$(CTAGS) $(SOURCES) ../lib/*.c innd.h ../include/*.h

innd:		$(P) $(OBJECTS) $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LIBNEWS) $(LIBS)

inndstart:	$(P) inndstart.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ inndstart.o $(LIBNEWS) $(LIBS)

lint:		$(ALL)
	@rm -f lint
	lint $(LINTFLAGS) $(SOURCES)  $(LINTLIB) $(LINTFILTER)  >lint
	lint $(LINTFLAGS) inndstart.c $(LINTLIB) $(LINTFILTER) >>lint

../include/dbz.h:
	(cd ../lib ; $(MAKE) ../include/dbz.h)
$(LIBNEWS) $(LINTLIB):
	(cd ../lib ; $(MAKE) install)

##  Profiling.  The rules are a bit brute-force, but good enough.
profiled:	inndp
	date >$@

inndp:		$(SOURCES)
	rm -f $(OBJECTS)
	$(MAKE) innd CFLAGS="$(CFLAGS) $(PROF)" LIBNEWS=../libinn_p.a
	mv innd inndp
	rm -f $(OBJECTS)

##
ccenter:	$(SOURCES)
	#load $(CFLAGS) $(SOURCES) $(LIBNEWS)

##  Low-level install actions.
$(INND):	innd
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(INNDSTART):	inndstart
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@

##  Dependencies.  Default list, below, is probably good enough.
depend:		Makefile $(SOURCES) ../include/dbz.h
	makedepend $(DEFS) $(SOURCES)

# DO NOT DELETE THIS LINE -- make depend depends on it.
$(OBJECTS):	innd.h ../include/clibrary.h ../include/configdata.h \
		../include/libinn.h ../include/logging.h \
		../include/macros.h ../include/nntp.h ../include/paths.h
ng.o:		../include/mydir.h
inndstart.o:	../include/clibrary.h ../include/configdata.h \
		../include/libinn.h ../include/logging.h \
		../include/macros.h ../include/paths.h
