##  $Revision: 1.16 $
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

##  =()<NNRPD	= @<_PATH_NNRPD>@>()=
NNRPD	= /usr/contrib/news/nnrpd
OWNER	= -O bin -G bin

##  =()<LIBS	= @<LIBS>@>()=
LIBS	= 
LIBNEWS	= ../libinn.a
LINTLIB	= ../llib-linn.ln

SOURCES	= \
	article.c group.c commands.c misc.c newnews.c nnrpd.c post.c loadave.c

OBJECTS	= \
	article.o group.o commands.o misc.o newnews.o nnrpd.o post.o loadave.o

ALL	= nnrpd

all:		$(ALL)

install:	$(NNRPD)

##  Low-level install actions.
$(NNRPD):	nnrpd
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@

clobber clean:
	rm -f *.o $(ALL)
	rm -f nnrpd nnrpdp profiled
	rm -f all install lint

tags ctags:	$(SOURCES)
	$(CTAGS) $(SOURCES) ../lib/*.c nnrpd.h ../include/*.h

nnrpd:		$(P) $(OBJECTS) $(LIBNEWS)
	@rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LIBNEWS) $(LIBS)

lint:		$(ALL)
	@rm -f lint
	lint $(LINTFLAGS) $(SOURCES) $(LINTLIB) $(LINTFILTER) >lint

../include/dbz.h:
	(cd ../lib ; $(MAKE) ../include/dbz.h)
$(LIBNEWS) $(LINTLIB):
	(cd ../lib ; $(MAKE) install )

##  Profiling.  The rules are a bit brute-force, but good enough.
profiled:	nnrpdp
	date >$@

nnrpdp:		$(SOURCES)
	rm -f $(OBJECTS)
	$(MAKE) nnrpd CFLAGS="$(CFLAGS) $(PROF)" LIBNEWS=../libinn_p.a
	mv nnrpd nnrpdp
	rm -f $(OBJECTS)

ccenter:	$(SOURCES)
	#load $(CFLAGS) $(SOURCES) $(LIBNEWS)

##  Dependencies.  Default list, below, is probably good enough.
depend:		Makefile $(SOURCES)
	makedepend $(DEFS) $(SOURCES)

# DO NOT DELETE THIS LINE -- make depend depends on it.
$(OBJECTS):	nnrpd.h \
		../include/clibrary.h ../include/configdata.h \
		../include/libinn.h ../include/logging.h \
		../include/macros.h ../include/nntp.h \
		../include/paths.h ../include/qio.h
group.o:	../include/mydir.h
misc.o:		../include/dbz.h
