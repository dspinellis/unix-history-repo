##  $Revision: 1.17 $
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

##  =()<NEWSBIN	= @<_PATH_NEWSBIN>@>()=
NEWSBIN	= /usr/contrib/news
OWNER	= -O bin -G bin

##  =()<NNTPLIB	= @<NNTPLIB>@>()=
NNTPLIB	= 
##  =()<LIBS	= @<LIBS>@>()=
LIBS	= 
LIBNEWS	= ../libinn.a
LINTLIB	= ../llib-linn.ln

SOURCES	=	archive.c batcher.c buffchan.c cvtbatch.c filechan.c \
		innxmit.c nntpget.c overchan.c shlock.c   shrinkfile.c
ALL	=	archive   batcher   buffchan   cvtbatch   filechan   \
		innxmit   nntpget   overchan   shlock     shrinkfile


all:		$(ALL)

install:	$(NEWSBIN)/archive $(NEWSBIN)/batcher $(NEWSBIN)/buffchan \
		$(NEWSBIN)/cvtbatch $(NEWSBIN)/filechan $(NEWSBIN)/nntpget \
		$(NEWSBIN)/innxmit $(NEWSBIN)/overchan $(NEWSBIN)/shlock \
		$(NEWSBIN)/shrinkfile

clobber clean:
	rm -f *.o $(ALL)
	rm -f all install lint profiled

tags ctags:	$(SOURCES)
	$(CTAGS) $(SOURCES) ../lib/*.c ../include/*.h

profiled:
	cp /dev/null profiled

archive:	$(P) archive.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ archive.o $(LIBNEWS) $(LIBS)

batcher:	$(P) batcher.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ batcher.o $(LIBNEWS) $(LIBS)

buffchan:	$(P) buffchan.o map.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ buffchan.o map.o $(LIBNEWS) $(LIBS)

cvtbatch:	$(P) cvtbatch.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ cvtbatch.o $(LIBNEWS) $(LIBS)

filechan:	$(P) filechan.o map.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ filechan.o map.o $(LIBNEWS) $(LIBS)

nntpget:	$(P) nntpget.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ nntpget.o $(LIBNEWS) $(LIBS)

innxmit:	$(P) innxmit.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ innxmit.o $(LIBNEWS) $(LIBS)

overchan:	$(P) overchan.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ overchan.o $(LIBNEWS) $(LIBS)

shlock:		$(P) shlock.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ shlock.o $(LIBNEWS) $(LIBS)

shrinkfile:	$(P) shrinkfile.o $(LIBNEWS)
	$(CC) $(LDFLAGS) -o $@ shrinkfile.o $(LIBNEWS) $(LIBS)


lint:		$(ALL)
	@rm -f lint
	lint $(LINTFLAGS) archive.c        $(LINTLIB) $(LINTFILTER)  >lint
	lint $(LINTFLAGS) batcher.c        $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) buffchan.c map.c $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) cvtbatch.c       $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) filechan.c map.c $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) nntpget.c        $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) innxmit.c        $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) overchan.c       $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) shlock.c         $(LINTLIB) $(LINTFILTER) >>lint
	lint $(LINTFLAGS) shrinkfile.c     $(LINTLIB) $(LINTFILTER) >>lint

$(LIBNEWS) $(LINTLIB):
	(cd ../lib ; $(MAKE) install)

##  Low-level install actions.
$(NEWSBIN)/archive:	archive
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/batcher:	batcher
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/buffchan:	buffchan
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/cvtbatch:	cvtbatch
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/filechan:	filechan
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/nntpget:	nntpget
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/innxmit:	innxmit
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/overchan:	overchan
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/shlock:	shlock
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@
$(NEWSBIN)/shrinkfile:	shrinkfile
	$(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD $? $@

##  Dependencies.  Default list, below, is probably good enough.
depend:		Makefile $(SOURCES)
	makedepend $(DEFS) $(SOURCES)

# DO NOT DELETE THIS LINE -- make depend depends on it.
archive.o:	../include/clibrary.h
archive.o:	../include/configdata.h
archive.o:	../include/libinn.h
archive.o:	../include/macros.h
archive.o:	../include/paths.h
archive.o:	../include/qio.h
batcher.o:	../include/clibrary.h
batcher.o:	../include/configdata.h
batcher.o:	../include/libinn.h
batcher.o:	../include/logging.h
batcher.o:	../include/macros.h
batcher.o:	../include/paths.h
buffchan.o:	../include/clibrary.h
buffchan.o:	../include/configdata.h
buffchan.o:	../include/libinn.h
buffchan.o:	../include/macros.h
buffchan.o:	../include/paths.h
buffchan.o:	../include/qio.h
cvtbatch.o:	../include/clibrary.h
cvtbatch.o:	../include/configdata.h
cvtbatch.o:	../include/libinn.h
cvtbatch.o:	../include/macros.h
cvtbatch.o:	../include/paths.h
cvtbatch.o:	../include/qio.h
filechan.o:	../include/clibrary.h
filechan.o:	../include/configdata.h
filechan.o:	../include/libinn.h
filechan.o:	../include/macros.h
filechan.o:	../include/paths.h
innxmit.o:	../include/clibrary.h
innxmit.o:	../include/configdata.h
innxmit.o:	../include/dbz.h
innxmit.o:	../include/libinn.h
innxmit.o:	../include/logging.h
innxmit.o:	../include/macros.h
innxmit.o:	../include/nntp.h
innxmit.o:	../include/paths.h
innxmit.o:	../include/qio.h
nntpget.o:	../include/clibrary.h
nntpget.o:	../include/configdata.h
nntpget.o:	../include/dbz.h
nntpget.o:	../include/libinn.h
nntpget.o:	../include/macros.h
nntpget.o:	../include/nntp.h
nntpget.o:	../include/paths.h
overchan.o:	../include/configdata.h
overchan.o:	../include/libinn.h
overchan.o:	../include/clibrary.h
overchan.o:	../include/macros.h
overchan.o:	../include/paths.h
overchan.o:	../include/qio.h
shlock.o:	../include/clibrary.h
shlock.o:	../include/configdata.h
shrinkfile.o:	../include/clibrary.h
shrinkfile.o:	../include/configdata.h
shrinkfile.o:	../include/libinn.h
shrinkfile.o:	../include/macros.h
