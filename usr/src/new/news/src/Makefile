# '@(#)Makefile.dst	1.26	10/15/87'
# Generic Makefile.
# This is converted to USG/v7/etc by localize.sh
# which should at least be a copy of localize.v7 or localize.usg

# We MUST use the Bourne shell during the install phase
SHELL=/bin/sh

# definitions

OSTYPE = v7

# HOME is the user name whose home dir has all the news stuff in it.
HOME=	exptools
# Use the -DHOME line if you want dynamic lookup in /etc/passwd
#HOMENAME=	-DHOME=\"$(HOME)\"
HOMENAME=

# Support for Parallel Make files on a Sequent
#P=&

NEWSUSR = news
NEWSGRP = news
SPOOLDIR = /usr/spool/news
BATCHDIR = /usr/spool/batch
LIBDIR = /usr/new/lib/news
BINDIR = /usr/new
DESTDIR =
UUXFLAGS = -r -z -n -gd

LNRNEWS = ln
DEBUG =
SCCSID = -DSCCSID

SERVER= -DSERVER 

DEFS =	-DRNEWS=\"$(BINDIR)/rnews\" -DSPOOLDIR=\"$(SPOOLDIR)\" \
	-DBATCHDIR=\"$(BATCHDIR)\" -DLIBDIR=\"$(LIBDIR)\" \
	-DBINDIR=\"$(BINDIR)\" -DNEWSUSR=\"$(NEWSUSR)\" \
	-DNEWSGRP=\"$(NEWSGRP)\" ${SERVER}

# Order is important here
INSCOMPRESS=
INSCOMPRESS= rm -f $(DESTDIR)$(LIBDIR)/compress; ln -s /usr/ucb/compress $(DESTDIR)$(LIBDIR)/compress
COMPRESS= compress
COMPRESS=

VFORK=

CFLAGS = ${DEBUG}  ${DEFS} ${SCCSID} ${VFORK} ${NETINCLUDE}
LFLAGS = ${DEBUG}
LIBS = 
LINTFLAGS = -chba  ${DEFS} ${NETINCLUDE}


TERMLIB = -ltermlib
FIXACTIVE =


CLIENTLIB = clientlib.o nntp.o
MISC = uname.o ${CLIENTLIB}
OBJECTS = funcs.o funcs2.o header.o ndir.o $(MISC) getdate.o 
IOBJECTS = inews.o ifuncs.o iextern.o control.o fullname.o \
	$(OBJECTS) ipathinit.o 

POBJECTS = postnews.o rextern.o rpathinit.o funcs.o $(MISC)
ROBJECTS = readnews.o rfuncs.o rfuncs2.o rextern.o readr.o \
	process.o digest.o $(OBJECTS) rpathinit.o 
VOBJECTS = readnews.o rfuncs.o rfuncs2.o rextern.o process.o \
	$(OBJECTS) visual.o virtterm.o rpathinit.o 
EXPOBJS = expire.o header.o funcs.o getdate.o iextern.o \
	funcs2.o ndir.o $(MISC) epathinit.o 

CSRCS = /usr/src/new/nntp/common/clientlib.c nntp.c

SRCS = funcs.c funcs2.c header.c
ISRCS = inews.c ifuncs.c iextern.c control.c fullname.c  $(SRCS)
PSRCS = postnews.c rextern.c funcs.c $(CSRCS)
RSRCS = readnews.c rfuncs.c rfuncs2.c rextern.c readr.c process.c  \
	digest.c $(SRCS) $(CSRCS)
VSRCS = readnews.c rfuncs.c rfuncs2.c rextern.c process.c $(SRCS) \
	visual.c virtterm.c $(CSRCS)
ESRCS = expire.c header.c funcs.c iextern.c \
	funcs2.c
OSRCS = uurec.c recnews.c sendnews.c batch.c caesar.c \
	recmail.c compress.c



UTILS = uurec recnews sendnews expire batch caesar recmail \
	encode decode $(COMPRESS)
SCRIPTS = sendbatch rmgroup checkgroups
OTHERS = inews $(UTILS) $(SCRIPTS)
COMMANDS = readnews checknews postnews vnews

# dependencies
all: $(P)  $(COMMANDS)

install: all help vnews.help installit
	chmod 755 ./installit
	-test -d $(DESTDIR)$(BINDIR) || mkdir $(DESTDIR)$(BINDIR)
	-test -d $(DESTDIR)$(LIBDIR) || mkdir $(DESTDIR)$(LIBDIR)
	for i in $(COMMANDS); do \
		./installit -m 755 -o $(NEWSUSR) -g $(NEWSGRP) -s $$i \
			$(DESTDIR)$(BINDIR); \
	done
	cp help vnews.help $(DESTDIR)$(LIBDIR)
	for i in $(UTILS); do \
		./installit -m 755 -o $(NEWSUSR) -g $(NEWSGRP) -s $$i \
			$(DESTDIR)$(LIBDIR); \
	done
	for i in $(SCRIPTS); do \
		./installit -m 755 -o $(NEWSUSR) -g $(NEWSGRP) $$i \
		$(DESTDIR)$(LIBDIR); \
	done
	./installit -m 6755 -o $(NEWSUSR) -g $(NEWSGRP) -s inews \
		$(DESTDIR)$(LIBDIR)
	-rm -f $(DESTDIR)$(BINDIR)/rnews 
	${LNRNEWS} $(DESTDIR)$(LIBDIR)/inews $(DESTDIR)$(BINDIR)/rnews
	chmod 6755 $(DESTDIR)$(LIBDIR)/inews $(DESTDIR)$(BINDIR)/rnews
	$(INSCOMPRESS)

defs.h:	defs.dist localize.sh Makefile.dst Makefile
	sh localize.sh
	@echo Localize has been run. Restart the make.
	@exit 1

Makefile: localize.sh defs.dist Makefile.dst
	sh localize.sh
	@echo Localize has been run. Restart the make.
	@exit 1

update: install.sh makeactive.sh
	sh install.sh $(SPOOLDIR) $(LIBDIR) $(BINDIR) $(NEWSUSR) $(NEWSGRP) $(OSTYPE)
	chmod 6755 $(LIBDIR)/inews

clean:
	rm -f $(COMMANDS) $(OTHERS) *.o a.out
	rm -f core index errs getdate.c

lint:  ilint vlint rlint elint plint olint
ilint: defs.h params.h iparams.h header.h $(ISRCS) pathinit.c
	lint $(LINTFLAGS) $(ISRCS) -DINEW pathinit.c
vlint: defs.h params.h rparams.h header.h $(VSRCS) pathinit.c
	lint $(LINTFLAGS) $(VSRCS) -DREAD pathinit.c
rlint: defs.h params.h rparams.h header.h $(RSRCS) pathinit.c
	lint $(LINTFLAGS) $(RSRCS) -DREAD pathinit.c
elint: defs.h params.h iparams.h header.h $(ESRCS) pathinit.c
	lint $(LINTFLAGS) $(ESRCS) -DEXP pathinit.c
plint: defs.h params.h iparams.h header.h $(PSRCS) pathinit.c
	lint $(LINTFLAGS) $(PSRCS) -DREAD pathinit.c

olint: defs.h params.h iparams.h header.h $(OSRCS)
	for i in $(OSRCS); do lint $(LINTFLAGS) $$i; done

inews:  $(P) $(IOBJECTS)
	$(CC) $(LFLAGS) $(IOBJECTS) -o inews $(LIBS)  

readnews:  $(P) $(ROBJECTS)
	$(CC) $(LFLAGS) $(ROBJECTS) -o readnews $(LIBS) $(NETLIB)

clientlib.o: /usr/src/new/nntp/common/clientlib.c
	$(CC) $(CFLAGS) -c /usr/src/new/nntp/common/clientlib.c

nntp.o: nntp.c
	$(CC) $(CFLAGS) -c nntp.c

funcs.o:  funcs.c params.h defs.h header.h Makefile
	$(CC) $(CFLAGS) -c funcs.c

funcs2.o:  funcs2.c params.h defs.h header.h Makefile
	$(CC) $(CFLAGS) -c funcs2.c

getdate.o:  getdate.y defs.h
	@echo "expect 8 shift/reduce conflicts"
	yacc getdate.y
	mv y.tab.c getdate.c
	$(CC) $(CFLAGS) -c getdate.c
	-rm -f getdate.c

inews.o:  inews.c iparams.h defs.h params.h header.h Makefile
	$(CC) $(CFLAGS) -c inews.c

ifuncs.o:  ifuncs.c iparams.h defs.h params.h header.h Makefile
	$(CC) $(CFLAGS) $(HOMENAME) -c ifuncs.c

iextern.o:  iextern.c iparams.h defs.h params.h Makefile header.h
	$(CC) $(CFLAGS) $(HOMENAME) -c iextern.c

postnews: $(P) $(POBJECTS)
	$(CC) $(CFLAGS) $(LFLAGS) $(POBJECTS) -o postnews $(NETLIB)

postnews.o: postnews.c defs.h params.h header.h  Makefile
	$(CC) $(CFLAGS) -c postnews.c 

readnews.o:  readnews.c rparams.h defs.h params.h header.h Makefile
	$(CC) $(CFLAGS) $(HOMENAME) -c readnews.c

rfuncs.o:  rfuncs.c rparams.h defs.h params.h header.h Makefile
	$(CC) $(CFLAGS) -c rfuncs.c

rfuncs2.o:  rfuncs2.c rparams.h defs.h params.h header.h Makefile
	$(CC) $(CFLAGS) -c rfuncs2.c

rextern.o:  rextern.c rparams.h defs.h params.h Makefile header.h
	$(CC) $(CFLAGS) -c rextern.c

readr.o:  readr.c rparams.h defs.h params.h Makefile ndir.h header.h
	$(CC) $(CFLAGS) -c readr.c

checknews.o:  checknews.c defs.h header.h Makefile params.h
	$(CC) $(CFLAGS) -c checknews.c

vnews:	$(P) $(VOBJECTS)
	$(CC) $(LFLAGS) $(VOBJECTS)  -o $@  $(TERMLIB) $(LIBS) $(NETLIB)

visual.o:  visual.c rparams.h defs.h params.h ndir.h header.h Makefile
	$(CC) $(CFLAGS) -c visual.c

control.o:  control.c defs.h iparams.h params.h header.h Makefile
	$(CC) $(CFLAGS) -c control.c

logdir.o: logdir.c Makefile
	$(CC) $(CFLAGS) -c logdir.c

uname.o:  uname.c defs.h params.h header.h Makefile
	$(CC) $(CFLAGS) -c uname.c

ndir.o: ndir.c ndir.h defs.h
	$(CC) $(CFLAGS) -c ndir.c

uurec:  uurec.c defs.h Makefile
	$(CC) $(CFLAGS) $(LFLAGS) uurec.c -o uurec

recnews:  recnews.c defs.h header.h params.h Makefile
	$(CC) $(CFLAGS) $(LFLAGS) recnews.c -o recnews

sendnews:  sendnews.o defs.h
	$(CC) $(LFLAGS) sendnews.o -o sendnews

batch:  batch.c Makefile defs.h
	$(CC) $(CFLAGS) $(LFLAGS) batch.c -o batch

encode:	encode.c
	$(CC) $(CFLAGS) $(LFLAGS) encode.c -o encode

decode:	decode.c
	$(CC) $(CFLAGS) $(LFLAGS) decode.c -o decode

caesar:  caesar.c Makefile
	$(CC) $(CFLAGS) $(LFLAGS) caesar.c -o caesar -lm

compress: compress.c
	$(CC) $(CFLAGS) $(LFLAGS) -o compress compress.c

recmail:  recmail.c defs.h params.h header.h
	$(CC) $(CFLAGS) $(LFLAGS) recmail.c -o recmail

process.o:  process.c rparams.h defs.h params.h header.h Makefile
	$(CC) $(CFLAGS) -c process.c

checknews:  $(P) checknews.o process.o cpathinit.o rextern.o $(NNTP) $(CLIENTLIB) 
	$(CC) $(LFLAGS) checknews.o process.o cpathinit.o rextern.o $(NNTP) $(CLIENTLIB) -o checknews $(NETLIB)

sendbatch: sendbatch.sh Makefile
	sed -e "s%LIBDIR%$(LIBDIR)%g" \
	 -e "s%UUXFLAGS%$(UUXFLAGS)%g" \
	 -e "s%BATCHDIR%$(BATCHDIR)%g" sendbatch.sh > sendbatch

rmgroup: rmgroup.sh Makefile
	sed -e "s%LIBDIR%$(LIBDIR)%g" \
	 -e "s%FIXACTIVE%$(FIXACTIVE)%g" \
	 -e "s%SPOOLDIR%$(SPOOLDIR)%g" rmgroup.sh > rmgroup

checkgroups: checkgroups.sh Makefile
	sed -e "s%LIBDIR%$(LIBDIR)%g" checkgroups.sh > checkgroups

sendnews.o: sendnews.c defs.h
	$(CC) $(CFLAGS) -c sendnews.c

fullname.o: defs.h params.h header.h Makefile
	$(CC) $(CFLAGS) -c fullname.c

expire: $(P) $(EXPOBJS)
	$(CC) $(LFLAGS) -o expire $(EXPOBJS) $(LIBS)

header.o:  header.c header.h defs.h patchlevel.h params.h Makefile
	$(CC) $(CFLAGS) -c header.c

expire.o:  expire.c defs.h Makefile params.h ndir.h header.h Makefile
	$(CC) $(CFLAGS) -c expire.c

digest.o:  digest.c Makefile
	$(CC) $(CFLAGS) -c digest.c

# Some silliness here to get pathinit for both readnews & inews
rpathinit.o:  pathinit.c rparams.h header.h params.h defs.h Makefile
	$(CC) $(CFLAGS) $(HOMENAME) -DREAD -c pathinit.c
	mv pathinit.o rpathinit.o

ipathinit.o:  pathinit.c iparams.h header.h params.h defs.h Makefile
	$(CC) $(CFLAGS) $(HOMENAME) -DINEW -c pathinit.c
	mv pathinit.o ipathinit.o

cpathinit.o:  pathinit.c iparams.h header.h params.h defs.h Makefile
	$(CC) $(CFLAGS) $(HOMENAME) -DCHKN -c pathinit.c
	mv pathinit.o cpathinit.o

epathinit.o:  pathinit.c iparams.h header.h params.h defs.h Makefile
	$(CC) $(CFLAGS) $(HOMENAME) -DEXP -c pathinit.c
	mv pathinit.o epathinit.o

tags:	/tmp
	ctags -w *.h *.c

$(LIBDIR):
	mkdir $(LIBDIR)
	chmod 755 $(LIBDIR)
	chown $(NEWSUSR) $(LIBDIR)
	chgrp $(NEWSGRP) $(LIBDIR)

$(BINDIR):
	mkdir $(BINDIR)
	chmod 755 $(BINDIR)
	chown $(NEWSUSR) $(BINDIR)
	chgrp $(NEWSGRP) $(BINDIR)
