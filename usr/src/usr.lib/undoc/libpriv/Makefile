#	%W%	(Melbourne)	%E%
#
# User privs maintanence/info programs
#
GET= sccs get
CFLAGS= -O

INCL= /usr/include

OBJS=	gdpriv.o rdprivf.o wrtprivf.o testupriv.o getudata.o putudata.o \
	getshare.o getloglim.o setloglim.o chkshare.o \
	getmush.o getlogsess.o getlogday.o getlogwk.o getloguse.o \
	getlpquota.o getdiscq.o mushreq.o mushlreq.o asctotime.o \
	addudata.o addlpdq.o addmush.o adddq.o addquota.o \
	setprivs.o putdiscq.o putlpquota.o putmush.o mushlset.o mushset.o \
	syncmush.o strucmp.o alldigits.o \
	send.o recv.o sendw.o reply.o mu_msg.o privzero.o quota.o setquota.o
SRCS=	gdpriv.c rdprivf.c wrtprivf.c testupriv.c getudata.c putudata.c \
	getshare.c getloglim.c setloglim.c chkshare.c \
	getmush.c getlogsess.c getlogday.c getlogwk.c getloguse.c \
	getlpquota.c getdiscq.c mushreq.c mushlreq.c asctotime.c \
	setprivs.c putdiscq.c putlpquota.c putmush.c mushlset.c mushset.c \
	addudata.c addlpdq.c addmush.c adddq.c addquota.c \
	syncmush.c strucmp.c alldigits.c \
	send.c recv.c sendw.c reply.c mu_msg.c privzero.c quota.c setquota.c
HDRS=	$(INCL)/udata.h $(INCL)/sys/mu_msg.h $(INCL)/sys/mush.h \
	$(INCL)/quota.h $(INCL)/lpdquota.h $(INCL)/mushmuck.h

all: libpriv.a

libpriv.a: $(OBJS)
	ar ru libpriv.a $?
	ranlib libpriv.a

install: libpriv.a
	install -mug lib lib 644 libpriv.a $(DESTDIR)/usr/lib
	ranlib $(DESTDIR)/usr/lib/libpriv.a

clean:
	rm -f *.o libpriv.a core a.out alterfile *junk*

sources: $(SRCS)

$(SRCS) alterfile.c addprivs.c:
	$(GET) $@

clobber:
	sccs clean

print:
	@pr $(HDRS) $(SRCS)

alterfile: alterfile.c udata.h
	$(CC) -o alterfile $(CFLAGS) alterfile.c

addprivs: addprivs.c libpriv.a
	$(CC) -o addprivs $(CFLAGS) addprivs.c libpriv.a

udata.h:
	@echo "Alterfile is only useful if there is a udata.h"
	@echo "file here that is different from $(INCL)/udata.h"
