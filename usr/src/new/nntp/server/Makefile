#
# Makefile for NNTP server
#

SRVROBJ = main.o serve.o access.o access_inet.o access_dnet.o active.o \
	ahbs.o globals.o group.o help.o ihave.o list.o misc.o netaux.o \
	newgroups.o newnews.o nextlast.o ngmatch.o post.o parsit.o scandir.o \
	slave.o spawn.o subnet.o time.o xhdr.o ../common/version.o

SRVRSRC = main.c serve.c access.c access_inet.c access_dnet.c active.c \
	ahbs.c globals.c group.c help.c ihave.c list.c misc.c netaux.c \
	newgroups.c newnews.c nextlast.c ngmatch.c post.c parsit.c scandir.c \
	slave.c spawn.c subnet.c time.c xhdr.c ../common/version.c

SRVRINC = common.h ../common/conf.h ../common/response_codes.h

SRCS	= ${SRVRSRC}

# -ldbm here if you've #define'ed DBM in ../common/conf.h
LIBS	=

CFLAGS	= -O

# Where nntpd is going to live

DESTDIR	= /etc

all:	nntpd

nntpd: ${SRVROBJ}
	${CC} ${CFLAGS} -o nntpd ${SRVROBJ} ${LIBS}

install: nntpd
	cp nntpd ${DESTDIR}/nntpd
	chmod 711 ${DESTDIR}/nntpd

lint:
	lint ${SRVRSRC}

clean:
	rm -f *.o nntpd make.out

distrib: clean
	rm -rf SCCS save tags

tags:	${SRVRSRC} ${SRVRINC}
	ctags ${SRVRSRC} ${SRVRINC}

# Dependencies

../common/version.o:
	${CC} ${CFLAGS} -c ../common/version.c
	mv version.o ../common/version.o

access.o: ../common/conf.h
access.o: ../common/response_codes.h
access.o: ../common/rfc977.h
access.o: access.c
access.o: common.h
access_dnet.o: ../common/conf.h
access_dnet.o: access_dnet.c
access_inet.o: ../common/conf.h
access_inet.o: access_inet.c
active.o: ../common/conf.h
active.o: ../common/response_codes.h
active.o: ../common/rfc977.h
active.o: active.c
active.o: common.h
ahbs.o: ../common/conf.h
ahbs.o: ../common/response_codes.h
ahbs.o: ../common/rfc977.h
ahbs.o: ahbs.c
ahbs.o: common.h
globals.o: ../common/conf.h
globals.o: ../common/response_codes.h
globals.o: ../common/rfc977.h
globals.o: common.h
globals.o: globals.c
group.o: ../common/conf.h
group.o: ../common/response_codes.h
group.o: ../common/rfc977.h
group.o: common.h
group.o: group.c
help.o: ../common/conf.h
help.o: ../common/response_codes.h
help.o: ../common/rfc977.h
help.o: common.h
help.o: help.c
ihave.o: ../common/conf.h
ihave.o: ../common/response_codes.h
ihave.o: ../common/rfc977.h
ihave.o: common.h
ihave.o: ihave.c
list.o: ../common/conf.h
list.o: ../common/response_codes.h
list.o: ../common/rfc977.h
list.o: common.h
list.o: list.c
main.o: ../common/conf.h
main.o: ../common/response_codes.h
main.o: ../common/rfc977.h
main.o: common.h
main.o: main.c
misc.o: ../common/conf.h
misc.o: ../common/response_codes.h
misc.o: ../common/rfc977.h
misc.o: common.h
misc.o: misc.c
netaux.o: ../common/conf.h
netaux.o: ../common/response_codes.h
netaux.o: ../common/rfc977.h
netaux.o: common.h
netaux.o: netaux.c
newgroups.o: ../common/conf.h
newgroups.o: ../common/response_codes.h
newgroups.o: ../common/rfc977.h
newgroups.o: common.h
newgroups.o: newgroups.c
newgroups.o: time.h
newnews.o: ../common/conf.h
newnews.o: ../common/response_codes.h
newnews.o: ../common/rfc977.h
newnews.o: common.h
newnews.o: newnews.c
newnews.o: time.h
nextlast.o: ../common/conf.h
nextlast.o: ../common/response_codes.h
nextlast.o: ../common/rfc977.h
nextlast.o: common.h
nextlast.o: nextlast.c
ngmatch.o: ../common/conf.h
ngmatch.o: ../common/response_codes.h
ngmatch.o: ../common/rfc977.h
ngmatch.o: common.h
ngmatch.o: ngmatch.c
parsit.o: parsit.c
post.o: ../common/conf.h
post.o: ../common/response_codes.h
post.o: ../common/rfc977.h
post.o: common.h
post.o: post.c
profile.o: profile.c
scandir.o: ../common/conf.h
scandir.o: ../common/response_codes.h
scandir.o: ../common/rfc977.h
scandir.o: common.h
scandir.o: scandir.c
serve.o: ../common/conf.h
serve.o: ../common/response_codes.h
serve.o: ../common/rfc977.h
serve.o: common.h
serve.o: serve.c
slave.o: ../common/conf.h
slave.o: ../common/response_codes.h
slave.o: ../common/rfc977.h
slave.o: common.h
slave.o: slave.c
spawn.o: ../common/conf.h
spawn.o: ../common/response_codes.h
spawn.o: ../common/rfc977.h
spawn.o: common.h
spawn.o: spawn.c
subnet.o: ../common/conf.h
subnet.o: subnet.c
time.o: time.c
xhdr.o: ../common/conf.h
xhdr.o: ../common/response_codes.h
xhdr.o: ../common/rfc977.h
xhdr.o: common.h
xhdr.o: xhdr.c
