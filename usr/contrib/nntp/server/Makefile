#
# Makefile for NNTP server
#

SRVROBJ = main.o serve.o access.o active.o ahbs.o group.o help.o \
	ihave.o list.o misc.o netaux.o newgroups.o newnews.o nextlast.o \
	post.o parsit.o scandir.o slave.o time.o version.o

SRVRSRC = main.c serve.c access.c active.c ahbs.c group.c help.c \
	ihave.c list.c misc.c netaux.c newgroups.c newnews.c nextlast.c \
 	post.c parsit.c scandir.c slave.c time.c version.c

SRVRINC = common.h

SRCS	= ${SRVRSRC}

# -ldbm here if you've #define'ed DBM in ../common/conf.h

CFLAGS= -O

# Where nntpd is going to live

BINDIR	= /etc

all:	nntpd

nntpd: ${SRVROBJ}
	cc ${CFLAGS} -o nntpd ${SRVROBJ}

install: nntpd
	cp nntpd ${BINDIR}/nntpd
	chmod 711 ${BINDIR}/nntpd

lint:
	lint ${SRVRSRC}

clean:
	rm -f *.o nntpd

tags:	${SRVRSRC} ${SRVRINC}
	ctags ${SRVRSRC} ${SRVRINC}

depend:
	cc -M ${CFLAGS} ${SRCS} | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE
# DEPENDENCIES MUST END AT END OF FILE

main.o: main.c /usr/include/sys/types.h /usr/include/sys/socket.h
main.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/signal.h
main.o: ./common.h /usr/include/stdio.h /usr/include/sys/types.h
main.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
main.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
main.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
main.o: ./../common/response_codes.h ./../common/conf.h
serve.o: serve.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
serve.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
serve.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
serve.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
serve.o: ./../common/response_codes.h ./../common/conf.h
serve.o: /usr/include/sys/time.h /usr/include/time.h
serve.o: /usr/include/sys/resource.h
access.o: access.c /usr/include/sys/types.h /usr/include/sys/socket.h
access.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/ctype.h
access.o: ./common.h /usr/include/stdio.h /usr/include/sys/types.h
access.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
access.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
access.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
access.o: ./../common/response_codes.h ./../common/conf.h
active.o: active.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
active.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
active.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
active.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
active.o: ./../common/response_codes.h ./../common/conf.h
ahb.o: ahb.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
ahb.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
ahb.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
ahb.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
ahb.o: ./../common/response_codes.h ./../common/conf.h
group.o: group.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
group.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
group.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
group.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
group.o: ./../common/response_codes.h ./../common/conf.h
help.o: help.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
help.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
help.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
help.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
help.o: ./../common/response_codes.h ./../common/conf.h
ihave.o: ihave.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
ihave.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
ihave.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
ihave.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
ihave.o: ./../common/response_codes.h ./../common/conf.h
last.o: last.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
last.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
last.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
last.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
last.o: ./../common/response_codes.h ./../common/conf.h
list.o: list.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
list.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
list.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
list.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
list.o: ./../common/response_codes.h ./../common/conf.h
misc.o: misc.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
misc.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
misc.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
misc.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
misc.o: ./../common/response_codes.h ./../common/conf.h
netaux.o: netaux.c
newgroups.o: newgroups.c ./common.h /usr/include/stdio.h
newgroups.o: /usr/include/sys/types.h /usr/include/sys/dir.h
newgroups.o: /usr/include/sys/stat.h /usr/include/errno.h /usr/include/ctype.h
newgroups.o: /usr/include/pwd.h /usr/include/sys/wait.h /usr/include/sys/file.h
newgroups.o: /usr/include/syslog.h /usr/include/ndbm.h
newgroups.o: ./../common/response_codes.h ./../common/conf.h
newnews.o: newnews.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
newnews.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
newnews.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
newnews.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
newnews.o: ./../common/response_codes.h ./../common/conf.h
next.o: next.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
next.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
next.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
next.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
next.o: ./../common/response_codes.h ./../common/conf.h
post.o: post.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
post.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
post.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
post.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
post.o: ./../common/response_codes.h ./../common/conf.h
parsit.o: parsit.c /usr/include/stdio.h
scandir.o: scandir.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
scandir.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
scandir.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
scandir.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
scandir.o: ./../common/response_codes.h ./../common/conf.h
slave.o: slave.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
slave.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
slave.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
slave.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
slave.o: ./../common/response_codes.h ./../common/conf.h
stat.o: stat.c ./common.h /usr/include/stdio.h /usr/include/sys/types.h
stat.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/errno.h
stat.o: /usr/include/ctype.h /usr/include/pwd.h /usr/include/sys/wait.h
stat.o: /usr/include/sys/file.h /usr/include/syslog.h /usr/include/ndbm.h
stat.o: ./../common/response_codes.h ./../common/conf.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
