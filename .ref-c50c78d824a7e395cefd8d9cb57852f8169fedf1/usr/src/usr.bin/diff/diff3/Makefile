autoconfig
backingstore
badsectforw
bdp
bottomhalf
busadapter
byte
capability
cbreak
character
cookedmode
demandpage
egid
erasechar
euid
fodkluster
gid
hardlimit
hardlock
interruptvectab
iopage
ipl
killchar
kluster
ldpctx
mmapioarch
msl
nbio
page
pagedaemon
panic
psn
rawmode
recvwindow
rgid
ruid
secondstorage
sharedtextseg
sigdrivenio
slidingwindowscheme
sndwindow
stickytext
svpctx
swapper
syspagetab
text
tophalf
uid
unbuffered
usrrequest
worderasechar
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	diff3.c
OBJS=
MAN=	diff3.0

all: diff3

diff3: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean:
	rm -f ${OBJS} core diff3

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 diff3 $(DESTDIR)/usr/lib/diff3
	install -c -o bin -g bin -m 755 diff3.sh $(DESTDIR)/usr/bin/diff3
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
