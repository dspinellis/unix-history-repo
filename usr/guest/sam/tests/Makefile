# test programs for new stuff; also,
# some performance evaluation programs
#
CFLAGS=-O
TESTS=	chmod chown client dgclient dgserver efault emfile exlock \
	getpeer grow fds fdclient fdserver hole iov ipc mkdir pipe \
	pup raw reader rename rmdir sendto server setuid \
	shlock sig sigstack sigstack2 sigstack3 sigtstp truncate \
	writer
PERF=	datain dataout

all:	${TESTS} ${PERF}

${TESTS} ${PERF}:
	${CC} ${CFLAGS} -o $@ $@.c

clean:
	rm -f a.out core errs ${TESTS} ${PERF}

depend:
	cat </dev/null >x.c
	for i in ${TESTS} ${PERF}; do \
		(echo $$i: $$i.c >>makedep; \
		/bin/grep '^#[ 	]*include' x.c $$i.c | sed \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/:[^"]*"\([^"]*\)".*/: \1/' \
			-e 's/\.c//' >>makedep); done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep x.c
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE
# DEPENDENCIES MUST END AT END OF FILE
chmod: chmod.c
chmod: /usr/include/sys/file.h
chmod: /usr/include/stdio.h
chown: chown.c
chown: /usr/include/sys/file.h
client: client.c
client: /usr/include/stdio.h
client: /usr/include/sys/types.h
client: /usr/include/sys/socket.h
client: /usr/include/sys/un.h
dgclient: dgclient.c
dgclient: /usr/include/stdio.h
dgclient: /usr/include/strings.h
dgclient: /usr/include/signal.h
dgclient: /usr/include/sys/types.h
dgclient: /usr/include/sys/socket.h
dgclient: /usr/include/sys/un.h
dgserver: dgserver.c
dgserver: /usr/include/sys/types.h
dgserver: /usr/include/sys/socket.h
dgserver: /usr/include/sys/un.h
dgserver: /usr/include/stdio.h
dgserver: /usr/include/signal.h
dgserver: /usr/include/setjmp.h
dgserver: /usr/include/errno.h
efault: efault.c
efault: /usr/include/errno.h
emfile: emfile.c
emfile: /usr/include/stdio.h
exlock: exlock.c
exlock: /usr/include/sys/file.h
getpeer: getpeer.c
getpeer: /usr/include/sys/types.h
getpeer: /usr/include/sys/socket.h
getpeer: /usr/include/netinet/in.h
getpeer: /usr/include/stdio.h
getpeer: /usr/include/netdb.h
grow: grow.c
grow: /usr/include/signal.h
fds: fds.c
fds: /usr/include/stdio.h
fds: /usr/include/sys/param.h
fds: /usr/include/sys/stat.h
fdclient: fdclient.c
fdclient: /usr/include/sys/types.h
fdclient: /usr/include/sys/socket.h
fdclient: /usr/include/sys/stat.h
fdclient: /usr/include/sys/file.h
fdclient: /usr/include/sys/uio.h
fdclient: /usr/include/sys/un.h
fdclient: /usr/include/stdio.h
fdclient: /usr/include/signal.h
fdserver: fdserver.c
fdserver: /usr/include/sys/types.h
fdserver: /usr/include/sys/socket.h
fdserver: /usr/include/sys/wait.h
fdserver: /usr/include/sys/uio.h
fdserver: /usr/include/sys/un.h
fdserver: /usr/include/stdio.h
fdserver: /usr/include/signal.h
fdserver: /usr/include/setjmp.h
fdserver: /usr/include/errno.h
hole: hole.c
hole: /usr/include/stdio.h
hole: /usr/include/sys/file.h
iov: iov.c
iov: /usr/include/stdio.h
iov: /usr/include/sys/types.h
iov: /usr/include/sys/socket.h
iov: /usr/include/sys/uio.h
ipc: ipc.c
ipc: /usr/include/stdio.h
ipc: /usr/include/sys/types.h
ipc: /usr/include/sys/socket.h
ipc: /usr/include/sys/un.h
mkdir: mkdir.c
mkdir: /usr/include/stdio.h
pipe: pipe.c
pipe: /usr/include/sys/types.h
pipe: /usr/include/sys/socket.h
pipe: /usr/include/signal.h
pup: pup.c
pup: /usr/include/stdio.h
pup: /usr/include/netdb.h
pup: /usr/include/sys/types.h
pup: /usr/include/sys/uio.h
pup: /usr/include/sys/socket.h
pup: /usr/include/netpup/pup.h
raw: raw.c
raw: /usr/include/stdio.h
raw: /usr/include/netdb.h
raw: /usr/include/sys/types.h
raw: /usr/include/sys/uio.h
raw: /usr/include/sys/socket.h
raw: /usr/include/netinet/in.h
raw: /usr/include/netinet/in_systm.h
raw: /usr/include/netinet/ip.h
reader: reader.c
reader: /usr/include/sys/file.h
rename: rename.c
rmdir: rmdir.c
rmdir: /usr/include/stdio.h
sendto: sendto.c
sendto: /usr/include/stdio.h
sendto: /usr/include/sys/types.h
sendto: /usr/include/sys/socket.h
sendto: /usr/include/netinet/in.h
sendto: /usr/include/netdb.h
server: server.c
server: /usr/include/sys/types.h
server: /usr/include/sys/socket.h
server: /usr/include/sys/wait.h
server: /usr/include/sys/un.h
server: /usr/include/stdio.h
server: /usr/include/signal.h
server: /usr/include/setjmp.h
server: /usr/include/errno.h
setuid: setuid.c
shlock: shlock.c
shlock: /usr/include/sys/file.h
sig: sig.c
sig: /usr/include/signal.h
sigstack: sigstack.c
sigstack: /usr/include/stdio.h
sigstack: /usr/include/signal.h
sigstack2: sigstack2.c
sigstack2: /usr/include/stdio.h
sigstack2: /usr/include/signal.h
sigstack3: sigstack3.c
sigstack3: /usr/include/stdio.h
sigstack3: /usr/include/signal.h
sigtstp: sigtstp.c
sigtstp: /usr/include/signal.h
truncate: truncate.c
writer: writer.c
writer: /usr/include/sys/file.h
datain: datain.c
datain: /usr/include/sys/types.h
datain: /usr/include/sys/socket.h
datain: /usr/include/sys/wait.h
datain: /usr/include/netinet/in.h
datain: /usr/include/signal.h
datain: /usr/include/errno.h
dataout: dataout.c
dataout: /usr/include/sys/types.h
dataout: /usr/include/sys/socket.h
dataout: /usr/include/netinet/in.h
dataout: /usr/include/signal.h
dataout: /usr/include/stdio.h
dataout: /usr/include/netdb.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
