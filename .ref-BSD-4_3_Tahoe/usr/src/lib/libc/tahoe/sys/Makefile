#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.8 (Berkeley) 6/27/88
#
DEFS=	-DSYSLIBC_SCCS
SRCS=	Ovadvise.s Ovfork.s _exit.s accept.s access.s acct.s adjtime.s \
	bind.s brk.s cerror.s chdir.s chmod.s chown.s chroot.s close.s \
	connect.s creat.s dup.s dup2.s execl.s execle.s exect.s execv.s \
	execve.s fchmod.s fchown.s fcntl.s flock.s fork.s fstat.s fsync.s \
	ftruncate.s getdtablesize.s getegid.s geteuid.s getgid.s getgroups.s \
	gethostid.s gethostname.s getitimer.s getpagesize.s getpeername.s \
	getpgrp.s getpid.s getppid.s getpriority.s getrlimit.s getrusage.s \
	getsockname.s getsockopt.s gettimeofday.s getuid.s ioctl.s kill.s \
	killpg.s link.s listen.s lseek.s lstat.s mkdir.s mknod.s mmap.s \
	mount.s open.s pipe.s profil.s ptrace.s quota.s read.s readlink.s \
	readv.s reboot.s recv.s recvfrom.s recvmsg.s rename.s rmdir.s sbrk.s \
	select.s send.s sendmsg.s sendto.s setgroups.s sethostid.s \
	sethostname.s setitimer.s setpgrp.s setpriority.s setquota.s \
	setregid.s setreuid.s setrlimit.s setsockopt.s settimeofday.s \
	shutdown.s sigblock.s sigpause.s sigreturn.s sigsetmask.s sigstack.s \
	sigvec.s socket.s socketpair.s stat.s swapon.s symlink.s sync.s \
	syscall.s truncate.s umask.s umount.s unlink.s utimes.s vhangup.s \
	wait.s wait3.s write.s writev.s
OBJS=	Ovadvise.o Ovfork.o _exit.o accept.o access.o acct.o adjtime.o \
	bind.o brk.o cerror.o chdir.o chmod.o chown.o chroot.o close.o \
	connect.o creat.o dup.o dup2.o execl.o execle.o exect.o execv.o \
	execve.o fchmod.o fchown.o fcntl.o flock.o fork.o fstat.o fsync.o \
	ftruncate.o getdtablesize.o getegid.o geteuid.o getgid.o getgroups.o \
	gethostid.o gethostname.o getitimer.o getpagesize.o getpeername.o \
	getpgrp.o getpid.o getppid.o getpriority.o getrlimit.o getrusage.o \
	getsockname.o getsockopt.o gettimeofday.o getuid.o ioctl.o kill.o \
	killpg.o link.o listen.o lseek.o lstat.o mkdir.o mknod.o mmap.o \
	mount.o open.o pipe.o profil.o ptrace.o quota.o read.o readlink.o \
	readv.o reboot.o recv.o recvfrom.o recvmsg.o rename.o rmdir.o sbrk.o \
	select.o send.o sendmsg.o sendto.o setgroups.o sethostid.o \
	sethostname.o setitimer.o setpgrp.o setpriority.o setquota.o \
	setregid.o setreuid.o setrlimit.o setsockopt.o settimeofday.o \
	shutdown.o sigblock.o sigpause.o sigreturn.o sigsetmask.o sigstack.o \
	sigvec.o socket.o socketpair.o stat.o swapon.o symlink.o sync.o \
	syscall.o truncate.o umask.o umount.o unlink.o utimes.o vhangup.o \
	wait.o wait3.o write.o writev.o
TAGSFILE=tags

.s.o:
	@/lib/cpp ${DEFS} -DPROF $*.s | ${AS} -o $*.o
	@-ld -x -o profiled/$*.o -r $*.o
	/lib/cpp ${DEFS} $*.s | ${AS} -o $*.o
	@-ld -x -r $*.o
	@mv a.out $*.o

all: ${OBJS}

link: ${OBJS}
	(cd ../../library; rm -f ${OBJS})
	(cd ../../profiled; rm -f ${OBJS})
	ln ${OBJS} ../../library
	(cd profiled; ln ${OBJS} ../../../profiled)

tags:
	(P=`pwd`; \
	egrep "^ENTRY(.*)|^SYSCALL(.*)" ${SRCS} | /bin/sed \
"s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 $$P/\1 /^\2(\3\4$$/;" \
		>> ${TAGSFILE})

clean:
	rm -f *.o profiled/* a.out core ${TAGSFILE}

depend:
	mkdep ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

Ovadvise.o: Ovadvise.s SYS.h /usr/include/sys/syscall.h
Ovfork.o: Ovfork.s
_exit.o: _exit.s SYS.h /usr/include/sys/syscall.h
accept.o: accept.s SYS.h /usr/include/sys/syscall.h
access.o: access.s SYS.h /usr/include/sys/syscall.h
acct.o: acct.s SYS.h /usr/include/sys/syscall.h
adjtime.o: adjtime.s SYS.h /usr/include/sys/syscall.h
bind.o: bind.s SYS.h /usr/include/sys/syscall.h
brk.o: brk.s SYS.h /usr/include/sys/syscall.h
cerror.o: cerror.s SYS.h /usr/include/sys/syscall.h
chdir.o: chdir.s SYS.h /usr/include/sys/syscall.h
chmod.o: chmod.s SYS.h /usr/include/sys/syscall.h
chown.o: chown.s SYS.h /usr/include/sys/syscall.h
chroot.o: chroot.s SYS.h /usr/include/sys/syscall.h
close.o: close.s SYS.h /usr/include/sys/syscall.h
connect.o: connect.s SYS.h /usr/include/sys/syscall.h
creat.o: creat.s SYS.h /usr/include/sys/syscall.h
dup.o: dup.s SYS.h /usr/include/sys/syscall.h
dup2.o: dup2.s SYS.h /usr/include/sys/syscall.h
execl.o: execl.s SYS.h /usr/include/sys/syscall.h
execle.o: execle.s SYS.h /usr/include/sys/syscall.h
exect.o: exect.s SYS.h /usr/include/sys/syscall.h /usr/include/machine/psl.h
execv.o: execv.s SYS.h /usr/include/sys/syscall.h
execve.o: execve.s SYS.h /usr/include/sys/syscall.h
fchmod.o: fchmod.s SYS.h /usr/include/sys/syscall.h
fchown.o: fchown.s SYS.h /usr/include/sys/syscall.h
fcntl.o: fcntl.s SYS.h /usr/include/sys/syscall.h
flock.o: flock.s SYS.h /usr/include/sys/syscall.h
fork.o: fork.s SYS.h /usr/include/sys/syscall.h
fstat.o: fstat.s SYS.h /usr/include/sys/syscall.h
fsync.o: fsync.s SYS.h /usr/include/sys/syscall.h
ftruncate.o: ftruncate.s SYS.h /usr/include/sys/syscall.h
getdtablesize.o: getdtablesize.s SYS.h /usr/include/sys/syscall.h
getegid.o: getegid.s SYS.h /usr/include/sys/syscall.h
geteuid.o: geteuid.s SYS.h /usr/include/sys/syscall.h
getgid.o: getgid.s SYS.h /usr/include/sys/syscall.h
getgroups.o: getgroups.s SYS.h /usr/include/sys/syscall.h
gethostid.o: gethostid.s SYS.h /usr/include/sys/syscall.h
gethostname.o: gethostname.s SYS.h /usr/include/sys/syscall.h
getitimer.o: getitimer.s SYS.h /usr/include/sys/syscall.h
getpagesize.o: getpagesize.s SYS.h /usr/include/sys/syscall.h
getpeername.o: getpeername.s SYS.h /usr/include/sys/syscall.h
getpgrp.o: getpgrp.s SYS.h /usr/include/sys/syscall.h
getpid.o: getpid.s SYS.h /usr/include/sys/syscall.h
getppid.o: getppid.s SYS.h /usr/include/sys/syscall.h
getpriority.o: getpriority.s SYS.h /usr/include/sys/syscall.h
getrlimit.o: getrlimit.s SYS.h /usr/include/sys/syscall.h
getrusage.o: getrusage.s SYS.h /usr/include/sys/syscall.h
getsockname.o: getsockname.s SYS.h /usr/include/sys/syscall.h
getsockopt.o: getsockopt.s SYS.h /usr/include/sys/syscall.h
gettimeofday.o: gettimeofday.s SYS.h /usr/include/sys/syscall.h
getuid.o: getuid.s SYS.h /usr/include/sys/syscall.h
ioctl.o: ioctl.s SYS.h /usr/include/sys/syscall.h
kill.o: kill.s SYS.h /usr/include/sys/syscall.h
killpg.o: killpg.s SYS.h /usr/include/sys/syscall.h
link.o: link.s SYS.h /usr/include/sys/syscall.h
listen.o: listen.s SYS.h /usr/include/sys/syscall.h
lseek.o: lseek.s SYS.h /usr/include/sys/syscall.h
lstat.o: lstat.s SYS.h /usr/include/sys/syscall.h
mkdir.o: mkdir.s SYS.h /usr/include/sys/syscall.h
mknod.o: mknod.s SYS.h /usr/include/sys/syscall.h
mmap.o: mmap.s SYS.h /usr/include/sys/syscall.h
mount.o: mount.s SYS.h /usr/include/sys/syscall.h
open.o: open.s SYS.h /usr/include/sys/syscall.h
pipe.o: pipe.s SYS.h /usr/include/sys/syscall.h
profil.o: profil.s SYS.h /usr/include/sys/syscall.h
ptrace.o: ptrace.s SYS.h /usr/include/sys/syscall.h
quota.o: quota.s SYS.h /usr/include/sys/syscall.h
read.o: read.s SYS.h /usr/include/sys/syscall.h
readlink.o: readlink.s SYS.h /usr/include/sys/syscall.h
readv.o: readv.s SYS.h /usr/include/sys/syscall.h
reboot.o: reboot.s SYS.h /usr/include/sys/syscall.h
recv.o: recv.s SYS.h /usr/include/sys/syscall.h
recvfrom.o: recvfrom.s SYS.h /usr/include/sys/syscall.h
recvmsg.o: recvmsg.s SYS.h /usr/include/sys/syscall.h
rename.o: rename.s SYS.h /usr/include/sys/syscall.h
rmdir.o: rmdir.s SYS.h /usr/include/sys/syscall.h
sbrk.o: sbrk.s SYS.h /usr/include/sys/syscall.h
select.o: select.s SYS.h /usr/include/sys/syscall.h
send.o: send.s SYS.h /usr/include/sys/syscall.h
sendmsg.o: sendmsg.s SYS.h /usr/include/sys/syscall.h
sendto.o: sendto.s SYS.h /usr/include/sys/syscall.h
setgroups.o: setgroups.s SYS.h /usr/include/sys/syscall.h
sethostid.o: sethostid.s SYS.h /usr/include/sys/syscall.h
sethostname.o: sethostname.s SYS.h /usr/include/sys/syscall.h
setitimer.o: setitimer.s SYS.h /usr/include/sys/syscall.h
setpgrp.o: setpgrp.s SYS.h /usr/include/sys/syscall.h
setpriority.o: setpriority.s SYS.h /usr/include/sys/syscall.h
setquota.o: setquota.s SYS.h /usr/include/sys/syscall.h
setregid.o: setregid.s SYS.h /usr/include/sys/syscall.h
setreuid.o: setreuid.s SYS.h /usr/include/sys/syscall.h
setrlimit.o: setrlimit.s SYS.h /usr/include/sys/syscall.h
setsockopt.o: setsockopt.s SYS.h /usr/include/sys/syscall.h
settimeofday.o: settimeofday.s SYS.h /usr/include/sys/syscall.h
shutdown.o: shutdown.s SYS.h /usr/include/sys/syscall.h
sigblock.o: sigblock.s SYS.h /usr/include/sys/syscall.h
sigpause.o: sigpause.s SYS.h /usr/include/sys/syscall.h
sigreturn.o: sigreturn.s SYS.h /usr/include/sys/syscall.h
sigsetmask.o: sigsetmask.s SYS.h /usr/include/sys/syscall.h
sigstack.o: sigstack.s SYS.h /usr/include/sys/syscall.h
sigvec.o: sigvec.s SYS.h /usr/include/sys/syscall.h
socket.o: socket.s SYS.h /usr/include/sys/syscall.h
socketpair.o: socketpair.s SYS.h /usr/include/sys/syscall.h
stat.o: stat.s SYS.h /usr/include/sys/syscall.h
swapon.o: swapon.s SYS.h /usr/include/sys/syscall.h
symlink.o: symlink.s SYS.h /usr/include/sys/syscall.h
sync.o: sync.s SYS.h /usr/include/sys/syscall.h
syscall.o: syscall.s SYS.h /usr/include/sys/syscall.h
truncate.o: truncate.s SYS.h /usr/include/sys/syscall.h
umask.o: umask.s SYS.h /usr/include/sys/syscall.h
umount.o: umount.s SYS.h /usr/include/sys/syscall.h
unlink.o: unlink.s SYS.h /usr/include/sys/syscall.h
utimes.o: utimes.s SYS.h /usr/include/sys/syscall.h
vhangup.o: vhangup.s SYS.h /usr/include/sys/syscall.h
wait.o: wait.s SYS.h /usr/include/sys/syscall.h
wait3.o: wait3.s SYS.h /usr/include/sys/syscall.h
write.o: write.s SYS.h /usr/include/sys/syscall.h
writev.o: writev.s SYS.h /usr/include/sys/syscall.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
