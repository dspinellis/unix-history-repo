#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) 9/5/85
#

SRCS=	Ovfork.c Ovadvise.c \
	accept.c access.c acct.c \
	adjtime.c bind.c brk.c \
	cerror.c chdir.c chmod.c chown.c chroot.c close.c connect.c \
	creat.c \
	dup.c dup2.c \
	execl.c execle.c exect.c execv.c execve.c _exit.c \
	fchmod.c fchown.c fcntl.c flock.c fork.c fstat.c fsync.c ftruncate.c \
	getdtablesize.c getegid.c geteuid.c getgid.c \
	getgroups.c gethostid.c gethostname.c getitimer.c getrlimit.c \
	getpagesize.c getpeername.c getpgrp.c getpid.c getppid.c \
	getpriority.c getrusage.c getsockname.c getsockopt.c \
	gettimeofday.c getuid.c \
	ioctl.c \
	kill.c killpg.c \
	link.c listen.c lseek.c lstat.c \
	mkdir.c mknod.c mount.c \
	open.c \
	pipe.c profil.c ptrace.c \
	quota.c \
	read.c readlink.c readv.c reboot.c recv.c recvfrom.c \
	recvmsg.c rename.c rmdir.c \
	sbrk.c select.c send.c sendmsg.c sendto.c setregid.c setgroups.c \
	sethostid.c sethostname.c setitimer.c setquota.c setrlimit.c \
	setpgrp.c setpriority.c setsockopt.c settimeofday.c setreuid.c \
	shutdown.c sigblock.c sigpause.c sigreturn.c \
	sigsetmask.c sigstack.c sigvec.c socket.c socketpair.c stat.c \
	swapon.c symlink.c sync.c syscall.c \
	truncate.c \
	umask.c umount.c unlink.c utimes.c \
	vhangup.c \
	wait.c wait3.c write.c writev.c
OBJS=	Ovfork.o Ovadvise.o \
	accept.o access.o acct.o \
	adjtime.o bind.o brk.o \
	cerror.o chdir.o chmod.o chown.o chroot.o close.o connect.o \
	creat.o \
	dup.o dup2.o \
	execl.o execle.o exect.o execv.o execve.o _exit.o \
	fchmod.o fchown.o fcntl.o flock.o fork.o fstat.o fsync.o ftruncate.o \
	getdtablesize.o getegid.o geteuid.o getgid.o \
	getgroups.o gethostid.o gethostname.o getitimer.o getrlimit.o \
	getpagesize.o getpeername.o getpgrp.o getpid.o getppid.o \
	getpriority.o getrusage.o getsockname.o getsockopt.o \
	gettimeofday.o getuid.o \
	ioctl.o \
	kill.o killpg.o \
	link.o listen.o lseek.o lstat.o \
	mkdir.o mknod.o mount.o \
	open.o \
	pipe.o profil.o ptrace.o \
	quota.o \
	read.o readlink.o readv.o reboot.o recv.o recvfrom.o \
	recvmsg.o rename.o rmdir.o \
	sbrk.o select.o send.o sendmsg.o sendto.o setregid.o setgroups.o \
	sethostid.o sethostname.o setitimer.o setquota.o setrlimit.o \
	setpgrp.o setpriority.o setsockopt.o settimeofday.o setreuid.o \
	shutdown.o sigblock.o sigpause.o sigreturn.o \
	sigsetmask.o sigstack.o sigvec.o socket.o socketpair.o stat.o \
	swapon.o symlink.o sync.o syscall.o \
	truncate.o \
	umask.o umount.o unlink.o utimes.o \
	vhangup.o \
	wait.o wait3.o write.o writev.o
TAGSFILE=tags

.c.o:
	/lib/cpp -E ${DEFS} -DPROF $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E ${DEFS} $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

syslib syslib_p: ${OBJS}
	@echo "building profiled syslib_p"
	@cd profiled; ar cru ../syslib_p ${OBJS}
	@echo "building normal syslib_p"
	@ar cru syslib ${OBJS}

tags:

clean:
	rm -f syslib syslib_p ${OBJS} profiled/*.o tags Makefile.bak

depend:
	for i in ${SRCS}; do \
	    cc -M ${DEFS} $$i | awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

Ovfork.o: Ovfork.c
Ovadvise.o: Ovadvise.c ./SYS.h /usr/include/syscall.h
accept.o: accept.c ./SYS.h /usr/include/syscall.h
access.o: access.c ./SYS.h /usr/include/syscall.h
acct.o: acct.c ./SYS.h /usr/include/syscall.h
adjtime.o: adjtime.c ./SYS.h /usr/include/syscall.h
bind.o: bind.c ./SYS.h /usr/include/syscall.h
brk.o: brk.c ./SYS.h /usr/include/syscall.h
cerror.o: cerror.c ./SYS.h /usr/include/syscall.h
chdir.o: chdir.c ./SYS.h /usr/include/syscall.h
chmod.o: chmod.c ./SYS.h /usr/include/syscall.h
chown.o: chown.c ./SYS.h /usr/include/syscall.h
chroot.o: chroot.c ./SYS.h /usr/include/syscall.h
close.o: close.c ./SYS.h /usr/include/syscall.h
connect.o: connect.c ./SYS.h /usr/include/syscall.h
creat.o: creat.c ./SYS.h /usr/include/syscall.h
dup.o: dup.c ./SYS.h /usr/include/syscall.h
dup2.o: dup2.c ./SYS.h /usr/include/syscall.h
execl.o: execl.c ./SYS.h /usr/include/syscall.h
execle.o: execle.c ./SYS.h /usr/include/syscall.h
exect.o: exect.c ./SYS.h /usr/include/syscall.h /usr/include/machine/psl.h
execv.o: execv.c ./SYS.h /usr/include/syscall.h
execve.o: execve.c ./SYS.h /usr/include/syscall.h
_exit.o: _exit.c ./SYS.h /usr/include/syscall.h
fchmod.o: fchmod.c ./SYS.h /usr/include/syscall.h
fchown.o: fchown.c ./SYS.h /usr/include/syscall.h
fcntl.o: fcntl.c ./SYS.h /usr/include/syscall.h
flock.o: flock.c ./SYS.h /usr/include/syscall.h
fork.o: fork.c ./SYS.h /usr/include/syscall.h
fstat.o: fstat.c ./SYS.h /usr/include/syscall.h
fsync.o: fsync.c ./SYS.h /usr/include/syscall.h
ftruncate.o: ftruncate.c ./SYS.h /usr/include/syscall.h
getdtablesize.o: getdtablesize.c ./SYS.h /usr/include/syscall.h
getegid.o: getegid.c ./SYS.h /usr/include/syscall.h
geteuid.o: geteuid.c ./SYS.h /usr/include/syscall.h
getgid.o: getgid.c ./SYS.h /usr/include/syscall.h
getgroups.o: getgroups.c ./SYS.h /usr/include/syscall.h
gethostid.o: gethostid.c ./SYS.h /usr/include/syscall.h
gethostname.o: gethostname.c ./SYS.h /usr/include/syscall.h
getitimer.o: getitimer.c ./SYS.h /usr/include/syscall.h
getrlimit.o: getrlimit.c ./SYS.h /usr/include/syscall.h
getpagesize.o: getpagesize.c ./SYS.h /usr/include/syscall.h
getpeername.o: getpeername.c ./SYS.h /usr/include/syscall.h
getpgrp.o: getpgrp.c ./SYS.h /usr/include/syscall.h
getpid.o: getpid.c ./SYS.h /usr/include/syscall.h
getppid.o: getppid.c ./SYS.h /usr/include/syscall.h
getpriority.o: getpriority.c ./SYS.h /usr/include/syscall.h
getrusage.o: getrusage.c ./SYS.h /usr/include/syscall.h
getsockname.o: getsockname.c ./SYS.h /usr/include/syscall.h
getsockopt.o: getsockopt.c ./SYS.h /usr/include/syscall.h
gettimeofday.o: gettimeofday.c ./SYS.h /usr/include/syscall.h
getuid.o: getuid.c ./SYS.h /usr/include/syscall.h
ioctl.o: ioctl.c ./SYS.h /usr/include/syscall.h
kill.o: kill.c ./SYS.h /usr/include/syscall.h
killpg.o: killpg.c ./SYS.h /usr/include/syscall.h
link.o: link.c ./SYS.h /usr/include/syscall.h
listen.o: listen.c ./SYS.h /usr/include/syscall.h
lseek.o: lseek.c ./SYS.h /usr/include/syscall.h
lstat.o: lstat.c ./SYS.h /usr/include/syscall.h
mkdir.o: mkdir.c ./SYS.h /usr/include/syscall.h
mknod.o: mknod.c ./SYS.h /usr/include/syscall.h
mount.o: mount.c ./SYS.h /usr/include/syscall.h
open.o: open.c ./SYS.h /usr/include/syscall.h
pipe.o: pipe.c ./SYS.h /usr/include/syscall.h
profil.o: profil.c ./SYS.h /usr/include/syscall.h
ptrace.o: ptrace.c ./SYS.h /usr/include/syscall.h
quota.o: quota.c ./SYS.h /usr/include/syscall.h
read.o: read.c ./SYS.h /usr/include/syscall.h
readlink.o: readlink.c ./SYS.h /usr/include/syscall.h
readv.o: readv.c ./SYS.h /usr/include/syscall.h
reboot.o: reboot.c ./SYS.h /usr/include/syscall.h
recv.o: recv.c ./SYS.h /usr/include/syscall.h
recvfrom.o: recvfrom.c ./SYS.h /usr/include/syscall.h
recvmsg.o: recvmsg.c ./SYS.h /usr/include/syscall.h
rename.o: rename.c ./SYS.h /usr/include/syscall.h
rmdir.o: rmdir.c ./SYS.h /usr/include/syscall.h
sbrk.o: sbrk.c ./SYS.h /usr/include/syscall.h
select.o: select.c ./SYS.h /usr/include/syscall.h
send.o: send.c ./SYS.h /usr/include/syscall.h
sendmsg.o: sendmsg.c ./SYS.h /usr/include/syscall.h
sendto.o: sendto.c ./SYS.h /usr/include/syscall.h
setregid.o: setregid.c ./SYS.h /usr/include/syscall.h
setgroups.o: setgroups.c ./SYS.h /usr/include/syscall.h
sethostid.o: sethostid.c ./SYS.h /usr/include/syscall.h
sethostname.o: sethostname.c ./SYS.h /usr/include/syscall.h
setitimer.o: setitimer.c ./SYS.h /usr/include/syscall.h
setquota.o: setquota.c ./SYS.h /usr/include/syscall.h
setrlimit.o: setrlimit.c ./SYS.h /usr/include/syscall.h
setpgrp.o: setpgrp.c ./SYS.h /usr/include/syscall.h
setpriority.o: setpriority.c ./SYS.h /usr/include/syscall.h
setsockopt.o: setsockopt.c ./SYS.h /usr/include/syscall.h
settimeofday.o: settimeofday.c ./SYS.h /usr/include/syscall.h
setreuid.o: setreuid.c ./SYS.h /usr/include/syscall.h
shutdown.o: shutdown.c ./SYS.h /usr/include/syscall.h
sigblock.o: sigblock.c ./SYS.h /usr/include/syscall.h
sigpause.o: sigpause.c ./SYS.h /usr/include/syscall.h
sigreturn.o: sigreturn.c ./SYS.h /usr/include/syscall.h
sigsetmask.o: sigsetmask.c ./SYS.h /usr/include/syscall.h
sigstack.o: sigstack.c ./SYS.h /usr/include/syscall.h
sigvec.o: sigvec.c ./SYS.h /usr/include/syscall.h
socket.o: socket.c ./SYS.h /usr/include/syscall.h
socketpair.o: socketpair.c ./SYS.h /usr/include/syscall.h
stat.o: stat.c ./SYS.h /usr/include/syscall.h
swapon.o: swapon.c ./SYS.h /usr/include/syscall.h
symlink.o: symlink.c ./SYS.h /usr/include/syscall.h
sync.o: sync.c ./SYS.h /usr/include/syscall.h
syscall.o: syscall.c ./SYS.h /usr/include/syscall.h
truncate.o: truncate.c ./SYS.h /usr/include/syscall.h
umask.o: umask.c ./SYS.h /usr/include/syscall.h
umount.o: umount.c ./SYS.h /usr/include/syscall.h
unlink.o: unlink.c ./SYS.h /usr/include/syscall.h
utimes.o: utimes.c ./SYS.h /usr/include/syscall.h
vhangup.o: vhangup.c ./SYS.h /usr/include/syscall.h
wait.o: wait.c ./SYS.h /usr/include/syscall.h
wait3.o: wait3.c ./SYS.h /usr/include/syscall.h
write.o: write.c ./SYS.h /usr/include/syscall.h
writev.o: writev.c ./SYS.h /usr/include/syscall.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
