# @(#)Makefile	4.19 (Berkeley) 7/31/83

OBJS=	Ovfork.o Ovadvise.o \
	accept.o access.o acct.o \
	bind.o brk.o \
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
	shutdown.o sigblock.o sigpause.o \
	sigsetmask.o sigstack.o sigvec.o socket.o socketpair.o stat.o \
	swapon.o symlink.o sync.o syscall.o \
	truncate.o \
	umask.o umount.o unlink.o utimes.o \
	vhangup.o \
	wait.o wait3.o write.o writev.o

.c.o:
	/lib/cpp -E -DPROF $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

syslib syslib_p: ${OBJS}
	@echo "building profiled syslib_p"
	@cd profiled; ar cru ../syslib_p ${OBJS}
	@echo "buiding normal syslib_p"
	@ar cru syslib ${OBJS}

clean:
	rm -f syslib syslib_p ${OBJS} profiled/*.o
