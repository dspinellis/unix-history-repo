# @(#)Makefile	4.10 (Berkeley) 2/26/83

OBJS=	Oalarm.o Obrk.o Osbrk.o Oftime.o Ogtty.o \
	Onice.o Opause.o Optrace.o Osignal.o Ostty.o \
	Otimes.o Oumask.o Outime.o \
	Ovfork.o Ovhangup.o Ovlimit.o Ovtimes.o Ovadvise.o \
	accept.o access.o acct.o bind.o \
	brk.o sbrk.o \
	cerror.o chdir.o chmod.o chown.o chroot.o close.o connect.o \
	domain.o dup.o dup2.o \
	execl.o execle.o exect.o execv.o execve.o _exit.o \
	fchmod.o fchown.o flock.o fork.o fstat.o ftruncate.o \
	getdopt.o getdprop.o getdtablesize.o getegid.o geteuid.o getgid.o \
	getgroups.o gethostid.o gethostname.o getitimer.o getrlimit.o \
	getpagesize.o getpgrp.o getpid.o getppid.o getpriority.o \
	getrusage.o getsockopt.o gettimeofday.o getuid.o \
	ioctl.o \
	kill.o killpg.o \
	link.o listen.o lseek.o lstat.o \
	madvise.o mincore.o mkdir.o mknod.o mmap.o mount.o \
	mprotect.o mremap.o munmap.o \
	open.o \
	pipe.o prof.o portal.o \
	read.o readlink.o readv.o reboot.o recv.o recvfrom.o \
	recvmsg.o rename.o revoke.o rmdir.o \
	select.o send.o sendmsg.o sendto.o setdopt.o \
	setegid.o setgid.o setregid.o setrgid.o setgroups.o sethostid.o \
	sethostname.o setitimer.o setrlimit.o setpgrp.o setpriority.o \
	setsockopt.o settimeofday.o seteuid.o setreuid.o setruid.o \
	setuid.o shutdown.o sigblock.o sigpause.o sigsetmask.o \
	sigstack.o sigvec.o socket.o socketpair.o sstk.o stat.o \
	swapoff.o swapon.o symlink.o sync.o syscall.o \
	truncate.o \
	unlink.o unmount.o utimes.o \
	wait.o wrap.o write.o writev.o

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
