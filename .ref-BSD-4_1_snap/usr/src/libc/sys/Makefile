# %W% (Berkeley) %G%
OBJS=	_exit.o abort.o abs.o access.o acct.o alarm.o alloca.o \
	cerror.o chdir.o chmod.o chown.o chroot.o close.o creat.o \
	dup.o execl.o execle.o execv.o execve.o exit.o \
	fabs.o fork.o fstat.o getcsw.o getgid.o getpid.o getuid.o \
	ioctl.o itol.o kill.o link.o locv.o lseek.o ltod.o ltoi.o \
	makdir.o mcount.o mdate.o mknod.o mount.o mpxcall.o \
	nargs.o nexect.o nice.o open.o pipe.o prof.o ptrace.o \
	read.o reset.o sbrk.o setgid.o setjmp.o setuid.o signal.o \
	stat.o stime.o sync.o syscall.o time.o times.o \
	umask.o umount.o unlink.o utime.o \
	vadvise.o vfork.o vhangup.o vlimit.o vread.o vtimes.o \
	vwait.o vwrite.o wait.o write.o

.s.o:
	${AS} -o $*.o $*.s
	-ld -x -r $*.o
	mv a.out $*.o

all: ${OBJS}

clean:
	rm -f ${OBJS}
