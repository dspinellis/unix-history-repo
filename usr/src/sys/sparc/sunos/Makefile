#	@(#)Makefile	8.1 (Berkeley) 6/11/93

sun_sysent.c sun_syscalls.c sun_syscall.h: makesyscalls.sh syscalls.master
	-mv -f sun_sysent.c sun_sysent.c.bak
	-mv -f sun_syscalls.c sun_syscalls.c.bak
	-mv -f sun_syscall.h sun_syscall.h.bak
	sh makesyscalls.sh syscalls.master
