#	@(#)Makefile	7.1 (Berkeley) %G%

news_sysent.c news_syscalls.c news_syscall.h: makesyscalls.sh syscalls.master
	-mv -f news_sysent.c news_sysent.c.bak
	-mv -f news_syscalls.c news_syscalls.c.bak
	-mv -f news_syscall.h news_syscall.h.bak
	sh makesyscalls.sh syscalls.master
