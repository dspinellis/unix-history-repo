
#
#      Copyright (c) 1984, 1985, 1986 AT&T
#      All Rights Reserved
#
#      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
#      CODE OF AT&T.
#      The copyright notice above does not 
#      evidence any actual or intended
#      publication of such source code.
#
/*
 *  Fourth Generation make, makefile
 */

.OPTIONS:	-m -u

.SOURCE:	sh shlib jsh

/*  These are files that contain documentation */

DOCFILES= shlib/makefile jsh/makefile sh/makefile sh/makelib sh/makesh \
	sh/Makefile sh.memo sh.1 ksh.mk makefile getopts substring \
	README RELEASE COMPATIBILITY 

KSHELL = 1
MULTIBYTE =		/* set to 1 for  multi-byte character sets */
BSD =			/* set to 1 for BSD Unix */
BSD_4_2 =		/* set to 1 for BSD 4.2 */
BSD_4_1c =		/* set to 1 for BSD 4.1c */
ARK = lib.a		/* name for library for common shell procedures */
EDITLIB = libedit.a	/* name for editing library */
SH = ksh		/* name of the shell program */
JSH = jsh		/* name of jobs control shell for shell layers */
ESH = 1			/* set for emacs/gmacs edit mode */
VSH = 1			/* set for vi edit mode */
JOBS = 1		/* needed if jobs monitor option is selected */
VFORK = 		/* needed if vfork option is selected */
JOBLIB =		/* set to name of library needed for job control */
LDFLAGS = -n		/* default loader option */
NOBUF =			/* set if _sibuf and _sobuf not in stdio library */
RAWONLY =		/* set to 1 if tty does not have alternate eof char */
INT16 =			/* set to -DINT16 for sixteen bit machines */
MYIO = 			/* set to stdio.c if you don't want to use native stdio */
ACCT =			/* set to 1 for shell accounting (Sys V only ) */
APOLLO =		/* set to apollo.c for DOMAIN/IX */
SYSCALL=		/* set to syscall.s for 3b20's */
EMACS = emacs.c		/* name of the file needed for ESH */
VI = vi.c		/* name of the file needed for VSH */
EDIT = edit.c		/* name of file needed if ESH|VSH selected */
JOBFILE = jobs.c	/* name of file needed if jobs option is selected */
VFORKFILE = 		/* name of file needed for VFORK option */
SUID_EXEC = suid_exec	/* set to null to disallow suid/sgid shell scripts */

#if release(bsd) || release(research)
BSD = 1
ECHO_N = 1
LDFLAGS = -z
#   if version(4.2) || version(4.3)
BSD_4_2 = 1
#   else
JOBLIB = -ljobs
#   endif
#   if machine(sun)
NOBUF = 1
#   endif
#endif

#undef apollo
#if machine(apollo)
VFORK = 1
VFORKFILE = vfork.c	/* name of file needed for VFORK option */
RAWONLY = 1
NOBUF = 1
APOLLO=apollo.c
#endif

#ifdef future
#if exists("/dev/sxt/.")
SXT = 1		/* set to 1 if there is an SXT driver */
#endif
#endif future

#if exists("/dev/fd/.")
DEVFD = 1	/* set to 1 if there is a /dev/fd directory */
#endif

#if architecture(ibm) || release(uts)
DRAWONLY = 1
U370 = U370	/* for ibms and amdahls */
NOBUF = 1
#endif

#if	architecture(pdp11) || architecture(i8086)
INT16 = 1
LDFLAGS = -i
#endif

.ALL:	$(SH) $(SUID_EXEC:T=V)

$(SH)::	(KSHELL) (BSD) (BSD_4_2) (BSD_4_1c) (INT16) (SXT)  \
	shlib/$(ARK) shlib/$(ARK) $(JOBLIB) \
	$(APOLLO) args.c blok.c brkincr.h builtin.c sh/builtins.h cmd.c \
	ctype.c defs.h sh/defs.c $(EDIT) edit.h $(EMACS) echo.c \
	error.c expand.c fault.c sh/flags.h history.c history.h \
	io.c io.h $(JOBFILE) jobs.h macro.c main.c mode.h msg.c name.c \
	sh/name.h sh/national.h print.c service.c string.c $(MYIO) \
	sh/shtype.h stak.c stak.h sym.h test.c test.h timeout.h \
	$(VFORKFILE) $(VI) word.c xec.c

args.o defs.o error.o history.o io.o $(JOBFILE:BS=.o) main.o msg.o \
	print.o xec.o: (ESH) (VSH)

args.o edit.o vi.o:	(RAWONLY)

builtin.o args.o defs.o error.o fault.o $(JOBFILE:BS=.o) macro.o main.o msg.o \
	print.o service.o word.o $(VFORKFILE:BS=.o) xec.o: (JOBS)

args.o cmd.o msg.o word.o xec.o:	(DEVFD)

builtin.o msg.o name.o:		(ECHO_N)

error.o fault.o main.o service.o xec.o:	(VFORK)

emacs.o edit.o history.o macro.o msg.o string.o vi.o assign.o gmatch.o:	(MULTIBYTE)

error.o main.o msg.o service.o xec.o:	(ACCT)

main.o msg.o service.o:	(SUID_EXEC)

defs.o:		(NOBUF)

(ECHO_N):
	if	test -f /bin/echo -a X"`/bin/echo -n`" = X
	then	@ ECHO_N=1
	fi

jsh/$(JSH)::	(BSD)  jsh/defs.h jsh/defs.c jsh.c mpx.c

	
shlib/$(ARK)::	(BSD)  (KSHELL) adjust.c arith.c assign.c \
	assnum.c shlib/builtins.h  cannon.c chkid.c convert.c failed.c \
	findnod.c shlib/flags.h gettree.c gmatch.c growaray.c gsort.c \
	linknod.c shlib/name.h shlib/national.h  namscan.c rjust.c \
	shlib/shtype.h tilde.c unassign.c utos.c valup.c

suid_exec::	suid_exec.c (BSD) (BSD_4_2)

editlib:	nokshell sh/$(EDITLIB)

nokshell:	.MAKE
	KSHELL=

sh/$(EDITLIB)::	nokshell (BSD)  edit.c edit.h editlib.c $(EMACS) \
		history.c history.h  $(VI)  $(SYSCALL)

msg.o ctype.o:	.READONLY

/* The following rule is only for the benefit of cpio, tar and print */
ConditionalFiles::	syscall.s apollo.c edit.c jobs.c vi.c emacs.c \
			stdio.c vfork.c $(DOCFILES)
	
