/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)crt0.c	8.1 (Berkeley) %G%";
#endif /* not lint */

/*
 *	C start up routine.
 */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

char **environ = (char **)0;
static char empty[1];
char *__progname = empty;

extern unsigned char etext[];
extern volatile void start() asm("start0");
extern unsigned char eprol[] asm("eprol");
extern void _mcleanup(void);

volatile void
start(void)
{
	struct kframe {
		int	regarea[16];	/* space for %i and %o variables */
		int	kargc;		/* argument count */
		char	*kargv[1];	/* actual size depends on kargc */
	};
	register struct kframe *sp asm("%sp");
	register int argc;
	register char **argv, **envp;
	extern int errno;

asm(".globl start");
asm("start:");
	argc = sp->kargc;
	argv = &sp->kargv[0];
	environ = envp = &argv[argc + 1];
	sp = (struct kframe *)((int)sp - 16);
asm("eprol:");

#ifdef paranoid
	/*
	 * The standard I/O library assumes that file descriptors 0, 1, and 2
	 * are open. If one of these descriptors is closed prior to the start 
	 * of the process, I/O gets very confused. To avoid this problem, we
	 * insure that the first three file descriptors are open before calling
	 * main(). Normally this is undefined, as it adds two unnecessary
	 * system calls.
	 */
    {
	register int fd;
	do {
		fd = open("/dev/null", 2);
	} while (fd >= 0 && fd < 3);
	close(fd);
    }
#endif

#ifdef MCRT0
	monstartup(eprol, etext);
	atexit(_mcleanup);
	errno = 0;
#endif
	if (argv[0])
		if ((__progname = strrchr(argv[0], '/')) == NULL)
			__progname = argv[0];
		else
			++__progname;
	exit(main(argc, argv, envp));
}

#ifdef CRT0
/*
 * null mcount and moncontrol,
 * just in case some routine is compiled for profiling
 */
asm(".globl mcount");
asm(".globl _moncontrol");
asm("mcount: _moncontrol: retl; nop");
#endif
