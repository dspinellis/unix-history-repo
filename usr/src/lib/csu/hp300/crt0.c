/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)crt0.c	8.1 (Berkeley) %G%";
#endif /* not lint */

/*
 *	C start up routine.
 *	Robert Henry, UCB, 20 Oct 81
 *
 *	We make the following (true) assumptions:
 *	1) when the kernel calls start, it does a jump to location 2,
 *	and thus avoids the register save mask.  We are NOT called
 *	with a calls!  see sys1.c:setregs().
 *	2) The only register variable that we can trust is sp,
 *	which points to the base of the kernel calling frame.
 *	Do NOT believe the documentation in exec(2) regarding the
 *	values of fp and ap.
 *	3) We can allocate as many register variables as we want,
 *	and don't have to save them for anybody.
 *	4) Because of the ways that asm's work, we can't have
 *	any automatic variables allocated on the stack, because
 *	we must catch the value of sp before any automatics are
 *	allocated.
 */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

char **environ = (char **)0;
static char empty[1];
char *__progname = empty;
static int fd;

extern	unsigned char	etext;
extern	unsigned char	eprol asm ("eprol");
extern			start() asm("start");

asm(".text; orb #0,d0");	/* 32 bits of zero at location 0 */

start()
{
	struct kframe {
		int	kargc;
		char	*kargv[1];	/* size depends on kargc */
		char	kargstr[1];	/* size varies */
		char	kenvstr[1];	/* size varies */
	};
	/*
	 *	ALL REGISTER VARIABLES!!!
	 */
	register struct kframe *kfp;	/* r10 */
	register char **targv;
	register char **argv;
	extern int errno;
	extern void _mcleanup();

#ifdef lint
	kfp = 0;
	initcode = initcode = 0;
#else
	asm("lea a6@(4),%0" : "=r" (kfp));	/* catch it quick */
#endif
	for (argv = targv = &kfp->kargv[0]; *targv++; /* void */)
		/* void */ ;
	if (targv >= (char **)(*argv))
		--targv;
	environ = targv;
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
	do	{
		fd = open("/dev/null", 2);
	} while (fd >= 0 && fd < 3);
	close(fd);
#endif

#ifdef MCRT0
	atexit(_mcleanup);
	monstartup(&eprol, &etext);
#endif
	errno = 0;
	if (argv[0])
		if ((__progname = strrchr(argv[0], '/')) == NULL)
			__progname = argv[0];
		else
			++__progname;
	exit(main(kfp->kargc, argv, environ));
}

#ifdef CRT0
/*
 * null moncontrol, just in case some routine is compiled for profiling
 */
moncontrol(val)
	int val;
{

}
#endif
