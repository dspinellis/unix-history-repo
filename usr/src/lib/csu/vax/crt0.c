/*-
 * Copyright (c) 1982 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)crt0.c	5.8 (Berkeley) %G%";
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
 */

char **environ = (char **)0;
static int fd;

extern	unsigned char	etext;
extern	unsigned char	eprol asm ("eprol");
extern			start() asm("start");

/*
 * Two kluges: store sp at entry in environ, and
 * install 16 bits of 0 at location 0 (a zero register save mask).
 * These two hacks remove limits on the use of local
 * and register variables in start().
 */
asm(".text; .word 0; movl sp,_environ; jbr start+2");

start()
{
	struct kframe {
		int	kargc;
		char	*kargv[1];	/* size depends on kargc */
		char	kargstr[1];	/* size varies */
		char	kenvstr[1];	/* size varies */
	};
	register struct kframe *kfp;
	register char **targv;
	register char **argv;
	extern int errno;

#ifdef lint
	kfp = 0;
	initcode = initcode = 0;
#else not lint
	kfp = (struct kframe *) environ;
#endif not lint
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
#endif paranoid

#ifdef MCRT0
	monstartup(&eprol, &etext);
#endif MCRT0
	errno = 0;
	exit(main(kfp->kargc, argv, environ));
}

#ifdef MCRT0
/*ARGSUSED*/
exit(code)
	register int code;
{
	monitor(0);
	_cleanup();
	_exit(code);
}
#endif MCRT0

#ifdef CRT0
/*
 * null mcount and moncontrol,
 * just in case some routine is compiled for profiling
 */
moncontrol(val)
	int val;
{

}
asm(".globl mcount");
asm("mcount: rsb");
#endif CRT0
