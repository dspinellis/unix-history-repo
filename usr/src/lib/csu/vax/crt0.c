/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)crt0.c	5.1 (Berkeley) %G%";
#endif not lint

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

char **environ = (char **)0;

asm("#define _start start");
asm("#define _eprol eprol");
extern	unsigned char	etext;
extern	unsigned char	eprol;
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
	register int r11;		/* needed for init */
	register struct kframe *kfp;	/* r10 */
	register char **targv;
	register char **argv;

#ifdef lint
	kfp = 0;
	initcode = initcode = 0;
#else not lint
	asm("	movl	sp,r10");	/* catch it quick */
#endif not lint
	for (argv = targv = &kfp->kargv[0]; *targv++; /* void */)
		/* void */ ;
	if (targv >= (char **)(*argv))
		--targv;
	environ = targv;
asm("eprol:");
#ifdef MCRT0
	monstartup(&eprol, &etext);
#endif MCRT0
	exit(main(kfp->kargc, argv, environ));
}
asm("#undef _start");
asm("#undef _eprol");

#ifdef MCRT0
/*ARGSUSED*/
exit(code)
	register int code;	/* r11 */
{
	monitor(0);
	_cleanup();
	asm("	movl r11,r0");
	asm("	chmk $1");
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
asm("	.globl	mcount");
asm("mcount:	rsb");
#endif CRT0
