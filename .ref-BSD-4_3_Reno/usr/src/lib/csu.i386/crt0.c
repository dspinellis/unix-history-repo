/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)crt0.c	5.2 (Berkeley) 5/14/90";
#endif /* not lint */

/*
 *	C start up routine.
 *	Robert Henry, UCB, 20 Oct 81
 *
 *	We make the following (true) assumption:
 *	1) The only register variable that we can trust is ebp,
 *	which points to the base of the kernel calling frame.
 */

char **environ = (char **)0;
static int fd;

asm("#define _start start");
asm("#define _eprol eprol");
asm("	.text");
asm("	.long 0xc000c000");
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
	register struct kframe *kfp;	/* r10 */
	register char **targv;
	register char **argv;
	extern int errno;

#ifdef lint
	kfp = 0;
	initcode = initcode = 0;
#else not lint
	asm("	lea	4(%ebp),%ebx");	/* catch it quick */
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
asm("#undef _start");
asm("#undef _eprol");

#ifdef MCRT0
/*ARGSUSED*/
exit(code)
	register int code;	/* r11 */
{
	monitor(0);
	_cleanup();
	asm("	pushl	8(bp)") ;
	asm("	movl $1,%eax");
	asm("	.byte 0x9a; .long 0; .word 0");
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
asm("mcount:	ret");
#endif CRT0
