/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)sigretro.h	5.1 (Berkeley) %G%
 */

/*
 * Define extra stuff not found in signal.h
 */

#ifndef SIGRETRO

#define	SIGRETRO				/* Can use this for cond code */

#ifndef SIG_HOLD

#define	SIG_HOLD	(int (*)()) 3		/* Phony action to hold sig */
#define	BADSIG		(int (*)()) -1		/* Return value on error */

#endif SIG_HOLD

#endif SIGRETRO
