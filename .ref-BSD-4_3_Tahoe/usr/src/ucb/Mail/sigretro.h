/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)sigretro.h	5.2 (Berkeley) 2/18/88
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
