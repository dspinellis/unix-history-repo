/*-
 * Copyright (c) 1982 The Regents of the University of California.
 * All rights reserved.
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
static char sccsid[] = "@(#)EXCEPT.c	1.5 (Berkeley) 4/9/90";
#endif /* not lint */

#include	<signal.h>

/*
 * catch runtime arithmetic errors
 */
EXCEPT(signum, type)
	int signum, type;
{
	signal(SIGFPE, EXCEPT);
#ifndef vax
	ERROR("Overflow, underflow, or division by zero in arithmetic operation\n");
	return;
#endif notvax
#ifdef vax
	/*
	 * The values for this switch statement come from page 12-5 of
	 * Volume 1 of the 1978 VAX 11/780 Architecture Handbook
	 */
	switch (type) {
	case FPE_INTOVF_TRAP:
		ERROR("Integer overflow\n");
		return;
	case FPE_INTDIV_TRAP:
		ERROR("Integer division by zero\n");
		return;
	case FPE_FLTOVF_TRAP:
	case FPE_FLTOVF_FAULT:
		ERROR("Real overflow\n");
		return;
	case FPE_FLTDIV_TRAP:
	case FPE_FLTDIV_FAULT:
		ERROR("Real division by zero\n");
		return;
	case FPE_FLTUND_TRAP:
	case FPE_FLTUND_FAULT:
		ERROR("Real underflow\n");
		return;
	case FPE_DECOVF_TRAP:
	case FPE_SUBRNG_TRAP:
	default:
		ERROR("Undefined arithmetic exception type (%d)\n", type);
		return;
	}
#endif vax
}
