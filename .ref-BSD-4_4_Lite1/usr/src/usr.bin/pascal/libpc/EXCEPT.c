/*-
 * Copyright (c) 1982, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)EXCEPT.c	8.1 (Berkeley) 6/6/93";
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
