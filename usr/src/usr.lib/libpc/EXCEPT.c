/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)EXCEPT.c 1.4 10/1/83";

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
