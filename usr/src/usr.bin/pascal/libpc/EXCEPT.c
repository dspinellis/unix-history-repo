/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)EXCEPT.c 1.2 %G%";

#include	<signal.h>
#include	"whoami.h"

/*
 * catch runtime arithmetic errors
 */
EXCEPT(signum, type)
	int signum, type;
{
	signal(SIGFPE, EXCEPT);
#ifndef VAX
	ERROR("Overflow, underflow, or division by zero in arithmetic operation\n");
	return;
#else
	/*
	 * The values for this switch statement come from page 12-5 of
	 * Volume 1 of the 1978 VAX 11/780 Architecture Handbook
	 */
	switch (type) {
	case 1:
		ERROR("Integer overflow\n");
		return;
	case 2:
		ERROR("Integer division by zero\n");
		return;
	case 3:
		ERROR("Real overflow\n");
		return;
	case 4:
		ERROR("Real division by zero\n");
		return;
	case 5:
		ERROR("Real underflow\n");
		return;
	default:
		ERROR("Panic: Computational error in interpreter\n");
		return;
	}
#endif
}
