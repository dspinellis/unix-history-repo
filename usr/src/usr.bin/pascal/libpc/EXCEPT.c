/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)EXCEPT.c 1.1 %G%";

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
#else
	switch (type) {
	case 1:
		ERROR("Integer overflow\n");
	case 2:
		ERROR("Integer division by zero\n");
	case 3:
		ERROR("Real overflow\n");
	case 4:
		ERROR("Real division by zero\n");
	case 5:
		ERROR("Real underflow\n");
	default:
		ERROR("Panic: Computational error in interpreter\n");
	}
#endif
}
