/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)subr.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/time.h>
#include <sys/ktrace.h>
#include <stdio.h>
#include "ktrace.h"

getpoints(s)
	char *s;
{
	int facs = 0;

	while (*s) {
		switch(*s) {
		case 'c':
			facs |= KTRFAC_SYSCALL | KTRFAC_SYSRET;
			break;
		case 'n':
			facs |= KTRFAC_NAMEI;
			break;
		case 'i':
			facs |= KTRFAC_GENIO;
			break;
		case 's':
			facs |= KTRFAC_PSIG;
			break;
		case 'w':
			facs |= KTRFAC_CSW;
			break;
		case '+':
			facs |= DEF_POINTS;
			break;
		default:
			return (-1);
		}
		s++;
	}
	return (facs);
}

timevaladd(t1, t2)
	struct timeval *t1, *t2;
{
	t1->tv_sec += t2->tv_sec;
	t1->tv_usec += t2->tv_usec;
	timevalfix(t1);
}

timevalsub(t1, t2)
	struct timeval *t1, *t2;
{
	t1->tv_sec -= t2->tv_sec;
	t1->tv_usec -= t2->tv_usec;
	timevalfix(t1);
}

timevalfix(t1)
	struct timeval *t1;
{
	if (t1->tv_usec < 0) {
		t1->tv_sec--;
		t1->tv_usec += 1000000;
	}
	if (t1->tv_usec >= 1000000) {
		t1->tv_sec++;
		t1->tv_usec -= 1000000;
	}
}
