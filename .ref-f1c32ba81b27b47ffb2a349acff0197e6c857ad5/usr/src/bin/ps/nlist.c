/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)nlist.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <nlist.h>
#include <errno.h>
#include <stdio.h>

struct	nlist psnl[] = {
	{"_ecmx"},
#define	X_ECMX		0
	{"_fscale"},
#define	X_FSCALE	1
	{"_ccpu"},
#define	X_CCPU		2
	{NULL}
};

fixpt_t	ccpu;				/* kernel _ccpu variable */
int	nlistread;			/* if nlist already read. */
int	ecmx;				/* kernel _ecmx variable */
int	fscale;				/* kernel _fscale variable */

#define kread(x, v) \
	kvm_read(psnl[x].n_value, (char *)&v, sizeof v) != sizeof(v)

donlist()
{
	extern int eval;
	int rval;

	rval = 0;
	nlistread = 1;
	if (kvm_nlist(psnl)) {
		(void)fprintf(stderr, "ps: kvm_nlist: %s\n", strerror(errno));
		eval = 1;
		return(1);
	}
	if (kread(X_FSCALE, fscale)) {
		(void)fprintf(stderr, "ps: fscale: %s\n", kvm_geterr());
		eval = rval = 1;
	}
	if (kread(X_ECMX, ecmx)) {
		(void)fprintf(stderr, "ps: ecmx: %s\n", kvm_geterr());
		eval = rval = 1;
	}
	if (kread(X_CCPU, ccpu)) {
		(void)fprintf(stderr, "ps: ccpu: %s\n", kvm_geterr());
		eval = rval = 1;
	}
	return(rval);
}

