/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)nlist.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/proc.h>
#include <nlist.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <kvm.h>

#ifdef SPPWAIT
#define NEWVM
#endif

struct	nlist psnl[] = {
	{"_fscale"},
#define	X_FSCALE	0
	{"_ccpu"},
#define	X_CCPU		1
#ifdef NEWVM
	{"_avail_start"},
#define	X_AVAILSTART	2
	{"_avail_end"},
#define	X_AVAILEND	3
#else
	{"_ecmx"},
#define	X_ECMX		2
#endif
	{NULL}
};

fixpt_t	ccpu;				/* kernel _ccpu variable */
int	nlistread;			/* if nlist already read. */
int	mempages;			/* number of pages of phys. memory */
int	fscale;				/* kernel _fscale variable */

extern kvm_t *kd;

#define kread(x, v) \
	kvm_read(kd, psnl[x].n_value, (char *)&v, sizeof v) != sizeof(v)

donlist()
{
	extern int eval;
	int rval;
#ifdef NEWVM
	int tmp;
#endif

	rval = 0;
	nlistread = 1;
	if (kvm_nlist(kd, psnl)) {
		nlisterr(psnl);
		eval = 1;
		return(1);
	}
	if (kread(X_FSCALE, fscale)) {
		(void)fprintf(stderr, "ps: fscale: %s\n", kvm_geterr(kd));
		eval = rval = 1;
	}
#ifdef NEWVM
	if (kread(X_AVAILEND, mempages)) {
		(void)fprintf(stderr, "ps: avail_start: %s\n", kvm_geterr(kd));
		eval = rval = 1;
	}
	if (kread(X_AVAILSTART, tmp)) {
		(void)fprintf(stderr, "ps: avail_end: %s\n", kvm_geterr(kd));
		eval = rval = 1;
	}
	mempages -= tmp;
#else
	if (kread(X_ECMX, mempages)) {
		(void)fprintf(stderr, "ps: ecmx: %s\n", kvm_geterr(kd));
		eval = rval = 1;
	}
#endif
	if (kread(X_CCPU, ccpu)) {
		(void)fprintf(stderr, "ps: ccpu: %s\n", kvm_geterr(kd));
		eval = rval = 1;
	}
	return(rval);
}

nlisterr(nl)
	struct nlist nl[];
{
	int i;

	fprintf(stderr, "ps: nlist: can't find following symbols:");
	for (i = 0; nl[i].n_name != NULL; i++)
		if (nl[i].n_value == 0)
			fprintf(stderr, " %s", nl[i].n_name);
	fprintf(stderr, "\n");
}
