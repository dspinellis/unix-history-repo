/*-
 * Copyright (c) 1980, 1993
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
static char sccsid[] = "@(#)functab.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * This file contains the implementation of a table for going
 * from object addresses to the functions in which they belong.
 */

#include "defs.h"
#include "mappings.h"
#include "sym.h"

#define MAXNFUNCS 1001		/* maximum number of functions allowed */

LOCAL SYM *functab[MAXNFUNCS];
LOCAL int nfuncs;

/*
 * Insert a new function into the table.
 * The table is ordered by object address.
 */

newfunc(f)
SYM *f;
{
	register int i, j;
	ADDRESS a;

	if (nfuncs >= MAXNFUNCS) {
		panic("too many procedures/functions");
	}
	a = codeloc(f);
	i = 0;
	while (i < nfuncs && codeloc(functab[i]) < a) {
		i++;
	}
	for (j = nfuncs; j > i; j--) {
		functab[j] = functab[j - 1];
	}
	functab[i] = f;
	nfuncs++;
}

/*
 * Return the function that begins at the given address.
 */

SYM *whatblock(addr)
ADDRESS addr;
{
	register int i, j, k;
	ADDRESS a;

	i = 0;
	j = nfuncs - 1;
	if (addr < codeloc(functab[i])) {
		return program;
	} else if (addr == codeloc(functab[i])) {
		return functab[i];
	} else if (addr >= codeloc(functab[j])) {
		return functab[j];
	}
	while (i <= j) {
		k = (i + j) / 2;
		a = codeloc(functab[k]);
		if (a == addr) {
			return functab[k];
		} else if (addr > a) {
			i = k+1;
		} else {
			j = k-1;
		}
	}
	if (addr > codeloc(functab[i])) {
		return functab[i];
	} else {
		return functab[i-1];
	}
	/* NOTREACHED */
}

/*
 * Clear out the functab, used when re-reading the object information.
 */

clrfunctab()
{
	nfuncs = 0;
}
