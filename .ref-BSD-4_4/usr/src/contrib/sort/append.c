/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Peter McIlroy.
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
static char sccsid[] = "@(#)append.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "sort.h"

#include <stdlib.h>
#include <string.h>

#define OUTPUT {							\
	if ((n = cpos - ppos) > 1) {					\
		for (; ppos < cpos; ++ppos)				\
			*ppos -= odepth;				\
		ppos -= n;						\
		radixsort(ppos, n, wts1, REC_D);			\
		for (; ppos < cpos; ppos++) {				\
			prec = (RECHEADER *) (*ppos - sizeof(TRECHEADER));\
			put(prec, fd);					\
		}							\
	} else put(prec, fd);						\
}

/*
 * copy sorted lines to output; check for uniqueness
 */
void
append(keylist, nelem, depth, fd, put, ftbl)
	u_char **keylist;
	int nelem;
	register int depth;
	FILE *fd;
	void (*put)(RECHEADER *, FILE *);
	struct field *ftbl;
{
	register u_char *wts, *wts1;
	register n, odepth;
	register u_char **cpos, **ppos, **lastkey;
	register u_char *cend, *pend, *start;
	register struct recheader *crec, *prec;

	if (*keylist == '\0' && UNIQUE)
		return;
	wts1 = wts = ftbl[0].weights;
	if ((!UNIQUE) && SINGL_FLD) {
		if (ftbl[0].flags & F && ftbl[0].flags & R)
			wts1 = Rascii;
		else if (ftbl[0].flags & F)
			wts1 = ascii;
		odepth = depth;
	}
	lastkey = keylist + nelem;
	depth += sizeof(TRECHEADER);
	if (SINGL_FLD && (UNIQUE || wts1 != wts)) {
		ppos = keylist;
		prec = (RECHEADER *) (*ppos - depth);
		if (UNIQUE)
			put(prec, fd);
		for (cpos = keylist+1; cpos < lastkey; cpos++) {
			crec = (RECHEADER *) (*cpos - depth);
			if (crec->length  == prec->length) {
				pend = (u_char *) &prec->offset + prec->length;
				cend = (u_char *) &crec->offset + crec->length;
				for (start = *cpos; cend >= start; cend--) {
					if (wts[*cend] != wts[*pend])
						break;
					pend--;
				}
				if (pend + 1 != *ppos) {
					if (!UNIQUE) {
						OUTPUT;
					} else
						put(crec, fd);
					ppos = cpos;
					prec = crec;
				}
			} else {
				if (!UNIQUE) {
					OUTPUT;
				} else
					put(crec, fd);
				ppos = cpos;
				prec = crec;
			}
		}
		if (!UNIQUE)  { OUTPUT; }
	} else if (UNIQUE) {
		ppos = keylist;
		prec = (RECHEADER *) (*ppos - depth);
		put(prec, fd);
		for (cpos = keylist+1; cpos < lastkey; cpos++) {
			crec = (RECHEADER *) (*cpos - depth);
			if (crec->offset == prec->offset) {
				pend = (u_char *) &prec->offset + prec->offset;
				cend = (u_char *) &crec->offset + crec->offset;
				for (start = *cpos; cend >= start; cend--) {
					if (wts[*cend] != wts[*pend])
						break;
					pend--;
				}
				if (pend + 1 != *ppos) {
					ppos = cpos;
					prec = crec;
					put(prec, fd);
				}
			} else {
				ppos = cpos;
				prec = crec;
				put(prec, fd);
			}
		}
	} else for (cpos = keylist; cpos < lastkey; cpos++) {
		crec = (RECHEADER *) (*cpos - depth);
		put(crec, fd);
	}
}

/*
 * output the already sorted eol bin.
 */
void
rd_append(binno, infl0, nfiles, outfd, buffer, bufend)
	u_char *buffer, *bufend;
	int binno, nfiles;
	union f_handle infl0;
	FILE *outfd;
{
	struct recheader *rec;
	rec = (RECHEADER *) buffer;
	if (!getnext(binno, infl0, nfiles, (RECHEADER *) buffer, bufend, 0)) {
		putline(rec, outfd);
		while (getnext(binno, infl0, nfiles, (RECHEADER *) buffer,
			bufend, 0) == 0) {
			if (!UNIQUE)
				putline(rec, outfd);
		}
	}
}

/*
 * append plain text--used after sorting the biggest bin.
 */
void
concat(a, b)
	FILE *a, *b;
{
        int nread;
        char buffer[4096];

	rewind(b);
        while ((nread = fread(buffer, 1, 4096, b)) > 0)
                EWRITE(buffer, 1, nread, a);
}
