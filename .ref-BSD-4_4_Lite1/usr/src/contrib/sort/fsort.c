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
static char sccsid[] = "@(#)fsort.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Read in the next bin.  If it fits in one segment sort it;
 * otherwise refine it by segment deeper by one character,
 * and try again on smaller bins.  Sort the final bin at this level
 * of recursion to keep the head of fstack at 0.
 * After PANIC passes, abort to merge sort.
*/
#include "sort.h"
#include "fsort.h"

#include <stdlib.h>
#include <string.h>

u_char **keylist = 0, *buffer = 0, *linebuf = 0;
struct tempfile fstack[MAXFCT];
extern char *toutpath;
#define FSORTMAX 4
int PANIC = FSORTMAX;

void
fsort(binno, depth, infiles, nfiles, outfd, ftbl)
	register int binno, depth, nfiles;
	register union f_handle infiles;
	FILE *outfd;
	register struct field *ftbl;
{
	register u_char *bufend, **keypos, *tmpbuf;
	u_char *weights;
	int ntfiles, mfct = 0, total, i, maxb, lastb, panic = 0;
	register int c, nelem;
	long sizes [NBINS+1];
	union f_handle tfiles, mstart = {MAXFCT-16};
	register int (*get)(int, union f_handle, int, RECHEADER *,
		u_char *, struct field *);
	register struct recheader *crec;
	struct field tfield[2];
	FILE *prevfd, *tailfd[FSORTMAX+1];

	memset(tailfd, 0, sizeof(tailfd));
	prevfd = outfd;
	memset(tfield, 0, sizeof(tfield));
	if (ftbl[0].flags & R)
		tfield[0].weights = Rascii;
	else
		tfield[0].weights = ascii;
	tfield[0].icol.num = 1;
	weights = ftbl[0].weights;
	if (!buffer) {
		buffer = malloc(BUFSIZE);
		keylist = malloc(MAXNUM * sizeof(u_char *));
		if (!SINGL_FLD)
			linebuf = malloc(MAXLLEN);
	}
	bufend = buffer + BUFSIZE;
	if (binno >= 0) {
		tfiles.top = infiles.top + nfiles;
		get = getnext;
	} else {
		tfiles.top = 0;
		if (SINGL_FLD)
			get = makeline;
		else
			get = makekey;
	}				
	for (;;) {
		memset(sizes, 0, sizeof(sizes));
		c = ntfiles = 0;
		if (binno == weights[REC_D] &&
		    !(SINGL_FLD && ftbl[0].flags & F)) {	/* pop */
			rd_append(weights[REC_D],
			    infiles, nfiles, prevfd, buffer, bufend);
			break;
		} else if (binno == weights[REC_D]) {
			depth = 0;		/* start over on flat weights */
			ftbl = tfield;
			weights = ftbl[0].weights;
		}
		while (c != EOF) {
			keypos = keylist;
			nelem = 0;
			crec = (RECHEADER *) buffer;
			while((c = get(binno, infiles, nfiles, crec, bufend,
			    ftbl)) == 0) {
				*keypos++ = crec->data + depth;
				if (++nelem == MAXNUM) {
					c = BUFFEND;
					break;
				}
				crec =(RECHEADER *)	((char *) crec +
				SALIGN(crec->length) + sizeof(TRECHEADER));
			}
			if (c == BUFFEND || ntfiles || mfct) {	/* push */
				if (panic >= PANIC) {
					fstack[MAXFCT-16+mfct].fd = ftmp();
					if (radixsort(keylist, nelem, weights,
					    REC_D))
						err(2, NULL);
					append(keylist, nelem, depth, fstack[
					 MAXFCT-16+mfct].fd, putrec, ftbl);
					mfct++;
					/* reduce number of open files */
					if (mfct == 16 ||(c == EOF && ntfiles)) {
						tmpbuf = malloc(bufend -
						    crec->data);
						memmove(tmpbuf, crec->data,
						    bufend - crec->data);
						fstack[tfiles.top + ntfiles].fd
						    = ftmp();
						fmerge(0, mstart, mfct, geteasy,
						  fstack[tfiles.top+ntfiles].fd,
						  putrec, ftbl);
						++ntfiles;
						mfct = 0;
						memmove(crec->data, tmpbuf,
						    bufend - crec->data);
						free(tmpbuf);
					}
				} else {
					fstack[tfiles.top + ntfiles].fd= ftmp();
					onepass(keylist, depth, nelem, sizes,
					weights, fstack[tfiles.top+ntfiles].fd);
					++ntfiles;
				}
			}
		}
		get = getnext;
		if (!ntfiles && !mfct) {	/* everything in memory--pop */
			if (nelem > 1)
			   if (radixsort(keylist, nelem, weights, REC_D))
				err(2, NULL);
			append(keylist, nelem, depth, outfd, putline, ftbl);
			break;					/* pop */
		}
		if (panic >= PANIC) {
			if (!ntfiles)
				fmerge(0, mstart, mfct, geteasy,
				    outfd, putline, ftbl);
			else
				fmerge(0, tfiles, ntfiles, geteasy,
				    outfd, putline, ftbl);
			break;
				
		}
		total = maxb = lastb = 0;	/* find if one bin dominates */
		for (i = 0; i < NBINS; i++)
		  if (sizes[i]) {
			if (sizes[i] > sizes[maxb])
				maxb = i;
			lastb = i;
			total += sizes[i];
		}
		if (sizes[maxb] < max((total / 2) , BUFSIZE))
			maxb = lastb;	/* otherwise pop after last bin */
		fstack[tfiles.top].lastb = lastb;
		fstack[tfiles.top].maxb = maxb;

			/* start refining next level. */
		get(-1, tfiles, ntfiles, crec, bufend, 0);	/* rewind */
		for (i = 0; i < maxb; i++) {
			if (!sizes[i])	/* bin empty; step ahead file offset */
				get(i, tfiles, ntfiles, crec, bufend, 0);
			else
				fsort(i, depth+1, tfiles, ntfiles, outfd, ftbl);
		}
		if (lastb != maxb) {
			if (prevfd != outfd)
				tailfd[panic] = prevfd;
			prevfd = ftmp();
			for (i = maxb+1; i <= lastb; i++)
				if (!sizes[i])
					get(i, tfiles, ntfiles, crec, bufend,0);
				else
					fsort(i, depth+1, tfiles, ntfiles,
					    prevfd, ftbl);
		}

		/* sort biggest (or last) bin at this level */
		depth++;
		panic++;
		binno = maxb;
		infiles.top = tfiles.top;	/* getnext will free tfiles, */
		nfiles = ntfiles;		/* so overwrite them */
	}
	if (prevfd != outfd) {
		concat(outfd, prevfd);
		fclose(prevfd);
	}
	for (i = panic; i >= 0; --i)
		if (tailfd[i]) {
			concat(outfd, tailfd[i]);
			fclose(tailfd[i]);
		}
}

/*
 This is one pass of radix exchange, dumping the bins to disk.
 */
#define swap(a, b, t) t = a, a = b, b = t
void
onepass(a, depth, n, sizes, tr, fd)
	u_char **a;
	int depth;
	long n, sizes[];
	u_char *tr;
	FILE *fd;
{
	long tsizes[NBINS+1];
	u_char **bin[257], **top[256], ***bp, ***bpmax, ***tp;
	static histo[256];
	int *hp;
	register int c;
	u_char **an, *t, **aj;
	register u_char **ak, *r;

	memset(tsizes, 0, sizeof(tsizes));
	depth += sizeof(TRECHEADER);
	an = a + n;
	for (ak = a; ak < an; ak++) {
		histo[c = tr[**ak]]++;
		tsizes[c] += ((RECHEADER *) (*ak -= depth))->length;
	}

	bin[0] = a;
	bpmax = bin + 256;
	tp = top, hp = histo;
	for (bp = bin; bp < bpmax; bp++) {
		*tp++ = *(bp+1) = *bp + (c = *hp);
		*hp++ = 0;
		if (c <= 1)
			continue;
	}
	for(aj = a; aj < an; *aj = r, aj = bin[c+1]) 
		for(r = *aj; aj < (ak = --top[c = tr[r[depth]]]) ;)			
			swap(*ak, r, t);

	for (ak = a, c = 0; c < 256; c++) {
		an = bin[c+1];
		n = an - ak;
		tsizes[c] += n * sizeof(TRECHEADER);
		/* tell getnext how many elements in this bin, this segment. */
		EWRITE(tsizes+c, sizeof(long), 1, fd);
		sizes[c] += tsizes[c];
		for (; ak < an; ++ak)
			putrec((RECHEADER *) *ak, fd);
	}
}
