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
static char sccsid[] = "@(#)srcline.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * lookup the source line number nearest from below to an address
 */

#include "defs.h"
#include "mappings.h"
#include "object.h"
#include "linetab.h"

LINENO srcline(addr)
ADDRESS addr;
{
	register ADDRESS i, j, k;
	ADDRESS a;

	if (nlhdr.nlines == 0) {
		return(0);
	}
	i = 0;
	j = nlhdr.nlines - 1;
	if (addr <= linetab[i].addr) {
		return(linetab[i].line);
	} else if (addr >= linetab[j].addr) {
		return(linetab[j].line);
	}
	while (i <= j) {
		k = (i + j) / 2;
		if ((a = linetab[k].addr) == addr) {
			return(linetab[k].line);
		} else if (addr > a) {
			i = k+1;
		} else {
			j = k-1;
		}
	}
	if (addr > linetab[i].addr) {
		return(linetab[i].line);
	} else {
		return(linetab[i-1].line);
	}
	/*NOTREACHED*/
}

/*
 * look for a line exactly corresponding to the given address
 */

LINENO linelookup(addr)
ADDRESS addr;
{
	register ADDRESS i, j, k;
	ADDRESS a;

	if (nlhdr.nlines == 0 || addr < linetab[0].addr) {
		return(0);
	}
	i = 0;
	j = nlhdr.nlines - 1;
	while (i <= j) {
		k = (i + j) / 2;
		if ((a = linetab[k].addr) == addr) {
			return(linetab[k].line);
		} else if (addr > a) {
			i = k+1;
		} else {
			j = k-1;
		}
	}
	return(0);
}
