/*
 * Copyright (c) 1993
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
static char sccsid[] = "@(#)intr.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include <sys/types.h>

#include <errno.h>
#include <kvm.h>
#include <nlist.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "extern.h"

static struct nlist nl[] = {
	{ "_intrcnt" },
#define	X_INTRCNT	0
	{ "_eintrcnt" },
#define	X_EINTRCNT	1
	{ "_intrnames" },
#define	X_INTRNAMES	2
	{ "_eintrnames" },
#define	X_EINTRNAMES	3
	0
};

void
dointr()
{
	register long *intrcnt, inttotal, uptime;
	register int nintr, inamlen;
	register char *intrname;

	knlist(nl);
	uptime = getuptime();
	nintr = nl[X_EINTRCNT].n_value - nl[X_INTRCNT].n_value;
	inamlen = nl[X_EINTRNAMES].n_value - nl[X_INTRNAMES].n_value;
	intrcnt = malloc((size_t)nintr);
	intrname = malloc((size_t)inamlen);
	if (intrcnt == NULL || intrname == NULL)
		errexit("malloc: %s\n", strerror(errno));
	kread(nl[X_INTRCNT].n_value, intrcnt, nintr, "intrcnt");
	kread(nl[X_INTRNAMES].n_value, intrname, inamlen, "intrnames");
	(void)printf("interrupt      total      rate\n");
	inttotal = 0;
	nintr /= sizeof(long);
	while (--nintr >= 0) {
		if (*intrcnt)
			(void)printf("%-12s %8ld %8ld\n", intrname,
			    *intrcnt, *intrcnt / uptime);
		intrname += strlen(intrname) + 1;
		inttotal += *intrcnt++;
	}
	(void)printf("Total        %8ld %8ld\n", inttotal, inttotal / uptime);
}
