/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit.
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
static char sccsid[] = "@(#)misc.c	5.6 (Berkeley) 2/26/91";
#endif /* not lint */

/*
 * misc.c
 * Facility: m4 macro processor
 * by: oz
 */
 
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mdef.h"
#include "extr.h" 
#include "pathnames.h"
 
/*
 * indx - find the index of second str in the
 *        first str.
 */
indx(s1, s2)
char *s1;
char *s2;
{
        register char *t;
        register char *p;
        register char *m;
 
        for (p = s1; *p; p++) {
                for (t = p, m = s2; *m && *m == *t; m++, t++)
                        ;
                if (!*m)
                        return(p - s1);
        }
        return (-1);
}
 
/*
 *  putback - push character back onto input
 *
 */
putback (c)
char c;
{
        if (bp < endpbb)
                *bp++ = c;
        else
                error("m4: too many characters pushed back");
}
 
/*
 *  pbstr - push string back onto input
 *          putback is replicated to improve
 *          performance.
 *
 */
pbstr(s)
register char *s;
{
        register char *es;
	register char *zp;

	es = s;
	zp = bp;

        while (*es)
                es++;
        es--;
        while (es >= s)
                if (zp < endpbb)
                        *zp++ = *es--;
        if ((bp = zp) == endpbb)
                error("m4: too many characters pushed back");
}
 
/*
 *  pbnum - convert number to string, push back on input.
 *
 */
pbnum (n)
int n;
{
        register int num;
 
        num = (n < 0) ? -n : n;
        do {
                putback(num % 10 + '0');
        }
        while ((num /= 10) > 0);

        if (n < 0) putback('-');
}
 
/*
 *  chrsave - put single char on string space
 *
 */
chrsave (c)
char c;
{
/***        if (sp < 0)
                putc(c, active);
        else ***/ if (ep < endest)
                *ep++ = c;
        else
                error("m4: string space overflow");
}
 
/*
 * getdiv - read in a diversion file, and
 *          trash it.
 */
getdiv(ind) {
        register int c;
        register FILE *dfil;
 
        if (active == outfile[ind])
                error("m4: undivert: diversion still active.");
        (void) fclose(outfile[ind]);
        outfile[ind] = NULL;
        m4temp[UNIQUE] = ind + '0';
        if ((dfil = fopen(m4temp, "r")) == NULL)
                error("m4: cannot undivert.");
        else
                while((c = getc(dfil)) != EOF)
                        putc(c, active);
        (void) fclose(dfil);

	if (unlink(m4temp) == -1)
                error("m4: cannot unlink.");
}
 
/*
 * Very fatal error. Close all files
 * and die hard.
 */
error(s)
char *s;
{
        killdiv();
        fprintf(stderr,"%s\n",s);
        exit(1);
}
 
/*
 * Interrupt handling
 */
static char *msg = "\ninterrupted.";
 
void
onintr() {
        error(msg);
}
 
/*
 * killdiv - get rid of the diversion files
 *
 */
killdiv() {
        register int n;
 
        for (n = 0; n < MAXOUT; n++)
                if (outfile[n] != NULL) {
                        (void) fclose (outfile[n]);
                        m4temp[UNIQUE] = n + '0';
                        (void) unlink (m4temp);
                }
}
 
usage() {
        fprintf(stderr, "usage: m4 [-Dname[=val]] [-Uname]\n");
        exit(1);
}
