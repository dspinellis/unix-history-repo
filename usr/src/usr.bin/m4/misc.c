/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	5.5 (Berkeley) %G%";
#endif /* not lint */

/*
 * misc.c
 * Facility: m4 macro processor
 * by: oz
 */
 
#include "mdef.h"
#include "extr.h" 
 
extern char *malloc();
 
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
 
/*
 * save a string somewhere..
 *
 */
char *strsave(s)
char *s;
{
	register int n;
        char *p;

        if ((p = malloc (n = strlen(s)+1)) != NULL)
                (void) memcpy(p, s, n);
        return (p);
}
 
usage() {
        fprintf(stderr, "Usage: m4 [-Dname[=val]] [-Uname]\n");
        exit(1);
}
