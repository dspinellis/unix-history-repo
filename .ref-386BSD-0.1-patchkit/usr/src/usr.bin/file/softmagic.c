/*
 * softmagic - interpret variable magic from /etc/magic
 *
 * Copyright (c) Ian F. Darwin, 1987.
 * Written by Ian F. Darwin.
 *
 * This software is not subject to any license of the American Telephone
 * and Telegraph Company or of the Regents of the University of California.
 *
 * Permission is granted to anyone to use this software for any purpose on
 * any computer system, and to alter it and redistribute it freely, subject
 * to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 *    software, no matter how awful, even if they arise from flaws in it.
 *
 * 2. The origin of this software must not be misrepresented, either by
 *    explicit claim or by omission.  Since few users ever read sources,
 *    credits must appear in the documentation.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.  Since few users
 *    ever read sources, credits must appear in the documentation.
 *
 * 4. This notice may not be removed or altered.
 */

#include <stdio.h>
#include "file.h"

#ifndef	lint
static char *moduleid = 
	"@(#)$Header: softmagic.c,v 1.7 87/11/06 11:25:31 ian Exp $";
#endif	/* lint */

extern char *progname;
extern char *magicfile;	/* name of current /etc/magic or clone */
extern int debug, nmagic;
extern FILE *efopen();
extern struct magic magic[];
static int magindex;

/*
 * softmagic - lookup one file in database 
 * (already read from /etc/magic by apprentice.c).
 * Passed the name and FILE * of one file to be typed.
 */
softmagic(buf)
char *buf;
{
	magindex = 0;
	if (match(buf))
		return 1;

	return 0;
}

/*
 * go through the whole list, stopping if you find a match.
 * Be sure to process every continuation of this match.
 */
match(s)
char	*s;
{
	while (magindex < nmagic) {
		/* if main entry matches, print it... */
		if (mcheck(s, &magic[magindex])) {
			mprint(&magic[magindex],s);
			/* and any continuations that match */
			while (magic[magindex+1].contflag &&
				magindex < nmagic) {
				++magindex;
				if (mcheck(s, &magic[magindex])){
					(void) putchar(' ');
					mprint(&magic[magindex],s);
				}
			}
			return 1;		/* all through */
		} else {
			/* main entry didn't match, flush its continuations */
			while (magic[magindex+1].contflag &&
				magindex < nmagic) {
				++magindex;
			}
		}
		++magindex;			/* on to the next */
	}
	return 0;				/* no match at all */
}


mprint(m,s)
struct magic *m;
char *s;
{
	register union VALUETYPE *p = (union VALUETYPE *)(s+m->offset);
	char *pp, *strchr();

	switch (m->type) {
	case BYTE:
		(void) printf(m->desc, p->b);
		break;
	case SHORT:
		(void) printf(m->desc, p->h);
		break;
	case LONG:
		(void) printf(m->desc, p->l);
		break;
	case STRING:
		if ((pp=strchr(p->s, '\n')) != NULL)
			*pp = '\0';
		(void) printf(m->desc, p->s);
		break;
	default:
		warning("invalid m->type (%d) in mprint()", m->type);
	}
}

int
mcheck(s, m)
char	*s;
struct magic *m;
{
	register union VALUETYPE *p = (union VALUETYPE *)(s+m->offset);
	register long l = m->value.l;
	register long v;

	if (debug) {
		(void) printf("mcheck: %10.10s ", s);
		mdump(m);
	}
	switch (m->type) {
	case BYTE:
		v = p->b; break;
	case SHORT:
		v = p->h; break;
	case LONG:
		v = p->l; break;
	case STRING:
		l = 0;
		/* What we want here is:
		 * v = strncmp(m->value.s, p->s, m->vallen);
		 * but ignoring any nulls.  bcmp doesn't give -/+/0
		 * and isn't universally available anyway.
		 */
		{
			register unsigned char *a = (unsigned char*)m->value.s;
			register unsigned char *b = (unsigned char*)p->s;
			register int len = m->vallen;

			while (--len >= 0)
				if ((v = *b++ - *a++) != 0)
					break;
		}
		break;
	default:
		warning("invalid type %d in mcheck()", m->type);
		return 0;
	}

	switch (m->reln) {
	case '=':
		return v == l;
	case '>':
		return v > l;
	case '<':
		return v < l;
	case '&':
		return v & l;
	default:
		warning("mcheck: can't happen: invalid relation %d", m->reln);
		return 0;
	}
}
