/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)misc.c	5.2 (Berkeley) %G%
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>
#include <stdio.h>
#include "mtree.h"
#include "extern.h"

extern int lineno;

typedef struct _key {
	char *name;			/* key name */
	u_int val;			/* value */

#define	NEEDVALUE	0x01
	u_int flags;
} KEY;

/* NB: the following table must be sorted lexically. */
static KEY keylist[] = {
	"cksum",	F_CKSUM,	NEEDVALUE,
	"gid",		F_GID,		NEEDVALUE,
	"gname",	F_GNAME,	NEEDVALUE,
	"ignore",	F_IGN,		0,
	"link",		F_SLINK,	NEEDVALUE,
	"mode",		F_MODE,		NEEDVALUE,
	"nlink",	F_NLINK,	NEEDVALUE,
	"size",		F_SIZE,		NEEDVALUE,
	"time",		F_TIME,		NEEDVALUE,
	"type",		F_TYPE,		NEEDVALUE,
	"uid",		F_UID,		NEEDVALUE,
	"uname",	F_UNAME,	NEEDVALUE,
};

u_int
parsekey(name, needvaluep)
	char *name;
	int *needvaluep;
{
	KEY *k, tmp;
	int keycompare __P((const void *, const void *));

	tmp.name = name;
	k = (KEY *)bsearch(&tmp, keylist, sizeof(keylist) / sizeof(KEY),
	    sizeof(KEY), keycompare);
	if (k == NULL)
		err("unknown keyword %s", name);

	if (needvaluep)
		*needvaluep = k->flags & NEEDVALUE ? 1 : 0;
	return (k->val);
}

int
keycompare(a, b)
	const void *a, *b;
{
	return (strcmp(((KEY *)a)->name, ((KEY *)b)->name));
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
        va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "mtree: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	if (lineno)
		(void)fprintf(stderr,
		    "mtree: failed at line %d of the specification\n", lineno);
	exit(1);
	/* NOTREACHED */
}
