/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)misc.c	5.1 (Berkeley) %G%
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
} KEY;

/* NB: the following table must be sorted lexically. */
static KEY keylist[] = {
	"cksum",	F_CKSUM,
	"gid",		F_GID,
	"gname",	F_GNAME,
	"ignore",	F_IGN,
	"link",		F_SLINK,
	"mode",		F_MODE,
	"nlink",	F_NLINK,
	"size",		F_SIZE,
	"time",		F_TIME,
	"type",		F_TYPE,
	"uid",		F_UID,
	"uname",	F_UNAME,
};

u_int
parsekey(name)
	char *name;
{
	KEY *k, tmp;
	int keycompare __P((const void *, const void *));

	tmp.name = name;
	k = (KEY *)bsearch(&tmp, keylist, sizeof(keylist) / sizeof(KEY),
	    sizeof(KEY), keycompare);
	if (k == NULL)
		err("unknown keyword %s", name);
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
