/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego and Lance
 * Visser of Convex Computer Corporation.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)args.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dd.h"
#include "extern.h"

static u_long get_bsz __P((char *));

static void f_bs __P((char *));
static void f_cbs __P((char *));
static void f_conv __P((char *));
static void f_count __P((char *));
static void f_files __P((char *));
static void f_ibs __P((char *));
static void f_if __P((char *));
static void f_obs __P((char *));
static void f_of __P((char *));
static void f_seek __P((char *));
static void f_skip __P((char *));

static struct arg {
	char *name;
	void (*f) __P((char *));
	u_int set, noset;
} args[] = {
	"bs",		f_bs,		C_BS,		C_BS|C_IBS|C_OBS,
	"cbs",		f_cbs,		C_CBS,		0,
	"conv",		f_conv,		0,		0,
	"count",	f_count,	C_COUNT,	C_COUNT,
	"files",	f_files,	C_FILES,	C_FILES,
	"ibs",		f_ibs,		C_IBS,		C_BS|C_IBS,
	"if",		f_if,		C_IF,		C_IF,
	"obs",		f_obs,		C_OBS,		C_BS|C_OBS,
	"of",		f_of,		C_OF,		C_OF,
	"seek",		f_seek,		C_SEEK,		C_SEEK,
	"skip",		f_skip,		C_SKIP,		C_SKIP,
};

static char *oper;

/*
 * args -- parse old JCL syntax of dd.
 */
void
jcl(argv)
	register char **argv;
{
	register struct arg *ap;
	struct arg tmp;
	char *arg;
	static int c_arg __P((const void *, const void *));

	in.dbsz = out.dbsz = 512;
	cfunc = def;

	while (oper = *++argv) {
		if ((arg = index(oper, '=')) == NULL)
			err("unknown operand %s", oper);
		*arg++ = '\0';
		if (!*arg)
			err("no value specified for %s", oper);
		tmp.name = oper;
		if (!(ap = (struct arg *)bsearch(&tmp, args,
		    sizeof(args)/sizeof(struct arg), sizeof(struct arg),
		    c_arg)))
			err("unknown operand %s", tmp.name);
		if (ddflags & ap->noset)
			err("%s: illegal argument combination or already set",
			    tmp.name);
		ddflags |= ap->set;
		ap->f(arg);
	}

	/* Final sanity checks. */

	if (ddflags & C_BS) {
		/*
		 * Bs is turned off by any conversion -- we assume the user
		 * just wanted to set both the input and output block sizes
		 * and didn't want the bs semantics, so we don't warn.
		 */
		if (ddflags & (C_BLOCK|C_LCASE|C_SWAB|C_UCASE|C_UNBLOCK))
			ddflags &= ~C_BS;

		/* Bs supersedes ibs and obs. */
		if (ddflags & C_BS && ddflags & (C_IBS|C_OBS))
			warn("bs supersedes ibs and obs");
	}

	/* Cbs has to be set for block/unblock to be specified. */
	if (ddflags & (C_BLOCK|C_UNBLOCK)) {
		if (cbsz == 0)
			err("block operations require cbs");
	} else 
		if (cbsz != 0)
			warn("cbs ignored if not doing block operation");

	if (in.dbsz == 0 || out.dbsz == 0)
		err("buffer sizes cannot be zero");


}

static int
c_arg(a, b)
	const void *a, *b;
{
	return (strcmp(((struct arg *)a)->name, ((struct arg *)b)->name));
}

static void
f_bs(arg)
	char *arg;
{
	in.dbsz = out.dbsz = (int)get_bsz(arg);
}

static void
f_cbs(arg)
	char *arg;
{
	cbsz = (int)get_bsz(arg);
}

static void
f_count(arg)
	char *arg;
{
	cpy_cnt = (u_int)get_bsz(arg);
	if (!cpy_cnt)				/* Well, that was quick. */
		terminate(0);
}

static void
f_files(arg)					/* POSIX extension */
	char *arg;
{
	files_cnt = (int)get_bsz(arg);
}

static void
f_ibs(arg)
	char *arg;
{
	if (!(ddflags & C_BS))
		in.dbsz = (int)get_bsz(arg);
}

static void
f_if(arg)
	char *arg;
{
	in.name = arg;
}

static void
f_obs(arg)
	char *arg;
{
	if (!(ddflags & C_BS))
		out.dbsz = (int)get_bsz(arg);
}

static void
f_of(arg)
	char *arg;
{
	out.name = arg;
}

static void
f_seek(arg)
	char *arg;
{
	out.offset = (u_int)get_bsz(arg);
}

static void
f_skip(arg)
	char *arg;
{
	in.offset = (u_int)get_bsz(arg);
}

static void f_ascii __P((void));
static void f_block __P((void));
static void f_ebcdic __P((void));
static void f_ibm __P((void));
static void f_oldascii __P((void));
static void f_oldebcdic __P((void));
static void f_oldibm __P((void));
static void f_unblock __P((void));

static struct conv {
	char *name;
	void (*f) __P((void));
	u_int set, noset;
} clist[] = {
	"ascii",	f_ascii,	C_UNBLOCK,	C_BLOCK,
	"block",	f_block,	C_BLOCK,	C_UNBLOCK,
	"ebcdic",	f_ebcdic,	C_BLOCK,	C_UNBLOCK,
	"ibm",		f_ibm,		C_BLOCK,	C_UNBLOCK,
	"lcase",	NULL,		C_LCASE,	C_UCASE,
	"noerror",	NULL,		C_NOERROR,	0,
	"notrunc",	NULL,		C_NOTRUNC,	0,
	"oldascii",	f_oldascii,	C_UNBLOCK,	C_BLOCK,
	"oldebcdic",	f_oldebcdic,	C_BLOCK,	C_UNBLOCK,
	"oldibm",	f_oldibm,	C_BLOCK,	C_UNBLOCK,
	"swab",		NULL,		C_SWAB,		0,
	"sync",		NULL,		C_SYNC,		0,
	"ucase",	NULL,		C_UCASE,	C_LCASE,
	"unblock",	f_unblock,	C_UNBLOCK,	C_BLOCK,
};

static void
f_conv(arg)
	char *arg;
{
	register struct conv *cp;
	struct conv tmp;
	static int c_conv __P((const void *, const void *));

	while (arg != NULL) {
		tmp.name = strsep(&arg, ",");
		if (!(cp = (struct conv *)bsearch(&tmp, clist,
		    sizeof(clist)/sizeof(struct conv), sizeof(struct conv),
		    c_conv)))
			err("unknown conversion %s", tmp.name);
		if (ddflags & cp->noset)
			err("%s: illegal conversion combination", tmp.name);
		ddflags |= cp->set;
		if (cp->f)
			(*cp->f)();
	}
}

static int
c_conv(a, b)
	const void *a, *b;
{
	return (strcmp(((struct conv *)a)->name, ((struct conv *)b)->name));
}

static void
f_ascii()					/* POSIX extension */
{
	extern u_char e2a_POSIX[];

	ctab = e2a_POSIX;
	cfunc = unblock;
}

static void
f_block()
{
	cfunc = block;
}

static void
f_ebcdic()					/* POSIX extension */
{
	extern u_char a2e_POSIX[];

	ctab = a2e_POSIX;
	cfunc = block;
}

static void
f_ibm()						/* POSIX extension */
{
	extern u_char a2ibm_POSIX[];

	ctab = a2ibm_POSIX;
	cfunc = block;
}

static void
f_oldascii()
{
	extern u_char e2a_32V[];

	ctab = e2a_32V;
	cfunc = unblock;
}

static void
f_oldebcdic()
{
	extern u_char a2e_32V[];
	ctab = a2e_32V;
	cfunc = block;
}

static void
f_oldibm()
{
	extern u_char a2ibm_32V[];

	ctab = a2ibm_32V;
	cfunc = block;
}

static void
f_unblock()
{
	cfunc = unblock;
}

/*
 * Convert an expression of the following forms to an unsigned long.
 * 	1) A positive decimal number.
 *	2) A positive decimal number followed by a k (mult by 1024).
 *	3) A positive decimal number followed by a b (mult by 512).
 *	4) A positive decimal number followed by a w (mult by sizeof int)
 *	   (POSIX extension for backwards compatibility).
 *	5) Two or more positive decimal numbers (with/without k,b or w).
 *	   seperated by x (also * for backwards compatibility), specifying
 *	   the product of the indicated values.
 */
static u_long
get_bsz(val)
	char *val;
{
	char *expr;
	u_long num, t;

	num = strtoul(val, &expr, 0);
	if (num == ULONG_MAX)			/* Overflow. */
		err("%s: %s", oper, strerror(errno));
	if (expr == val)			/* No digits. */
		err("%s: illegal numeric value", oper);

	switch(*expr) {
	case 'b':
		t = num;
		num *= 512;
		if (t > num)
			goto erange;
		++expr;
		break;
	case 'k':
		t = num;
		num *= 1024;
		if (t > num)
			goto erange;
		++expr;
		break;
	case 'w':				/* POSIX extension. */
		t = num;
		num *= sizeof(int);
		if (t > num)
			goto erange;
		++expr;
		break;
	}

	switch(*expr) {
		case '\0':
			break;
		case 'x':
		case '*':
			t = num;
			num *= get_bsz(expr + 1);
			if (t > num)
erange:				err("%s: %s", oper, strerror(ERANGE));
			break;
		default:
			err("%s: illegal numeric value", oper);
	}
	return(num);
}
