/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Cimarron D. Taylor of the University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)option.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>
#include <stdio.h>
#include "find.h"

typedef struct _option {
	char *name;		/* option name */
	int token;		/* token value */
	PLAN *(*create)();	/* create function */
#define	O_NONE		0x01	/* no call required */
#define	O_ZERO		0x02	/* pass: nothing */
#define	O_ARGV		0x04	/* pass: argv, increment argv */
#define	O_ARGVP		0x08	/* pass: *argv, T_OK || T_EXEC */
#define	O_MASK		0x0f	/* mask of op bits */
#define	O_OLD		0x10	/* deprecated syntax */
#define	O_NEW		0x20	/* new syntax */
	int flags;
} OPTION;

PLAN	*c_atime(), *c_ctime(), *c_depth(), *c_exec(), *c_follow(),
	*c_fstype(), *c_group(), *c_inum(), *c_links(), *c_ls(),
	*c_mtime(), *c_name(), *c_newer(), *c_nogroup(), *c_nouser(),
	*c_perm(), *c_print(), *c_prune(), *c_size(), *c_type(),
	*c_user(), *c_xdev(), *c_openparen(), *c_closeparen(), *c_not(),
	*c_or();

OPTION options[] = {
	"!",		T_NOT,		c_not,		O_ZERO,
	"(",		T_OPENPAREN,	c_openparen,	O_ZERO,
	")",		T_CLOSEPAREN,	c_closeparen,	O_ZERO,
	"a",		T_AND,		NULL,		O_NONE|O_OLD,
	"and",		T_AND,		NULL,		O_NONE|O_NEW,
	"atime",	T_ATIME,	c_atime,	O_ARGV,
	"ctime",	T_CTIME,	c_ctime,	O_ARGV,
	"depth",	T_DEPTH,	c_depth,	O_ZERO|O_OLD,
	"exec",		T_EXEC,		c_exec,		O_ARGVP,
	"follow",	T_FOLLOW,	c_follow,	O_ZERO|O_OLD,
	"fstype",	T_FSTYPE,	c_fstype,	O_ARGV,
	"group",	T_GROUP,	c_group,	O_ARGV,
	"inum",		T_INUM,		c_inum,		O_ARGV,
	"links",	T_LINKS,	c_links,	O_ARGV,
	"ls",		T_LS,		c_ls,		O_ZERO,
	"mtime",	T_MTIME,	c_mtime,	O_ARGV,
	"name",		T_NAME,		c_name,		O_ARGV,
	"newer",	T_NEWER,	c_newer,	O_ARGV,
	"nogroup",	T_NOGROUP,	c_nogroup,	O_ZERO,
	"nouser",	T_NOUSER,	c_nouser,	O_ZERO,
	"o",		T_OR,		c_or,		O_ZERO|O_OLD,
	"ok",		T_OK,		c_exec,		O_ARGVP,
	"or",		T_OR,		c_or,		O_ZERO|O_NEW,
	"perm",		T_PERM,		c_perm,		O_ARGV,
	"print",	T_PRINT,	c_print,	O_ZERO,
	"prune",	T_PRUNE,	c_prune,	O_ZERO,
	"size",		T_SIZE,		c_size,		O_ARGV,
	"type",		T_TYPE,		c_type,		O_ARGV,
	"user",		T_USER,		c_user,		O_ARGV,
	"xdev",		T_XDEV,		c_xdev,		O_ZERO|O_OLD,
	{ NULL },
};

/*
 * find_create --
 *	create a node corresponding to a command line argument.
 *
 * TODO:
 *	add create/process function pointers to node, so we can skip
 *	this switch stuff.
 */
PLAN *
find_create(argvp)
	char ***argvp;
{
	register OPTION *p;
	OPTION tmp;
	PLAN *new;
	char **argv;
	int typecompare();

	argv = *argvp;
	tmp.name = *argv++;

	/* strip off any leading dash */
	if (*tmp.name == '-')
		++tmp.name;

	p = (OPTION *)bsearch(&tmp, options, sizeof(options)/sizeof(OPTION),
	    sizeof(OPTION), typecompare);
	if (!p || isdeprecated && p->flags&O_NEW ||
	    !isdeprecated && p->flags&O_OLD) {
		(void)fprintf(stderr, "find: unknown option %s.\n", *--argv);
		exit(1);
	}
	if (p->flags & (O_ARGV|O_ARGVP) && !*argv) {
		(void)fprintf(stderr,
		    "find: %s requires additional arguments.\n", *--argv);
		exit(1);
	}

	switch(p->flags&O_MASK) {
	case O_NONE:
		new = NULL;
		break;
	case O_ZERO:
		new = (p->create)();
		break;
	case O_ARGV:
		new = (p->create)(*argv++);
		break;
	case O_ARGVP:
		new = (p->create)(&argv, p->token == T_OK);
		break;
	}
	*argvp = argv;
	return(new);
}

typecompare(a, b)
	OPTION *a, *b;
{
	return(strcmp(a->name, b->name));
}
