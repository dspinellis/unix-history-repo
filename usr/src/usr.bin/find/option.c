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
static char sccsid[] = "@(#)option.c	5.10 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "find.h"

typedef struct _option {
	char *name;		/* option name */
	enum ntype token;	/* token type */
	PLAN *(*create)();	/* create function */
#define	O_NONE		0x01	/* no call required */
#define	O_ZERO		0x02	/* pass: nothing */
#define	O_ARGV		0x04	/* pass: argv, increment argv */
#define	O_ARGVP		0x08	/* pass: *argv, N_OK || N_EXEC */
	int flags;
} OPTION;

/* NB: the following table must be sorted in machine (strcmp) order */
OPTION options[] = {
	"!",		N_NOT,		c_not,		O_ZERO,
	"(",		N_OPENPAREN,	c_openparen,	O_ZERO,
	")",		N_CLOSEPAREN,	c_closeparen,	O_ZERO,
	"-a",		N_AND,		NULL,		O_NONE,
	"-and",		N_AND,		NULL,		O_NONE,
	"-atime",	N_ATIME,	c_atime,	O_ARGV,
	"-ctime",	N_CTIME,	c_ctime,	O_ARGV,
	"-depth",	N_DEPTH,	c_depth,	O_ZERO,
	"-exec",	N_EXEC,		c_exec,		O_ARGVP,
	"-follow",	N_FOLLOW,	c_follow,	O_ZERO,
	"-fstype",	N_FSTYPE,	c_fstype,	O_ARGV,
	"-group",	N_GROUP,	c_group,	O_ARGV,
	"-inum",	N_INUM,		c_inum,		O_ARGV,
	"-links",	N_LINKS,	c_links,	O_ARGV,
	"-ls",		N_LS,		c_ls,		O_ZERO,
	"-mtime",	N_MTIME,	c_mtime,	O_ARGV,
	"-name",	N_NAME,		c_name,		O_ARGV,
	"-newer",	N_NEWER,	c_newer,	O_ARGV,
	"-nogroup",	N_NOGROUP,	c_nogroup,	O_ZERO,
	"-nouser",	N_NOUSER,	c_nouser,	O_ZERO,
	"-o",		N_OR,		c_or,		O_ZERO,
	"-ok",		N_OK,		c_exec,		O_ARGVP,
	"-or",		N_OR,		c_or,		O_ZERO,
	"-path", 	N_PATH,		c_path,		O_ARGV,
	"-perm",	N_PERM,		c_perm,		O_ARGV,
	"-print",	N_PRINT,	c_print,	O_ZERO,
	"-prune",	N_PRUNE,	c_prune,	O_ZERO,
	"-size",	N_SIZE,		c_size,		O_ARGV,
	"-type",	N_TYPE,		c_type,		O_ARGV,
	"-user",	N_USER,		c_user,		O_ARGV,
	"-xdev",	N_XDEV,		c_xdev,		O_ZERO,
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
	PLAN *new;
	char **argv;
	OPTION *option();

	argv = *argvp;

	if ((p = option(*argv)) == NULL) {
		(void)fprintf(stderr, "find: unknown option %s.\n", *argv);
		exit(1);
	}
	++argv;
	if (p->flags & (O_ARGV|O_ARGVP) && !*argv) {
		(void)fprintf(stderr,
		    "find: %s requires additional arguments.\n", *--argv);
		exit(1);
	}

	switch(p->flags) {
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
		new = (p->create)(&argv, p->token == N_OK);
		break;
	}
	*argvp = argv;
	return(new);
}

OPTION *
option(name)
	char *name;
{
	OPTION tmp;
	int typecompare __P((const void *, const void *));

	tmp.name = name;
	return((OPTION *)bsearch(&tmp, options,
	    sizeof(options)/sizeof(OPTION), sizeof(OPTION), typecompare));
}
	
typecompare(a, b)
	const void *a, *b;
{
	return(strcmp(((OPTION *)a)->name, ((OPTION *)b)->name));
}
