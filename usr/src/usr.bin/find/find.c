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
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)find.c	4.26 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>
#include <stdio.h>
#include <strings.h>
#include <errno.h>
#include "find.h"

FTS *tree;			/* pointer to top of FTS hierarchy */
time_t now;			/* time find was run */
dev_t curdev = (dev_t)-1;	/* device number of current tree */
int ftsoptions;			/* options passed to ftsopen() */
int deprecated;			/* old or new syntax */
int depth;			/* set by -depth option */
int output_specified;		/* one of -print, -ok or -exec was specified */
int xdev;			/* set by -xdev option */

main(argc, argv)
	int argc;
	char **argv;
{
	PLAN *plan;
	char **p, **paths;
	PLAN *find_formplan();
	time_t time();
    
	(void)time(&now);			/* initialize the time-of-day */

	if (argc < 2)
		usage();

	paths = argv;
	ftsoptions = FTS_MULTIPLE|FTS_NOSTAT|FTS_PHYSICAL;

	/*
	 * if arguments start with an option, it's new syntax; otherwise,
	 * if has a "-option" anywhere it must be old syntax.
	 */
	if (argv[1][0] != '-')
		for (p = argv + 1; *p; ++p)
			if (**p == '-') {
				deprecated = 1;
				oldsyntax(&argv);
				break;
			}
	if (!deprecated)
		newsyntax(argc, &argv);
    
	plan = find_formplan(argv);		/* execution plan */
	find_execute(plan, paths);
}

/*
 * find_formplan --
 *	process the command line and create a "plan" corresponding to the
 *	command arguments.
 */
PLAN *
find_formplan(argv)
	char **argv;
{
	PLAN *plan, *tail, *new;
	PLAN *c_print(), *find_create(), *find_squish_not(), *find_squish_or();
	PLAN *find_squish_paren();

	/*
	 * for each argument in the command line, determine what kind of node
	 * it is, create the appropriate node type and add the new plan node
	 * to the end of the existing plan.  The resulting plan is a linked
	 * list of plan nodes.  For example, the string:
	 *
	 *	% find . -name foo -newer bar -print
	 *
	 * results in the plan:
	 *
	 *	[-name foo]--> [-newer bar]--> [-print]
	 *
	 * in this diagram, `[-name foo]' represents the plan node generated
	 * by c_name() with an argument of foo and `-->' represents the
	 * plan->next pointer.
	 */
	for (plan = NULL; *argv;) {
		new = find_create(&argv);
		if (plan == NULL)
			tail = plan = new;
		else {
			tail->next = new;
			tail = new;
		}
	}
    
	/*
	 * if the user didn't specify one of -print, -ok or -exec, then -print
	 * is assumed so we add a -print node on the end.  It is possible that
	 * the user might want the -print someplace else on the command line,
	 * but there's no way to know that.
	 */
	if (!output_specified) {
		new = c_print();
		if (plan == NULL)
			tail = plan = new;
		else {
			tail->next = new;
			tail = new;
		}
	}
    
	/*
	 * the command line has been completely processed into a search plan
	 * except for the (, ), !, and -o operators.  Rearrange the plan so
	 * that the portions of the plan which are affected by the operators
	 * are moved into operator nodes themselves.  For example:
	 *
	 *	[!]--> [-name foo]--> [-print]
	 *
	 * becomes
	 *
	 *	[! [-name foo] ]--> [-print]
	 *
	 * and
	 *
	 *	[(]--> [-depth]--> [-name foo]--> [)]--> [-print]
	 *
	 * becomes
	 *
	 *	[expr [-depth]-->[-name foo] ]--> [-print]
	 *
	 * operators are handled in order of precedence.
	 */

	plan = find_squish_paren(plan);		/* ()'s */
	plan = find_squish_not(plan);		/* !'s */
	plan = find_squish_or(plan);		/* -o's */
	return(plan);
}
 
/*
 * find_execute --
 *	take a search plan and an array of search paths and executes the plan
 *	over all FTSENT's returned for the given search paths.
 */
find_execute(plan, paths)
	PLAN *plan;		/* search plan */
	char **paths;		/* array of pathnames to traverse */
{
	FTSENT *entry;		/* current fts entry */
	PLAN *p;
    
	if (!(tree = ftsopen(paths, ftsoptions, NULL))) {
		(void)fprintf(stderr, "find: ftsopen: %s.\n", strerror(errno));
		exit(1);
	}
	while (entry = ftsread(tree)) {
		switch(entry->info) {
		case FTS_DNR:
			(void)fprintf(stderr,
			    "find: %s: unable to read.\n", entry->path);
			continue;
		case FTS_DNX:
			(void)fprintf(stderr,
			    "find: %s: unable to search.\n", entry->path);
			continue;
		case FTS_ERR:
			(void)fprintf(stderr,
			    "find: %s: %s.\n", entry->path, strerror(errno));
			continue;
		case FTS_D:
			if (depth)
				continue;
			break;
		case FTS_DC:
			(void)fprintf(stderr,
			    "find: directory cycle: %s.\n", entry->path);
			continue;
		case FTS_DP:
			if (!depth)
				continue;
		case FTS_NS:
			if (!(ftsoptions & FTS_NOSTAT)) {
				(void)fprintf(stderr,
				    "find: can't stat: %s.\n", entry->path);
				continue;
			}
			break;
		}

		/* always keep curdev up to date, -fstype uses it. */
		if (xdev && curdev != entry->statb.st_dev &&
		    curdev != -1 && ftsset(tree, entry, FTS_SKIP)) {
			(void)fprintf(stderr, "find: %s: %s.\n",
			    entry->path, strerror(errno));
			exit(1);
		}

		/*
		 * call all the functions in the execution plan until one is
		 * false or all have been executed.  This is where we do all
		 * the work specified by the user on the command line.
		 */
		for (p = plan; p && (p->eval)(p, entry); p = p->next);

		curdev = entry->statb.st_dev;
	}
	(void)ftsclose(tree);
}
