/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Cimarron D. Taylor of the University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)find.c	4.32 (Berkeley) 7/1/90";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "find.h"

FTS *tree;			/* pointer to top of FTS hierarchy */
time_t now;			/* time find was run */
int ftsoptions;			/* options passed to ftsopen() */
int deprecated;			/* old or new syntax */
int depth;			/* set by -depth option */
int output_specified;		/* one of -print, -ok or -exec was specified */

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
	ftsoptions = FTS_NOSTAT|FTS_PHYSICAL;

	/*
	 * if arguments start with an option, treat it like new syntax;
	 * otherwise, if has a "-option" anywhere (which isn't an argument
	 * to another command) treat it as old syntax.
	 */
	if (argv[1][0] != '-')
		for (p = argv + 1; *p; ++p) {
			if (!strcmp(*p, "exec") || !strcmp(*p, "ok")) {
				while (p[1] && strcmp(*++p, ";"));
				continue;
			}
			if (**p == '-') {
				deprecated = 1;
				oldsyntax(&argv);
				break;
			}
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
		if (!(new = find_create(&argv)))
			continue;
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
		switch(entry->fts_info) {
		case FTS_DNR:
			(void)fprintf(stderr,
			    "find: %s: unable to read.\n", entry->fts_path);
			continue;
		case FTS_DNX:
			(void)fprintf(stderr,
			    "find: %s: unable to search.\n", entry->fts_path);
			continue;
		case FTS_ERR:
			(void)fprintf(stderr,
			    "find: %s: %s.\n", entry->fts_path,
			    strerror(errno));
			continue;
		case FTS_D:
			if (depth)
				continue;
			break;
		case FTS_DC:
			(void)fprintf(stderr,
			    "find: directory cycle: %s.\n", entry->fts_path);
			continue;
		case FTS_DP:
			if (!depth)
				continue;
		case FTS_NS:
			if (!(ftsoptions & FTS_NOSTAT)) {
				(void)fprintf(stderr,
				    "find: can't stat: %s.\n", entry->fts_path);
				continue;
			}
			break;
		}

		/*
		 * call all the functions in the execution plan until one is
		 * false or all have been executed.  This is where we do all
		 * the work specified by the user on the command line.
		 */
		for (p = plan; p && (p->eval)(p, entry); p = p->next);
	}
	(void)ftsclose(tree);
}
