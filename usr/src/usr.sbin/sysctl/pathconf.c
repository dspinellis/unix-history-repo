/*
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pathconf.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/unistd.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PC_NAMES { \
	{ 0, 0 }, \
	{ "link_max", CTLTYPE_INT }, \
	{ "max_canon", CTLTYPE_INT }, \
	{ "max_input", CTLTYPE_INT }, \
	{ "name_max", CTLTYPE_INT }, \
	{ "path_max", CTLTYPE_INT }, \
	{ "pipe_buf", CTLTYPE_INT }, \
	{ "chown_restricted", CTLTYPE_INT }, \
	{ "no_trunc", CTLTYPE_INT }, \
	{ "vdisable", CTLTYPE_INT }, \
}
#define PC_MAXID 10

struct ctlname pcnames[] = PC_NAMES;
char names[BUFSIZ];

struct list {
	struct	ctlname *list;
	int	size;
};
struct list pclist = { pcnames, PC_MAXID };

int	Aflag, aflag, nflag, wflag, stdinflag;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	char *path;
	int ch;

	while ((ch = getopt(argc, argv, "Aan")) != EOF) {
		switch (ch) {

		case 'A':
			Aflag = 1;
			break;

		case 'a':
			aflag = 1;
			break;

		case 'n':
			nflag = 1;
			break;

		default:
			usage();
		}
	}
	argc -= optind;
	argv += optind;

	if (argc == 0)
		usage();
	path = *argv++;
	if (strcmp(path, "-") == 0)
		stdinflag = 1;
	argc--;
	if (Aflag || aflag) {
		listall(path, &pclist);
		exit(0);
	}
	if (argc == 0)
		usage();
	while (argc-- > 0)
		parse(path, *argv, 1);
	exit(0);
}

/*
 * List all variables known to the system.
 */
listall(path, lp)
	char *path;
	struct list *lp;
{
	int lvl2;

	if (lp->list == 0)
		return;
	for (lvl2 = 0; lvl2 < lp->size; lvl2++) {
		if (lp->list[lvl2].ctl_name == 0)
			continue;
		parse(path, lp->list[lvl2].ctl_name, Aflag);
	}
}

/*
 * Parse a name into an index.
 * Lookup and print out the attribute if it exists.
 */
parse(pathname, string, flags)
	char *pathname;
	char *string;
	int flags;
{
	int indx, value;
	char *bufp, buf[BUFSIZ];

	bufp = buf;
	snprintf(buf, BUFSIZ, "%s", string);
	if ((indx = findname(string, "top", &bufp, &pclist)) == -1)
		return;
	if (bufp) {
		fprintf(stderr, "name %s in %s is unknown\n", *bufp, string);
		return;
	}
	if (stdinflag)
		value = fpathconf(0, indx);
	else
		value = pathconf(pathname, indx);
	if (value == -1) {
		if (flags == 0)
			return;
		switch (errno) {
		case EOPNOTSUPP:
			fprintf(stderr, "%s: value is not available\n", string);
			return;
		case ENOTDIR:
			fprintf(stderr, "%s: specification is incomplete\n",
			    string);
			return;
		case ENOMEM:
			fprintf(stderr, "%s: type is unknown to this program\n",
			    string);
			return;
		default:
			perror(string);
			return;
		}
	}
	if (!nflag)
		fprintf(stdout, "%s = ", string);
	fprintf(stdout, "%d\n", value);
}

/*
 * Scan a list of names searching for a particular name.
 */
findname(string, level, bufp, namelist)
	char *string;
	char *level;
	char **bufp;
	struct list *namelist;
{
	char *name;
	int i;

	if (namelist->list == 0 || (name = strsep(bufp, ".")) == NULL) {
		fprintf(stderr, "%s: incomplete specification\n", string);
		return (-1);
	}
	for (i = 0; i < namelist->size; i++)
		if (namelist->list[i].ctl_name != NULL &&
		    strcmp(name, namelist->list[i].ctl_name) == 0)
			break;
	if (i == namelist->size) {
		fprintf(stderr, "%s level name %s in %s is invalid\n",
		    level, name, string);
		return (-1);
	}
	return (i);
}

usage()
{

	(void)fprintf(stderr, "usage:\t%s\n\t%s\n\t%s\n",
	    "pathname [-n] variable ...",
	    "pathname [-n] -a", "pathname [-n] -A");
	exit(1);
}
