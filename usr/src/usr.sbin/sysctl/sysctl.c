/*
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)sysctl.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/sysctl.h>
#include <sys/socket.h>
#include <vm/vm_param.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

char *topname[] = CTL_NAMES;
char *kernname[] = CTL_KERN_NAMES;
char *vmname[] = CTL_VM_NAMES;
char *netname[] = CTL_NET_NAMES;
char *hwname[] = CTL_HW_NAMES;

struct list {
	char	**list;
	int	size;
} secondlevel[] = {
	{ 0, 0 },			/* CTL_UNSPEC */
	{ kernname, KERN_MAXID },	/* CTL_KERN */
	{ vmname, VM_MAXID },		/* CTL_VM */
	{ 0, 0 },			/* CTL_FS */
	{ netname, NET_MAXID },		/* CTL_NET */
	{ 0, 0 },			/* CTL_DEBUG */
	{ hwname, HW_MAXID },		/* CTL_HW */
	{ 0, 0 },			/* CTL_MACHDEP */
};

int	Aflag, aflag, wflag;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	int ch;

	while ((ch = getopt(argc, argv, "Aaw")) != EOF) {
		switch (ch) {

		case 'A':
			Aflag = 1;
			break;

		case 'a':
			aflag = 1;
			break;

		case 'w':
			wflag = 1;
			break;

		default:
			usage();
		}
	}
	argc -= optind;
	argv += optind;

	if (Aflag || aflag) {
		listall();
		exit(0);
	}
	if (argc == 0)
		usage();
	while (argc-- > 0)
		parse(*argv, 1);
	exit(0);
}

/*
 * List all variables known to the system.
 */
listall()
{
	struct list *lp;
	int lvl1, lvl2;
	char *cp, name[BUFSIZ];

	for (lvl1 = 1; lvl1 < CTL_MAXID; lvl1++) {
		lp = &secondlevel[lvl1];
		if (lp->list == 0)
			continue;
		strcpy(name, topname[lvl1]);
		cp = &name[strlen(name)];
		*cp++ = '.';
		for (lvl2 = 1; lvl2 < lp->size; lvl2++) {
			strcpy(cp, lp->list[lvl2]);
			parse(name, Aflag);
		}
	}
}

/*
 * Parse a name into a MIB entry.
 * Lookup and print out the MIB entry if it exists.
 * Set a new value if requested.
 */
parse(string, flags)
	char *string;
	int flags;
{
	int indx, size;
	int isclockrate = 0;
	void *newval = 0;
	int intval, newsize = 0;
	struct list top, *lp;
	int mib[CTL_MAXNAME];
	char *cp, *bufp, buf[BUFSIZ], strval[BUFSIZ];

	bufp = buf;
	snprintf(buf, BUFSIZ, "%s", string);
	if ((cp = strchr(string, '=')) != NULL) {
		if (!wflag) {
			fprintf(stderr, "Must specify -w to set variables\n");
			exit(2);
		}
		*strchr(buf, '=') = '\0';
		*cp++ = '\0';
		while (isspace(*cp))
			cp++;
		if (isdigit(*cp)) {
			intval = atoi(cp);
			newval = &intval;
			newsize = sizeof intval;
		} else {
			newval = cp;
			newsize = strlen(cp);
		}
	}
	top.list = topname;
	top.size = CTL_MAXID;
	if ((indx = findname(string, "top", &bufp, &top)) == -1)
		return;
	mib[0] = indx;
	lp = &secondlevel[indx];
	if (lp->list == 0) {
		fprintf(stderr, "%s: class is not implemented\n",
		    topname[indx]);
		return;
	}
	if ((indx = findname(string, "second", &bufp, lp)) == -1)
		return;
	mib[1] = indx;
	if (bufp) {
		fprintf(stderr, "name %s in %s is unknown\n", *bufp, string);
		return;
	}
	switch (mib[0]) {

	case CTL_KERN:
		switch (mib[1]) {
		case KERN_VNODE:
		case KERN_FILE:
			if (flags == 0)
				return;
			fprintf(stderr,
			    "Use pstat to view %s information\n", string);
			return;
		case KERN_PROC:
			if (flags == 0)
				return;
			fprintf(stderr,
			    "Use ps to view %s information\n", string);
			return;
		case KERN_CLOCKRATE:
			isclockrate = 1;
			break;
		}
		break;

	case CTL_HW:
		break;

	case CTL_VM:
		if (mib[1] == VM_LOADAVG) {
			double loads[3];

			getloadavg(loads, 3);
			fprintf(stdout, "%s: %.2f %.2f %.2f\n", string,
			    loads[0], loads[1], loads[2]);
			return;
		}
		if (flags == 0)
			return;
		fprintf(stderr,
		    "Use vmstat or systat to view %s information\n", string);
		return;

	case CTL_NET:
		if (flags == 0)
			return;
		fprintf(stderr, "Use netstat to view %s information\n", string);
		return;

	case CTL_FS:
	case CTL_DEBUG:
	case CTL_MACHDEP:
		break;

	default:
		fprintf(stderr, "Illegal top level value: %d\n", mib[0]);
		return;
	
	}

	size = BUFSIZ;
	if (sysctl(mib, 2, buf, &size, newsize ? newval : 0, newsize) == -1) {
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
	if (isclockrate) {
		struct clockinfo *clkp = (struct clockinfo *)buf;

		fprintf(stdout,
		    "%s: hz = %d, tick = %d, profhz = %d, stathz = %d\n",
		    string, clkp->hz, clkp->tick, clkp->profhz, clkp->stathz);
		return;
	}
	if (size == sizeof(int) && !isprint(buf[0]))
		if (newsize == 0)
			fprintf(stdout, "%s = %d\n", string, *(int *)buf);
		else
			fprintf(stdout, "%s: %d -> %d\n", string, *(int *)buf,
			    *(int *)newval);
	else
		if (newsize == 0)
			fprintf(stdout, "%s = %s\n", string, buf);
		else
			fprintf(stdout, "%s: %s -> %s\n", string, buf, newval);
	return;
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
		if (!strcmp(name, namelist->list[i]))
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
	    "sysctl [-w] variable ...", "sysctl -a", "sysctl -A");
	exit(1);
}
