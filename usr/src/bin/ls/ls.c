/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ls.c	5.53 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>	
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <fts.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include "ls.h"
#include "extern.h"

static void	display __P((int, FTSENT *, FTSENT *));
static int	mastercmp __P((const FTSENT **, const FTSENT **));
static void	traverse __P((int, char **, int));

static void (*printfcn) __P((FTSENT *, int, u_long, int));
static int (*sortfcn) __P((const FTSENT *, const FTSENT *));

int termwidth = 80;		/* default terminal width */

/* flags */
int f_accesstime;		/* use time of last access */
int f_column;			/* columnated format */
int f_group;			/* show group ownership of a file */
int f_flags;			/* show flags associated with a file */
int f_ignorelink;		/* indirect through symbolic link operands */
int f_inode;			/* print inode */
int f_kblocks;			/* print size in kilobytes */
int f_listalldot;		/* list . and .. as well */
int f_listdir;			/* list actual directory, not contents */
int f_listdot;			/* list files beginning with . */
int f_longform;			/* long listing format */
int f_needstat;                 /* need stat(2) information */
int f_newline;			/* if precede with newline */
int f_nonprint;			/* show unprintables as ? */
int f_nosort;			/* don't sort output */
int f_recursive;		/* ls subdirectories also */
int f_reversesort;		/* reverse whatever sort is used */
int f_sectime;			/* print the real time for all files */
int f_singlecol;		/* use single column output */
int f_size;			/* list size in short listing */
int f_statustime;		/* use time of last mode change */
int f_dirname;			/* if precede with directory name */
int f_timesort;			/* sort by time vice name */
int f_type;			/* add type character for non-regular files */

void
main(argc, argv)
	int argc;
	char *argv[];
{
	struct winsize win;
	int ch, fts_options;
	char *p;

	/* Terminal defaults to -Cq, non-terminal defaults to -1. */
	if (isatty(1)) {
		f_nonprint = 1;
		if (ioctl(1, TIOCGWINSZ, &win) == -1 || !win.ws_col) {
			if (p = getenv("COLUMNS"))
				termwidth = atoi(p);
		}
		else
			termwidth = win.ws_col;
		f_column = 1;
	} else
		f_singlecol = 1;

	/* Root is -A automatically. */
	if (!getuid())
		f_listdot = 1;

	fts_options = 0;
	while ((ch = getopt(argc, argv, "1ACFLRTacdfgikloqrstu")) != EOF) {
		switch (ch) {
		/*
		 * The -1, -C and -l options all override each other so shell
		 * aliasing works right.
		 */
		case '1':
			f_singlecol = 1;
			f_column = f_longform = 0;
			break;
		case 'C':
			f_column = 1;
			f_longform = f_singlecol = 0;
			break;
		case 'l':
			f_longform = 1;
			f_column = f_singlecol = 0;
			break;
		/* -c and -u override each other */
		case 'c':
			f_statustime = 1;
			f_accesstime = 0;
			break;
		case 'u':
			f_accesstime = 1;
			f_statustime = 0;
			break;
		case 'F':
			f_type = 1;
			break;
		case 'L':
			f_ignorelink = 1;
			break;
		case 'R':
			f_recursive = 1;
			break;
		case 'a':
			fts_options |= FTS_SEEDOT;
			f_listalldot = 1;
			/* FALLTHROUGH */
		case 'A':
			f_listdot = 1;
			break;
		case 'S':
			Sflg++; /* fall into... */
		case 'd':
			f_listdir = 1;
			break;
		case 'f':
			f_nosort = 1;
			break;
		case 'g':
			f_group = 1;
			break;
		case 'i':
			f_inode = 1;
			break;
		case 'k':
			f_kblocks = 1;
			break;
		case 'o':
			f_flags = 1;
			break;
		case 'q':
			f_nonprint = 1;
			break;
		case 'r':
			f_reversesort = 1;
			break;
		case 's':
			f_size = 1;
			break;
		case 'T':
			f_sectime = 1;
			break;
		case 't':
			f_timesort = 1;
			break;
		default:
		case '?':
			usage();
		}
	}
	argc -= optind;
	argv += optind;

	/* The -d option turns off the -R option. */
	if (f_listdir)
		f_recursive = 0;

	/* If options require that the files be stat'ed. */
	f_needstat =
	    f_longform || f_timesort || f_size || f_type;
	if (!f_needstat)
		fts_options |= FTS_NOSTAT;

	/* Select a sort function. */
	if (f_reversesort) {
		if (!f_timesort)
			sortfcn = revnamecmp;
		else if (f_accesstime)
			sortfcn = revacccmp;
		else if (f_statustime)
			sortfcn = revstatcmp;
		else /* Use modification time. */
			sortfcn = revmodcmp;
	} else {
		if (!f_timesort)
			sortfcn = namecmp;
		else if (f_accesstime)
			sortfcn = acccmp;
		else if (f_statustime)
			sortfcn = statcmp;
		else /* Use modification time. */
			sortfcn = modcmp;
	}

	/* Select a print function. */
	if (f_singlecol)
		printfcn = printscol;
	else if (f_longform)
		printfcn = printlong;
	else
		printfcn = printcol;

	/* If -l or -F, and not ignoring the link, use lstat(). */
	fts_options |= (f_longform || f_type) && !f_ignorelink ?
	    FTS_PHYSICAL : FTS_LOGICAL;

	if (argc)
		traverse(argc, argv, fts_options);
	else {
		static char dot[] = ".", *dotav[] = { dot, NULL };
		traverse(1, dotav, fts_options);
	}
	exit(0);
}

#define	IS_DDOT(name) \
	((name)[0] == '.' && ((name)[1] == '\0' || \
	    ((name)[1] == '.' && (name)[2] == '\0')))

/*
 * Traverse() walks the logical directory structure specified by the argv list
 * in the order specified by the mastercmp() comparison function.  During the
 * traversal it passes linked lists of structures to display() which represent
 * a superset (may be exact set) of the files to be displayed.
 */
static void
traverse(argc, argv, options)
	int argc, options;
	char *argv[];
{
	register FTS *ftsp;
	register FTSENT *p;
	register char *name;
	register int is_ddot;
	
	if ((ftsp =
	    fts_open(argv, options, f_nosort ? NULL : mastercmp)) == NULL)
		err(1, "fts_open: %s", strerror(errno));
	display(argc, NULL, fts_children(ftsp));
	if (f_listdir)
		return;
	while (p = fts_read(ftsp))
		switch(p->fts_info) {
		case FTS_DC:
			err(0, "%s: directory causes a cycle", p->fts_name);
			break;
		case FTS_DNR:
		case FTS_ERR:
			err(0, "%s: %s",
			    p->fts_name, strerror(p->fts_errno));
			break;
		case FTS_D:
			name = p->fts_name;
			is_ddot = IS_DDOT(name);
			if (!is_ddot ||
			    (is_ddot && p->fts_level == FTS_ROOTLEVEL)) {
				if (name[0] == '.' && !f_listdot && !is_ddot)
					break;
				display(argc, p, fts_children(ftsp));
				if (!f_recursive)
					(void)fts_set(ftsp, p, FTS_SKIP);
			}
			break;
		}
	(void)fts_close(ftsp);
}

/*
 * Display() takes a linked list of FTSENT structures and based on the flags
 * set on the command line passes the list along with any other necessary
 * information to the print function (printfcn()).  P always points to the
 * parent directory of the display list.
 */
static void
display(argc, p, list)
	int argc;
	register FTSENT *p;
	FTSENT *list;
{
	register FTSENT *cur;
	u_long btotal;
	int entries, maxlen;
	
	/*
	 * If list is NULL there are two possibilities: that the parent
	 * directory p has no children, or that fts_children() returned an
	 * error.  We ignore the error case since, it will be replicated
	 * on the next call to fts_read() on the post-order visit to the
	 * directory p, and will be signalled in traverse().
	 */
	if (list == NULL && (p->fts_level != FTS_ROOTLEVEL || argc != 1)) {
		(void)printf("\n%s:\n", p->fts_path);
		return;
	}

	/*
	 * P can only be NULL if list is the argv list.  This list must be
	 * handled slightly differently due to the fact that there does not
	 * exist a proper parent directory and different rules apply to it.
	 */
	btotal = 0;
	maxlen = 0;
	if (p == NULL)
		for (cur = list, entries = 0; cur; cur = cur->fts_link) {
			if (cur->fts_info == FTS_ERR ||
			    cur->fts_errno == ENOENT ||
			    cur->fts_info == FTS_NS && f_needstat) {
				err(0, "%s: %s",
				    cur->fts_name, strerror(cur->fts_errno));
				cur->fts_number = NO_PRINT;
				continue;
			}
			/*
			 * If cur is a directory, do not print it out now.  Its
			 * contents will be printed out when fts_read() reads
			 * it.
			 */
			if (cur->fts_info == FTS_D && !f_listdir) {
				cur->fts_number = NO_PRINT;
				continue;
			}
			/*
			 * If file is dot file and no -a or -A is set,
			 * do not display.
			 */
			if (cur->fts_name[0] == '.' && !f_listdot) {
				cur->fts_number = NO_PRINT;
				continue;
			}
			++entries;
			if (f_nonprint)
				prcopy(cur->fts_name, cur->fts_name,
				    cur->fts_namelen);
			if (f_column && cur->fts_namelen > maxlen)
				maxlen = cur->fts_namelen;
		}
	else {
		if (p->fts_level != FTS_ROOTLEVEL || argc != 1)
			(void)printf("\n%s:\n", p->fts_path);
		
		for (cur = list, entries = 0; cur; cur = cur->fts_link) {
			if (cur->fts_info == FTS_ERR ||
			    cur->fts_errno == ENOENT ||
			    cur->fts_info == FTS_NS && f_needstat) {
				err(0, "%s: %s",
				    cur->fts_name, strerror(cur->fts_errno));
				cur->fts_number = NO_PRINT;
				continue;
			}
			/*
			 * If file is dot file and no -a or -A is set,
			 * do not display.
			 */
			if (cur->fts_name[0] == '.' && !f_listdot) {
				cur->fts_number = NO_PRINT;
				continue;
			}
			++entries;
			if (f_nonprint)
				prcopy(cur->fts_name, cur->fts_name,
				       cur->fts_namelen);
			if (f_column && cur->fts_namelen > maxlen)
				maxlen = cur->fts_namelen;
			
			if (f_longform || f_size)
				btotal += cur->fts_statp->st_blocks;
		}
	}

	if (entries)
		printfcn(list, entries, btotal, maxlen);
}

/*
 * Ordering for mastercmp:
 * If ordering the argv (fts_level = FTS_ROOTLEVEL) return non-directories
 * as larger than directories.  Within either group, use the sort function.
 * All other levels use the sort function.  Error entries remain unsorted.
 */
static int
mastercmp(a, b)
	const FTSENT **a, **b;
{
	register int a_info, b_info;

	a_info = (*a)->fts_info;
	if (a_info == FTS_ERR)
		return (0);
	b_info = (*b)->fts_info;
	if (b_info == FTS_ERR)
		return (0);
	if (f_needstat && (a_info == FTS_NS || b_info == FTS_NS))
		return (namecmp(*a, *b));

	if (a_info == b_info)  
		return (sortfcn(*a, *b));

	if ((*a)->fts_level == FTS_ROOTLEVEL)
		if (a_info == FTS_D)        
			return (1);
		else if (b_info == FTS_D)
			return (-1);
		else
			return (sortfcn(*a, *b));
	else
		return (sortfcn(*a, *b));
}
