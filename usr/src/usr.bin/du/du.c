/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Newcomb.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)du.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <dirent.h>
#include <stdio.h>
#include <fts.h>
#include <string.h>
#include <stdlib.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register FTS *fts;
	register FTSENT *p;
	register int kvalue, listdirs, listfiles;
	int ch, ftsoptions;
	char **save;

	ftsoptions = FTS_PHYSICAL;
	kvalue = listfiles = 0;
	listdirs = 1;
	save = argv;
	while ((ch = getopt(argc, argv, "aksx")) != EOF)
		switch(ch) {
		case 'a':
			listfiles = 1;
			break;
		case 'k':
			kvalue = 1;
			break;
		case 's':
			listfiles = listdirs = 0;
			break;
		case 'x':
			ftsoptions |= FTS_XDEV;
			break;
		case '?':
		default:
			(void)fprintf(stderr,
			    "usage: du [-aksx] [name ...]\n");
			exit(1);
		}
	argv += optind;

	if (!*argv) {
		argv = save;
		argv[0] = ".";
		argv[1] = NULL;
	}

	if (!(fts = fts_open(argv, ftsoptions, (int (*)())NULL))) {
		(void)fprintf(stderr, "du: %s.\n", strerror(errno));
		exit(1);
	}

	while (p = fts_read(fts))
		switch(p->fts_info) {
		case FTS_DNR:
			(void)fprintf(stderr,
			    "du: %s: unable to read.\n", p->fts_path);
			break;
		case FTS_DNX:
			(void)fprintf(stderr,
			    "du: %s: unable to search.\n", p->fts_path);
			break;
		case FTS_D:
		case FTS_DC:
			break;
		case FTS_DP:
			p->fts_parent->fts_number += 
			    p->fts_number += p->fts_statb.st_blocks;
			/*
			 * If listing each directory, or not listing files
			 * or directories and this is post-order of the
			 * root of a traversal, display the total.
			 */
			if (listdirs || !listfiles && !p->fts_level)
				(void)printf("%ld\t%s\n", kvalue ?
				    howmany(p->fts_number, 2) :
				    p->fts_number, p->fts_path);
			break;
		case FTS_ERR:
			(void)fprintf(stderr,
			    "du: %s: %s.\n", p->fts_path, strerror(errno));
			exit(1);
		case FTS_NS:
			(void)fprintf(stderr,
			    "du: unable to stat: %s.\n", p->fts_path);
			break;
		default:
			if (p->fts_statb.st_nlink > 1 && linkchk(p))
				break;
			/*
			 * If listing each file, or a non-directory file was
			 * the root of a traversal, display the total.
			 */
			if (listfiles || !p->fts_level)
				(void)printf("%ld\t%s\n", kvalue ?
				    howmany(p->fts_statb.st_blocks, 2) :
				    p->fts_statb.st_blocks, p->fts_path);
			p->fts_parent->fts_number += p->fts_statb.st_blocks;
		}
	exit(0);
}

typedef struct _ID {
	dev_t	dev;
	ino_t	inode;
} ID;

linkchk(p)
	register FTSENT *p;
{
	static ID *files;
	static int maxfiles, nfiles;
	register ID *fp, *start;
	register ino_t ino;
	register dev_t dev;

	ino = p->fts_statb.st_ino;
	dev = p->fts_statb.st_dev;
	if (start = files)
		for (fp = start + nfiles - 1; fp >= start; --fp)
			if (ino == fp->inode && dev == fp->dev)
				return(1);

	if (nfiles == maxfiles && !(files = (ID *)realloc((char *)files,
	    (u_int)(sizeof(ID) * (maxfiles += 128))))) {
		(void)fprintf(stderr, "du: %s\n", strerror(errno));
		exit(1);
	}
	files[nfiles].inode = ino;
	files[nfiles].dev = dev;
	++nfiles;
	return(0);
}
