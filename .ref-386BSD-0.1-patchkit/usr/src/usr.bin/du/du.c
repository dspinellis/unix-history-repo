/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Newcomb.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)du.c	5.12 (Berkeley) 6/20/91";
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
	int aflag, ch, ftsoptions, sflag;
	char **save;

	ftsoptions = FTS_PHYSICAL;
	kvalue = 0;
	save = argv;
	aflag = sflag = 0;
	while ((ch = getopt(argc, argv, "aksx")) != EOF)
		switch(ch) {
		case 'a':
			aflag = 1;
			break;
		case 'k':
			kvalue = 1;
			break;
		case 's':
			sflag = 1;
			break;
		case 'x':
			ftsoptions |= FTS_XDEV;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;

	if (aflag) {
		if (sflag)
			usage();
		listdirs = listfiles = 1;
	} else if (sflag)
		listdirs = listfiles = 0;
	else {
		listfiles = 0;
		listdirs = 1;
	}

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
		case FTS_D:
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
		case FTS_DNR:
		case FTS_ERR:
		case FTS_NS:
			(void)fprintf(stderr,
			    "du: %s: %s.\n", p->fts_path, strerror(errno));
			break;
		case FTS_SL:
			if (p->fts_level == FTS_ROOTLEVEL) {
				(void)fts_set(fts, p, FTS_FOLLOW);
				break;
			}
			/* FALLTHROUGH */
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

usage()
{
	(void)fprintf(stderr, "usage: du [-a | -s] [-kx] [file ...]\n");
	exit(1);
}
