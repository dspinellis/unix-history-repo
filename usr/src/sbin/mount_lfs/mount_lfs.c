/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
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
static char copyright[] =
"@(#) Copyright (c) 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mount_lfs.c	8.1 (Berkeley) 6/8/93";
#endif /* not lint */

#include <sys/param.h>
#include <sys/mount.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "pathnames.h"

#define DEFAULT_ROOTUID -2	/* copied from mount's UFS code */

void usage __P((void));
void invoke_cleaner __P((char *));

int short_rds, cleaner_debug;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct ufs_args args;
	int ch, mntflags;
	int noclean;
	char *fs_name;

	mntflags = noclean = 0;
	while ((ch = getopt(argc, argv, "F:nsd")) != EOF)
		switch(ch) {
		case 'F':
			mntflags = atoi(optarg);
			break;
		case 'n':
			noclean = 1;
			break;
		case 's':
			short_rds = 1;
			break;
		case 'd':
			cleaner_debug = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 2)
		usage();

        args.fspec = argv[0];	/* the name of the device file */
	fs_name = argv[1];	/* the mount point */

	/* copied from mount's UFS code */
	args.exroot = DEFAULT_ROOTUID;
	if (mntflags & MNT_RDONLY)
		args.exflags = MNT_EXRDONLY;
	else
		args.exflags = 0;

	if (mount(MOUNT_LFS, fs_name, mntflags, &args)) {
		(void)fprintf(stderr, "mount_lfs: %s\n", strerror(errno));
		exit(1);
	}

	if (!noclean) {
		/*
		 * invoke the lfs_cleanerd for this filesystem as its arg!
		 */
		invoke_cleaner(fs_name);
		/* never returns */
	}

	exit(0);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: mount_lfs [ -nsd ] [ -F fsoptions ] device mount_point\n");
	exit(1);
}

void
invoke_cleaner(name)
	char *name;
{
	char *args[6], **ap = args;

	/* build the argument list */
	*ap++ = _PATH_LFS_CLEANERD;
	if (short_rds)
		*ap++ = "-s";
	if (cleaner_debug)
		*ap++ = "-d";
	*ap++ = name;
	*ap = NULL;

	execv(args[0], args);
	err(1, "exec of %x failed", _PATH_LFS_CLEANERD);
}
