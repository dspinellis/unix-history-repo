/*
 * Copyright (c) 1980, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)whereis.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/dir.h>
#include <stdio.h>
#include <ctype.h>

static char *bindirs[] = {
	"/bin",
	"/sbin",
	"/usr/ucb",
	"/usr/bin",
	"/usr/sbin",
	"/usr/old",
	"/usr/contrib",
	"/usr/games",
	"/usr/local",
	"/usr/libexec",
	"/usr/include",
	"/usr/hosts",
	"/usr/share", /*?*/
	"/etc",
#ifdef notdef
	/* before reorg */
	"/etc",
	"/bin",
	"/usr/bin",
	"/usr/games",
	"/lib",
	"/usr/ucb",
	"/usr/lib",
	"/usr/local",
	"/usr/new",
	"/usr/old",
	"/usr/hosts",
	"/usr/include",
#endif
	0
};
/* This needs to be redone - man pages live with sources */
static char *mandirs[] = {
	"/usr/man/man1",
	"/usr/man/man2",
	"/usr/man/man3",
	"/usr/man/man4",
	"/usr/man/man5",
	"/usr/man/man6",
	"/usr/man/man7",
	"/usr/man/man8",
	"/usr/man/manl",
	"/usr/man/mann",
	"/usr/man/mano",
	0
};
static char *srcdirs[]  = {
	"/usr/src/bin",
	"/usr/src/sbin",
	"/usr/src/etc",
	"/usr/src/pgrm",
	"/usr/src/usr.bin",
	"/usr/src/usr.sbin",
	"/usr/src/usr.ucb",
	"/usr/src/usr.new",
	"/usr/src/usr.lib",
	"/usr/src/libexec",
	"/usr/src/libdata",
	"/usr/src/share",
	"/usr/src/contrib",
	"/usr/src/athena",
	"/usr/src/devel",
	"/usr/src/games",
	"/usr/src/local",
	"/usr/src/man",
	"/usr/src/root",
	"/usr/src/old",
	"/usr/src/include",
	/* still need libs */
#ifdef notdef /* before reorg */
	"/usr/src/bin",
	"/usr/src/usr.bin",
	"/usr/src/etc",
	"/usr/src/ucb",
	"/usr/src/games",
	"/usr/src/usr.lib",
	"/usr/src/lib",
	"/usr/src/local",
	"/usr/src/new",
	"/usr/src/old",
	"/usr/src/include",
	"/usr/src/lib/libc/gen",
	"/usr/src/lib/libc/stdio",
	"/usr/src/lib/libc/sys",
	"/usr/src/lib/libc/net/common",
	"/usr/src/lib/libc/net/inet",
	"/usr/src/lib/libc/net/misc",
	"/usr/src/ucb/pascal",
	"/usr/src/ucb/pascal/utilities",
	"/usr/src/undoc",
#endif
	0
};

char	sflag = 1;
char	bflag = 1;
char	mflag = 1;
char	**Sflag;
int	Scnt;
char	**Bflag;
int	Bcnt;
char	**Mflag;
int	Mcnt;
char	uflag;
/*
 * whereis name
 * look for source, documentation and binaries
 */
main(argc, argv)
	int argc;
	char *argv[];
{

	argc--, argv++;
	if (argc == 0) {
usage:
		fprintf(stderr, "whereis [ -sbmu ] [ -SBM dir ... -f ] name...\n");
		exit(1);
	}
	do
		if (argv[0][0] == '-') {
			register char *cp = argv[0] + 1;
			while (*cp) switch (*cp++) {

			case 'f':
				break;

			case 'S':
				getlist(&argc, &argv, &Sflag, &Scnt);
				break;

			case 'B':
				getlist(&argc, &argv, &Bflag, &Bcnt);
				break;

			case 'M':
				getlist(&argc, &argv, &Mflag, &Mcnt);
				break;

			case 's':
				zerof();
				sflag++;
				continue;

			case 'u':
				uflag++;
				continue;

			case 'b':
				zerof();
				bflag++;
				continue;

			case 'm':
				zerof();
				mflag++;
				continue;

			default:
				goto usage;
			}
			argv++;
		} else
			lookup(*argv++);
	while (--argc > 0);
	exit(0);
}

getlist(argcp, argvp, flagp, cntp)
	char ***argvp;
	int *argcp;
	char ***flagp;
	int *cntp;
{

	(*argvp)++;
	*flagp = *argvp;
	*cntp = 0;
	for ((*argcp)--; *argcp > 0 && (*argvp)[0][0] != '-'; (*argcp)--)
		(*cntp)++, (*argvp)++;
	(*argcp)++;
	(*argvp)--;
}


zerof()
{

	if (sflag && bflag && mflag)
		sflag = bflag = mflag = 0;
}
int	count;
int	print;


lookup(cp)
	register char *cp;
{
	register char *dp;

	for (dp = cp; *dp; dp++)
		continue;
	for (; dp > cp; dp--) {
		if (*dp == '.') {
			*dp = 0;
			break;
		}
	}
	for (dp = cp; *dp; dp++)
		if (*dp == '/')
			cp = dp + 1;
	if (uflag) {
		print = 0;
		count = 0;
	} else
		print = 1;
again:
	if (print)
		printf("%s:", cp);
	if (sflag) {
		looksrc(cp);
		if (uflag && print == 0 && count != 1) {
			print = 1;
			goto again;
		}
	}
	count = 0;
	if (bflag) {
		lookbin(cp);
		if (uflag && print == 0 && count != 1) {
			print = 1;
			goto again;
		}
	}
	count = 0;
	if (mflag) {
		lookman(cp);
		if (uflag && print == 0 && count != 1) {
			print = 1;
			goto again;
		}
	}
	if (print)
		printf("\n");
}

looksrc(cp)
	char *cp;
{
	if (Sflag == 0) {
		find(srcdirs, cp);
	} else
		findv(Sflag, Scnt, cp);
}

lookbin(cp)
	char *cp;
{
	if (Bflag == 0)
		find(bindirs, cp);
	else
		findv(Bflag, Bcnt, cp);
}

lookman(cp)
	char *cp;
{
	if (Mflag == 0) {
		find(mandirs, cp);
	} else
		findv(Mflag, Mcnt, cp);
}

findv(dirv, dirc, cp)
	char **dirv;
	int dirc;
	char *cp;
{

	while (dirc > 0)
		findin(*dirv++, cp), dirc--;
}

find(dirs, cp)
	char **dirs;
	char *cp;
{

	while (*dirs)
		findin(*dirs++, cp);
}

findin(dir, cp)
	char *dir, *cp;
{
	DIR *dirp;
	struct direct *dp;

	dirp = opendir(dir);
	if (dirp == NULL)
		return;
	while ((dp = readdir(dirp)) != NULL) {
		if (itsit(cp, dp->d_name)) {
			count++;
			if (print)
				printf(" %s/%s", dir, dp->d_name);
		}
	}
	closedir(dirp);
}

itsit(cp, dp)
	register char *cp, *dp;
{
	register int i = strlen(dp);

	if (dp[0] == 's' && dp[1] == '.' && itsit(cp, dp+2))
		return (1);
	while (*cp && *dp && *cp == *dp)
		cp++, dp++, i--;
	if (*cp == 0 && *dp == 0)
		return (1);
	while (isdigit(*dp))
		dp++;
	if (*cp == 0 && *dp++ == '.') {
		--i;
		while (i > 0 && *dp)
			if (--i, *dp++ == '.')
				return (*dp++ == 'C' && *dp++ == 0);
		return (1);
	}
	return (0);
}
