/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)chmod.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * chmod options mode files
 * where
 *	mode	is [ugoa][+-=][rwxXstugo] or a octal number
 *	options are -R
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>

#define	USER	05700	/* user's bits */
#define	GROUP	02070	/* group's bits */
#define	OTHER	00007	/* other's bits */
#define	ALL	01777	/* all (note absence of setuid, etc) */

#define	READ	00444	/* read permit */
#define	WRITE	00222	/* write permit */
#define	EXEC	00111	/* exec permit */
#define	SETID	06000	/* set[ug]id */
#define	STICKY	01000	/* sticky bit */

char	*modestring, *ms;
int	um;
int	status;
int	rflag, debug;

main(argc,argv)
char **argv;
{
	register i;
	register char *p, *flags;
	struct	stat st;

usage:
	if (argc < 3) {
		fprintf(stderr
			,"Usage: chmod [-R] [ugoa][+-=][rwxXstugo] file ...\n");
		exit(-1);
	}

	argv++, --argc;
	if (*argv[0] == '-') {
		if (strcmp(argv[0], "-R") == 0) {
			rflag++;
			argv++, argc--;
		}
	}

	modestring = argv[0];

	um = umask(0);
	(void) newmode(0);
	for (i = 1; i < argc; i++) {
		p = argv[i];
		if (stat(p, &st) < 0) {
			fprintf(stderr, "chmod: can't access %s\n", p);
			++status;
			continue;
		}
		if (rflag && st.st_mode & S_IFDIR) {
			status += chmodr(p, newmode(st.st_mode));
		} else if (chmod(p, newmode(st.st_mode)) < 0) {
			fprintf(stderr, "chmod: can't change %s\n", p);
			++status;
			continue;
		}
	}
	exit(status);
}

chmodr(dir, mode)
	char	*dir;
{
	register DIR		*dirp;
	register struct direct	*dp;
	register struct stat	st;
	char			savedir[1024];

	if (getwd(savedir) == 0) {
		fprintf(stderr, "chmod: %s\n", savedir);
		exit(255);
	}

	/*
	** chmod what we are given before doing it's contents
	*/
	chmod(dir, newmode(mode));
	
	chdir(dir);
	if ((dirp = opendir(".")) == NULL) {
		perror(dir);
		return(1);
	}
	dp = readdir(dirp);
	dp = readdir(dirp); /* read "." and ".." */
	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
		if (stat(dp->d_name, &st) < 0) {
			fprintf(stderr, "chmod: can't access %s\n", dp->d_name);
			return(1);
		}
		chmod(dp->d_name, newmode(st.st_mode));
		if (st.st_mode & S_IFDIR)
			chmodr(dp->d_name, mode);
	}
	closedir(dirp);
	chdir(savedir);
	return(0);
}

newmode(nm)
unsigned nm;
{
	register o, m, b;
	int savem;

	ms = modestring;
	savem = nm;
	m = abs();
	if (!*ms)
		return(m);
	do {
		m = who();
		while (o = what()) {
			b = where(nm);
			switch (o) {
			case '+':
				nm |= b & m;
				break;
			case '-':
				nm &= ~(b & m);
				break;
			case '=':
				nm &= ~m;
				nm |= b & m;
				break;
			}
		}
	} while (*ms++ == ',');
	if (*--ms) {
		fprintf(stderr, "chmod: invalid mode\n");
		exit(255);
	}
	return(nm);
}

abs()
{
	register c, i;

	i = 0;
	while ((c = *ms++) >= '0' && c <= '7')
		i = (i << 3) + (c - '0');
	ms--;
	return(i);
}

who()
{
	register m;

	m = 0;
	for (;;) switch (*ms++) {
	case 'u':
		m |= USER;
		continue;
	case 'g':
		m |= GROUP;
		continue;
	case 'o':
		m |= OTHER;
		continue;
	case 'a':
		m |= ALL;
		continue;
	default:
		ms--;
		if (m == 0)
			m = ALL & ~um;
		return m;
	}
}

what()
{

	switch (*ms) {
	case '+':
	case '-':
	case '=':
		return *ms++;
	}
	return(0);
}

where(om)
register om;
{
	register m;

 	m = 0;
	switch (*ms) {
	case 'u':
		m = (om & USER) >> 6;
		goto dup;
	case 'g':
		m = (om & GROUP) >> 3;
		goto dup;
	case 'o':
		m = (om & OTHER);
	dup:
		m &= (READ|WRITE|EXEC);
		m |= (m << 3) | (m << 6);
		++ms;
		return m;
	}
	for (;;) switch (*ms++) {
	case 'r':
		m |= READ;
		continue;
	case 'w':
		m |= WRITE;
		continue;
	case 'x':
		m |= EXEC;
		continue;
	case 'X':
		if ((om & S_IFDIR) || (om & EXEC))
			m |= EXEC;
		continue;
	case 's':
		m |= SETID;
		continue;
	case 't':
		m |= STICKY;
		continue;
	default:
		ms--;
		return m;
	}
}
