/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)chmod.c	5.6 (Berkeley) %G%";
#endif

/*
 * chmod options mode files
 * where
 *	mode is [ugoa][+-=][rwxXstugo] or an octal number
 *	options are -Rf
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>

char	*modestring, *ms;
int	um;
int	status;
int	fflag;
int	rflag;

main(argc, argv)
	char *argv[];
{
	register char *p, *flags;
	register int i;
	struct stat st;

	if (argc < 3) {
		fprintf(stderr,
		    "Usage: chmod [-Rf] [ugoa][+-=][rwxXstugo] file ...\n");
		exit(-1);
	}
	argv++, --argc;
	while (argc > 0 && argv[0][0] == '-') {
		for (p = &argv[0][1]; *p; p++) switch (*p) {

		case 'R':
			rflag++;
			break;

		case 'f':
			fflag++;
			break;

		default:
			goto done;
		}
		argc--, argv++;
	}
done:
	modestring = argv[0];
	um = umask(0);
	(void) newmode(0);
	for (i = 1; i < argc; i++) {
		p = argv[i];
		/* do stat for directory arguments */
		if (lstat(p, &st) < 0) {
			status += Perror(p);
			continue;
		}
		if (rflag && (st.st_mode&S_IFMT) == S_IFDIR) {
			status += chmodr(p, newmode(st.st_mode));
			continue;
		}
		if ((st.st_mode&S_IFMT) == S_IFLNK && stat(p, &st) < 0) {
			status += Perror(p);
			continue;
		}
		if (chmod(p, newmode(st.st_mode)) < 0) {
			status += Perror(p);
			continue;
		}
	}
	exit(status);
}

chmodr(dir, mode)
	char *dir;
{
	register DIR *dirp;
	register struct direct *dp;
	struct stat st;
	char savedir[1024];
	int ecode;

	if (getwd(savedir) == 0)
		fatal(255, "%s", savedir);
	/*
	 * Change what we are given before doing it's contents
	 */
	if (chmod(dir, newmode(mode)) < 0 && Perror(dir))
		return (1);
	if (chdir(dir) < 0) {
		Perror(dir);
		return (1);
	}
	if ((dirp = opendir(".")) == NULL) {
		Perror(dir);
		return (1);
	}
	dp = readdir(dirp);
	dp = readdir(dirp); /* read "." and ".." */
	ecode = 0;
	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
		if (lstat(dp->d_name, &st) < 0) {
			ecode = Perror(dp->d_name);
			if (ecode)
				break;
			continue;
		}
		if ((st.st_mode&S_IFMT) == S_IFDIR) {
			ecode = chmodr(dp->d_name, newmode(st.st_mode));
			if (ecode)
				break;
			continue;
		}
		if ((st.st_mode&S_IFMT) == S_IFLNK)
			continue;
		if (chmod(dp->d_name, newmode(st.st_mode)) < 0 &&
		    (ecode = Perror(dp->d_name)))
			break;
	}
	closedir(dirp);
	if (chdir(savedir) < 0)
		fatal(255, "can't change back to %s", savedir);
	return (ecode);
}

error(fmt, a)
	char *fmt, *a;
{

	if (!fflag) {
		fprintf(stderr, "chmod: ");
		fprintf(stderr, fmt, a);
		putc('\n', stderr);
	}
	return (!fflag);
}

fatal(status, fmt, a)
	int status;
	char *fmt, *a;
{

	fflag = 0;
	(void) error(fmt, a);
	exit(status);
}

Perror(s)
	char *s;
{

	if (!fflag) {
		fprintf(stderr, "chmod: ");
		perror(s);
	}
	return (!fflag);
}

newmode(nm)
	unsigned nm;
{
	register o, m, b;
	int savem;

	ms = modestring;
	savem = nm;
	m = abs();
	if (*ms == '\0')
		return (m);
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
	if (*--ms)
		fatal(255, "invalid mode");
	return (nm);
}

abs()
{
	register c, i;

	i = 0;
	while ((c = *ms++) >= '0' && c <= '7')
		i = (i << 3) + (c - '0');
	ms--;
	return (i);
}

#define	USER	05700	/* user's bits */
#define	GROUP	02070	/* group's bits */
#define	OTHER	00007	/* other's bits */
#define	ALL	01777	/* all (note absence of setuid, etc) */

#define	READ	00444	/* read permit */
#define	WRITE	00222	/* write permit */
#define	EXEC	00111	/* exec permit */
#define	SETID	06000	/* set[ug]id */
#define	STICKY	01000	/* sticky bit */

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
		return (m);
	}
}

what()
{

	switch (*ms) {
	case '+':
	case '-':
	case '=':
		return (*ms++);
	}
	return (0);
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
		return (m);
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
		return (m);
	}
}
