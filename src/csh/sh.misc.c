/* Copyright (c) 1979 Regents of the University of California */
#include "sh.h"

/*
 * C Shell
 */

letter(c)
	register char c;
{

	return (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_');
}

digit(c)
	register char c;
{

	return (c >= '0' && c <= '9');
}

any(c, s)
	register int c;
	register char *s;
{

	while (*s)
		if (*s++ == c)
			return(1);
	return(0);
}

char *
calloc(i, j)
	register int i;
	int j;
{
	register char *cp, *dp;

	i *= j;
	cp = (char *) malloc(i);
	if (cp == 0) {
		child++;
		error("Out of memory");
	}
	dp = cp;
	if (i > 0)
		do
			*dp++ = 0;
		while (--i);
	return (cp);
}

cfree(p)
	char *p;
{

	free(p);
}

char **
blkend(up)
	register char **up;
{

	while (*up)
		up++;
	return (up);
}
 
blkpr(av)
	register int *av;
{

	for (; *av; av++) {
		printf("%s", *av);
		if (av[1])
			printf(" ");
	}
}

blklen(av)
	register char **av;
{
	register int i = 0;

	while (*av++)
		i++;
	return (i);
}

char **
blkcpy(oav, bv)
	char **oav;
	register char **bv;
{
	register char **av = oav;

	while (*av++ = *bv++)
		continue;
	return (oav);
}

char **
blkcat(up, vp)
	char **up, **vp;
{

	blkcpy(blkend(up), vp);
	return (up);
}

blkfree(av0)
	char **av0;
{
	register char **av = av0;

	while (*av)
		xfree(*av++);
	xfree(av0);
}

char **
saveblk(v)
	register char **v;
{
	register int len = blklen(v) + 1;
	register char **newv = (char **) calloc(len, sizeof (char **));
	char **onewv = newv;

	while (*v)
		*newv++ = savestr(*v++);
	return (onewv);
}

char *
strspl(cp, dp)
	register char *cp, *dp;
{
	register char *ep = calloc(1, strlen(cp) + strlen(dp) + 1);

	strcpy(ep, cp);
	return (strcat(ep, dp));
}

char **
blkspl(up, vp)
	register char **up, **vp;
{
	register char **wp = (char **) calloc(blklen(up) + blklen(vp) + 1, sizeof (char **));

	blkcpy(wp, up);
	return (blkcat(wp, vp));
}

lastchr(cp)
	register char *cp;
{

	if (!*cp)
		return (0);
	while (cp[1])
		cp++;
	return (*cp);
}

/*
 * This routine is called after an error to close up
 * any units which may have been left open accidentally.
 */
closem()
{
	register int f;

	for (f = 0; f < NOFILE; f++)
		if (f != SHIN && f != SHOUT && f != SHDIAG && f != OLDSTD)
			close(f);
}

/*
 * Close files before executing a file.
 * We could be MUCH more intelligent, since (on a version 7 system)
 * we need only close files here during a source, the other
 * shell fd's being in units 16-19 which are closed automatically!
 */
closech()
{
	register int f;

	if (didcch)
		return;
	didcch = 1;
	SHIN = 0; SHOUT = 1; SHDIAG = 2; OLDSTD = 0;
	for (f = 3; f < NOFILE; f++)
		close(f);
}

donefds()
{

	close(0), close(1), close(2);
	didfds = 0;
}

/*
 * Move descriptor i to j.
 * If j is -1 then we just want to get i to a safe place,
 * i.e. to a unit > 2.  This also happens in dcopy.
 */
dmove(i, j)
	register int i, j;
{

	if (i == j || i < 0)
		return (i);
	j = dcopy(i, j);
	if (j != i)
		close(i);
	return (j);
}

dcopy(i, j)
	register int i, j;
{

	if (i == j || i < 0 || j < 0 && i > 2)
		return (i);
	close(j);
	return (renum(i, j));
}

renum(i, j)
	register int i, j;
{
	register int k = dup(i);

	if (k < 0)
		return (-1);
	if (j == -1 && k > 2)
		return (k);
	if (k != j) {
		j = renum(k, j);
		close(k);
		return (j);
	}
	return (k);
}

copy(to, from, size)
	register char *to, *from;
	register int size;
{

	if (size)
		do
			*to++ = *from++;
		while (--size != 0);
}

/*
 * Left shift a command argument list, discarding
 * the first c arguments.  Used in "shift" commands
 * as well as by commands like "repeat".
 */
lshift(v, c)
	register char **v;
	register int c;
{
	register char **u = v;

	while (*u && --c >= 0)
		xfree(*u++);
	blkcpy(v, u);
}

number(cp)
	char *cp;
{

	if (*cp == '-') {
		cp++;
		if (!digit(*cp++))
			return (0);
	}
	while (*cp && digit(*cp))
		cp++;
	return (*cp == 0);
}

char **
copyblk(v)
	register char **v;
{
	register char **nv = (char **) calloc(blklen(v) + 1, sizeof (char **));

	return (blkcpy(nv, v));
}

char *
strend(cp)
	register char *cp;
{

	while (*cp)
		cp++;
	return (cp);
}

char *
strip(cp)
	char *cp;
{
	register char *dp = cp;

	while (*dp++ &= TRIM)
		continue;
	return (cp);
}

udvar(name)
	char *name;
{

	setname(name);
	bferr("Undefined variable");
}
