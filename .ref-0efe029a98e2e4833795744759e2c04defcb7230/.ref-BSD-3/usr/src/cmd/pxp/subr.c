/* Copyright (c) 1979 Regents of the University of California */
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"

#ifndef PI1
/*
 * Does the string fp end in '.' and the character c ?
 */
dotted(fp, c)
	register char *fp;
	char c;
{
	register int i;

	i = strlen(fp);
	return (i > 1 && fp[i - 2] == '.' && fp[i - 1] == c);
}

/*
 * Toggle the option c.
 */
togopt(c)
	char c;
{
	register char *tp;

	tp = &opts[c-'a'];
	*tp = 1 - *tp;
}

/*
 * Set the time vector "tvec" to the
 * modification time stamp of the current file.
 */
#include <sys/types.h>
#include <stat.h>
gettime()
{
	struct stat stb;

	stat(filename, &stb);
	tvec = stb.st_mtime;
}

/*
 * Convert a "ctime" into a Pascal styple time line
 */
myctime(tv)
	long *tv;
{
	register char *cp, *dp;
	char *cpp;
	register i;
	static char mycbuf[26];

	cpp = ctime(tv);
	dp = mycbuf;
	cp = cpp;
	cpp[16] = 0;
	while (*dp++ = *cp++);
	dp--;
	cp = cpp+19;
	cpp[24] = 0;
	while (*dp++ = *cp++);
	return (mycbuf);
}

/*
 * Is "fp" in the command line list of names ?
 */
inpflist(fp)
	char *fp;
{
	register i, *pfp;

	pfp = pflist;
	for (i = pflstc; i > 0; i--)
		if (strcmp(fp, *pfp++) == 0)
			return (1);
	return (0);
}
#endif

extern	int errno;
extern	char *sys_errlist[];

/*
 * Boom!
 */
Perror(file, error)
	char *file, *error;
{

	errno = 0;
	sys_errlist[0] = error;
	perror(file);
}

char *
alloc(size)
	int size;
{

	return (calloc(size, 1));
}

copy(to, from, bytes)
	register char *to, *from;
	register int bytes;
{

	if (bytes != 0)
		do
			*to++ = *from++;
		while (--bytes);
}

/*
 * Is ch one of the characters in the string cp ?
 */
any(cp, ch)
	register char *cp;
	char ch;
{

	while (*cp)
		if (*cp++ == ch)
			return (1);
	return (0);
}

opush(c)
	register CHAR c;
{

	c =- 'a';
	optstk[c] =<< 1;
	optstk[c] =| opts[c];
	opts[c] = 1;
#ifdef PI0
	send(ROPUSH, c);
#endif
}

opop(c)
	register CHAR c;
{

	c =- 'a';
	opts[c] = optstk[c] & 1;
	optstk[c] =>> 1;
#ifdef PI0
	send(ROPOP, c);
#endif
}
