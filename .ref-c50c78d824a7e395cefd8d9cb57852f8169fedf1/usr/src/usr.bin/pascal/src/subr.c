/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)subr.c	5.1 (Berkeley) %G%";
#endif not lint

#include "whoami.h"
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

	tp = &opt( c );
	*tp = 1 - *tp;
}

/*
 * Set the time vector "tvec" to the
 * modification time stamp of a file.
 */
gettime( filename )
    char *filename;
{
#include <sys/stat.h>
	struct stat stb;

	stat(filename, &stb);
	tvec = stb.st_mtime;
}

/*
 * Convert a "ctime" into a Pascal styple time line
 */
char *
myctime(tv)
	int *tv;
{
	register char *cp, *dp;
	extern char *ctime();
	char *cpp;
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
	register i;
	register char **pfp;

	pfp = pflist;
	for (i = pflstc; i > 0; i--)
		if (pstrcmp(fp, *pfp++) == 0)
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

	fprintf( stderr , "%s: %s\n" , file , error );
}

int *
pcalloc(num, size)
	int num, size;
{
	register int *p1, *p2, nbyte;

	nbyte = (num*size+( ( sizeof ( int ) ) - 1 ) ) & ~( ( sizeof ( int ) ) - 1 );
	if ((p1 = (int *) malloc((unsigned) nbyte)) == 0)
		return (0);
	p2 =  p1;
	nbyte /= sizeof ( int );
	do {
		*p2++ = 0;
	} while (--nbyte);
	return (p1);
}

/*
 * Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0
 */
pstrcmp(s1, s2)
	register char *s1, *s2;
{

	while (*s1 == *s2++)
		if (*s1++=='\0')
			return (0);
	return (*s1 - *--s2);
}

/*
 * Copy string s2 to s1.
 * S1 must be large enough.
 * Return s1.
 */
char *
pstrcpy(s1, s2)
	register char *s1, *s2;
{
	register char *os1;

	os1 = s1;
	while (*s1++ = *s2++)
		continue;
	return (os1);
}

/*
 * Strlen is currently a freebie of perror
 * Take the length of a string.
 * Note that this does not include the trailing null!
strlen(cp)
	register char *cp;
{
	register int i;

	for (i = 0; *cp != 0; cp++)
		i++;
	return (i);
}
 */
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

	c -= 'A';
	optstk[c] <<= 1;
	optstk[c] |= opts[c];
	opts[c] = 1;
#ifdef PI0
	send(ROPUSH, c);
#endif
}

opop(c)
	register CHAR c;
{

	c -= 'A';
	opts[c] = optstk[c] & 1;
	optstk[c] >>= 1;
#ifdef PI0
	send(ROPOP, c);
#endif
}
