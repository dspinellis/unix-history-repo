#include "whoami"
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "0.h"

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
gettime()
{
	int stbuf[18];

	stat(filename, stbuf);
	tvec[0] = stbuf[16];
	tvec[1] = stbuf[17];
}

/*
 * Convert a "ctime" into a Pascal styple time line
 */
myctime(tv)
	int *tv;
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

calloc(num, size)
	int num, size;
{
	register int p1, *p2, nbyte;

	nbyte = (num*size+1) & ~01;
	if ((p1 = alloc(nbyte)) == -1 || p1==0)
		return (-1);
	p2 = p1;
	nbyte =>> 1;		/* 2 bytes/word */
	do {
		*p2++ = 0;
	} while (--nbyte);
	return (p1);
}

/*
 * Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0
 */
strcmp(s1, s2)
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
strcpy(s1, s2)
	register char *s1, *s2;
{
	register os1;

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

	c =- 'a';
	optstk[c] =<< 1;
	optstk[c] =| opts[c];
	opts[c] = 1;
}

opop(c)
	register CHAR c;
{

	c =- 'a';
	opts[c] = optstk[c] & 1;
	optstk[c] =>> 1;
}
