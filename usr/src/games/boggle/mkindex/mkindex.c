/* vi: set tabstop=4 : */

#include <stdio.h>

#include "bog.h"

main(argc, argv)
int argc;
char **argv;
{
	int clen, rlen, prev;
	long off, start;
	char buf[MAXWORDLEN + 1], *p, *nextword();

	prev = '\0';
	off = start = 0L;
	while (nextword(stdin, buf, &clen, &rlen) != (char *) NULL) {
		if (*buf != prev) {
			if (prev != '\0')
				printf("%c %6ld %6ld\n", prev, start, off - 1);
			prev = *buf;
			start = off;
		}
		off += clen + 1;
	}
	printf("%c %6ld %6ld\n", prev, start, off - 1);
	exit(0);
}

/*
 * Return the next word in the compressed dictionary in 'buffer' or
 * NULL on end-of-file
 * Also set clen to the length of the compressed word (for mkindex) and
 * rlen to the strlen() of the real word
 */
char *
nextword(fp, buffer, clen, rlen)
FILE *fp;
char *buffer;
int *clen, *rlen;
{
    register int ch, pcount;
	register char *p, *q;
	static char buf[MAXWORDLEN + 1];
	static int first = 1;
	static int lastch = 0;

   	if (first) {
        if ((pcount = getc(fp)) == EOF)
			return((char *) NULL);
		first = 0;
	}
	else if ((pcount = lastch) == EOF)
		return((char *) NULL);

	p = buf + (*clen = pcount);
 
    while ((ch = getc(fp)) != EOF && ch >= 'a')
			*p++ = ch;
	    lastch = ch;
    *p = '\0';

	*rlen = (int) (p - buf);
	*clen = *rlen - *clen;

	p = buf;
	q = buffer;
	while ((*q++ = *p) != '\0') {
		if (*p++ == 'q')
			*q++ = 'u';
	}
    return(buffer);
}
 
