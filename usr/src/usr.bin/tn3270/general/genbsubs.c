/*
 *	Copyright (c) 1984, 1985, 1986 by the Regents of the
 *	University of California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */

#ifndef	lint
static	char	sccsid[] = "@(#)genbsubs.c	3.1  10/29/86";
#endif	/* ndef lint */

/* The output of bunequal is the offset of the byte which didn't match;
 * if all the bytes match, then we return n.
 * bunequal(s1, s2, n) */

int
bunequal(s1, s2, n)
register char *s1, *s2;
register n;
{
    register int i = 0;

    while (i++ < n) {
	if (*s1++ != *s2++) {
	    break;
	}
    }
    return(i-1);
}

/* bskip(s1, n, b) : finds the first occurrence of any byte != 'b' in the 'n'
 * bytes beginning at 's1'.
 */

int
bskip(s1, n, b)
register char *s1;
register int n;
register int b;
{
    register int i = 0;

    while (i++ < n) {
	if (*s1++ != b) {
	    break;
	}
    }
    return(i-1);
}

/*
 * memNIchr(const void *s, int c, size_t n, int and)
 *
 * Like memchr, but the comparison is '((*s)&and) == c'.
 *
 */

unsigned char *
memNIchr(s, c, n, and)
char *s;
int c;
unsigned int n;
int and;
{
    register unsigned char _c, *_s, _and;

    _and = and;
    _c = (c&_and);
    _s = (unsigned char *)s;
    while (n--) {
	if (((*_s)&_and) == _c) {
	    return _s;
	}
	_s++;
    }
    return 0;
}

/*
 * memNDchr(const void *s, int c, size_t n, int and)
 *
 * Like memchr, but the comparison is '((*s)&and) == c',
 * and we increment our way through s by "stride" ('s += stride').
 *
 * We optimize for the most used strides of +1 and -1.
 */

unsigned char *
memNSchr(s, c, n, and, stride)
char *s;
int c;
unsigned int n;
int and;
int stride;
{
    register unsigned char _c, *_s, _and;

    _and = and;
    _c = (c&_and);
    _s = (unsigned char *)s;
    switch (stride) {
    case 1:
	while (n--) {
	    if (((*_s)&_and) == _c) {
		return _s;
	    }
	    _s++;
	}
	break;
    case -1:
	while (n--) {
	    if (((*_s)&_and) == _c) {
		return _s;
	    }
	    _s--;
	}
	break;
    default:
	while (n--) {
	    if (((*_s)&_and) == _c) {
		return _s;
	    }
	    _s += stride;
	}
    }
    return 0;
}
