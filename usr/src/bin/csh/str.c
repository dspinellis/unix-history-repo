/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)str.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * tc.str.c: Short string package
 * 	     This has been a lesson of how to write buggy code!
 */
#ifdef SHORT_STRINGS

#if __STDC__
# include <stdarg.h>
#else
# include <varargs.h>
#endif

#include "csh.h"
#include "extern.h"

Char  **
blk2short(src)
    register char **src;
{
    size_t     n;
    register Char **sdst, **dst;

    /*
     * Count
     */
    for (n = 0; src[n] != NULL; n++);
    sdst = dst = (Char **) xmalloc((size_t) ((n + 1) * sizeof(Char *)));

    for (; *src != NULL; src++)
	*dst++ = SAVE(*src);
    *dst = NULL;
    return (sdst);
}

char  **
short2blk(src)
    register Char **src;
{
    size_t     n;
    register char **sdst, **dst;

    /*
     * Count
     */
    for (n = 0; src[n] != NULL; n++);
    sdst = dst = (char **) xmalloc((size_t) ((n + 1) * sizeof(char *)));

    for (; *src != NULL; src++)
	*dst++ = strsave(short2str(*src));
    *dst = NULL;
    return (sdst);
}

#define MALLOC_INCR	1024
Char   *
str2short(src)
    register char *src;
{
    static Char *sdst;
    static size_t dstsize = 0;
    register Char *dst, *edst;

    if (src == NULL)
	return (NULL);

    if (sdst == (NULL)) {
	dstsize = MALLOC_INCR;
	sdst = (Char *) xmalloc((size_t) dstsize * sizeof(Char));
    }

    dst = sdst;
    edst = &dst[dstsize];
    while (*src) {
	*dst++ = (Char) ((unsigned char) *src++);
	if (dst == edst) {
	    dstsize += MALLOC_INCR;
	    sdst = (Char *) xrealloc((ptr_t) sdst,
				     (size_t) dstsize * sizeof(Char));
	    edst = &sdst[dstsize];
	    dst = &edst[-MALLOC_INCR];
	}
    }
    *dst = 0;
    return (sdst);
}

char   *
short2qstr(src)
    register Char *src;
{
    static char *sdst = NULL;
    static size_t dstsize = 0;
    register char *dst, *edst;

    if (src == NULL)
	return (NULL);

    if (sdst == NULL) {
	dstsize = MALLOC_INCR;
	sdst = (char *) xmalloc((size_t) dstsize * sizeof(char));
    }
    dst = sdst;
    edst = &dst[dstsize];
    while (*src) {
	if (*src & QUOTE) {
	    *dst++ = '\\';
	    if (dst == edst) {
		dstsize += MALLOC_INCR;
		sdst = (char *) xrealloc((ptr_t) sdst,
					 (size_t) dstsize * sizeof(char));
		edst = &sdst[dstsize];
		dst = &edst[-MALLOC_INCR];
	    }
	}
	*dst++ = (char) *src++;
	if (dst == edst) {
	    dstsize += MALLOC_INCR;
	    sdst = (char *) xrealloc((ptr_t) sdst,
				     (size_t) dstsize * sizeof(char));
	    edst = &sdst[dstsize];
	    dst = &edst[-MALLOC_INCR];
	}
    }
    *dst = 0;
    return (sdst);
}
char   *
short2str(src)
    register Char *src;
{
    static char *sdst = NULL;
    static size_t dstsize = 0;
    register char *dst, *edst;

    if (src == NULL)
	return (NULL);

    if (sdst == NULL) {
	dstsize = MALLOC_INCR;
	sdst = (char *) xmalloc((size_t) dstsize * sizeof(char));
    }
    dst = sdst;
    edst = &dst[dstsize];
    while (*src) {
	*dst++ = (char) *src++;
	if (dst == edst) {
	    dstsize += MALLOC_INCR;
	    sdst = (char *) xrealloc((ptr_t) sdst,
				     (size_t) dstsize * sizeof(char));
	    edst = &sdst[dstsize];
	    dst = &edst[-MALLOC_INCR];
	}
    }
    *dst = 0;
    return (sdst);
}

Char   *
s_strcpy(dst, src)
    register Char *dst, *src;
{
    register Char *sdst;

    sdst = dst;
    while (*dst++ = *src++);
    return (sdst);
}

Char   *
s_strncpy(dst, src, n)
    register Char *dst, *src;
    register size_t n;
{
    register Char *sdst;

    sdst = dst;
    while (--n >= 0 && (*dst++ = *src++));
    while (--n >= 0)
	*dst++ = '\0';
    return (sdst);
}

Char   *
s_strcat(dst, src)
    register Char *dst, *src;
{
    register short *sdst;

    sdst = dst;
    while (*dst++);
    --dst;
    while (*dst++ = *src++);
    return (sdst);
}

#ifdef NOTUSED
Char   *
s_strncat(dst, src, n)
    register Char *dst, *src;
    register size_t n;
{
    register Char *sdst;

    sdst = dst;
    while (*dst++);
    --dst;
    while (*src && --n >= 0)
	*dst++ = *src++;
    *dst++ = '\0';
    return (sdst);
}

#endif

Char   *
s_strchr(str, ch)
    register Char *str;
    int ch;
{
    do
	if (*str == ch)
	    return (str);
    while (*str++);
    return (NULL);
}

Char   *
s_strrchr(str, ch)
    register Char *str;
    int ch;
{
    register Char *rstr;

    rstr = NULL;
    do
	if (*str == ch)
	    rstr = str;
    while (*str++);
    return (rstr);
}

size_t
s_strlen(str)
    register Char *str;
{
    register size_t n;

    for (n = 0; *str++; n++);
    return (n);
}

int
s_strcmp(str1, str2)
    register Char *str1, *str2;
{
    for (; *str1 && *str1 == *str2; str1++, str2++);
    /*
     * The following case analysis is necessary so that characters which look
     * negative collate low against normal characters but high against the
     * end-of-string NUL.
     */
    if (*str1 == '\0' && *str2 == '\0')
	return (0);
    else if (*str1 == '\0')
	return (-1);
    else if (*str2 == '\0')
	return (1);
    else
	return (*str1 - *str2);
}

int
s_strncmp(str1, str2, n)
    register Char *str1, *str2;
    register size_t n;
{
    for (; --n >= 0 && *str1 == *str2; str1++, str2++);

    if (n < 0)
	return (0);
    /*
     * The following case analysis is necessary so that characters which look
     * negative collate low against normal characters but high against the
     * end-of-string NUL.
     */
    if (*str1 == '\0' && *str2 == '\0')
	return (0);
    else if (*str1 == '\0')
	return (-1);
    else if (*str2 == '\0')
	return (1);
    else
	return (*str1 - *str2);
}

Char   *
s_strsave(s)
    register Char *s;
{
    Char   *n;
    register Char *p;

    if (s == 0)
	s = STRNULL;
    for (p = s; *p++;);
    n = p = (Char *) xmalloc((size_t) ((p - s) * sizeof(Char)));
    while (*p++ = *s++);
    return (n);
}

Char   *
s_strspl(cp, dp)
    Char   *cp, *dp;
{
    Char   *ep;
    register Char *p, *q;

    if (!cp)
	cp = STRNULL;
    if (!dp)
	dp = STRNULL;
    for (p = cp; *p++;);
    for (q = dp; *q++;);
    ep = (Char *) xmalloc((size_t)
			  (((p - cp) + (q - dp) - 1) * sizeof(Char)));
    for (p = ep, q = cp; *p++ = *q++;);
    for (p--, q = dp; *p++ = *q++;);
    return (ep);
}

Char   *
s_strend(cp)
    register Char *cp;
{
    if (!cp)
	return (cp);
    while (*cp)
	cp++;
    return (cp);
}

#ifdef NOTUSED
Char   *
s_strstr(s, t)
    register Char *s, *t;
{
    do {
	register Char *ss = s;
	register Char *tt = t;

	do
	    if (*tt == '\0')
		return (s);
	while (*ss++ == *tt++);
    } while (*s++ != '\0');
    return (NULL);
}
#endif

#endif				/* SHORT_STRINGS */
