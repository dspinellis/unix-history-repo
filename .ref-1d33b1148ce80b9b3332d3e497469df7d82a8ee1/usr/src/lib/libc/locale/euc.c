/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Paul Borman at Krystal Technologies.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)euc.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <errno.h>
#include <rune.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

rune_t	_EUC_sgetrune __P((const char *, size_t, char const **));
int	_EUC_sputrune __P((rune_t, char *, size_t, char **));

typedef struct {
	int	count[4];
	rune_t	bits[4];
	rune_t	mask;
} _EucInfo;

int
_EUC_init(rl)
	_RuneLocale *rl;
{
	_EucInfo *ei;
	int x;
	char *v, *e;

	rl->sgetrune = _EUC_sgetrune;
	rl->sputrune = _EUC_sputrune;

	if (!rl->variable) {
		free(rl);
		return (EFTYPE);
	}
	v = (char *) rl->variable;

	while (*v == ' ' || *v == '\t')
		++v;

	if ((ei = malloc(sizeof(_EucInfo))) == NULL) {
		free(rl);
		return (ENOMEM);
	}
	for (x = 0; x < 4; ++x) {
		ei->count[x] = (int) strtol(v, &e, 0);
		if (v == e || !(v = e)) {
			free(rl);
			free(ei);
			return (EFTYPE);
		}
		while (*v == ' ' || *v == '\t')
			++v;
		ei->bits[x] = (int) strtol(v, &e, 0);
		if (v == e || !(v = e)) {
			free(rl);
			free(ei);
			return (EFTYPE);
		}
		while (*v == ' ' || *v == '\t')
			++v;
	}
	ei->mask = (int)strtol(v, &e, 0);
	if (v == e || !(v = e)) {
		free(rl);
		free(ei);
		return (EFTYPE);
	}
	if (sizeof(_EucInfo) <= rl->variable_len) {
		memcpy(rl->variable, ei, sizeof(_EucInfo));
		free(ei);
	} else {
		rl->variable = &ei;
	}
	rl->variable_len = sizeof(_EucInfo);
	_CurrentRuneLocale = rl;
	__mb_cur_max = 3;
	return (0);
}

#define	CEI	((_EucInfo *)(_CurrentRuneLocale->variable))

#define	_SS2	0x008e
#define	_SS3	0x008f

static inline int
_euc_set(c)
	u_int c;
{
	c &= 0xff;

	return ((c & 0x80) ? c == _SS3 ? 3 : c == _SS2 ? 2 : 1 : 0);
}
rune_t
_EUC_sgetrune(string, n, result)
	const char *string;
	size_t n;
	char const **result;
{
	rune_t rune = 0;
	int len, set;

	if (n < 1 || (len = CEI->count[set = _euc_set(*string)]) > n) {
		if (result)
			*result = string;
		return (_INVALID_RUNE);
	}
	switch (set) {
	case 3:
	case 2:
		--len;
		++string;
		/* FALLTHROUGH */
	case 1:
	case 0:
		while (len-- > 0)
			rune = (rune << 8) | ((u_int)(*string++) & 0xff);
		break;
	}
	if (result)
		*result = string;
	return ((rune & ~CEI->mask) | CEI->bits[set]);
}

int
_EUC_sputrune(c, string, n, result)
	rune_t c;
	char *string, **result;
	size_t n;
{
	rune_t m = c & CEI->mask;
	rune_t nm = c & ~m;
	int i, len;

	if (m == CEI->bits[1]) {
CodeSet1:
		/* Codeset 1: The first byte must have 0x80 in it. */
		i = len = CEI->count[1];
		if (n >= len) {
			if (result)
				*result = string + len;
			while (i-- > 0)
				*string++ = (nm >> (i << 3)) | 0x80;
		} else
			if (result)
				*result = (char *) 0;
	} else {
		if (m == CEI->bits[0]) {
			i = len = CEI->count[0];
			if (n < len) {
				if (result)
					*result = NULL;
				return (len);
			}
		} else
			if (m == CEI->bits[2]) {
				i = len = CEI->count[2];
				if (n < len) {
					if (result)
						*result = NULL;
					return (len);
				}
				*string++ = _SS2;
				--i;
			} else
				if (m == CEI->bits[3]) {
					i = len = CEI->count[3];
					if (n < len) {
						if (result)
							*result = NULL;
						return (len);
					}
					*string++ = _SS3;
					--i;
				} else
					goto CodeSet1;	/* Bletch */
		while (i-- > 0)
			*string++ = (nm >> (i << 3)) & 0xff;
		if (result)
			*result = string;
	}
	return (len);
}
