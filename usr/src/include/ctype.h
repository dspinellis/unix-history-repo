/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Paul Borman at Krystal Technologies.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ctype.h	8.1 (Berkeley) %G%
 */

#ifndef	_CTYPE_H_
#define _CTYPE_H_

#ifndef _ANSI_SOURCE
#include <runetype.h>
#endif

#define	_A	0x00000100L		/* Alpha */
#define	_C	0x00000200L		/* Control */
#define	_D	0x00000400L		/* Digit */
#define	_G	0x00000800L		/* Graph */
#define	_L	0x00001000L		/* Lower */
#define	_P	0x00002000L		/* Punct */
#define	_S	0x00004000L		/* Space */
#define	_U	0x00008000L		/* Upper */
#define	_X	0x00010000L		/* X digit */
#define	_B	0x00020000L		/* Blank */
#define	_R	0x00040000L		/* Print */
#define	_I	0x00080000L		/* Ideogram */
#define	_T	0x00100000L		/* Special */
#define	_Q	0x00200000L		/* Phonogram */

#define isalnum(c)      __istype((c), (_A|_D))
#define isalpha(c)      __istype((c),     _A)
#define iscntrl(c)      __istype((c),     _C)
#define isdigit(c)      __isctype((c),    _D)	/* ANSI -- locale independent */
#define isgraph(c)      __istype((c),     _G)
#define islower(c)      __istype((c),     _L)
#define isprint(c)      __istype((c),     _R)
#define ispunct(c)      __istype((c),     _P)
#define isspace(c)      __istype((c),     _S)
#define isupper(c)      __istype((c),     _U)
#define isxdigit(c)     __isctype((c),    _X)	/* ANSI -- locale independent */

#if !defined(_ANSI_SOURCE) && !defined(_POSIX_SOURCE)
#define	isascii(c)	((c & ~0x7F) == 0)
#define toascii(c)	((c) & 0x7F)
#define	digittoint(c)	__istype((c), 0xFF)
#define	isideogram(c)	__istype((c), _I)
#define	isphonogram(c)	__istype((c), _T)
#define	isspecial(c)	__istype((c), _Q)
#define isblank(c)	__istype((c), _B)
#define	isrune(c)	__istype((c),  0xFFFFFF00L)
#define	isnumber(c)	__istype((c), _D)
#define	ishexnumber(c)	__istype((c), _X)
#endif

/* See comments in <machine/ansi.h> about _BSD_RUNE_T_. */
__BEGIN_DECLS
extern unsigned long	___runetype __P((_BSD_RUNE_T_));
extern _BSD_RUNE_T_	___tolower __P((_BSD_RUNE_T_));
extern _BSD_RUNE_T_	___toupper __P((_BSD_RUNE_T_));
__END_DECLS

/*
 * If your compiler supports prototypes and inline functions,
 * #define _USE_CTYPE_INLINE_.  Otherwise, use the C library
 * functions.
 */
#if !defined(_USE_CTYPE_CLIBRARY_) && defined(__GNUC__) || defined(__cplusplus)
#define	_USE_CTYPE_INLINE_	1
#endif

#if defined(_USE_CTYPE_INLINE_)
static inline int
__istype(_BSD_RUNE_T_ c, unsigned long f)
{
	return((((c & _CRMASK) ? ___runetype(c) :
	    _CurrentRuneLocale->runetype[c]) & f) ? 1 : 0);
}

static inline int
__isctype(_BSD_RUNE_T_ c, unsigned long f)
{
	return((((c & _CRMASK) ? 0 :
	    _DefaultRuneLocale.runetype[c]) & f) ? 1 : 0);
}

/* _ANSI_LIBRARY is defined by lib/libc/gen/isctype.c. */
#if !defined(_ANSI_LIBRARY)
static inline _BSD_RUNE_T_
toupper(_BSD_RUNE_T_ c)
{
	return((c & _CRMASK) ?
	    ___toupper(c) : _CurrentRuneLocale->mapupper[c]);
}

static inline _BSD_RUNE_T_
tolower(_BSD_RUNE_T_ c)
{
	return((c & _CRMASK) ?
	    ___tolower(c) : _CurrentRuneLocale->maplower[c]);
}
#endif /* !_ANSI_LIBRARY */

#else /* !_USE_CTYPE_INLINE_ */

__BEGIN_DECLS
extern int		__istype __P((_BSD_RUNE_T_, unsigned long));
extern int		__isctype __P((_BSD_RUNE_T_, unsigned long));
extern _BSD_RUNE_T_	toupper __P((_BSD_RUNE_T_));
extern _BSD_RUNE_T_	tolower __P((_BSD_RUNE_T_));
__END_DECLS
#endif /* _USE_CTYPE_INLINE_ */

#endif /* !_CTYPE_H_ */
