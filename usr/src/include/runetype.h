/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Paul Borman at Krystal Technologies.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)runetype.h	8.1 (Berkeley) %G%
 */

#ifndef	_RUNETYPE_H_
#define	_RUNETYPE_H_

#include <machine/ansi.h>
#include <sys/cdefs.h>

#ifdef  _BSD_WCHAR_T_
typedef _BSD_WCHAR_T_	rune_t;
typedef _BSD_WCHAR_T_	wchar_t;
#undef  _BSD_WCHAR_T_
#endif

#define	_CACHED_RUNES	(1 <<8 )	/* Must be a power of 2 */
#define	_CRMASK		(~(_CACHED_RUNES - 1))

/*
 * The lower 8 bits of runetype[] contain the digit value of the rune.
 */
typedef struct {
	rune_t		min;		/* First rune of the range */
	rune_t		max;		/* Last rune (inclusive) of the range */
	rune_t		map;		/* What first maps to in maps */
	unsigned long	*types;		/* Array of types in range */
} _RuneEntry;

typedef struct {
	int		nranges;	/* Number of ranges stored */
	_RuneEntry	*ranges;	/* Pointer to the ranges */
} _RuneRange;

typedef struct {
	char		magic[8];	/* Magic saying what version we are */
	char		encoding[32];	/* ASCII name of this encoding */

	rune_t		(*sgetrune)
	    __P((const char *, unsigned int, char const **));
	int		(*sputrune)
	    __P((rune_t, char *, unsigned int, char **));
	rune_t		invalid_rune;

	unsigned long	runetype[_CACHED_RUNES];
	rune_t		maplower[_CACHED_RUNES];
	rune_t		mapupper[_CACHED_RUNES];

	/*
	 * The following are to deal with Runes larger than _CACHED_RUNES - 1.
	 * Their data is actually contiguous with this structure so as to make
	 * it easier to read/write from/to disk.
	 */
	_RuneRange	runetype_ext;
	_RuneRange	maplower_ext;
	_RuneRange	mapupper_ext;

	void		*variable;	/* Data which depends on the encoding */
	int		variable_len;	/* how long that data is */
} _RuneLocale;

#define	_RUNE_MAGIC_1	"RuneMagi"	/* Indicates version 0 of RuneLocale */

extern _RuneLocale _DefaultRuneLocale;
extern _RuneLocale *_CurrentRuneLocale;

#endif	/* !_RUNETYPE_H_ */
