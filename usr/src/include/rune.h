/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Paul Borman at Krystal Technologies.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)rune.h	8.1 (Berkeley) %G%
 */

#ifndef	_RUNE_H_
#define	_RUNE_H_

#include <runetype.h>

#define	_PATH_LOCALE	"/usr/share/locale"

#define _INVALID_RUNE   _CurrentRuneLocale->invalid_rune

#define __sgetrune      _CurrentRuneLocale->sgetrune
#define __sputrune      _CurrentRuneLocale->sputrune

#define sgetrune(s, n, r)       (*__sgetrune)((s), (n), (r))
#define sputrune(c, s, n, r)    (*__sputrune)((c), (s), (n), (r))

__BEGIN_DECLS
char	*mbrune __P((const char *, rune_t));
char	*mbrrune __P((const char *, rune_t));
char	*mbmb __P((const char *, char *));
int	 setrunelocale __P((char *));
void	 setinvalidrune __P((rune_t));
__END_DECLS

#endif	/*! _RUNE_H_ */
