/*
 * unctrl.h
 *
 * 1/26/81 (Berkeley) @(#)unctrl.h	1.1
 */

extern char	*_unctrl[];

# define	unctrl(ch)	(_unctrl[(unsigned) ch])
