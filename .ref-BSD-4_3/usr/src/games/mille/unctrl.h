/* $Header: /va/arnold/=lib/=curses/RCS/unctrl.h,v 1.1 85/02/06 12:03:20 arnold Exp $ */

/*
 * unctrl.h
 */

extern char	*_unctrl[];

# define	unctrl(ch)	(_unctrl[ch & 0177])
