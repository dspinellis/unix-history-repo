/* Copyright (c) 1979 Regents of the University of California */

/*
 * Character constants and bits
 *
 * The editor uses the QUOTE bit as a flag to pass on with characters
 * e.g. to the putchar routine.  The editor never uses a simple char variable.
 * Only arrays of and pointers to characters are used and parameters and
 * registers are never declared character.
 *
 * 1/26/81 (Berkeley) @(#)cr_ex.h	1.1
 */
# define	QUOTE	0200
# define	TRIM	0177
# define	CTRL(c)	('c' & 037)
# define	NL	CTRL(j)
# define	CR	CTRL(m)
# define	DELETE	0177		/* See also ATTN, QUIT in ex_tune.h */
# define	ESCAPE	033
