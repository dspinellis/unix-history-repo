/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)hangman.h	5.1 (Berkeley) %G%
 */

# include	<curses.h>
# include	<sys/types.h>
# include	<sys/stat.h>
# include	<ctype.h>
# include	<signal.h>

# define	MINLEN	6
# define	MAXERRS	7
# define	DICT	"/usr/dict/words"

# define	MESGY	12
# define	MESGX	0
# define	PROMPTY	11
# define	PROMPTX	0
# define	KNOWNY	10
# define	KNOWNX	1
# define	NUMBERY	4
# define	NUMBERX	(COLS - 1 - 26)
# define	AVGY	5
# define	AVGX	(COLS - 1 - 26)
# define	GUESSY	2
# define	GUESSX	(COLS - 1 - 26)


typedef struct {
	short	y, x;
	char	ch;
} ERR_POS;

extern bool	Guessed[];

extern char	Word[], Known[], *Noose_pict[];

extern int	Errors, Wordnum;

extern double	Average;

extern ERR_POS	Err_pos[];

extern FILE	*Dict;

extern off_t	Dict_size;

int	die();

off_t	abs();
