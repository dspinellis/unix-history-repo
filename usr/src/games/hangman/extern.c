/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)extern.c	5.2 (Berkeley) 6/18/88";
#endif /* not lint */

# include	"hangman.h"

bool	Guessed[26];

char	Word[BUFSIZ],
	Known[BUFSIZ],
	*Noose_pict[] = {
		"     ______",
		"     |    |",
		"     |",
		"     |",
		"     |",
		"     |",
		"   __|_____",
		"   |      |___",
		"   |_________|",
		NULL
	};

int	Errors,
	Wordnum = 0;

double	Average = 0.0;

ERR_POS	Err_pos[MAXERRS] = {
	{  2, 10, 'O' },
	{  3, 10, '|' },
	{  4, 10, '|' },
	{  5,  9, '/' },
	{  3,  9, '/' },
	{  3, 11, '\\' },
	{  5, 11, '\\' }
};

FILE	*Dict = NULL;

off_t	Dict_size;
