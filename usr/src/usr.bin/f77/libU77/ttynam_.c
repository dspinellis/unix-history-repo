/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ttynam_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Return name of tty port associated with lunit
 *
 * calling sequence:
 *	character*19 string, ttynam
 * 	string = ttynam (lunit)
 * where:
 *	the character string will be filled with the name of
 *	the port, preceded with '/dev/', and blank padded.
 *	(19 is the max length ever required)
 */

#include "../libI77/fiodefs.h"

extern unit units[];

ttynam_(name, strlen, lu)
char *name; long strlen; long *lu;
{
	char *t = NULL, *ttyname();

	if (0 <= *lu && *lu < MXUNIT && units[*lu].ufd)
		t = ttyname(fileno(units[*lu].ufd));
	if (t == NULL)
		t = "";
	b_char(t, name, strlen);
}
