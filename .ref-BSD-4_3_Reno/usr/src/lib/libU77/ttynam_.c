/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ttynam_.c	5.1	6/7/85
 */

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
