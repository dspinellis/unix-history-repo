/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)lstFake.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*-
 * lstFake.c --
 *	This is a file whose sole purpose is to force ranlib to
 *	place enough entries in the library's table of contents to
 *	prevent it (the table of contents) from looking like an object
 *	file. As of this writing, the table had 0410 (shared text) entries
 *	in it, so we define five junk variables to up the number beyond
 *	the range of the magic numbers.
 */

int _junk_one__ = 1;
int _junk_two__ = 2;
int _junk_three__ = 3;
int _junk_four__ = 4;
int _junk_five__ = 5;
