/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)myhistedit.h	8.2 (Berkeley) %G%
 */

#include <histedit.h>

extern History *hist;
extern EditLine *el;
extern int displayhist;

void histedit __P((void));
void sethistsize __P((void));
int histcmd __P((int, char **));
int not_fcnumber __P((char *));
int str_to_event __P((char *, int));

