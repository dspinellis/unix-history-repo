/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)system_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/* f77 interface to system routine */

system_(s, n)
register char *s;
long int n;
{
char buff[1001];
register char *bp, *blast;

blast = buff + (n<1000 ? n : 1000L);

for(bp = buff ; bp<blast && *s!='\0' ; )
	*bp++ = *s++;
*bp = '\0';
system(buff);
}
