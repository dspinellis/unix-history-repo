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
static char sccsid[] = "@(#)iargc_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

long int iargc_()
{
extern int xargc;
return ( xargc - 1 );
}
