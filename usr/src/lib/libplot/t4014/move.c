/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	4.2 (Berkeley) %G%";
#endif /* not lint */

move(xi,yi){
	putch(035);
	cont(xi,yi);
}
