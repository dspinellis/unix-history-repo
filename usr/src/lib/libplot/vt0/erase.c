/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	4.2 (Berkeley) %G%";
#endif /* not lint */

extern vti;
erase(){
	int i;
	i=0401;
	write(vti,&i,2);
}
