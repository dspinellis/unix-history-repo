/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)REMOVE.c	1.5 (Berkeley) 4/9/90";
#endif /* not lint */

#include "h00vars.h"

REMOVE(name, namlim)

	char			*name;
	long			namlim;
{
	register int	cnt;
	register int	maxnamlen = namlim;
	char		namebuf[NAMSIZ];

	/*
	 * trim trailing blanks, and insure that the name 
	 * will fit into the file structure
	 */
	for (cnt = 0; cnt < maxnamlen; cnt++)
		if (name[cnt] == '\0' || name[cnt] == ' ')
			break;
	if (cnt >= NAMSIZ) {
		ERROR("%s: File name too long\n", name);
		return;
	}
	maxnamlen = cnt;
	/*
	 * put the name into the buffer with null termination
	 */
	for (cnt = 0; cnt < maxnamlen; cnt++)
		namebuf[cnt] = name[cnt];
	namebuf[cnt] = '\0';
	/*
	 * unlink the file
	 */
	if (unlink(namebuf)) {
		PERROR("Could not remove ", namebuf);
		return;
	}
}
