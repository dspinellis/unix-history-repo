/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)REMOVE.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

REMOVE(name, maxnamlen)

	char			*name;
	int			maxnamlen;
{
	register int	cnt;
	char		namebuf[NAMSIZ];

	/*
	 * trim trailing blanks, and insure that the name 
	 * will fit into the file structure
	 */
	for (cnt = 0; cnt < maxnamlen; )
		if (name[cnt] == '\0' || name[cnt++] == ' ')
			break;
	if (cnt >= NAMSIZ) {
		ERROR(ENAMESIZE, name);
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
		ERROR(EREMOVE, namebuf);
		return;
	}
}
