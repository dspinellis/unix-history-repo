/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)READE.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

READE(curfile, name)

	register struct iorec	*curfile;
	char			*name;
{
	long			data;

	register short	*sptr;
	register int	len;
	register int	nextlen;
	register int	cnt;
	char		*cp;
	char		namebuf[NAMSIZ];
	int		retval;

	if (curfile->funit & FWRITE) {
		ERROR(EREADIT, curfile->pfname);
		return;
	}
	UNSYNC(curfile);
	retval = fscanf(curfile->fbuf,
	    "%*[ \t\n]%74[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789]",
	    namebuf);
	if (retval == EOF) {
		ERROR(EPASTEOF, curfile->pfname);
		return;
	}
	if (retval == 0) {
		ERROR(ENUMNTFD, namebuf);
		return;
	}
	curfile->funit |= SYNC;
	for (len = 0; len < NAMSIZ && namebuf[len]; len++)
		/* void */;
	len++;
	sptr = (short *)name;
	cnt = *sptr++;
	cp = name + sizeof (short) + *sptr;
	do	{
		nextlen = *sptr++;
		nextlen = *sptr - nextlen;
		if (nextlen == len && RELEQ(len, namebuf, cp)) {
			return *((short *) name) - cnt;
		}
		cp += nextlen;
	} while (--cnt);
	ERROR(ENUMNTFD, namebuf);
}
