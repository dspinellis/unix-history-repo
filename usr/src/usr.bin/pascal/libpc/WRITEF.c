/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)WRITEF.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

WRITEF(curfile, d1, d2, d3, d4, d5, d6)

	register struct iorec	*curfile;
	FILE			*d1;
	char			*d2;
	long			d3, d4, d5, d6;
{
	if (curfile->funit & FREAD) {
		ERROR(EWRITEIT, curfile->pfname);
		return;
	}
#	ifdef VAX
		fprintf(d1, d2, d3, d4, d5, d6);
#	else
		{
		register char	*cp;
		short		scnt;

		scnt = 0;
		for (cp = (char *)d2; *cp; )
			switch(*cp++) {
			case '*':
			case 's':
			case 'c':
				scnt++;
			}
		switch(scnt) {
		case 0:
			fprintf(d1, d2, d3, d4);
			break;
		case 1:
			fprintf(d1, d2, (int)d3, d4, d5);
			break;
		case 2:
			fprintf(d1, d2, (int)d3, (int)d4, d5, d6);
			break;
		default:
			fprintf(stderr, "Panic: bad argcount %d to WRITEF\n",
				scnt);
			ERROR(EWRITE, curfile->pfname);
			return;
		}
	}
#	endif VAX
	if (ferror(curfile->fbuf)) {
		ERROR(EWRITE, curfile->pfname);
		return;
	}
}
