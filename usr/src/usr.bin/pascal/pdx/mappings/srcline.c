/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)srcline.c 1.1 %G%";

/*
 * lookup the source line number nearest from below to an address
 */

#include "defs.h"
#include "mappings.h"
#include "object.h"
#include "linetab.h"

LINENO srcline(addr)
ADDRESS addr;
{
	register ADDRESS i, j, k;
	ADDRESS a;

	if (nlhdr.nlines == 0) {
		return(0);
	}
	i = 0;
	j = nlhdr.nlines - 1;
	if (addr <= linetab[i].addr) {
		return(linetab[i].line);
	} else if (addr >= linetab[j].addr) {
		return(linetab[j].line);
	}
	while (i <= j) {
		k = (i + j) / 2;
		if ((a = linetab[k].addr) == addr) {
			return(linetab[k].line);
		} else if (addr > a) {
			i = k+1;
		} else {
			j = k-1;
		}
	}
	if (addr > linetab[i].addr) {
		return(linetab[i].line);
	} else {
		return(linetab[i-1].line);
	}
	/*NOTREACHED*/
}

/*
 * look for a line exactly corresponding to the given address
 */

LINENO linelookup(addr)
ADDRESS addr;
{
	register ADDRESS i, j, k;
	ADDRESS a;

	if (nlhdr.nlines == 0 || addr < linetab[0].addr) {
		return(0);
	}
	i = 0;
	j = nlhdr.nlines - 1;
	while (i <= j) {
		k = (i + j) / 2;
		if ((a = linetab[k].addr) == addr) {
			return(linetab[k].line);
		} else if (addr > a) {
			i = k+1;
		} else {
			j = k-1;
		}
	}
	return(0);
}
