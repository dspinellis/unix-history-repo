/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)srcfile.c 1.1 %G%";

/*
 * get the source file name associated with a given address
 */

#include "defs.h"
#include "mappings.h"
#include "object.h"
#include "filetab.h"

char *srcfilename(addr)
ADDRESS addr;
{
	register ADDRESS i, j, k;
	ADDRESS a;
	FILETAB *ftp;

	if (addr < filetab[0].addr) {
		return(NIL);
	}
	i = 0;
	j = nlhdr.nfiles - 1;
	while (i < j) {
		k = (i + j) / 2;
		ftp = &filetab[k];
		if ((a = ftp->addr) == addr) {
			return(ftp->filename);
		} else if (addr > a) {
			i = k + 1;
		} else {
			j = k - 1;
		}
	}
	if (addr >= filetab[i].addr) {
		return(filetab[i].filename);
	} else {
		return(filetab[i-1].filename);
	}
	/*NOTREACHED*/
}
