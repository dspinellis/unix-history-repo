/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)objaddr.c 1.1 %G%";

/*
 * lookup the object address of a given line from the given file
 */

#include "defs.h"
#include "mappings.h"
#include "object.h"
#include "source.h"
#include "filetab.h"
#include "linetab.h"

ADDRESS objaddr(line, name)
LINENO line;
char *name;
{
	register FILETAB *ftp;
	register LINENO i, j;

	if (nlhdr.nlines == 0) {
		return(-1);
	}
	if (name == NULL) {
		name = cursource;
	}
	for (ftp = &filetab[0]; ftp < &filetab[nlhdr.nfiles]; ftp++) {
		if (strcmp(ftp->filename, name) == 0) {
			break;
		}
	}
	if (ftp == &filetab[nlhdr.nfiles]) {
		error("unknown source file \"%s\"", name);
	}
	i = ftp->lineindex;
	if (ftp == &filetab[nlhdr.nfiles-1]) {
		j = nlhdr.nlines;
	} else {
		j = (ftp + 1)->lineindex;
	}
	while (i < j) {
		if (linetab[i].line == line) {
			return(linetab[i].addr);
		}
		i++;
	}
	return(-1);
}
