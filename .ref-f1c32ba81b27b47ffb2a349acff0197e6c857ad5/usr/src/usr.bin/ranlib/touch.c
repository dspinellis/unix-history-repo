/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Hugh Smith at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)touch.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <fcntl.h>
#include <dirent.h>
#include <ranlib.h>
#include <ar.h>
#include <time.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <archive.h>

extern CHDR chdr;			/* converted header */
extern char *archive;			/* archive name */

touch()
{
	int afd;

	afd = open_archive(O_RDWR);

	if (!get_arobj(afd) ||
	    strncmp(RANLIBMAG, chdr.name, sizeof(RANLIBMAG) - 1)) {
		(void)fprintf(stderr,
		    "ranlib: %s: no symbol table.\n", archive);
		return(1);
	}
	settime(afd);
	return(0);
}

settime(afd)
	int afd;
{
	struct ar_hdr *hdr;
	off_t size;
	char buf[50];

	size = SARMAG + sizeof(hdr->ar_name);
	if (lseek(afd, size, SEEK_SET) == (off_t)-1)
		error(archive);
	(void)sprintf(buf, "%-12ld", time((time_t *)NULL) + RANLIBSKEW);
	if (write(afd, buf, sizeof(hdr->ar_date)) != sizeof(hdr->ar_date))
		error(archive);
}
