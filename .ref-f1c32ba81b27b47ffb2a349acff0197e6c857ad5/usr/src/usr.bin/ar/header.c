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
static char sccsid[] = "@(#)header.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <dirent.h>
#include <stdio.h>
#include <ar.h>
#include "archive.h"

typedef struct ar_hdr HDR;
extern CHDR chdr;			/* converted header */
static char hb[sizeof(HDR) + 1];	/* real header */
extern char *archive;			/* archive name */

/* Convert ar header field to an integer. */
#define	AR_ATOI(from, to, len, base) { \
	bcopy(from, buf, len); \
	buf[len] = '\0'; \
	to = strtol(buf, (char **)NULL, base); \
}

/*
 * get_header --
 *	read the archive header for this member
 */
get_header(fd)
	int fd;
{
	struct ar_hdr *hdr;
	register int len, nr;
	register char *p, buf[20];

	nr = read(fd, hb, sizeof(HDR));
	if (nr != sizeof(HDR)) {
		if (!nr)
			return(0);
		if (nr < 0)
			error(archive);
		badfmt();
	}

	hdr = (struct ar_hdr *)hb;
	if (strncmp(hdr->ar_fmag, ARFMAG, sizeof(ARFMAG) - 1))
		badfmt();

	/* Convert the header into the internal format. */
#define	DECIMAL	10
#define	OCTAL	 8

	AR_ATOI(hdr->ar_date, chdr.date, sizeof(hdr->ar_date), DECIMAL);
	AR_ATOI(hdr->ar_uid, chdr.uid, sizeof(hdr->ar_uid), DECIMAL);
	AR_ATOI(hdr->ar_gid, chdr.gid, sizeof(hdr->ar_gid), DECIMAL);
	AR_ATOI(hdr->ar_mode, chdr.mode, sizeof(hdr->ar_mode), OCTAL);
	AR_ATOI(hdr->ar_size, chdr.size, sizeof(hdr->ar_size), DECIMAL);

	/* Leading spaces should never happen. */
	if (hdr->ar_name[0] == ' ')
		badfmt();

	/*
	 * Long name support.  Set the "real" size of the file, and the
	 * long name flag/size.
	 */
	if (!bcmp(hdr->ar_name, AR_EFMT1, sizeof(AR_EFMT1) - 1)) {
		len = atoi(hdr->ar_name + sizeof(AR_EFMT1) - 1);
		if (len <= 0 || len > MAXNAMLEN)
			badfmt();
		nr = read(fd, chdr.name, len);
		if (nr != len) {
			if (nr < 0)
				error(archive);
			badfmt();
		}
		chdr.name[len] = 0;
		chdr.size -= (chdr.lname = len);
	} else {
		bcopy(hdr->ar_name, chdr.name, sizeof(hdr->ar_name));

		/* Only strip off trailing spaces. */
		for (p = chdr.name + sizeof(hdr->ar_name) - 1; *p == ' '; --p);
		*++p = '\0';
		chdr.lname = 0;
	}
	return(1);
}

/*
 * put_header --
 *	Write the archive member header to a file.
 */
put_header(cfp, sb)
	CF *cfp;
	struct stat *sb;
{
	register int lname;
	register char *name;
	struct ar_hdr *hdr;
	char *rname();

	/*
	 * If passed an sb structure, reading a file from disk.  Get stat(2)
	 * information, build a name and construct a header.  (Files are named
	 * by their last component in the archive.)  If not, then just write
	 * the last header read.
	 */
	if (sb) {
		name = rname(cfp->rname);
		(void)fstat(cfp->rfd, sb);

		if ((lname = strlen(name)) > sizeof(hdr->ar_name) ||
		    index(name, ' ')) {
			(void)sprintf(hb, HDR1, AR_EFMT1, lname, sb->st_mtime,
			    sb->st_uid, sb->st_gid, sb->st_mode,
			    sb->st_size + lname, ARFMAG);
		} else {
			lname = 0;
			(void)sprintf(hb, HDR2, name, sb->st_mtime, sb->st_uid,
			    sb->st_gid, sb->st_mode, sb->st_size, ARFMAG);
		}
	} else {
		lname = chdr.lname;
		name = chdr.name;
	}

	if (write(cfp->wfd, hb, sizeof(HDR)) != sizeof(HDR) ||
	    lname && write(cfp->wfd, name, lname) != lname)
		error(cfp->wname);
}
