/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)open.c	8.1 (Berkeley) %G%
 *  
 *
 * Copyright (c) 1989, 1990, 1991 Carnegie Mellon University
 * All Rights Reserved.
 *
 * Author: Alessandro Forin
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */

#include <stand/stand.h>
#include <stand/ufs.h>

/*
 *	File primitives proper
 */

struct fs_ops file_system[] = {
	{ ufs_open, ufs_close, ufs_read, ufs_write, ufs_seek, ufs_stat }
};
#define	NFSYS	(sizeof(file_system) / sizeof(struct fs_ops))

struct open_file files[SOPEN_MAX];

open(fname, mode)
	char *fname;
	int mode;
{
	register struct open_file *f;
	register int fd, i, error;
	char *file;

	/* find a free file descriptor */
	for (fd = 0, f = files; fd < SOPEN_MAX; fd++, f++)
		if (f->f_flags == 0)
			goto fnd;
	return (-1);
fnd:
	/*
	 * Try to open the device.
	 * Convert open mode (0,1,2) to F_READ, F_WRITE.
	 */
	f->f_flags = mode + 1;
	f->f_dev = (struct devsw *)0;
	file = (char *)0;
	error = devopen(f, fname, &file);
	if (error || f->f_dev == (struct devsw *)0)
		goto err;

	/* see if we opened a raw device; otherwise, 'file' is the file name. */
	if (file == (char *)0) {
		f->f_flags |= F_RAW;
		return (0);
	}

	/* pass file name to the different filesystem open routines */
	for (i = 0; i < NFSYS; i++) {
		/* convert mode (0,1,2) to FREAD, FWRITE. */
		error = (file_system[i].open)(file, f);
		if (error == 0) {
			f->f_ops = &file_system[i];
			return (fd);
		}
	}
	if (!error)
		error = ENOENT;

err:
	errno = error;
	return (-1);
}
