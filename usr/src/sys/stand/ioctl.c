/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ioctl.c	8.1 (Berkeley) %G%
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

ioctl(fd, cmd, arg)
	int fd;
	int cmd;
	char *arg;
{
	register struct open_file *f = &files[fd];

	if ((unsigned)fd >= SOPEN_MAX || f->f_flags == 0) {
		errno = EBADF;
		return (-1);
	}
	if (f->f_flags & F_RAW) {
		errno = (f->f_dev->dv_ioctl)(f, cmd, arg);
		if (errno)
			return (-1);
		return (0);
	}
	return (-1);
}
