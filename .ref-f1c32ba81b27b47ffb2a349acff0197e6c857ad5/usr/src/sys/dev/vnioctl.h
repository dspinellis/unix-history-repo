/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: fdioctl.h 1.1 90/07/09$
 *
 *	@(#)vnioctl.h	7.2 (Berkeley) %G%
 */

/*
 * Ioctl definitions for file (vnode) disk pseudo-device.
 */

#define FDISKFILE	"/etc/fdisks"	/* default config file */

struct fd_ioctl {
	char	*fd_file;	/* pathname of file to mount */
	int	fd_size;	/* (returned) size of disk */
};

/*
 * Before you can use a unit, it must be configured with FDIOCSET.
 * The configuration persists across opens and closes of the device;
 * an FDIOCCLR must be used to reset a configuration.  An attempt to
 * FDIOCSET an already active unit will return EBUSY.
 */
#define FDIOCSET	_IOWR('F', 0, struct fd_ioctl)	/* enable disk */
#define FDIOCCLR	_IOW('F', 1, struct fd_ioctl)	/* disable disk */
