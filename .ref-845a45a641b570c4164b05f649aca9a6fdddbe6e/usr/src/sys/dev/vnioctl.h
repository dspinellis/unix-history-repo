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
 *	@(#)vnioctl.h	7.3 (Berkeley) %G%
 */

/*
 * Ioctl definitions for file (vnode) disk pseudo-device.
 */

#define FDISKFILE	"/etc/fdisks"	/* default config file */

struct vn_ioctl {
	char	*vn_file;	/* pathname of file to mount */
	int	vn_size;	/* (returned) size of disk */
};

/*
 * Before you can use a unit, it must be configured with VNIOCSET.
 * The configuration persists across opens and closes of the device;
 * an VNIOCCLR must be used to reset a configuration.  An attempt to
 * VNIOCSET an already active unit will return EBUSY.
 */
#define VNIOCSET	_IOWR('F', 0, struct vn_ioctl)	/* enable disk */
#define VNIOCCLR	_IOW('F', 1, struct vn_ioctl)	/* disable disk */
