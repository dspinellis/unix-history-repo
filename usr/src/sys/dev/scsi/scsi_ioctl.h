/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scsi_ioctl.h	5.1 (Berkeley) %G%
 *
 * from: $Header: scsi_ioctl.h,v 1.2 92/05/15 11:24:02 torek Exp $ (LBL)
 */

/*
 * SCSI ioctls (`format' mode).
 *
 * Format mode allows a privileged process to issue direct SCSI commands
 * to a drive (it is intended primarily to allow on-line formatting).
 * SDIOCSFORMAT sets format mode (nonzero arg => on, zero arg => off).
 * When in format mode, only the process that issued the SDIOCSFORMAT
 * can read or write the drive.
 *
 * In format mode, the process is expected to
 *	- do SDIOCSCSICOMMAND to supply cdb for next SCSI op
 *	- do read or write as appropriate for cdb
 *	- if I/O error, optionally do SDIOCSENSE to get completion
 *	  status and sense data from last SCSI operation.
 */

struct scsi_fmt_sense {
	u_int	status;		/* completion status of last op */
	u_char	sense[28];	/* sense data (if any) from last op */
};

#define	SDIOCSFORMAT		_IOW('S', 1, int)
#define	SDIOCGFORMAT		_IOR('S', 2, int)
#define	SDIOCSCSICOMMAND	_IOW('S', 3, struct scsi_cdb)
#define	SDIOCSENSE		_IOR('S', 4, struct scsi_fmt_sense)
