/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: scu.h,v 4.300 91/07/02 16:38:38 root Rel41 $ SONY
 *
 *	@(#)scu.h	7.2 (Berkeley) %G%
 */

/*
 * screg.h
 */

#ifndef __SCU__
#define __SCU__ 1

#include <sys/ioctl.h>

#define	RAWSCSI_USE_PIO		0
#define	RAWSCSI_USE_DMA		1

#define	SCSIIOCCMD		_IOWR('S', 0, struct sc_ureq)
#define	SCSIIOCGTIMEO		_IOR('S', 1, int)
#define	SCSIIOCSTIMEO		_IOW('S', 2, int)

/*
 *	scsi user request parameter block
 */
struct sc_ureq {
/*00*/	u_int	scu_istatus;
/*04*/	u_int	scu_tstatus;
/*08*/	u_int	scu_identify;
/*0c*/	u_int	scu_message;
/*10*/	u_int	scu_bytesec;
/*14*/	u_char	scu_cdb[12];
/*20*/	u_char	scu_param[20];
/*34*/	u_char	*scu_addr;
/*38*/	u_int	scu_count;
/*3c*/
};

#endif /* !__SCU__ */
