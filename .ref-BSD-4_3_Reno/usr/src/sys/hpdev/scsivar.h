/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)scsivar.h	7.1 (Berkeley) 5/8/90
 */

struct	scsi_softc {
	struct	hp_ctlr *sc_hc;
	struct	devqueue sc_dq;
	struct	devqueue sc_sq;
	u_char	sc_flags;
	u_char	sc_sync;
	u_char	sc_scsi_addr;
	u_char	sc_stat[2];
	u_char	sc_msg[7];
};

/* sc_flags */
#define	SCSI_IO		0x80	/* DMA I/O in progress */
#define	SCSI_DMA32	0x40	/* 32-bit DMA should be used */
#define	SCSI_ALIVE	0x01	/* controller initialized */
#ifdef DEBUG
#define	SCSI_PAD	0x02	/* 'padded' transfer in progress */
#endif
