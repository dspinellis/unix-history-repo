/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scsivar.h	7.1 (Berkeley) %G%
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
