/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scsivar.h	7.1 (Berkeley) %G%
 */

struct	scsi_softc {
	struct	hp_ctlr *sc_hc;
	u_char	*sc_buf;				/* Data Buffer Pointor*/
	u_char	*sc_cdb;				/* CDB Buffer Pointor */
	int	*sc_lock;				/* Lock Flag addres   */
	int	sc_flags;				/* SPC Status Flags   */
	int	sc_phase;				/* Current SCSI Phase */
	int	sc_target;				/* Current Target ID  */
	int	sc_len;					/* Buffer Length      */
	int	sc_cdblen;				/* CDB length         */
	u_char	sc_stat;
	u_char	sc_msg[7];
};


/* sc_lock  */

#define	SC_IN_PROGRESS		 0
#define SC_IO_COMPLETE		 1
#define	SC_DISCONNECTED		 2

#define	SC_IO_FAILED		-1
#define	SC_DEV_NOT_FOUND	-2

/* sc_flags */

#define	SC_SEL_TIMEOUT	0x00000001
