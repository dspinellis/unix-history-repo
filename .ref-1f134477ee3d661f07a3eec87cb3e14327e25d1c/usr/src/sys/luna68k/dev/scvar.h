/*
 * Copyright (c) 1990, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scvar.h	8.1 (Berkeley) %G%
 */


struct	scsi_queue {
	struct	scsi_queue *dq_forw;
	struct	scsi_queue *dq_back;
	int	dq_ctlr;
	int	dq_unit;
	int	dq_slave;
	struct	driver *dq_driver;
	int	dq_flags;
	int	dq_imax;
	int	dq_imin;
	int	dq_omax;
	int	dq_omin;
	struct	scsi_fmt_cdb *dq_cdb;
	struct	buf *dq_bp;
	u_char	*dq_xferp;				/* Current Pointor */
	int	dq_xfercnt;				/* Data Counter    */
};

/* dq_flags */

#define	DQ_DISCONNECT	0x00000001

struct	sc_softc {
	struct	hp_ctlr *sc_hc;
	struct	scsi_queue sc_sq;
	struct	scsi_queue sc_wq;
	u_char	*sc_cdb;				/* CDB Buffer Pointor */
	u_char	*sc_buf;				/* Data Buffer Pointor*/
	int	*sc_lock;				/* Lock Flag addres   */
	int	sc_flags;				/* SPC Status Flags   */
	int	sc_phase;				/* Current SCSI Phase */
	int	sc_cdblen;				/* CDB length         */
	int	sc_len;					/* Buffer Length      */
	u_char	sc_stat;
	u_char	sc_msg[7];
};


/* sc_lock  */

#define	SC_IN_PROGRESS		 0
#define SC_IO_COMPLETE		 1
#define	SC_DISCONNECTED		 2

#define SC_BUSY			-1
#define	SC_IO_FAILED		-2
#define	SC_DEV_NOT_FOUND	-3
#define	SC_IO_TIMEOUT		-4

/* sc_flags */

#define	SC_SEL_TIMEOUT	0x00000001
