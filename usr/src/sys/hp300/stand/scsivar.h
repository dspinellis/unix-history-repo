/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory and the Systems
 * Programming Group of the University of Utah Computer Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scsivar.h	7.1 (Berkeley) %G%
 */

struct	scsi_softc {
	int	sc_ba;
	char	*sc_addr;
	char	sc_alive;
	char	sc_scsi_addr;
	char	sc_stat;
	char	sc_msg;
};

extern	struct scsi_softc scsi_softc[];
