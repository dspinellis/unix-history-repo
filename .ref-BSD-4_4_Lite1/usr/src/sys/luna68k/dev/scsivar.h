/*
 * Copyright (c) 1990, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)scsivar.h	8.1 (Berkeley) 6/10/93
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

struct	scsi_softc {
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
