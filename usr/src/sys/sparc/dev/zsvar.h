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
 *	@(#)zsvar.h	7.1 (Berkeley) %G%
 *
 * from: $Header: zsvar.h,v 1.4 92/06/17 05:35:54 torek Exp $ (LBL)
 */

/*
 * Software state, per zs channel.
 *
 * The receive ring size and type are carefully chosen to make the
 * zs hardware interrupt handler go fast.  We need 8 bits for the
 * received character and 8 bits for the corresponding RR1 status.
 * The character is known to be in the upper byte of the pair.
 */
#define ZLRB_RING_SIZE 256
#define	ZLRB_RING_MASK 255

struct zs_chanstate {
	struct	zs_chanstate *cs_next;	/* linked list for zshard() */
	volatile struct zschan *cs_zc;	/* points to hardware regs */
	int	cs_unit;		/* unit number */
	struct	tty *cs_ttyp;		/* ### */

	/*
	 * We must keep a copy of the write registers as they are
	 * mostly write-only and we sometimes need to set and clear
	 * individual bits (e.g., in WR3).  Not all of these are
	 * needed but 16 bytes is cheap and this makes the addressing
	 * simpler.  Unfortunately, we can only write to some registers
	 * when the chip is not actually transmitting, so whenever
	 * we are expecting a `transmit done' interrupt the wreg array
	 * is allowed to `get ahead' of the current values.  In a
	 * few places we must change the current value of a register,
	 * rather than (or in addition to) the pending value; for these
	 * cs_creg[] contains the current value.
	 */
	u_char	cs_creg[16];		/* current values */
	u_char	cs_preg[16];		/* pending values */
	u_char	cs_heldchange;		/* change pending (creg != preg) */

	/*
	 * The transmit byte count and address are used for pseudo-DMA
	 * output in the hardware interrupt code.  PDMA can be suspended
	 * to get pending changes done; heldtbc is used for this.  It can
	 * also be stopped for ^S; this sets TS_TTSTOP in tp->t_state.
	 */
	int	cs_tbc;			/* transmit byte count */
	caddr_t	cs_tba;			/* transmit buffer address */
	int	cs_heldtbc;		/* held tbc while xmission stopped */

	/*
	 * Printing an overrun error message often takes long enough to
	 * cause another overrun, so we only print one per second.
	 */
	long	cs_rotime;		/* time of last ring overrun */
	long	cs_fotime;		/* time of last fifo overrun */

	/* pure software data, per channel */
	int	cs_speed;		/* default baud rate (from ROM) */
	char	cs_softcar;		/* software carrier */
	char	cs_conk;		/* is console keyboard, decode L1-A */
	char	cs_brkabort;		/* abort (as if via L1-A) on BREAK */
	char	cs_kgdb;		/* enter debugger on frame char */
	char	cs_consio;		/* port does /dev/console I/O */

	/*
	 * Status change interrupts merely copy the new status and
	 * schedule a software interrupt to deal with it.  To make
	 * checking easier, cs_rr0 is guaranteed nonzero on status
	 * changes.  cs_txint indicates a software transmit interrupt
	 * (a txint where cs_tbc was 0).  A software receive interrupt
	 * is implicit in cs_rbget != cs_rbput.
	 */
	u_char	cs_txint;		/* software tx interrupt */
	u_short	cs_rr0;			/* rr0 | 0x100, after change */
	u_int	cs_rbget;		/* receive ring buffer `get' index */
	volatile u_int cs_rbput;	/* receive ring buffer `put' index */
	u_short	cs_rbuf[ZLRB_RING_SIZE];/* packed data: (char << 8) + rr1 */
};

/*
 * Macros to read and write individual registers (except 0) in a channel.
 *
 * On the SparcStation the 1.6 microsecond recovery time is
 * handled in hardware.
 */
#define	ZS_READ(c, r)		((c)->zc_csr = (r), (c)->zc_csr)
#define	ZS_WRITE(c, r, v)	((c)->zc_csr = (r), (c)->zc_csr = (v))
