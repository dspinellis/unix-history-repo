/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
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
 *	@(#)cyreg.h	7.8 (Berkeley) 6/28/90
 */

/*
 * Tapemaster controller definitions.
 */

/*
 * With 20-bit addressing, the intermediate buffer
 * must be allocated early in startup().
 */
#define	CYMAXIO	(64*1024)		/* max i/o size + 1 */
char	*cybuf;

/* for byte swapping Multibus values */
#define	htoms(x) (u_short)((((x)>>8)&0xff) | (((x)<<8)&0xff00))

#define	b_repcnt  b_bcount
#define	b_command b_resid

/*
 * System configuration pointer.
 * Memory address is jumpered on controller.
 */
struct	cyscp {
	char	csp_buswidth;	/* system bus width */
#define	CSP_16BITS	1	/* 16-bit system bus */
#define	CSP_8BITS	0	/* 8-bit system bus */
	char	csp_unused;
	u_char	csp_scb[4];	/* point to system config block */
};

/*
 * System configuration block
 */
struct	cyscb {
	char	csb_fixed;	/* fixed value code (must be 3) */
	char	csb_unused;	/* unused */
	u_char	csb_ccb[4];	/* pointer to channel control block */
};

#define	CSB_FIXED	0x3

/*
 * Channel control block definitions
 */
struct	cyccb {
	char	cbcw;		/* channel control word */
	char	cbgate;		/* tpb access gate */
	u_char	cbtpb[4];	/* first tape parameter block */
};

#define	GATE_OPEN	(char)(0x00)
#define	GATE_CLOSED	(char)(0xff)

#define	CY_GO(addr)	movob((addr), 0xff)	/* channel attention */
#define	CY_RESET(addr)  movob((addr)+1, 0xff) 	/* software controller reset */

#define	CBCW_IE		0x11		/* interrupt on cmd completion */
#define	CBCW_CLRINT	0x09		/* clear active interrupt */

/*
 * Tape parameter block definitions
 */
struct	cytpb {
	u_long	tpcmd;		/* command, see below */
	u_short	tpcontrol;	/* control word */
	u_short	tpcount;	/* return count */
	u_short	tpsize;		/* buffer size */
	u_short	tprec;		/* records/overrun */
	u_char	tpdata[4];	/* pointer to source/dest */
	u_short	tpstatus;	/* status */
	u_char	tplink[4];	/* pointer to next parameter block */
};

/* control field bit definitions */
#define	CYCW_UNIT	(0x000c<<8) 	/* unit select mask, 2 bit field */
#define	CYCW_IE		(0x0020<<8)	/* interrupt enable */
#define	CYCW_LOCK	(0x0080<<8)	/* bus lock flag */
#define	CYCW_REV	(0x0400>>8)	/* reverse flag */
#define	CYCW_SPEED	(0x0800>>8)	/* speed/density */
#define	    CYCW_25IPS	0
#define	    CYCW_100IPS	(0x0800>>8)
#define	CYCW_WIDTH  	(0x8000>>8)	/* width */
#define	    CYCW_8BITS	0
#define	    CYCW_16BITS	(0x8000>>8)

#define	CYCW_BITS	"\20\3REV\005100IPS\00716BITS\16IE\20LOCK"

/*
 * Controller commands
 */

/* control status/commands */
#define	CY_CONFIG	(0x00<<24)	/* configure */
#define	CY_NOP		(0x20<<24)	/* no operation */
#define	CY_SETPAGE	(0x08<<24)	/* set page (addr bits 20-23) */
#define	CY_SENSE	(0x28<<24)	/* drive status */
#define	CY_CLRINT	(0x9c<<24)	/* clear Multibus interrupt */

/* tape position commands */
#define	CY_REW		(0x34<<24)	/* rewind tape */
#define	CY_OFFL		(0x38<<24)	/* off_line and unload */
#define	CY_WEOF		(0x40<<24)	/* write end-of-file mark */
#define	CY_SFORW	(0x70<<24)	/* space record forward */
#define	CY_SREV		(CY_SFORW|CYCW_REV) /* space record backwards */
#define	CY_FSF		(0x44<<24)	/* space file forward */
#define	CY_BSF		(CY_FSF|CYCW_REV) /* space file backwards */
#define	CY_ERASE	(0x4c<<24)	/* erase record */

/* data transfer commands */
#define	CY_BRCOM	(0x10<<24)	/* read buffered */
#define	CY_BWCOM	(0x14<<24)	/* write buffered */
#define	CY_RCOM		(0x2c<<24)	/* read tape unbuffered */
#define	CY_WCOM		(0x30<<24)	/* write tape unbuffered */

/* status field bit definitions */
#define	CYS_WP		(0x0002<<8)	/* write protected, no write ring */
#define	CYS_BSY		(0x0004<<8)	/* formatter busy */
#define	CYS_RDY		(0x0008<<8)	/* drive ready */
#define	CYS_EOT		(0x0010<<8)	/* end of tape detected */
#define	CYS_BOT		(0x0020<<8)	/* tape is at load point */
#define	CYS_OL		(0x0040<<8)	/* drive on_line */
#define	CYS_FM		(0x0080<<8)	/* filemark detected */
#define	CYS_ERR		(0x1f00>>8)	/* error value mask */
#define	CYS_CR		(0x2000>>8)	/* controller executed retries */
#define	CYS_CC		(0x4000>>8)	/* command completed successfully */
#define	CYS_CE		(0x8000>>8)	/* command execution has begun */

#define	CYS_BITS "\20\6CR\7CC\10CE\12WP\13BSY\14RDY\15EOT/BOT\16BOT\17OL\20FM"

/* error codes for CYS_ERR */
#define	CYER_TIMOUT	0x01	/* timed out data busy false */
#define	CYER_TIMOUT1	0x02	/* data busy false,formatter,ready */
#define	CYER_TIMOUT2	0x03	/* time out ready busy false */
#define	CYER_TIMOUT3	0x04	/* time out ready busy true */
#define	CYER_TIMOUT4	0x05	/* time out data busy true */
#define	CYER_NXM	0x06	/* time out memory */
#define	CYER_BLANK	0x07	/* blank tape */
#define	CYER_DIAG	0x08	/* micro-diagnostic */
#define	CYER_EOT	0x09	/* EOT forward, BOT rev. */
#define	CYER_BOT	0x09	/* EOT forward, BOT rev. */
#define	CYER_HERR	0x0a	/* retry unsuccessful */
#define	CYER_FIFO	0x0b	/* FIFO over/under flow */
#define	CYER_PARITY	0x0d	/* drive to tapemaster parity error */
#define	CYER_CKSUM	0x0e	/* prom checksum */
#define	CYER_STROBE	0x0f	/* time out tape strobe */
#define	CYER_NOTRDY	0x10	/* tape not ready */
#define	CYER_PROT	0x11	/* write, no enable ring */
#define	CYER_JUMPER	0x13	/* missing diagnostic jumper */
#define	CYER_LINK	0x14	/* bad link, link inappropriate */
#define	CYER_FM		0x15	/* unexpected filemark */
#define	CYER_PARAM	0x16	/* bad parameter, byte count ? */
#define	CYER_HDWERR	0x18	/* unidentified hardware error */
#define	CYER_NOSTRM	0x19	/* streaming terminated */

#ifdef CYERROR
char	*cyerror[] = {
	"no error",
	"timeout",
	"timeout1",
	"timeout2",
	"timeout3",
	"timeout4", 
	"non-existent memory",
	"blank tape",
	"micro-diagnostic",
	"eot/bot detected",
	"retry unsuccessful",
	"fifo over/under-flow",
	"#0xc",
	"drive to controller parity error",
	"prom checksum",
	"time out tape strobe (record length error)",
	"tape not ready",
	"write protected",
	"#0x12",
	"missing diagnostic jumper",
	"invalid link pointer",
	"unexpected file mark",
	"invalid byte count/parameter",
	"#0x17",
	"unidentified hardware error",
	"streaming terminated"
};
#define	NCYERROR	(sizeof (cyerror) / sizeof (cyerror[0]))
#endif

/*
 * Masks defining hard and soft errors (must check against 1<<CYER_code).
 */
#define	CYMASK(e)	(1 << (e))
#define	CYER_HARD	(CYMASK(CYER_TIMOUT)|CYMASK(CYER_TIMOUT1)|\
	CYMASK(CYER_TIMOUT2)|CYMASK(CYER_TIMOUT3)|CYMASK(CYER_TIMOUT4)|\
	CYMASK(CYER_NXM)|CYMASK(CYER_DIAG)|CYMASK(CYER_JUMPER)|\
	CYMASK(CYER_STROBE)|CYMASK(CYER_PROT)|CYMASK(CYER_CKSUM)|\
	CYMASK(CYER_HERR)|CYMASK(CYER_BLANK))
#define	CYER_RSOFT	(CYMASK(CYER_FIFO)|CYMASK(CYER_NOTRDY)|\
	CYMASK(CYER_PARITY))
#define	CYER_WSOFT	(CYMASK(CYER_HERR)|CYMASK(CYER_FIFO)|\
	CYMASK(CYER_NOTRDY)|CYMASK(CYER_PARITY))
