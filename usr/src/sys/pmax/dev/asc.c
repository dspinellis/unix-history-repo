/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)asc.c	7.8 (Berkeley) %G%
 */

/* 
 * Mach Operating System
 * Copyright (c) 1991,1990,1989 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS 
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */

/*
 * HISTORY
 * $Log:	scsi_53C94_hdw.c,v $
 * Revision 2.5  91/02/05  17:45:07  mrt
 * 	Added author notices
 * 	[91/02/04  11:18:43  mrt]
 * 
 * 	Changed to use new Mach copyright
 * 	[91/02/02  12:17:20  mrt]
 * 
 * Revision 2.4  91/01/08  15:48:24  rpd
 * 	Added continuation argument to thread_block.
 * 	[90/12/27            rpd]
 * 
 * Revision 2.3  90/12/05  23:34:48  af
 * 	Recovered from pmax merge.. and from the destruction of a disk.
 * 	[90/12/03  23:40:40  af]
 * 
 * Revision 2.1.1.1  90/11/01  03:39:09  af
 * 	Created, from the DEC specs:
 * 	"PMAZ-AA TURBOchannel SCSI Module Functional Specification"
 * 	Workstation Systems Engineering, Palo Alto, CA. Aug 27, 1990.
 * 	And from the NCR data sheets
 * 	"NCR 53C94, 53C95, 53C96 Advances SCSI Controller"
 * 	[90/09/03            af]
 */

/*
 *	File: scsi_53C94_hdw.h
 * 	Author: Alessandro Forin, Carnegie Mellon University
 *	Date:	9/90
 *
 *	Bottom layer of the SCSI driver: chip-dependent functions
 *
 *	This file contains the code that is specific to the NCR 53C94
 *	SCSI chip (Host Bus Adapter in SCSI parlance): probing, start
 *	operation, and interrupt routine.
 */

/*
 * This layer works based on small simple 'scripts' that are installed
 * at the start of the command and drive the chip to completion.
 * The idea comes from the specs of the NCR 53C700 'script' processor.
 *
 * There are various reasons for this, mainly
 * - Performance: identify the common (successful) path, and follow it;
 *   at interrupt time no code is needed to find the current status
 * - Code size: it should be easy to compact common operations
 * - Adaptability: the code skeleton should adapt to different chips without
 *   terrible complications.
 * - Error handling: and it is easy to modify the actions performed
 *   by the scripts to cope with strange but well identified sequences
 *
 */

#include <asc.h>
#if NASC > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/dkstat.h>
#include <sys/buf.h>
#include <sys/conf.h>
#include <sys/errno.h>

#include <machine/machConst.h>

#include <pmax/dev/device.h>
#include <pmax/dev/scsi.h>
#include <pmax/dev/ascreg.h>

#include <pmax/pmax/asic.h>
#include <pmax/pmax/kmin.h>
#include <pmax/pmax/pmaxtype.h>

#define ASC_OFFSET_53C94	0x0		/* from module base */
#define ASC_OFFSET_DMAR		0x40000		/* DMA Address Register */
#define ASC_OFFSET_RAM		0x80000		/* SRAM Buffer */
#define ASC_OFFSET_ROM		0xc0000		/* Diagnostic ROM */

#define	ASC_RAM_SIZE		0x20000		/* 128k (32k*32) */

#define	readback(a)	{ register int foo; foo = (a); }
/*
 * DMA Address Register
 */
#define ASC_DMAR_MASK		0x1ffff		/* 17 bits, 128k */
#define ASC_DMAR_WRITE		0x80000000	/* DMA direction bit */
#define	ASC_DMA_ADDR(x)		((unsigned)(x) & ASC_DMAR_MASK)

/*
 * Synch xfer parameters, and timing conversions
 */
#define SCSI_MIN_PERIOD		50	/* in 4 nsecs units */
#define ASC_MIN_PERIOD25	5	/* in CLKS/BYTE, 1 CLK = 40nsecs */
#define ASC_MIN_PERIOD12	3	/* in CLKS/BYTE, 1 CLK = 80nsecs */
#define ASC_MAX_PERIOD25	35	/* in CLKS/BYTE, 1 CLK = 40nsecs */
#define ASC_MAX_PERIOD12	18	/* in CLKS/BYTE, 1 CLK = 80nsecs */
#define ASC_MAX_OFFSET		15	/* pure number */

extern int pmax_boardtype;

/*
 * In 4ns ticks.
 */
int	asc_to_scsi_period[] = {
	32,
	33,
	34,
	35,
	5,
	5,
	6,
	7,
	8,
	9,
	10,
	11,
	12,
	13,
	14,
	15,
	16,
	17,
	18,
	19,
	20,
	21,
	22,
	23,
	24,
	25,
	26,
	27,
	28,
	29,
	30,
	31,
};

/*
 * Internal forward declarations.
 */
static void asc_reset();
static void asc_startcmd();

#ifdef DEBUG
int	asc_debug = 1;
int	asc_debug_cmd;
int	asc_debug_bn;
int	asc_debug_sz;
#define NLOG 16
struct asc_log {
	u_int	status;
	u_char	state;
	u_char	msg;
	int	target;
} asc_log[NLOG], *asc_logp = asc_log;
#define PACK(unit, status, ss, ir) \
	((unit << 24) | (status << 16) | (ss << 8) | ir)
#endif

/*
 * Scripts are entries in a state machine table.
 * A script has four parts: a pre-condition, an action, a command to the chip,
 * and an index into asc_scripts for the next state. The first triggers error
 * handling if not satisfied and in our case it is formed by the
 * values of the interrupt register and status register, this
 * basically captures the phase of the bus and the TC and BS
 * bits.  The action part is just a function pointer, and the
 * command is what the 53C94 should be told to do at the end
 * of the action processing.  This command is only issued and the
 * script proceeds if the action routine returns TRUE.
 * See asc_intr() for how and where this is all done.
 */
typedef struct script {
	int		condition;	/* expected state at interrupt time */
	int		(*action)();	/* extra operations */
	int		command;	/* command to the chip */
	struct script	*next;		/* index into asc_scripts for next state */
} script_t;

/* Matching on the condition value */
#define	SCRIPT_MATCH(ir, csr)		((ir) | (ASC_PHASE(csr) << 8))

/* forward decls of script actions */
static int script_nop();		/* when nothing needed */
static int asc_end();			/* all come to an end */
static int asc_get_status();		/* get status from target */
static int asc_dma_in();		/* start reading data from target */
static int asc_last_dma_in();		/* cleanup after all data is read */
static int asc_resume_in();		/* resume data in after a message */
static int asc_resume_dma_in();		/* resume DMA after a disconnect */
static int asc_dma_out();		/* send data to target via dma */
static int asc_last_dma_out();		/* cleanup after all data is written */
static int asc_resume_out();		/* resume data out after a message */
static int asc_resume_dma_out();	/* resume DMA after a disconnect */
static int asc_sendsync();		/* negotiate sync xfer */
static int asc_replysync();		/* negotiate sync xfer */
static int asc_msg_in();		/* process a message byte */
static int asc_disconnect();		/* process an expected disconnect */

/* Define the index into asc_scripts for various state transitions */
#define	SCRIPT_DATA_IN		0
#define	SCRIPT_CONTINUE_IN	2
#define	SCRIPT_DATA_OUT		3
#define	SCRIPT_CONTINUE_OUT	5
#define	SCRIPT_SIMPLE		6
#define	SCRIPT_GET_STATUS	7
#define	SCRIPT_MSG_IN		9
#define	SCRIPT_REPLY_SYNC	11
#define	SCRIPT_TRY_SYNC		12
#define	SCRIPT_DISCONNECT	15
#define	SCRIPT_RESEL		16
#define	SCRIPT_RESUME_IN	17
#define	SCRIPT_RESUME_DMA_IN	18
#define	SCRIPT_RESUME_OUT	19
#define	SCRIPT_RESUME_DMA_OUT	20
#define	SCRIPT_RESUME_NO_DATA	21

/*
 * Scripts
 */
script_t asc_scripts[] = {
	/* start data in */
	{SCRIPT_MATCH(ASC_INT_FC | ASC_INT_BS, ASC_PHASE_DATAI),	/*  0 */
		asc_dma_in, ASC_CMD_XFER_INFO | ASC_CMD_DMA,
		&asc_scripts[SCRIPT_DATA_IN + 1]},
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_STATUS),			/*  1 */
		asc_last_dma_in, ASC_CMD_I_COMPLETE,
		&asc_scripts[SCRIPT_GET_STATUS]},

	/* continue data in after a chuck is finished */
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_DATAI),			/*  2 */
		asc_dma_in, ASC_CMD_XFER_INFO | ASC_CMD_DMA,
		&asc_scripts[SCRIPT_DATA_IN + 1]},

	/* start data out */
	{SCRIPT_MATCH(ASC_INT_FC | ASC_INT_BS, ASC_PHASE_DATAO),	/*  3 */
		asc_dma_out, ASC_CMD_XFER_INFO | ASC_CMD_DMA,
		&asc_scripts[SCRIPT_DATA_OUT + 1]},
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_STATUS),			/*  4 */
		asc_last_dma_out, ASC_CMD_I_COMPLETE,
		&asc_scripts[SCRIPT_GET_STATUS]},

	/* continue data out after a chuck is finished */
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_DATAO),			/*  5 */
		asc_dma_out, ASC_CMD_XFER_INFO | ASC_CMD_DMA,
		&asc_scripts[SCRIPT_DATA_OUT + 1]},

	/* simple command with no data transfer */
	{SCRIPT_MATCH(ASC_INT_FC | ASC_INT_BS, ASC_PHASE_STATUS),	/*  6 */
		script_nop, ASC_CMD_I_COMPLETE,
		&asc_scripts[SCRIPT_GET_STATUS]},

	/* get status and finish command */
	{SCRIPT_MATCH(ASC_INT_FC, ASC_PHASE_MSG_IN),			/*  7 */
		asc_get_status, ASC_CMD_MSG_ACPT,
		&asc_scripts[SCRIPT_GET_STATUS + 1]},
	{SCRIPT_MATCH(ASC_INT_DISC, 0),					/*  8 */
		asc_end, ASC_CMD_NOP,
		&asc_scripts[SCRIPT_GET_STATUS + 1]},

	/* message in */
	{SCRIPT_MATCH(ASC_INT_FC, ASC_PHASE_MSG_IN),			/*  9 */
		asc_msg_in, ASC_CMD_MSG_ACPT,
		&asc_scripts[SCRIPT_MSG_IN + 1]},
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_MSG_IN),			/* 10 */
		script_nop, ASC_CMD_XFER_INFO,
		&asc_scripts[SCRIPT_MSG_IN]},

	/* send synchonous negotiation reply */
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_MSG_OUT),			/* 11 */
		asc_replysync, ASC_CMD_XFER_INFO,
		&asc_scripts[SCRIPT_REPLY_SYNC]},

	/* try to negotiate synchonous transfer parameters */
	{SCRIPT_MATCH(ASC_INT_FC | ASC_INT_BS, ASC_PHASE_MSG_OUT),	/* 12 */
		asc_sendsync, ASC_CMD_XFER_INFO,
		&asc_scripts[SCRIPT_TRY_SYNC + 1]},
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_MSG_IN),			/* 13 */
		script_nop, ASC_CMD_XFER_INFO,
		&asc_scripts[SCRIPT_MSG_IN]},
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_COMMAND),			/* 14 */
		script_nop, ASC_CMD_XFER_INFO | ASC_CMD_DMA,
		&asc_scripts[SCRIPT_RESUME_NO_DATA]},

	/* handle a disconnect */
	{SCRIPT_MATCH(ASC_INT_DISC, ASC_PHASE_DATAO),			/* 15 */
		asc_disconnect, ASC_CMD_ENABLE_SEL,
		&asc_scripts[SCRIPT_RESEL]},

	/* reselect sequence: this is just a placeholder so match fails */
	{SCRIPT_MATCH(0, ASC_PHASE_MSG_IN),				/* 16 */
		script_nop, ASC_CMD_MSG_ACPT,
		&asc_scripts[SCRIPT_RESEL]},

	/* resume data in after a message */
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_DATAI),			/* 17 */
		asc_resume_in, ASC_CMD_XFER_INFO | ASC_CMD_DMA,
		&asc_scripts[SCRIPT_DATA_IN + 1]},

	/* resume partial DMA data in after a message */
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_DATAI),			/* 18 */
		asc_resume_dma_in, ASC_CMD_XFER_INFO | ASC_CMD_DMA,
		&asc_scripts[SCRIPT_DATA_IN + 1]},

	/* resume data out after a message */
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_DATAO),			/* 19 */
		asc_resume_out, ASC_CMD_XFER_INFO | ASC_CMD_DMA,
		&asc_scripts[SCRIPT_DATA_OUT + 1]},

	/* resume partial DMA data out after a message */
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_DATAO),			/* 20 */
		asc_resume_dma_out, ASC_CMD_XFER_INFO | ASC_CMD_DMA,
		&asc_scripts[SCRIPT_DATA_OUT + 1]},

	/* resume after a message when there is no more data */
	{SCRIPT_MATCH(ASC_INT_BS, ASC_PHASE_STATUS),			/* 21 */
		script_nop, ASC_CMD_I_COMPLETE,
		&asc_scripts[SCRIPT_GET_STATUS]},
};

/*
 * State kept for each active SCSI device.
 */
typedef struct scsi_state {
	script_t *script;	/* saved script while processing error */
	int	statusByte;	/* status byte returned during STATUS_PHASE */
	int	error;		/* errno to pass back to device driver */
	u_char	*dmaBufAddr;	/* DMA buffer address */
	u_int	dmaBufSize;	/* DMA buffer size */
	int	dmalen;		/* amount to transfer in this chunk */
	int	dmaresid;	/* amount not transfered if chunk suspended */
	int	buflen;		/* total remaining amount of data to transfer */
	char	*buf;		/* current pointer within scsicmd->buf */
	int	flags;		/* see below */
	int	msglen;		/* number of message bytes to read */
	int	msgcnt;		/* number of message bytes received */
	u_char	sync_period;	/* DMA synchronous period */
	u_char	sync_offset;	/* DMA synchronous xfer offset or 0 if async */
	u_char	msg_out;	/* next MSG_OUT byte to send */
	u_char	msg_in[16];	/* buffer for multibyte messages */
} State;

/* state flags */
#define DISCONN		0x01	/* true if currently disconnected from bus */
#define DMA_IN_PROGRESS	0x02	/* true if data DMA started */
#define DMA_IN		0x04	/* true if reading from SCSI device */
#define DMA_OUT		0x10	/* true if writing to SCSI device */
#define DID_SYNC	0x20	/* true if synchronous offset was negotiated */
#define TRY_SYNC	0x40	/* true if try neg. synchronous offset */

#define ASC_NCMD	7
/*
 * State kept for each active SCSI host interface (53C94).
 */
struct asc_softc {
	asc_regmap_t	*regs;		/* chip address */
	volatile int	*dmar;		/* DMA address register address */
	u_char		*buff;		/* RAM buffer address (uncached) */
	int		myid;		/* SCSI ID of this interface */
	int		myidmask;	/* ~(1 << myid) */
	int		state;		/* current SCSI connection state */
	int		target;		/* target SCSI ID if busy */
	script_t	*script;	/* next expected interrupt & action */
	ScsiCmd		*cmd[ASC_NCMD];	/* active command indexed by SCSI ID */
	State		st[ASC_NCMD];	/* state info for each active command */
	void		(*dma_start)();	/* Start dma routine */
	void		(*dma_end)();	/* End dma routine */
	u_char		*dma_next;
	int		dma_xfer;	/* Dma len still to go */
	int		min_period;	/* Min transfer period clk/byte */
	int		max_period;	/* Max transfer period clk/byte */
	int		ccf;		/* CCF, whatever that really is? */
	int		timeout_250;	/* 250ms timeout */
	int		tb_ticks;	/* 4ns. ticks/tb channel ticks */
} asc_softc[NASC];

#define	ASC_STATE_IDLE		0	/* idle state */
#define	ASC_STATE_BUSY		1	/* selecting or currently connected */
#define ASC_STATE_TARGET	2	/* currently selected as target */
#define ASC_STATE_RESEL		3	/* currently waiting for reselect */

typedef struct asc_softc *asc_softc_t;

/*
 * Dma operations.
 */
#define	ASCDMA_READ	1
#define	ASCDMA_WRITE	2
static void tb_dma_start(), tb_dma_end(), asic_dma_start(), asic_dma_end();
u_long asc_iobuf[33792];

/*
 * Definition of the controller for the auto-configuration program.
 */
int	asc_probe();
void	asc_start();
void	asc_intr();
struct	driver ascdriver = {
	"asc", asc_probe, asc_start, 0, asc_intr,
};

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
asc_probe(cp)
	register struct pmax_ctlr *cp;
{
	register asc_softc_t asc;
	register asc_regmap_t *regs;
	int unit, id, s, i;
	u_int bufadr;

	if ((unit = cp->pmax_unit) >= NASC)
		return (0);
	if (badaddr(cp->pmax_addr + ASC_OFFSET_53C94, 1))
		return (0);
	asc = &asc_softc[unit];

	/*
	 * Initialize hw descriptor, cache some pointers
	 */
	asc->regs = (asc_regmap_t *)(cp->pmax_addr + ASC_OFFSET_53C94);

	/*
	 * Set up machine dependencies.
	 * 1) how to do dma
	 * 2) timing based on turbochannel frequency
	 */
	switch (pmax_boardtype) {
	case DS_3MIN:
	case DS_MAXINE:
	    if (unit == 0) {
		bufadr = MACH_PHYS_TO_UNCACHED(MACH_CACHED_TO_PHYS(asc_iobuf));
		bufadr = (bufadr + NBPG - 1) & ~(NBPG - 1);
		asc->buff = (u_char *)bufadr;
		*((volatile int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_DMAPTR))
			= -1;
		*((volatile int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_DMANPTR))
			= -1;
		*((volatile int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_SCR)) = 0;
		asc->dma_start = asic_dma_start;
		asc->dma_end = asic_dma_end;
		break;
	    }
	    /*
	     * Fall through for turbochannel option.
	     */
	case DS_3MAX:
	default:
	    asc->dmar = (volatile int *)(cp->pmax_addr + ASC_OFFSET_DMAR);
	    asc->buff = (u_char *)(cp->pmax_addr + ASC_OFFSET_RAM);
	    asc->dma_start = tb_dma_start;
	    asc->dma_end = tb_dma_end;
	};
	/*
	 * Now for timing. The 3max has a 25Mhz tb whereas the 3min and
	 * maxine are 12.5Mhz.
	 */
	switch (pmax_boardtype) {
	case DS_3MAX:
		asc->min_period = ASC_MIN_PERIOD25;
		asc->max_period = ASC_MAX_PERIOD25;
		asc->ccf = ASC_CCF(25);
		asc->timeout_250 = ASC_TIMEOUT_250(25, asc->ccf);
		asc->tb_ticks = 10;
		break;
	case DS_3MIN:
	case DS_MAXINE:
	default:
		asc->min_period = ASC_MIN_PERIOD12;
		asc->max_period = ASC_MAX_PERIOD12;
		asc->ccf = ASC_CCF(13);
		asc->timeout_250 = ASC_TIMEOUT_250(13, asc->ccf);
		asc->tb_ticks = 20;
		break;
	};

	asc->state = ASC_STATE_IDLE;
	asc->target = -1;

	regs = asc->regs;

	/*
	 * Reset chip, fully.  Note that interrupts are already enabled.
	 */
	s = splbio();

	/* preserve our ID for now */
	asc->myid = regs->asc_cnfg1 & ASC_CNFG1_MY_BUS_ID;
	asc->myidmask = ~(1 << asc->myid);

	asc_reset(asc, regs);

	/*
	 * Our SCSI id on the bus.
	 * The user can set this via the prom on 3maxen/pmaxen.
	 * If this changes it is easy to fix: make a default that
	 * can be changed as boot arg.
	 */
#ifdef	unneeded
	regs->asc_cnfg1 = (regs->asc_cnfg1 & ~ASC_CNFG1_MY_BUS_ID) |
			      (scsi_initiator_id[unit] & 0x7);
#endif
	id = regs->asc_cnfg1 & ASC_CNFG1_MY_BUS_ID;
	splx(s);

	/*
	 * Statically partition the DMA buffer between targets.
	 * This way we will eventually be able to attach/detach
	 * drives on-fly.  And 18k/target is plenty for normal use.
	 */
#define PER_TGT_DMA_SIZE	((ASC_RAM_SIZE/7) & ~(sizeof(int)-1))

	/*
	 * Give each target its own DMA buffer region.
	 * We may want to try ping ponging buffers later.
	 */
	for (i = 0; i < ASC_NCMD; i++) {
		asc->st[i].dmaBufAddr = asc->buff + PER_TGT_DMA_SIZE * i;
		asc->st[i].dmaBufSize = PER_TGT_DMA_SIZE;
	}
	printf("asc%d at nexus0 csr 0x%x priority %d SCSI id %d\n",
		unit, cp->pmax_addr, cp->pmax_pri, id);
	return (1);
}

/*
 * Start activity on a SCSI device.
 * We maintain information on each device separately since devices can
 * connect/disconnect during an operation.
 */
void
asc_start(scsicmd)
	register ScsiCmd *scsicmd;	/* command to start */
{
	register struct scsi_device *sdp = scsicmd->sd;
	register asc_softc_t asc = &asc_softc[sdp->sd_ctlr];
	int s;

	s = splbio();
	/*
	 * Check if another command is already in progress.
	 * We may have to change this if we allow SCSI devices with
	 * separate LUNs.
	 */
	if (asc->cmd[sdp->sd_drive]) {
		printf("asc%d: device %s busy at start\n", sdp->sd_ctlr,
			sdp->sd_driver->d_name);
		(*sdp->sd_driver->d_done)(scsicmd->unit, EBUSY,
			scsicmd->buflen, 0);
		splx(s);
	}
	asc->cmd[sdp->sd_drive] = scsicmd;
	asc_startcmd(asc, sdp->sd_drive);
	splx(s);
}

static void
asc_reset(asc, regs)
	asc_softc_t asc;
	asc_regmap_t *regs;
{

	/*
	 * Reset chip and wait till done
	 */
	regs->asc_cmd = ASC_CMD_RESET;
	MachEmptyWriteBuffer(); DELAY(25);

	/* spec says this is needed after reset */
	regs->asc_cmd = ASC_CMD_NOP;
	MachEmptyWriteBuffer(); DELAY(25);

	/*
	 * Set up various chip parameters
	 */
	regs->asc_ccf = asc->ccf;
	MachEmptyWriteBuffer(); DELAY(25);
	regs->asc_sel_timo = asc->timeout_250;
	/* restore our ID */
	regs->asc_cnfg1 = asc->myid | ASC_CNFG1_P_CHECK;
	regs->asc_cnfg2 = /* ASC_CNFG2_RFB | */ ASC_CNFG2_SCSI2;
	regs->asc_cnfg3 = 0;
	/* zero anything else */
	ASC_TC_PUT(regs, 0);
	regs->asc_syn_p = asc->min_period;
	regs->asc_syn_o = 0;	/* async for now */
	MachEmptyWriteBuffer();
}

/*
 * Start a SCSI command on a target.
 */
static void
asc_startcmd(asc, target)
	asc_softc_t asc;
	int target;
{
	register asc_regmap_t *regs;
	register ScsiCmd *scsicmd;
	register State *state;
	int len;

	/*
	 * See if another target is currently selected on this SCSI bus.
	 */
	if (asc->target >= 0)
		return;

	regs = asc->regs;

	/*
	 * Check to see if a reselection is in progress and if so,
	 * try to cancel it or respond to the reselection if it won.
	 */
	if (asc->state == ASC_STATE_RESEL) {
		regs->asc_cmd = ASC_CMD_DISABLE_SEL;
		readback(regs->asc_cmd);
		while (!(regs->asc_status & ASC_CSR_INT))
			DELAY(1);
		asc_intr(asc - asc_softc);
		/* we will be busy if a reselecting device won */
		if (asc->state == ASC_STATE_BUSY)
			return;
	}

	asc->state = ASC_STATE_BUSY;
	asc->target = target;

	/* cache some pointers */
	scsicmd = asc->cmd[target];
	state = &asc->st[target];

#ifdef DEBUG
	if (asc_debug > 1) {
		printf("asc_startcmd: %s target %d cmd %x len %d\n",
			scsicmd->sd->sd_driver->d_name, target,
			scsicmd->cmd[0], scsicmd->buflen);
	}
	asc_debug_cmd = scsicmd->cmd[0];
	if (scsicmd->cmd[0] == SCSI_READ_EXT) {
		asc_debug_bn = (scsicmd->cmd[2] << 24) |
			(scsicmd->cmd[3] << 16) |
			(scsicmd->cmd[4] << 8) |
			scsicmd->cmd[5];
		asc_debug_sz = (scsicmd->cmd[7] << 8) | scsicmd->cmd[8];
	}
	asc_logp->status = PACK(asc - asc_softc, 0, 0, asc_debug_cmd);
	asc_logp->target = asc->target;
	asc_logp->state = 0;
	asc_logp->msg = 0xff;
	if (++asc_logp >= &asc_log[NLOG])
		asc_logp = asc_log;
#endif

	/*
	 * Init the chip and target state.
	 */
	regs->asc_cmd = ASC_CMD_FLUSH;
	readback(regs->asc_cmd);
	DELAY(2);
	state->flags = state->flags & DID_SYNC;
	state->error = 0;
	state->script = (script_t *)0;
	state->msg_out = SCSI_NO_OP;

	/*
	 * Copy command data to the DMA buffer.
	 */
	len = scsicmd->cmdlen;
	state->dmalen = len;
	bcopy(scsicmd->cmd, state->dmaBufAddr, len);

	/* check for simple SCSI command with no data transfer */
	if ((state->buflen = scsicmd->buflen) == 0) {
		/* check for sync negotiation */
		if ((scsicmd->flags & SCSICMD_USE_SYNC) &&
		    !(state->flags & DID_SYNC)) {
			asc->script = &asc_scripts[SCRIPT_TRY_SYNC];
			state->flags |= TRY_SYNC;
		} else
			asc->script = &asc_scripts[SCRIPT_SIMPLE];
		state->buf = (char *)0;
	} else if (scsicmd->flags & SCSICMD_DATA_TO_DEVICE) {
		asc->script = &asc_scripts[SCRIPT_DATA_OUT];
		state->buf = scsicmd->buf;
		state->flags |= DMA_OUT;
	} else {
		asc->script = &asc_scripts[SCRIPT_DATA_IN];
		state->buf = scsicmd->buf;
		state->flags |= DMA_IN;
	}

	/* preload the FIFO with the message to be sent */
	regs->asc_fifo = SCSI_DIS_REC_IDENTIFY;
	MachEmptyWriteBuffer();

	/* start the asc */
	(*asc->dma_start)(asc, state, state->dmaBufAddr, ASCDMA_WRITE);
	ASC_TC_PUT(regs, len);
	readback(regs->asc_cmd);

	regs->asc_dbus_id = target;
	readback(regs->asc_dbus_id);
	regs->asc_syn_p = state->sync_period;
	readback(regs->asc_syn_p);
	regs->asc_syn_o = state->sync_offset;
	readback(regs->asc_syn_o);

	if (state->flags & TRY_SYNC)
		regs->asc_cmd = ASC_CMD_SEL_ATN_STOP;
	else
		regs->asc_cmd = ASC_CMD_SEL_ATN | ASC_CMD_DMA;
	readback(regs->asc_cmd);
}

/*
 * Interrupt routine
 *	Take interrupts from the chip
 *
 * Implementation:
 *	Move along the current command's script if
 *	all is well, invoke error handler if not.
 */
void
asc_intr(unit)
	int unit;
{
	register asc_softc_t asc = &asc_softc[unit];
	register asc_regmap_t *regs = asc->regs;
	register State *state;
	register script_t *scpt;
	register int ss, ir, status;

	/* collect ephemeral information */
	status = regs->asc_status;
again:
	ss = regs->asc_ss;
	ir = regs->asc_intr;	/* this resets the previous two */
	scpt = asc->script;

#ifdef DEBUG
	asc_logp->status = PACK(unit, status, ss, ir);
	asc_logp->target = (asc->state == ASC_STATE_BUSY) ? asc->target : -1;
	asc_logp->state = scpt - asc_scripts;
	asc_logp->msg = -1;
	if (++asc_logp >= &asc_log[NLOG])
		asc_logp = asc_log;
	if (asc_debug > 2)
		printf("asc_intr: status %x ss %x ir %x cond %d:%x\n",
			status, ss, ir, scpt - asc_scripts, scpt->condition);
#endif

	/* check the expected state */
	if (SCRIPT_MATCH(ir, status) == scpt->condition) {
		/*
		 * Perform the appropriate operation, then proceed.
		 */
		if ((*scpt->action)(asc, status, ss, ir)) {
			regs->asc_cmd = scpt->command;
			readback(regs->asc_cmd);
			asc->script = scpt->next;
		}
		goto done;
	}

	/* check for message in or out */
	if ((ir & ~ASC_INT_FC) == ASC_INT_BS) {
		register int len, fifo;

		state = &asc->st[asc->target];
		switch (ASC_PHASE(status)) {
		case ASC_PHASE_DATAI:
		case ASC_PHASE_DATAO:
			ASC_TC_GET(regs, len);
			fifo = regs->asc_flags & ASC_FLAGS_FIFO_CNT;
			printf("asc_intr: data overrun: buflen %d dmalen %d tc %d fifo %d\n",
				state->buflen, state->dmalen, len, fifo);
			goto abort;

		case ASC_PHASE_MSG_IN:
			break;

		case ASC_PHASE_MSG_OUT:
			regs->asc_fifo = state->msg_out;
			state->msg_out = SCSI_NO_OP;
			regs->asc_cmd = ASC_CMD_XFER_INFO;
			readback(regs->asc_cmd);
			goto done;

		case ASC_PHASE_STATUS:
			/* probably an error in the SCSI command */
			asc->script = &asc_scripts[SCRIPT_GET_STATUS];
			regs->asc_cmd = ASC_CMD_I_COMPLETE;
			readback(regs->asc_cmd);
			goto done;

		default:
			goto abort;
		}

		if (state->script)
			goto abort;

		/* check for DMA in progress */
		ASC_TC_GET(regs, len);
		fifo = regs->asc_flags & ASC_FLAGS_FIFO_CNT;
		/* flush any data in the FIFO */
		if (fifo) {
			if (state->flags & DMA_OUT)
				len += fifo;
			else if (state->flags & DMA_IN) {
				u_char *cp;

				printf("asc_intr: IN: dmalen %d len %d fifo %d\n",
					state->dmalen, len, fifo); /* XXX */
				len += fifo;
				cp = state->dmaBufAddr + (state->dmalen - len);
				while (fifo-- > 0)
					*cp++ = regs->asc_fifo;
			} else
				printf("asc_intr: dmalen %d len %d fifo %d\n",
					state->dmalen, len, fifo); /* XXX */
			regs->asc_cmd = ASC_CMD_FLUSH;
			readback(regs->asc_cmd);
			MachEmptyWriteBuffer();
			DELAY(2);
		}
		if (len) {
			/* save number of bytes still to be sent or received */
			state->dmaresid = len;
			/* setup state to resume to */
			if (state->flags & DMA_IN)
				state->script =
					&asc_scripts[SCRIPT_RESUME_DMA_IN];
			else if (state->flags & DMA_OUT)
				state->script =
					&asc_scripts[SCRIPT_RESUME_DMA_OUT];
			else
				state->script = asc->script;
		} else {
			/* setup state to resume to */
			if (state->flags & DMA_IN) {
				if (state->flags & DMA_IN_PROGRESS) {
					state->flags &= ~DMA_IN_PROGRESS;
					(*asc->dma_end)(asc, state, ASCDMA_READ);
					len = state->dmalen;
					bcopy(state->dmaBufAddr, state->buf,
						len);
					state->buf += len;
					state->buflen -= len;
				}
				if (state->buflen)
					state->script =
					    &asc_scripts[SCRIPT_RESUME_IN];
				else
					state->script =
					    &asc_scripts[SCRIPT_RESUME_NO_DATA];
			} else if (state->flags & DMA_OUT) {
				/*
				 * If this is the last chunk, the next expected
				 * state is to get status.
				 */
				if (state->flags & DMA_IN_PROGRESS) {
					state->flags &= ~DMA_IN_PROGRESS;
					(*asc->dma_end)(asc, state, ASCDMA_WRITE);
					len = state->dmalen;
					state->buf += len;
					state->buflen -= len;
				}
				if (state->buflen)
					state->script =
					    &asc_scripts[SCRIPT_RESUME_OUT];
				else
					state->script =
					    &asc_scripts[SCRIPT_RESUME_NO_DATA];
			} else if (asc->script == &asc_scripts[SCRIPT_SIMPLE])
				state->script =
					    &asc_scripts[SCRIPT_RESUME_NO_DATA];
			else
				state->script = asc->script;
		}

		/* setup to receive a message */
		asc->script = &asc_scripts[SCRIPT_MSG_IN];
		state->msglen = 0;
		regs->asc_cmd = ASC_CMD_XFER_INFO;
		readback(regs->asc_cmd);
		goto done;
	}

	/* check for SCSI bus reset */
	if (ir & ASC_INT_RESET) {
		register int i;

		printf("asc%d: SCSI bus reset!!\n", asc - asc_softc);
		/* need to flush any pending commands */
		for (i = 0; i < ASC_NCMD; i++) {
			if (!asc->cmd[i])
				continue;
			asc->st[i].error = EIO;
			asc_end(asc, 0, 0, 0);
		}
		/* rearbitrate synchronous offset */
		for (i = 0; i < ASC_NCMD; i++) {
			asc->st[i].sync_offset = 0;
			asc->st[i].flags = 0;
		}
		asc->target = -1;
		return;
	}

	/* check for command errors */
	if (ir & ASC_INT_ILL)
		goto abort;

	/* check for disconnect */
	if (ir & ASC_INT_DISC) {
		state = &asc->st[asc->target];
		switch (ASC_SS(ss)) {
		case 0: /* device did not respond */
			state->error = ENXIO;
			asc_end(asc, status, ss, ir);
			return;

		default:
			goto abort;
		}
	}

	/* check for reselect */
	if (ir & ASC_INT_RESEL) {
		unsigned fifo, id, msg;

		fifo = regs->asc_flags & ASC_FLAGS_FIFO_CNT;
		if (fifo < 2)
			goto abort;
		/* read unencoded SCSI ID and convert to binary */
		msg = regs->asc_fifo & asc->myidmask;
		for (id = 0; (msg & 1) == 0; id++)
			msg >>= 1;
		/* read identify message */
		msg = regs->asc_fifo;
#ifdef DEBUG
		if (asc_logp == asc_log)
			asc_log[NLOG - 1].msg = msg;
		else
			asc_logp[-1].msg = msg;
#endif
		if (asc->state != ASC_STATE_RESEL)
			goto abort;
		asc->state = ASC_STATE_BUSY;
		asc->target = id;
		state = &asc->st[id];
		asc->script = state->script;
		state->script = (script_t *)0;
		if (!(state->flags & DISCONN))
			goto abort;
		state->flags &= ~DISCONN;
		regs->asc_syn_p = state->sync_period;
		regs->asc_syn_o = state->sync_offset;
		regs->asc_cmd = ASC_CMD_MSG_ACPT;
		readback(regs->asc_cmd);
		goto done;
	}

	/* check if we are being selected as a target */
	if (ir & (ASC_INT_SEL | ASC_INT_SEL_ATN))
		goto abort;

	/* must be just a ASC_INT_FC */
done:
	MachEmptyWriteBuffer();
	/* watch out for HW race conditions and setup & hold time violations */
	ir = regs->asc_status;
	while (ir != (status = regs->asc_status))
		ir = status;
	if (status & ASC_CSR_INT)
		goto again;
	return;

abort:
#ifdef DEBUG
	asc_DumpLog("asc_intr");
#endif
#if 0
	panic("asc_intr");
#else
	for (;;);
#endif
}

/*
 * All the many little things that the interrupt
 * routine might switch to.
 */

/* ARGSUSED */
static int
script_nop(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	return (1);
}

/* ARGSUSED */
static int
asc_get_status(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register int data;

	/*
	 * Get the last two bytes in the FIFO.
	 */
	if ((data = regs->asc_flags & ASC_FLAGS_FIFO_CNT) != 2) {
		printf("asc_get_status: fifo cnt %d\n", data); /* XXX */
		if (data < 2) {
			asc->regs->asc_cmd = ASC_CMD_MSG_ACPT;
			readback(asc->regs->asc_cmd);
			return (0);
		}
		do {
			data = regs->asc_fifo;
		} while ((regs->asc_flags & ASC_FLAGS_FIFO_CNT) > 2);
	}

	/* save the status byte */
	asc->st[asc->target].statusByte = data = regs->asc_fifo;
#ifdef DEBUG
	if (asc_logp == asc_log)
		asc_log[NLOG - 1].msg = data;
	else
		asc_logp[-1].msg = data;
#endif

	/* get the (presumed) command_complete message */
	if ((data = regs->asc_fifo) == SCSI_COMMAND_COMPLETE)
		return (1);

#ifdef DEBUG
	printf("asc_get_status: status %x cmd %x\n",
		asc->st[asc->target].statusByte, data);
	asc_DumpLog("asc_get_status");
#endif
	return (0);
}

/* ARGSUSED */
static int
asc_end(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register ScsiCmd *scsicmd;
	register State *state;
	register int i, target;

	asc->state = ASC_STATE_IDLE;
	target = asc->target;
	asc->target = -1;
	scsicmd = asc->cmd[target];
	asc->cmd[target] = (ScsiCmd *)0;
	state = &asc->st[target];

#ifdef DEBUG
	if (asc_debug > 1) {
		printf("asc_end: %s target %d cmd %x err %d resid %d\n",
			scsicmd->sd->sd_driver->d_name, target,
			scsicmd->cmd[0], state->error, state->buflen);
	}
#endif
#ifdef DIAGNOSTIC
	if (target < 0 || !scsicmd)
		panic("asc_end");
#endif

	/* look for disconnected devices */
	for (i = 0; i < ASC_NCMD; i++) {
		if (!asc->cmd[i] || !(asc->st[i].flags & DISCONN))
			continue;
		asc->regs->asc_cmd = ASC_CMD_ENABLE_SEL;
		readback(asc->regs->asc_cmd);
		asc->state = ASC_STATE_RESEL;
		asc->script = &asc_scripts[SCRIPT_RESEL];
		break;
	}

	/* look for another device that is ready */
	for (i = 0; i < ASC_NCMD; i++) {
		/* don't restart a disconnected command */
		if (!asc->cmd[i] || (asc->st[i].flags & DISCONN))
			continue;
		asc_startcmd(asc, i);
		break;
	}

	/* signal device driver that the command is done */
	(*scsicmd->sd->sd_driver->d_done)(scsicmd->unit, state->error,
		state->buflen, state->statusByte);

	return (0);
}

/* ARGSUSED */
static int
asc_dma_in(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];
	register int len;

	/* check for previous chunk in buffer */
	if (state->flags & DMA_IN_PROGRESS) {
		/*
		 * Only count bytes that have been copied to memory.
		 * There may be some bytes in the FIFO if synchonous transfers
		 * are in progress.
		 */
		(*asc->dma_end)(asc, state, ASCDMA_READ);
		ASC_TC_GET(regs, len);
		len = state->dmalen - len;
		bcopy(state->dmaBufAddr, state->buf, len);
		state->buf += len;
		state->buflen -= len;
	}

	/* setup to start reading the next chunk */
	len = state->buflen;
	if (len > state->dmaBufSize)
		len = state->dmaBufSize;
	state->dmalen = len;
	(*asc->dma_start)(asc, state, state->dmaBufAddr, ASCDMA_READ);
	ASC_TC_PUT(regs, len);
#ifdef DEBUG
	if (asc_debug > 2)
		printf("asc_dma_in: buflen %d, len %d\n", state->buflen, len);
#endif

	/* check for next chunk */
	state->flags |= DMA_IN_PROGRESS;
	if (len != state->buflen) {
		regs->asc_cmd = ASC_CMD_XFER_INFO | ASC_CMD_DMA;
		readback(regs->asc_cmd);
		asc->script = &asc_scripts[SCRIPT_CONTINUE_IN];
		return (0);
	}
	return (1);
}

/* ARGSUSED */
static int
asc_last_dma_in(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];
	register int len, fifo;

	/* copy data from buffer to main memory */
	(*asc->dma_end)(asc, state, ASCDMA_READ);
	ASC_TC_GET(regs, len);
	fifo = regs->asc_flags & ASC_FLAGS_FIFO_CNT;
#ifdef DEBUG
	if (asc_debug > 2)
		printf("asc_last_dma_in: buflen %d dmalen %d tc %d fifo %d\n",
			state->buflen, state->dmalen, len, fifo);
#endif
	if (fifo) {
		/* device must be trying to send more than we expect */
		regs->asc_cmd = ASC_CMD_FLUSH;
		readback(regs->asc_cmd);
		MachEmptyWriteBuffer();
	}
	state->flags &= ~DMA_IN_PROGRESS;
	len = state->dmalen - len;
	state->buflen -= len;
	bcopy(state->dmaBufAddr, state->buf, len);

	return (1);
}

/* ARGSUSED */
static int
asc_resume_in(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];
	register int len;

	/* setup to start reading the next chunk */
	len = state->buflen;
	if (len > state->dmaBufSize)
		len = state->dmaBufSize;
	state->dmalen = len;
	(*asc->dma_start)(asc, state, state->dmaBufAddr, ASCDMA_READ);
	ASC_TC_PUT(regs, len);
#ifdef DEBUG
	if (asc_debug > 2)
		printf("asc_resume_in: buflen %d, len %d\n", state->buflen,
			len);
#endif

	/* check for next chunk */
	state->flags |= DMA_IN_PROGRESS;
	if (len != state->buflen) {
		regs->asc_cmd = ASC_CMD_XFER_INFO | ASC_CMD_DMA;
		readback(regs->asc_cmd);
		asc->script = &asc_scripts[SCRIPT_CONTINUE_IN];
		return (0);
	}
	return (1);
}

/* ARGSUSED */
static int
asc_resume_dma_in(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];
	register int len, off;

	/* setup to finish reading the current chunk */
	len = state->dmaresid;
	off = state->dmalen - len;
	if ((off & 1) && state->sync_offset) {
		printf("asc_resume_dma_in: odd xfer dmalen %d len %d off %d\n",
			state->dmalen, len, off); /* XXX */
		regs->asc_res_fifo = state->dmaBufAddr[off];
	}
	(*asc->dma_start)(asc, state, state->dmaBufAddr + off, ASCDMA_READ);
	ASC_TC_PUT(regs, len);
#ifdef DEBUG
	if (asc_debug > 2)
		printf("asc_resume_dma_in: buflen %d dmalen %d len %d off %d\n",
			state->dmalen, state->buflen, len, off);
#endif

	/* check for next chunk */
	state->flags |= DMA_IN_PROGRESS;
	if (state->dmalen != state->buflen) {
		regs->asc_cmd = ASC_CMD_XFER_INFO | ASC_CMD_DMA;
		readback(regs->asc_cmd);
		asc->script = &asc_scripts[SCRIPT_CONTINUE_IN];
		return (0);
	}
	return (1);
}

/* ARGSUSED */
static int
asc_dma_out(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];
	register int len, fifo;

	if (state->flags & DMA_IN_PROGRESS) {
		/* check to be sure previous chunk was finished */
		ASC_TC_GET(regs, len);
		fifo = regs->asc_flags & ASC_FLAGS_FIFO_CNT;
		if (len || fifo)
			printf("asc_dma_out: buflen %d dmalen %d tc %d fifo %d\n",
				state->buflen, state->dmalen, len, fifo); /* XXX */
		len += fifo;
		len = state->dmalen - len;
		state->buf += len;
		state->buflen -= len;
	}

	/* setup for this chunck */
	len = state->buflen;
	if (len > state->dmaBufSize)
		len = state->dmaBufSize;
	state->dmalen = len;
	bcopy(state->buf, state->dmaBufAddr, len);
	(*asc->dma_start)(asc, state, state->dmaBufAddr, ASCDMA_WRITE);
	ASC_TC_PUT(regs, len);
#ifdef DEBUG
	if (asc_debug > 2)
		printf("asc_dma_out: buflen %d, len %d\n", state->buflen, len);
#endif

	/* check for next chunk */
	state->flags |= DMA_IN_PROGRESS;
	if (len != state->buflen) {
		regs->asc_cmd = ASC_CMD_XFER_INFO | ASC_CMD_DMA;
		readback(regs->asc_cmd);
		asc->script = &asc_scripts[SCRIPT_CONTINUE_OUT];
		return (0);
	}
	return (1);
}

/* ARGSUSED */
static int
asc_last_dma_out(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];
	register int len, fifo;

	ASC_TC_GET(regs, len);
	fifo = regs->asc_flags & ASC_FLAGS_FIFO_CNT;
#ifdef DEBUG
	if (asc_debug > 2)
		printf("asc_last_dma_out: buflen %d dmalen %d tc %d fifo %d\n",
			state->buflen, state->dmalen, len, fifo);
#endif
	if (fifo) {
		len += fifo;
		regs->asc_cmd = ASC_CMD_FLUSH;
		readback(regs->asc_cmd);
		MachEmptyWriteBuffer();
	}
	state->flags &= ~DMA_IN_PROGRESS;
	len = state->dmalen - len;
	state->buflen -= len;
	return (1);
}

/* ARGSUSED */
static int
asc_resume_out(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];
	register int len;

	/* setup for this chunck */
	len = state->buflen;
	if (len > state->dmaBufSize)
		len = state->dmaBufSize;
	state->dmalen = len;
	bcopy(state->buf, state->dmaBufAddr, len);
	(*asc->dma_start)(asc, state, state->dmaBufAddr, ASCDMA_WRITE);
	ASC_TC_PUT(regs, len);
#ifdef DEBUG
	if (asc_debug > 2)
		printf("asc_resume_out: buflen %d, len %d\n", state->buflen,
			len);
#endif

	/* check for next chunk */
	state->flags |= DMA_IN_PROGRESS;
	if (len != state->buflen) {
		regs->asc_cmd = ASC_CMD_XFER_INFO | ASC_CMD_DMA;
		readback(regs->asc_cmd);
		asc->script = &asc_scripts[SCRIPT_CONTINUE_OUT];
		return (0);
	}
	return (1);
}

/* ARGSUSED */
static int
asc_resume_dma_out(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];
	register int len, off;

	/* setup to finish writing this chunk */
	len = state->dmaresid;
	off = state->dmalen - len;
	if (off & 1) {
		printf("asc_resume_dma_out: odd xfer dmalen %d len %d off %d\n",
			state->dmalen, len, off); /* XXX */
		regs->asc_fifo = state->dmaBufAddr[off];
		off++;
		len--;
	}
	(*asc->dma_start)(asc, state, state->dmaBufAddr + off, ASCDMA_WRITE);
	ASC_TC_PUT(regs, len);
#ifdef DEBUG
	if (asc_debug > 2)
		printf("asc_resume_dma_out: buflen %d dmalen %d len %d off %d\n",
			state->dmalen, state->buflen, len, off);
#endif

	/* check for next chunk */
	state->flags |= DMA_IN_PROGRESS;
	if (state->dmalen != state->buflen) {
		regs->asc_cmd = ASC_CMD_XFER_INFO | ASC_CMD_DMA;
		readback(regs->asc_cmd);
		asc->script = &asc_scripts[SCRIPT_CONTINUE_OUT];
		return (0);
	}
	return (1);
}

/* ARGSUSED */
static int
asc_sendsync(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];

	/* send the extended synchronous negotiation message */
	regs->asc_fifo = SCSI_EXTENDED_MSG;
	MachEmptyWriteBuffer();
	regs->asc_fifo = 3;
	MachEmptyWriteBuffer();
	regs->asc_fifo = SCSI_SYNCHRONOUS_XFER;
	MachEmptyWriteBuffer();
	regs->asc_fifo = SCSI_MIN_PERIOD;
	MachEmptyWriteBuffer();
	regs->asc_fifo = ASC_MAX_OFFSET;
	/* state to resume after we see the sync reply message */
	state->script = asc->script + 2;
	state->msglen = 0;
	return (1);
}

/* ARGSUSED */
static int
asc_replysync(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];

#ifdef DEBUG
	if (asc_debug > 2)
		printf("asc_replysync: %x %x\n",
			asc_to_scsi_period[state->sync_period] * asc->tb_ticks,
			state->sync_offset);
#endif
	/* send synchronous transfer in response to a request */
	regs->asc_fifo = SCSI_EXTENDED_MSG;
	MachEmptyWriteBuffer();
	regs->asc_fifo = 3;
	MachEmptyWriteBuffer();
	regs->asc_fifo = SCSI_SYNCHRONOUS_XFER;
	MachEmptyWriteBuffer();
	regs->asc_fifo = asc_to_scsi_period[state->sync_period] * asc->tb_ticks;
	MachEmptyWriteBuffer();
	regs->asc_fifo = state->sync_offset;
	regs->asc_cmd = ASC_CMD_XFER_INFO;
	readback(regs->asc_cmd);

	/* return to the appropriate script */
	if (!state->script) {
#ifdef DEBUG
		asc_DumpLog("asc_replsync");
#endif
		panic("asc_replysync");
	}
	asc->script = state->script;
	state->script = (script_t *)0;
	return (0);
}

/* ARGSUSED */
static int
asc_msg_in(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register asc_regmap_t *regs = asc->regs;
	register State *state = &asc->st[asc->target];
	register int msg;
	int i;

	/* read one message byte */
	msg = regs->asc_fifo;
#ifdef DEBUG
	if (asc_logp == asc_log)
		asc_log[NLOG - 1].msg = msg;
	else
		asc_logp[-1].msg = msg;
#endif

	/* check for multi-byte message */
	if (state->msglen != 0) {
		/* first byte is the message length */
		if (state->msglen < 0) {
			state->msglen = msg;
			return (1);
		}
		if (state->msgcnt >= state->msglen)
			goto abort;
		state->msg_in[state->msgcnt++] = msg;

		/* did we just read the last byte of the message? */
		if (state->msgcnt != state->msglen)
			return (1);

		/* process an extended message */
#ifdef DEBUG
		if (asc_debug > 2)
			printf("asc_msg_in: msg %x %x %x\n",
				state->msg_in[0],
				state->msg_in[1],
				state->msg_in[2]);
#endif
		switch (state->msg_in[0]) {
		case SCSI_SYNCHRONOUS_XFER:
			state->flags |= DID_SYNC;
			state->sync_offset = state->msg_in[2];

			/* convert SCSI period to ASC period */
			i = state->msg_in[1] / asc->tb_ticks;
			if (i < asc->min_period)
				i = asc->min_period;
			else if (i >= asc->max_period) {
				/* can't do sync transfer, period too long */
				printf("asc%d: SCSI device %d: sync xfer period too long (%d)\n",
					asc - asc_softc, asc->target, i);
				i = asc->max_period;
				state->sync_offset = 0;
			}
			if ((i * asc->tb_ticks) != state->msg_in[1])
				i++;
			state->sync_period = i & 0x1F;

			/*
			 * If this is a request, check minimums and
			 * send back an acknowledge.
			 */
			if (!(state->flags & TRY_SYNC)) {
				regs->asc_cmd = ASC_CMD_SET_ATN;
				readback(regs->asc_cmd);
				MachEmptyWriteBuffer();

				if (state->sync_period < asc->min_period)
					state->sync_period =
						asc->min_period;
				if (state->sync_offset > ASC_MAX_OFFSET)
					state->sync_offset =
						ASC_MAX_OFFSET;
				asc->script = &asc_scripts[SCRIPT_REPLY_SYNC];
				regs->asc_syn_p = state->sync_period;
				readback(regs->asc_syn_p);
				regs->asc_syn_o = state->sync_offset;
				readback(regs->asc_syn_o);
				regs->asc_cmd = ASC_CMD_MSG_ACPT;
				readback(regs->asc_cmd);
				return (0);
			}

			regs->asc_syn_p = state->sync_period;
			readback(regs->asc_syn_p);
			regs->asc_syn_o = state->sync_offset;
			readback(regs->asc_syn_o);
			goto done;

		default:
			printf("asc%d: SCSI device %d: rejecting extended message 0x%x\n",
				asc - asc_softc, asc->target,
				state->msg_in[0]);
			goto reject;
		}
	}

	/* process first byte of a message */
#ifdef DEBUG
	if (asc_debug > 2)
		printf("asc_msg_in: msg %x\n", msg);
#endif
	switch (msg) {
#if 0
	case SCSI_MESSAGE_REJECT:
		printf(" did not like SYNCH xfer "); /* XXX */
		state->flags |= DID_SYNC;
		regs->asc_cmd = ASC_CMD_MSG_ACPT;
		readback(regs->asc_cmd);
		status = asc_wait(regs, ASC_CSR_INT);
		ir = regs->asc_intr;
		/* some just break out here, some dont */
		if (ASC_PHASE(status) == ASC_PHASE_MSG_OUT) {
			regs->asc_fifo = SCSI_ABORT;
			regs->asc_cmd = ASC_CMD_XFER_INFO;
			readback(regs->asc_cmd);
			status = asc_wait(regs, ASC_CSR_INT);
			ir = regs->asc_intr;
		}
		if (ir & ASC_INT_DISC) {
			asc_end(asc, status, 0, ir);
			return (0);
		}
		goto status;
#endif

	case SCSI_EXTENDED_MSG: /* read an extended message */
		/* setup to read message length next */
		state->msglen = -1;
		state->msgcnt = 0;
		return (1);

	case SCSI_NO_OP:
		break;

	case SCSI_SAVE_DATA_POINTER:
		/* expect another message */
		return (1);

	case SCSI_RESTORE_POINTERS:
		/*
		 * Need to do the following if resuming synchonous data in
		 * on an odd byte boundary.
		regs->asc_cnfg2 |= ASC_CNFG2_RFB;
		 */
		break;

	case SCSI_DISCONNECT:
		if (state->flags & DISCONN)
			goto abort;
		state->flags |= DISCONN;
		regs->asc_cmd = ASC_CMD_MSG_ACPT;
		readback(regs->asc_cmd);
		asc->script = &asc_scripts[SCRIPT_DISCONNECT];
		return (0);

	default:
		printf("asc%d: SCSI device %d: rejecting message 0x%x\n",
			asc - asc_softc, asc->target, msg);
	reject:
		/* request a message out before acknowledging this message */
		state->msg_out = SCSI_MESSAGE_REJECT;
		regs->asc_cmd = ASC_CMD_SET_ATN;
		readback(regs->asc_cmd);
		MachEmptyWriteBuffer();
	}

done:
	/* return to original script */
	regs->asc_cmd = ASC_CMD_MSG_ACPT;
	readback(regs->asc_cmd);
	if (!state->script) {
	abort:
#ifdef DEBUG
		asc_DumpLog("asc_msg_in");
#endif
		panic("asc_msg_in");
	}
	asc->script = state->script;
	state->script = (script_t *)0;
	return (0);
}

/* ARGSUSED */
static int
asc_disconnect(asc, status, ss, ir)
	register asc_softc_t asc;
	register int status, ss, ir;
{
	register State *state = &asc->st[asc->target];

	asc->target = -1;
	asc->state = ASC_STATE_RESEL;
	return (1);
}

/*
 * DMA handling routines. For a turbochannel device, just set the dmar
 * for the I/O ASIC, handle the actual DMA interface.
 */
static void
tb_dma_start(asc, state, cp, flag)
	asc_softc_t asc;
	State *state;
	caddr_t cp;
	int flag;
{

	if (flag == ASCDMA_WRITE)
		*asc->dmar = ASC_DMAR_WRITE | ASC_DMA_ADDR(cp);
	else
		*asc->dmar = ASC_DMA_ADDR(cp);
}

static void
tb_dma_end(asc, state, flag)
	asc_softc_t asc;
	State *state;
	int flag;
{

}

static void
asic_dma_start(asc, state, cp, flag)
	asc_softc_t asc;
	State *state;
	caddr_t cp;
	int flag;
{
	register volatile u_int *ssr = (volatile u_int *)
		MACH_PHYS_TO_UNCACHED(KMIN_REG_CSR);
	u_int phys, nphys;

	/* stop DMA engine first */
	*ssr &= ~ASIC_CSR_DMAEN_SCSI;
	* ((volatile int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_SCR)) = 0;

	phys = MACH_CACHED_TO_PHYS(cp);
	cp = (caddr_t)pmax_trunc_page(cp + NBPG);
	nphys = MACH_CACHED_TO_PHYS(cp);

	asc->dma_next = cp;
	asc->dma_xfer = state->dmalen - (nphys - phys);

	*(volatile int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_DMAPTR) =
		ASIC_DMA_ADDR(phys);
	*(volatile int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_DMANPTR) =
		ASIC_DMA_ADDR(nphys);
	if (flag == ASCDMA_READ)
		*ssr |= ASIC_CSR_SCSI_DIR | ASIC_CSR_DMAEN_SCSI;
	else
		*ssr = (*ssr & ~ASIC_CSR_SCSI_DIR) | ASIC_CSR_DMAEN_SCSI;
	MachEmptyWriteBuffer();
}

static void
asic_dma_end(asc, state, flag)
	asc_softc_t asc;
	State *state;
	int flag;
{
	register volatile u_int *ssr = (volatile u_int *)
		MACH_PHYS_TO_UNCACHED(KMIN_REG_CSR);
	int nb;

	*ssr &= ~ASIC_CSR_DMAEN_SCSI;
	*((volatile int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_DMAPTR)) = -1;
	*((volatile int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_DMANPTR)) = -1;
	MachEmptyWriteBuffer();

	if (flag == ASCDMA_READ) {
		MachFlushDCache(MACH_PHYS_TO_CACHED(
		    MACH_UNCACHED_TO_PHYS(state->dmaBufAddr)), state->dmalen);
		if (nb = *((int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_SCR))) {
			/* pick up last upto6 bytes, sigh. */
			register u_short *to;
			register int w;
	
			/* Last byte really xferred is.. */
			to = (u_short *)(state->dmaBufAddr + state->dmalen - (nb << 1));
			w = *(int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_SDR0);
			*to++ = w;
			if (--nb > 0) {
				w >>= 16;
				*to++ = w;
			}
			if (--nb > 0) {
				w = *(int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_SDR1);
				*to++ = w;
			}
		}
	}
}

/*
 * Called by asic_intr() for scsi dma pointer update interrupts.
 */
void
asc_dma_intr()
{
	asc_softc_t asc = &asc_softc[0];
	u_int next_phys;

	asc->dma_xfer -= NBPG;
	if (asc->dma_xfer <= -NBPG) {
		volatile u_int *ssr = (volatile u_int *)
			MACH_PHYS_TO_UNCACHED(KMIN_REG_CSR);
		*ssr &= ~ASIC_CSR_DMAEN_SCSI;
	} else {
		asc->dma_next += NBPG;
		next_phys = MACH_CACHED_TO_PHYS(asc->dma_next);
	}
#ifdef notdef
	else
		next_phys = MACH_CACHED_TO_PHYS(asc_iobuf);
#endif
	*(volatile int *)MACH_PHYS_TO_UNCACHED(KMIN_REG_SCSI_DMANPTR) =
		ASIC_DMA_ADDR(next_phys);
	MachEmptyWriteBuffer();
}

#ifdef DEBUG
asc_DumpLog(str)
	char *str;
{
	register struct asc_log *lp;
	register u_int status;

	printf("asc: %s: cmd %x bn %d cnt %d\n", str, asc_debug_cmd,
		asc_debug_bn, asc_debug_sz);
	lp = asc_logp + 1;
	if (lp > &asc_log[NLOG])
		lp = asc_log;
	while (lp != asc_logp) {
		status = lp->status;
		printf("asc%d tgt %d status %x ss %x ir %x cond %d:%x msg %x\n",
			status >> 24,
			lp->target,
			(status >> 16) & 0xFF,
			(status >> 8) & 0xFF,
			status & 0XFF,
			lp->state,
			asc_scripts[lp->state].condition,
			lp->msg);
		if (++lp >= &asc_log[NLOG])
			lp = asc_log;
	}
}
#endif

#endif	/* NASC > 0 */
