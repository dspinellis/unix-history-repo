/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: scsi_1185.c,v 4.300 91/06/09 06:22:20 root Rel41 $ SONY
 *
 *	@(#)scsi_1185.c	7.4 (Berkeley) %G%
 */

/*
 * Copyright (c) 1989- by SONY Corporation.
 */
/*
 *	scsi_1185.c
 *
 *	CXD1185Q
 *	SCSI bus low level common routines
 *				for one cpu machine
 */
/*
 * MODIFY HISTORY:
 *
 *	DMAC_WAIT	--- DMAC_0266 wo tukau-baai, DMAC mata-wa SCSI-chip ni
 *				tuzukete access suru-baai,
 *				kanarazu wait wo ireru-beshi !
 *
 */

#include <sys/types.h>
#include <machine/pte.h>
#include <machine/cpu.h>

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/map.h>
#include <sys/buf.h>
#include <vm/vm.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/conf.h>
#include <sys/dkstat.h>
#include <sys/kernel.h>

#include <news3400/hbdev/hbvar.h>
#include <news3400/hbdev/screg_1185.h>
#include <news3400/hbdev/scsic.h>

#ifdef news3400
# include <news3400/hbdev/dmac_0448.h>
# ifndef NDMACMAP
# define NDMACMAP	144
# endif
#endif

#include <news3400/iodev/scsireg.h>

#ifdef mips
#define	VOLATILE	volatile
#else
#define	VOLATILE
#endif

#define ABORT_SYNCTR_MES_FROM_TARGET
#define SCSI_1185AQ
#define RESET_RECOVER

#define DMAC_MAP_INIT			/* for nws-3700 parity error */

#define APAD_ALWAYS_ON

# define	CHECK_LOOP_CNT	60
# define	RSL_LOOP_CNT	60

#ifndef DMAC_MAP_INIT
# define MAP_OVER_ACCESS		/* for nws-3700 parity error */
#endif

#undef	CHECK_MRQ

#ifdef NOT_SUPPORT_SYNCTR
# define	MAX_OFFSET_BYTES	0
#else
# define	MAX_OFFSET_BYTES	MAX_OFFSET
#endif

#define	NTARGET		8

#define	act_point	spoint
#define	act_trcnt	stcnt
#define	act_tag		stag
#define	act_offset	soffset

#define	splscsi		splsc

#if defined(mips) && defined(CPU_SINGLE)
#define nops(x)		{ int i; for (i = 0; i < (x); i++) ; }
#define	vtophys(v)	MACH_UNMAPPED_TO_PHYS(v)
#define	DMAC_WAIT0	;
#else
#define	DMAC_WAIT0	DMAC_WAIT
#endif

int	perr_flag[NTARGET];

#ifndef NOT_SUPPORT_SYNCTR
VOLATILE char sync_tr[NTARGET];
#endif

#ifdef DMAC_MAP_INIT
int	dmac_map_init = 0;
#endif

#ifdef SCSI_1185AQ
int	scsi_1185AQ = 0;
#endif

struct sc_chan_stat chan_stat[NTARGET];	/* SCSI channel status */
int	sel_stat[NTARGET];		/* target select status */
#define	SEL_WAIT	0
#define	SEL_START	1
#define	SEL_TIMEOUT	2
#define	SEL_ARBF	3
#define	SEL_SUCCESS	4
#define	SEL_RSLD	5
#define	SEL_RSL_WAIT	6

/*
 *	command flag status
 */
#define	CF_SET		1
#define	CF_SEND		2
#define	CF_ENOUGH	3
#define	CF_EXEC		4

#define	SEL_TIMEOUT_VALUE	0x7a

VOLATILE int int_stat1;
VOLATILE int int_stat2;

VOLATILE int min_flag;

VOLATILE char mout_flag[NTARGET];
#define MOUT_IDENTIFY	1
#define MOUT_SYNC_TR	2

VOLATILE int	last_cmd;
VOLATILE char	min_cnt[NTARGET];
VOLATILE u_char	*min_point[NTARGET];
VOLATILE int	pad_cnt[NTARGET];

VOLATILE static u_char *act_cmd_pointer;
static VOLATILE struct sc_chan_stat *wbq_actf = 0; /* forword active pointer */
static VOLATILE struct sc_chan_stat *wbq_actl = 0; /* last active pointer */
static char ScsiSoftError[] = "SCSI soft error";

static int pad_start;

#if defined(mips) && defined(CPU_SINGLE)
#define dma_reset(x) { \
	int s = splscsi(); \
	dmac_gsel = (x); dmac_cctl = DM_RST; dmac_cctl = 0; \
	splx(s); \
}
#endif

WAIT_STATR_BITCLR(bitmask)
	register int bitmask;
{
	register int iloop;
	register VOLATILE int dummy;

	iloop = 0;
	do {
		dummy = sc_statr;
		DMAC_WAIT0;
		if (iloop++ > CHECK_LOOP_CNT)
			return (-1);
	} while (dummy & bitmask);
	return (0);
}

WAIT_STATR_BITSET(bitmask)
	register int bitmask;
{
	register int iloop;
	register VOLATILE int dummy;

	iloop = 0;
	do {
		dummy = sc_statr;
		DMAC_WAIT0;
		if (iloop++ > CHECK_LOOP_CNT)
			return (-1);
	} while ((dummy & bitmask) == 0);
	return (0);
}

SET_CMD(CMD)
	register int CMD;
{

	(void) WAIT_STATR_BITCLR(R0_CIP);
	last_cmd = (CMD);
	sc_comr = (CMD);
	DMAC_WAIT0;
}

SET_CNT(COUNT)
	register int COUNT;
{

	sc_tclow = (COUNT) & 0xff;
	DMAC_WAIT0;
	sc_tcmid = ((COUNT) >> 8) & 0xff;
	DMAC_WAIT0;
	sc_tchi = ((COUNT) >> 16) & 0xff;
	DMAC_WAIT0;
}

GET_CNT()
{
	register VOLATILE int COUNT;

	COUNT = sc_tclow;
	DMAC_WAIT0;
	COUNT += (sc_tcmid << 8) & 0xff00;
	DMAC_WAIT0;
	COUNT += (sc_tchi << 16) & 0xff0000;
	DMAC_WAIT0;
	return (COUNT);
}

GET_INTR(DATA1, DATA2)
	register VOLATILE int *DATA1;
	register VOLATILE int *DATA2;
{
	register VOLATILE int dummy;

	(void) WAIT_STATR_BITCLR(R0_CIP);
	while (sc_statr & R0_MIRQ) {
		DMAC_WAIT0;
		*DATA1 |= sc_intrq1;
		DMAC_WAIT0;
		*DATA2 |= sc_intrq2;
		DMAC_WAIT0;
	}
}


sc_send(chan, ie, sc)
	register int chan;
	register int ie;
	register struct scsi *sc;
{
	register VOLATILE struct sc_chan_stat *cs;
	register struct scsi_stat *ss;
	register int i;

	cs = &chan_stat[chan];
	ss = &scsi_stat;

	if (sc == NULL || cs->sc != NULL) {
		printf("SCSI%d:sc_send() NULL sc or NOT NULL cs->sc\n", chan);
		printf("ie=0x%x sc=0x%x cs->sc=0x%x\n", ie, sc, cs->sc);
		if (sc) {
			printf("cdb=");
			for (i = 0; i < 6; i++)
				printf("0x%x ", sc->sc_cdb.un_reserved[i]);
			printf("\n");
		}
		panic(ScsiSoftError);
		/*NOTREACHED*/
	}

	if ((sc->sc_cdb.un_reserved[0] == SCOP_RESET)
			&& (sc->sc_cdb.un_reserved[1] == SCOP_RESET)) {
		/*
		 * SCSI bus reset command procedure
		 *	(vender unique by Sony Corp.)
		 */
#ifdef SCSI_1185AQ
		if (sc_idenr & 0x08) {
			scsi_1185AQ = 1;
		}
#endif
		cs->sc = sc;
		scsi_hardreset();
		sc->sc_istatus = INST_EP;
		cs->sc = NULL;
		return;
	}

	if (sc->sc_map && (sc->sc_map->mp_pages > 0)) {
		/*
		 * use map table
		 */
		sc->sc_coffset = sc->sc_map->mp_offset & PGOFSET;
		if (sc->sc_map->mp_pages > NSCMAP) {
			printf("SCSI%d: map table overflow\n", chan);
			sc->sc_istatus = INST_EP|INST_LB|INST_PRE;
			return;
		}
	} else {
		/*
		 * no use map table
		 */
		sc->sc_coffset = (u_int)sc->sc_cpoint & PGOFSET;
	}
	sc->sc_ctag = 0;

	cs->sc = sc;
	cs->comflg = OFF;

	cs->intr_flg = ie;
	cs->chan_num = chan;
	perr_flag[chan] = 0;
	mout_flag[chan] = 0;
	min_cnt[chan] = 0;

	sel_stat[chan] = SEL_WAIT;
	append_wb(cs);
	sc_start();
}

/*
 *	SCSI start up routine
 */
sc_start()
{
	register VOLATILE struct sc_chan_stat *cs;
	register struct scsi_stat *ss;
	register int s;
	register VOLATILE int chan;
	register VOLATILE int dummy;

	ss = &scsi_stat;

	s = splclock();
	chan = get_wb_chan();
	if ((chan < 0) || (ss->ipc >= 0))
		goto sc_start_exit;
	if (sel_stat[chan] != SEL_WAIT) {
		/*
		 * already started
		 */
		goto sc_start_exit;
	}
	sel_stat[chan] = SEL_START;
	(void) splscsi();

	cs = &chan_stat[chan];

	dummy = sc_cmonr;
	DMAC_WAIT0;
	if (dummy & (R4_MBSY|R4_MSEL)) {
		sel_stat[chan] = SEL_WAIT;
		goto sc_start_exit;
	}

	/*
	 *	send SELECT with ATN command
	 */
	ss->dma_stat = OFF;
	pad_start = 0;
	dummy = sc_statr;
	DMAC_WAIT0;
	if (dummy & R0_CIP) {
		sel_stat[chan] = SEL_WAIT;
		goto sc_start_exit;
	}
	sc_idenr = (chan << SC_TG_SHIFT) | SC_OWNID;
	DMAC_WAIT0;
#ifdef SCSI_1185AQ
	if (scsi_1185AQ)
		sc_intok1 = Ra_STO|Ra_ARBF;
	else
		sc_intok1 = Ra_STO|Ra_RSL|Ra_ARBF;
#else
	sc_intok1 = Ra_STO|Ra_RSL|Ra_ARBF;
#endif
	DMAC_WAIT0;
	/*
	 * BUGFIX for signal reflection on BSY
	 *	!Rb_DCNT
	 */
	sc_intok2 = Rb_FNC|Rb_SRST|Rb_PHC|Rb_SPE;
	DMAC_WAIT0;

	dummy = sc_cmonr;
	DMAC_WAIT0;
	if (dummy & (R4_MBSY|R4_MSEL)) {
		sel_stat[chan] = SEL_WAIT;
		goto sc_start_exit;
	}
	SET_CMD(SCMD_SEL_ATN);

sc_start_exit:
	splx(s);
}

/*
 *	SCSI interrupt service routine
 */
scintr()
{
	register struct scsi_stat *ss;
	register int iloop;
	register VOLATILE int chan;
	register VOLATILE int dummy;
	int s_int1, s_int2;

scintr_loop:

#if defined(CHECK_MRQ) && defined(news3400)
	while (dmac_gstat & CH_MRQ(CH_SCSI))
		DMAC_WAIT;
#endif

	for (iloop = 0; iloop < 100; iloop++) {
		dummy = sc_statr;
		DMAC_WAIT;
		if ((dummy & R0_CIP) == 0)
			break;
	}

	/*
	 * get SCSI interrupt request
	 */
	while (sc_statr & R0_MIRQ) {
		DMAC_WAIT0;
		s_int1 = sc_intrq1;
		DMAC_WAIT0;
		s_int2 = sc_intrq2;
		DMAC_WAIT0;
		int_stat1 |= s_int1;
		int_stat2 |= s_int2;
	}

	if (int_stat2 & R3_SRST) {
		/*
		 * RST signal is drived
		 */
		int_stat2 &= ~R3_SRST;
		scsi_softreset();
		goto scintr_exit;
	}

	ss = &scsi_stat;
	if ((ss->ipc < 0) && (ss->wrc <= 0) && (ss->wbc <= 0)) {
		int_stat1 = 0;
		int_stat2 = 0;
		goto scintr_exit;
	}

	chan = get_wb_chan();
	if ((chan >= 0) && (sel_stat[chan] == SEL_START) &&
		(last_cmd == SCMD_SEL_ATN)) {
		/*
		 *	Check the result of SELECTION command
		 */
		if (int_stat1 & R2_RSL) {
			/*
			 * RESELECTION occur
			 */
			if (ss->wrc > 0) {
				sel_stat[chan] = SEL_RSLD;
			} else {
				/*
				 * Ghost RESELECTION ???
				 */
				int_stat1 &= ~R2_RSL;
			}
		}
		if (int_stat1 & R2_ARBF) {
			/*
			 * ARBITRATION fault
			 */
			int_stat1 &= ~R2_ARBF;
			sel_stat[chan] = SEL_ARBF;
		}
		if (int_stat1 & R2_STO) {
			/*
			 * SELECTION timeout
			 */
			int_stat1 &= ~R2_STO;
			if ((int_stat2&(R3_PHC|R3_RMSG)) != (R3_PHC|R3_RMSG)) {
				ss->ipc = chan;
				ss->ip = &chan_stat[chan];
				sel_stat[chan] = SEL_TIMEOUT;
				chan_stat[chan].sc->sc_istatus
					= INST_EP|INST_TO;
				release_wb();
			}
		}

		/*
		 *	SELECTION command done
		 */
		switch (sel_stat[chan]) {

		case SEL_START:
			if ((int_stat2 & R3_FNC) == 0)
				break;
			/*
			 * SELECTION success
			 */
			sc_intok2 = Rb_FNC|Rb_DCNT|Rb_SRST|Rb_PHC|Rb_SPE;
			ss->ipc = chan;
			ss->ip = &chan_stat[chan];
			ss->ip->sc->sc_istatus |= INST_IP;
			ss->dma_stat = OFF;
			pad_start = 0;
			sel_stat[chan] = SEL_SUCCESS;
			release_wb();
#ifndef NOT_SUPPORT_SYNCTR
			sc_syncr = sync_tr[chan];
			DMAC_WAIT0;
#endif
			DMAC_WAIT0;
			break;

		case SEL_TIMEOUT:
			/*
			 * SELECTION time out
			 */
			sc_discon();
			goto scintr_exit;

		/* case SEL_RSLD: */
		/* case SEL_ARBF: */
		default:
			/*
			 * SELECTION failed
			 */
			sel_stat[chan] = SEL_WAIT;
			break;
		}
		if ((int_stat1 & R2_RSL) == 0)
			int_stat2 &= ~R3_FNC;
	}

	if (ss->ip != NULL) {
		/*
		 * check In Process channel's request
		 */
		if (ss->dma_stat != OFF) {
			/*
			 * adjust pointer & counter
			 */
			adjust_transfer(ss->ip);
		}
		if (int_stat2 & R3_SPE) {
			register int VOLATILE statr;
			register int VOLATILE cmonr;

			statr = sc_statr;
			DMAC_WAIT0;
			cmonr = sc_cmonr;
			int_stat2 &= ~R3_SPE;
			perr_flag[ss->ip->chan_num] = 1;
		}
	}

	if (int_stat2 & R3_DCNT) {
		/*
		 * Bus Free
		 */
		sc_discon();
		int_stat2 &= ~R3_DCNT;
	}

	if ((ss->ipc >= 0) && (sel_stat[ss->ipc] == SEL_RSL_WAIT)) {
		sel_stat[ss->ipc] = SEL_RSLD;
		ss->ipc = -1;
		int_stat1 |= R2_RSL;
	}
	if (int_stat1 & R2_RSL) {
		/*
		 * Reselection
		 */
		sc_resel();
		int_stat1 &= ~R2_RSL;
		if (sel_stat[ss->ipc] == SEL_RSL_WAIT)
			goto scintr_exit;
	}


	if ((ss->ipc >= 0) && (ss->ipc != SC_OWNID) &&
	    (sel_stat[ss->ipc] == SEL_SUCCESS)) {
		if (int_stat2 & R3_PHC) {
			/*
			 * Phase change
			 */
			int_stat2 &= ~(R3_PHC|R3_RMSG);
			sc_pmatch();
		} else if (int_stat2 & R3_RMSG) {
			/*
			 * message Phase
			 */
			if (min_flag > 0) {
				int_stat2 &= ~(R3_PHC|R3_RMSG);
				sc_pmatch();
			}
		}
		else if (ss->dma_stat != OFF) {
			dummy = sc_cmonr;
			DMAC_WAIT0;
			if ((dummy & (R4_MMSG|R4_MCD|R4_MREQ)) == R4_MREQ) {
				/*
				 * still DATA transfer phase
				 */
				sc_dio_pad(ss->ip);
			}
		}
		else if (ss->ip->comflg == CF_SEND) {
			dummy = sc_cmonr;
			DMAC_WAIT0;
			if ((dummy & SC_PMASK) == COM_OUT) {
				/*
				 * command out phase
				 */
				sc_cout(ss->ip);
			}
		}
	} else {
		if (int_stat2 & (R3_PHC|R3_RMSG))
			goto scintr_exit;
	}

	if ((int_stat1 & (R2_STO|R2_RSL|R2_ARBF))
	    || (int_stat2 & (R3_DCNT|R3_SRST|R3_PHC|R3_SPE))) {
		/*
		 * still remain intrq
		 */
		goto scintr_loop;
	}

scintr_exit:
	return (1);
}

/*
 *	SCSI bus reset routine
 *		scsi_hardreset() is occered a reset interrupt.
 *		And call scsi_softreset().
 */
scsi_hardreset()
{
	register int s;
#ifdef DMAC_MAP_INIT
	register int i;
#endif

	s = splscsi();

	scsi_chipreset();
	DMAC_WAIT0;
	int_stat1 = 0;
	int_stat2 = 0;
	SET_CMD(SCMD_AST_RST);			/* assert RST signal */

#ifdef DMAC_MAP_INIT
	if (dmac_map_init == 0) {
		dmac_map_init++;
		for (i = 0; i < NDMACMAP; i++) {
# if defined(mips) && defined(CPU_SINGLE)
			dmac_gsel = CH_SCSI;
			dmac_ctag = (u_char)i;
			dmac_cmap = (u_short)0;
# endif
		}
	}
#endif
	splx(s);
}

/*
 * I/O port (sc_ioptr) bit assign
 *	
 *	Rf_PRT3		-	<reserved>
 *	Rf_PRT2		-	<reserved>
 *	Rf_PRT1		out	Floppy Disk Density control
 *	Rf_PRT0		out	Floppy Disk Eject control
 */

scsi_chipreset()
{
	register int s;
	register int iloop;
	register VOLATILE int save_ioptr;
	register VOLATILE int dummy;
	int s_int1, s_int2;

	s = splscsi();

#if defined(mips) && defined(CPU_SINGLE)
	dmac_gsel = CH_SCSI;
	dmac_cwid = 4;				/* initialize DMAC SCSI chan */
	*(unsigned VOLATILE char *)PINTEN |= DMA_INTEN;
	dma_reset(CH_SCSI);
#endif
	sc_envir = 0;				/* 1/4 clock */
	DMAC_WAIT0;
	save_ioptr = sc_ioptr;
	DMAC_WAIT0;
	last_cmd = SCMD_CHIP_RST;
	sc_comr = SCMD_CHIP_RST;		/* reset chip */
	DMAC_WAIT;
	(void) WAIT_STATR_BITCLR(R0_CIP);
	/*
	 * SCMD_CHIP_RST command reset all register
	 *				except sc_statr<7:6> & sc_cmonr.
	 * So, bit R0_MIRQ & R3_FNC will be not set.
	 */
	sc_idenr = SC_OWNID;
	DMAC_WAIT0;

	sc_intok1 = Ra_STO|Ra_RSL|Ra_ARBF;
	DMAC_WAIT0;
	sc_intok2 = Rb_FNC|Rb_SRST|Rb_PHC|Rb_SPE|Rb_RMSG;
	DMAC_WAIT0;

	sc_ioptr = save_ioptr;
	DMAC_WAIT;

	sc_moder = Rc_TMSL;			/* RST drive time = 25.5 us */
	DMAC_WAIT0;
	sc_timer = 0x2;
	DMAC_WAIT0;

	sc_moder = Rc_SPHI;			/* selection timeout = 252 ms */
	DMAC_WAIT0;
	sc_timer = SEL_TIMEOUT_VALUE;
	DMAC_WAIT0;

#ifdef SCSI_1185AQ
	if (scsi_1185AQ)
		SET_CMD(SCMD_ENB_SEL);		/* enable reselection */
#endif

	int_stat1 &= ~R2_RSL;			/* ignore RSL inter request */

	splx(s);
}

scsi_softreset()
{
	register VOLATILE struct sc_chan_stat *cs;
	register struct scsi_stat *ss;
	register int (*handler)();
	register int i;
#ifdef mips
	extern struct sc_data sc_data[];
	register struct sc_data *scdp;
#endif

	wbq_actf = NULL;
	wbq_actl = NULL;
	ss = &scsi_stat;
	ss->wbc = 0;
	ss->wrc = 0;
	ss->ip = NULL;
	ss->ipc = -1;
	ss->dma_stat = OFF;
	pad_start = 0;

	for (i = 0; i < NTARGET; ++i) {
		if (i == SC_OWNID)
			continue;
		cs = &chan_stat[i];
		cs->wb_next = NULL;
#ifndef NOT_SUPPORT_SYNCTR
		sync_tr[i] = 0;			/* asynchronous mode */
#endif
		sel_stat[i] = SEL_WAIT;
		if (cs->sc != NULL) {
			if ((cs->sc->sc_istatus & INST_EP) == 0)
				cs->sc->sc_istatus = (INST_EP|INST_HE);
			cs->sc = NULL;
#ifdef mips
			scdp = &sc_data[cs->chan_num];
			MachFlushDCache(scdp->scd_scaddr, sizeof(struct scsi));

			if (MACH_IS_USPACE(scdp->scd_vaddr)) {
				panic("scsi_softreset: user address is not supported");
			} else if (MACH_IS_CACHED(scdp->scd_vaddr)) {
			    MachFlushDCache(scdp->scd_vaddr, scdp->scd_count);
			} else if (MACH_IS_MAPPED(scdp->scd_vaddr)) {
#ifdef notyet /* KU:XXX */
				clean_k2dcache(scdp->scd_vaddr, scdp->scd_count);
#else
				MachFlushCache();
#endif
			}
#endif /* mips */
			if ((cs->intr_flg == SCSI_INTEN)
				    && (handler = scintsw[i].sci_inthandler)) {
#ifdef noyet /* KU:XXX */
				intrcnt[INTR_SCSI00 + i]++;
#endif
				(*handler)(scintsw[i].sci_ctlr);
			}
		}
	}
}

/*
 *	RESELECTION interrupt service routine
 *		( RESELECTION phase )
 */
sc_resel()
{
	register struct sc_chan_stat *cs;
	register struct scsi_stat *ss;
	register VOLATILE int chan;
	register VOLATILE int statr;
	register int iloop;

	min_flag = 0;
	chan = (sc_idenr & R6_SID_MASK) >> SC_TG_SHIFT;

	if (chan == SC_OWNID)
		return;

	statr = sc_statr;
	DMAC_WAIT0;
	if (statr & R0_CIP) {
		if (last_cmd == SCMD_SEL_ATN) {
			/*
			 * SELECTION command dead lock ?
			 *	save interrupt request
			 */
			while (sc_statr & R0_MIRQ) {
				DMAC_WAIT0;
				int_stat1 |= sc_intrq1;
				DMAC_WAIT0;
				int_stat2 |= sc_intrq2;
				DMAC_WAIT0;
			}
			scsi_chipreset();
		}
	}

	cs = &chan_stat[chan];
	if (cs->sc == NULL) {
		scsi_hardreset();
		return;
	}
	if ((cs->sc->sc_istatus & INST_WR) == 0) {
		scsi_hardreset();
		return;
	}

	ss = &scsi_stat;
	if (ss->ipc >= 0) {
		scsi_hardreset();
		return;
	}

	ss->ip = cs;
	ss->ipc = chan;

	sc_intok2 = Rb_FNC|Rb_DCNT|Rb_SRST|Rb_PHC|Rb_SPE;
	DMAC_WAIT0;

	iloop = 0;
	while ((int_stat2 & R3_FNC) == 0) {
		/*
		 * Max 6 usec wait
		 */
		if (iloop++ > RSL_LOOP_CNT) {
			sel_stat[chan] = SEL_RSL_WAIT;
			return;
		}
		GET_INTR(&int_stat1, &int_stat2);
	}
	int_stat2 &= ~R3_FNC;
	
	sel_stat[chan] = SEL_SUCCESS;

	ss->wrc--;
	ss->dma_stat = OFF;
	pad_start = 0;
	cs->sc->sc_istatus |= INST_IP;
	cs->sc->sc_istatus &= ~INST_WR;

#ifndef NOT_SUPPORT_SYNCTR
	sc_syncr = sync_tr[chan];
	DMAC_WAIT0;
#endif
}

/*
 *	DISCONNECT interrupt service routine
 *		( Target disconnect / job done )
 */
sc_discon()
{
	register VOLATILE struct sc_chan_stat *cs;
	register struct scsi_stat *ss;
	register int (*handler)();
	register VOLATILE int dummy;
#ifdef mips
	extern struct sc_data sc_data[];
	register struct sc_data *scdp;
#endif

	/*
	 * Signal reflection on BSY is occured.
	 *	Not Bus Free Phase, ignore.
	 *
	 *	But, CXD1185Q reset INIT bit of sc_statr.
	 *	So, can't issue Transfer Information command.
	 *	
	 *	What shall we do ?  Bus reset ?
	 */
	if ((int_stat2 & R3_DCNT) && ((sc_intok2 & Rb_DCNT) == 0))
		return;

	sc_intok2 = Rb_FNC|Rb_SRST|Rb_PHC|Rb_SPE;
	DMAC_WAIT0;

	min_flag = 0;
	dummy = sc_cmonr;
	DMAC_WAIT0;
	if (dummy & R4_MATN) {
		SET_CMD(SCMD_NGT_ATN);
		(void) WAIT_STATR_BITSET(R0_MIRQ);
		GET_INTR(&int_stat1, &int_stat2);	/* clear interrupt */
	}

	if ((int_stat1 & R2_RSL) == 0)
		int_stat2 &= ~R3_FNC;

	ss = &scsi_stat;
	cs = ss->ip;
	if ((cs == NULL) || (ss->ipc < 0))
		goto sc_discon_exit;

	if ((sel_stat[cs->chan_num] != SEL_SUCCESS)
			&& (sel_stat[cs->chan_num] != SEL_TIMEOUT))
		printf("sc_discon: eh!\n");

	/*
	 * indicate abnormal terminate
	 */
	if ((cs->sc->sc_istatus & (INST_EP|INST_WR)) == 0)
		cs->sc->sc_istatus |= (INST_EP|INST_PRE|INST_LB);

	cs->sc->sc_istatus &= ~INST_IP;
	ss->dma_stat = OFF;
	pad_start = 0;
	ss->ip = NULL;
	ss->ipc = -1;

	if ((cs->sc->sc_istatus & INST_WR) == 0) {
		if (perr_flag[cs->chan_num] > 0)
			cs->sc->sc_istatus |= INST_EP|INST_PRE;
		cs->sc = NULL;
#ifdef mips
		scdp = &sc_data[cs->chan_num];
		MachFlushDCache(scdp->scd_scaddr, sizeof(struct scsi));

		if (MACH_IS_USPACE(scdp->scd_vaddr)) {
			panic("sc_discon: user address is not supported");
		} else if (MACH_IS_CACHED(scdp->scd_vaddr)) {
			MachFlushDCache(scdp->scd_vaddr, scdp->scd_count);
		} else if (MACH_IS_MAPPED(scdp->scd_vaddr)) {
#ifdef notyet /* KU:XXX */
			clean_k2dcache(scdp->scd_vaddr, scdp->scd_count);
#else
			MachFlushCache();
#endif
		}
#endif /* mips */
		if ((cs->intr_flg == SCSI_INTEN)
		    && (handler = scintsw[cs->chan_num].sci_inthandler)) {
#ifdef notyet /* KU:XXX */
			intrcnt[INTR_SCSI00 + cs->chan_num]++;
#endif
			(*handler)(scintsw[cs->chan_num].sci_ctlr);
		}
	}

sc_discon_exit:
	sc_start();
}

/*
 *	SCSI phase match interrupt service routine
 */
sc_pmatch()
{
	register VOLATILE struct sc_chan_stat *cs;
	register VOLATILE int phase;
	register VOLATILE int phase2;
	register VOLATILE int cmonr;

	int_stat2 &= ~R3_FNC;			/* XXXXXXXX */

	cs = scsi_stat.ip;
	if (cs == NULL)
		return;

# if defined(mips) && defined(CPU_SINGLE)
	dma_reset(CH_SCSI);
# endif
	phase = sc_cmonr & SC_PMASK;
	DMAC_WAIT0;
	for (;;) {
		phase2 = phase;
		cmonr = sc_cmonr;
		DMAC_WAIT0;
		phase = cmonr & SC_PMASK;
		if (phase == phase2) {
			if ((phase == DAT_IN) || (phase == DAT_OUT))
				break;
			else if (cmonr & R4_MREQ)
				break;
		}
	}


	scsi_stat.dma_stat = OFF;
	pad_start = 0;

	if (phase == COM_OUT) {
		min_flag = 0;
		if (cs->comflg != CF_SEND)
			cs->comflg = CF_SET;
		sc_cout(cs);
	} else {
		cs->comflg = CF_ENOUGH;
		sc_intok2 &= ~Rb_FNC;
		if (phase == MES_IN) {
			min_flag++;
			sc_min(cs);
		} else {
			min_flag = 0;

			switch (phase) {

			case MES_OUT:
				sc_mout(cs);
				break;

			case DAT_IN:
			case DAT_OUT:
				sc_dio(cs);
				break;

			case STAT_IN:
				sc_sin(cs);
				break;

			default:
				printf("SCSI%d: unknown phase\n", cs->chan_num);
				break;
			}
		}
	}
}


flush_fifo()
{
	register VOLATILE int dummy;
	VOLATILE int tmp;
	VOLATILE int tmp0;

	dummy = sc_ffstr;
	DMAC_WAIT0;
	if (dummy & R5_FIFOREM) {
		/*
		 * flush FIFO
		 */
		SET_CMD(SCMD_FLSH_FIFO);
		tmp = 0;
		do {
			do {
				dummy = sc_statr;
				DMAC_WAIT0;
			} while (dummy & R0_CIP);
			GET_INTR(&tmp0, &tmp); /* clear interrupt */
		} while ((tmp & R3_FNC) == 0);
	}
}

/*
 *	SCSI command send routine
 */
int
sc_cout(cs)
	register struct sc_chan_stat *cs;
{
	register struct scsi *sc;
	register int iloop;
	register int cdb_bytes;
	register VOLATILE int dummy;
	register VOLATILE int statr;

	if (cs->comflg == CF_SET) {
		cs->comflg = CF_SEND;

		flush_fifo();

		sc = cs->sc;
		switch (sc->sc_opcode & CMD_TYPEMASK) {
		case CMD_T0:
			cdb_bytes = 6;
			break;

		case CMD_T1:
			cdb_bytes = 10;
			break;

		case CMD_T5:
			cdb_bytes = 12;
			break;

		default:
			cdb_bytes = 6;
			sc_intok2 |= Rb_FNC;
			break;
		}

		/*
		 * set Active pointers
		 */
		act_cmd_pointer = sc->sc_cdb.un_reserved;
		cs->act_trcnt = sc->sc_ctrnscnt;
		cs->act_point = sc->sc_cpoint;
		cs->act_tag = sc->sc_ctag;
		cs->act_offset = sc->sc_coffset;

	} else {
		cdb_bytes = 1;
		iloop = 0;
		do {
			dummy = sc_cmonr;
			DMAC_WAIT0;
			if ((dummy & SC_PMASK) != COM_OUT)
				return;
			statr = sc_statr;
			DMAC_WAIT0;
			if (statr & R0_MIRQ)
				return;
		} while ((dummy & R4_MREQ) == 0);
		statr = sc_statr;
		DMAC_WAIT0;
		if (statr & R0_MIRQ)
			return;
	}


	SET_CNT(cdb_bytes);
	SET_CMD(SCMD_TR_INFO|R0_TRBE);

	for (iloop = 0; iloop < cdb_bytes; iloop++) {
		do {
			dummy = sc_cmonr;
			DMAC_WAIT0;
			if ((dummy & SC_PMASK) != COM_OUT)
				return;
		} while ((dummy & R4_MREQ) == 0);
		statr = sc_statr;
		DMAC_WAIT0;
		if (statr & R0_MIRQ)
			return;
		sc_datr = *act_cmd_pointer++;
		do {
			dummy = sc_cmonr;
			DMAC_WAIT0;
		} while ((dummy & R4_MACK) != 0);
	}
}

#define GET_MIN_COUNT	127

/*
 *	SCSI message accept routine
 */
sc_min(cs)
	register struct sc_chan_stat *cs;
{
	register struct scsi *sc;
	register struct scsi_stat *ss;
	register VOLATILE int dummy;

	sc = cs->sc;
	ss = &scsi_stat;

	sc_intok2 = Rb_FNC|Rb_DCNT|Rb_SRST|Rb_PHC|Rb_SPE|Rb_RMSG;
	DMAC_WAIT0;

	if (min_flag == 1)
		flush_fifo();

	dummy = sc_cmonr;
	DMAC_WAIT0;
	if ((dummy & R4_MREQ) == 0) {
		printf("sc_min: !REQ cmonr=%x\n", dummy);
		print_scsi_stat();
		scsi_hardreset();
		return;
	}

    retry_cmd_issue:
	int_stat2 &= ~R3_FNC;
	SET_CMD(SCMD_TR_INFO);
	do {
		do {
			dummy = sc_statr;
			DMAC_WAIT0;
		} while (dummy & R0_CIP);
		GET_INTR(&int_stat1, &int_stat2);	/* clear interrupt */
	} while ((int_stat2 & R3_FNC) == 0);
	int_stat2 &= ~R3_FNC;

	dummy = sc_ffstr;
	if (dummy & R5_FIE) {
		DMAC_WAIT;
		dummy = sc_ffstr;
		DMAC_WAIT0;
		if (dummy & R5_FIE) {
			dummy = sc_statr;
			DMAC_WAIT0;
			if ((dummy & R0_INIT) == 0) {
				/*
				 * CXD1185 detect BSY false
				 */
				scsi_hardreset();
				return;
			}
		}
	}
	dummy = sc_datr;				/* get message byte */
	DMAC_WAIT0;

	if (min_cnt[cs->chan_num] == 0) {
		sc->sc_message = sc->sc_identify;
		if (dummy == MSG_EXTND) {
			/* Extended Message */
			min_cnt[cs->chan_num] = GET_MIN_COUNT;
			min_point[cs->chan_num] = sc->sc_param;
			bzero((caddr_t)sc->sc_param, 8);
			*min_point[cs->chan_num]++ = dummy;
		} else {
			switch ((dummy & MSG_IDENT)? MSG_IDENT : dummy) {

			case MSG_CCOMP:
				sc->sc_istatus |= INST_EP;
				break;

			case MSG_MREJ:
#ifndef NOT_SUPPORT_SYNCTR
				if (mout_flag[cs->chan_num] == MOUT_SYNC_TR)
					sync_tr[cs->chan_num] = 0;
#endif
				break;

			case MSG_IDENT:
			case MSG_RDP:
	ss->dma_stat = OFF;
	pad_start = 0;
				cs->comflg = OFF;
				/*
				 * restore the saved value to Active pointers
				 */
				act_cmd_pointer = sc->sc_cdb.un_reserved;
				cs->act_trcnt = sc->sc_ctrnscnt;
				cs->act_point = sc->sc_cpoint;
				cs->act_tag = sc->sc_ctag;
				cs->act_offset = sc->sc_coffset;
				break;

			case MSG_SDP:
				/*
				 * save Active pointers
				 */
				sc->sc_ctrnscnt = cs->act_trcnt;
				sc->sc_ctag = cs->act_tag;
				sc->sc_coffset = cs->act_offset;
				sc->sc_cpoint = cs->act_point;
				break;

			case MSG_DCNT:
				sc->sc_istatus |= INST_WR;
				ss->wrc++;
				break;

			default:
				sc->sc_message = MSG_MREJ;
				SET_CMD(SCMD_AST_ATN);
				printf("SCSI%d:sc_min() Unknown mes=0x%x, \n",
					cs->chan_num, dummy);
			}
		}
	} else {
		*min_point[cs->chan_num]++ = dummy;
		if (min_cnt[cs->chan_num] == GET_MIN_COUNT)
			min_cnt[cs->chan_num] = dummy;
		else
			min_cnt[cs->chan_num]--;
		if (min_cnt[cs->chan_num] <= 0) {
#ifdef ABORT_SYNCTR_MES_FROM_TARGET
			if ((sc->sc_param[2] == 0x01)
			    && (mout_flag[cs->chan_num] == MOUT_SYNC_TR)) {
#else
			if (sc->sc_param[2] == 0x01) { /*}*/
#endif
				register int i;
				/*
				 * receive Synchronous transfer message reply
				 *	calculate transfer period val
				 *	tpm * 4/1000 us = 4/16 * (tpv + 1)
				 */
#define	TPM2TPV(tpm)	(((tpm)*16 + 999) / 1000 - 1)
#ifndef NOT_SUPPORT_SYNCTR
				i = sc->sc_param[3];	/* get tpm */
				i = TPM2TPV(i) << 4;
				if (sc->sc_param[4] == 0)
					sync_tr[cs->chan_num] = 0;
				else
					sync_tr[cs->chan_num] = i | sc->sc_param[4];
#endif /* !NOT_SUPPORT_SYNCTR */
			} else {
				sc->sc_message = MSG_MREJ;
				SET_CMD(SCMD_AST_ATN);	/* assert ATN */
			}
		}
	}
	SET_CMD(SCMD_NGT_ACK);
}

/*
 *	SCSI message send routine
 */
int
sc_mout(cs)
	register struct sc_chan_stat *cs;
{
	register struct scsi *sc = cs->sc;
	register u_char *mp;
	register int cnt;
	register int iloop;
	register VOLATILE int dummy;
	VOLATILE int tmp;
	VOLATILE int tmp0;

	flush_fifo();

	if (mout_flag[cs->chan_num] == 0) {
		mout_flag[cs->chan_num] = MOUT_IDENTIFY;
		if (sc->sc_message != 0) {
			sc_intok2 = Rb_FNC|Rb_DCNT|Rb_SRST|Rb_PHC|Rb_SPE|Rb_RMSG;
			DMAC_WAIT0;
			if ((sc->sc_message == MSG_EXTND)
					&& (sc->sc_param[2] == 0x01)) {
				cnt = 5;
				mp = sc->sc_param;
				sc->sc_param[3] = MIN_TP;
				if (sc->sc_param[4] > MAX_OFFSET_BYTES)
					sc->sc_param[4] = MAX_OFFSET_BYTES;
				mout_flag[cs->chan_num] = MOUT_SYNC_TR;
			} else {
				cnt = 1;
				mp = &sc->sc_message;
			}

			SET_CNT(cnt);
			SET_CMD(SCMD_TR_INFO|R0_TRBE);
			sc_datr = sc->sc_identify;
			DMAC_WAIT0;
			for (iloop = 1; iloop < cnt; iloop++) {
				sc_datr = *mp++;
				DMAC_WAIT;
			}
			do {
				dummy = sc_cmonr;
				DMAC_WAIT0;
				if ((dummy & R4_MBSY) == 0)
					return;
				dummy = sc_statr;
				DMAC_WAIT0;
			} while (dummy & R0_CIP);

			tmp = 0;
			GET_INTR(&tmp0, &tmp);		/* clear interrupt */
			if ((tmp & R3_FNC) == 0) {
				(void) WAIT_STATR_BITSET(R0_MIRQ);
				GET_INTR(&tmp0, &tmp);	/* clear interrupt */
			}

			do {
				dummy = sc_cmonr;
				DMAC_WAIT0;
				if ((dummy & R4_MBSY) == 0)
					return;
			} while ((dummy & R4_MREQ) == 0);
			SET_CMD(SCMD_NGT_ATN);
			(void) WAIT_STATR_BITCLR(R0_CIP);
			GET_INTR(&tmp0, &tmp);		/* clear interrupt */

			dummy = sc_cmonr;
			DMAC_WAIT0;
			if ((dummy & R4_MREQ) == 0) {
				printf("sc_mout: !REQ cmonr=%x\n", dummy);
				print_scsi_stat();
				scsi_hardreset();
				return;
			}

			SET_CMD(SCMD_TR_INFO);
			sc_datr = *mp++;
			DMAC_WAIT0;
		} else {
			dummy = sc_cmonr;
			DMAC_WAIT0;
			if (dummy & R4_MATN) {
				SET_CMD(SCMD_NGT_ATN);
				(void) WAIT_STATR_BITCLR(R0_CIP);
				GET_INTR(&tmp0, &tmp);	/* clear interrupt */
			}

			iloop = 0;
			do {
				dummy = sc_cmonr;
				DMAC_WAIT0;
				if (iloop++ > CHECK_LOOP_CNT)
					break;
			} while ((dummy & R4_MREQ) == 0);
			SET_CMD(SCMD_TR_INFO);
			sc_datr = sc->sc_identify;
			DMAC_WAIT0;
		}
	} else {
		dummy = sc_cmonr;
		DMAC_WAIT0;
		if (dummy & R4_MATN) {
			SET_CMD(SCMD_NGT_ATN);
			(void) WAIT_STATR_BITCLR(R0_CIP);
			GET_INTR(&tmp0, &tmp);		/* clear interrupt */
		}

		dummy = sc_cmonr;
		DMAC_WAIT0;
		if ((dummy & R4_MREQ) == 0) {
			printf("sc_mout: !REQ cmonr=%x\n", dummy);
			print_scsi_stat();
			scsi_hardreset();
			return;
		}

		SET_CMD(SCMD_TR_INFO);
		sc_datr = sc->sc_message;
		DMAC_WAIT0;
	}
}

/*
 *	SCSI status accept routine
 */
sc_sin(cs)
	register VOLATILE struct sc_chan_stat *cs;
{
	register VOLATILE int dummy;
	register int iloop;

	flush_fifo();

	dummy = sc_cmonr;
	DMAC_WAIT0;
	if ((dummy & R4_MREQ) == 0) {
		printf("sc_sin: !REQ cmonr=%x\n", dummy);
		print_scsi_stat();
		scsi_hardreset();
		return;
	}

	sc_intok2 = Rb_FNC|Rb_DCNT|Rb_SRST|Rb_PHC|Rb_SPE|Rb_RMSG;
	DMAC_WAIT0;

	SET_CMD(SCMD_TR_INFO);

	(void) WAIT_STATR_BITCLR(R0_CIP);

	int_stat2 &= ~R3_FNC;
	iloop = 0;
	do {
		if (iloop++ > CHECK_LOOP_CNT)
			break;
		GET_INTR(&int_stat1, &int_stat2);	/* clear interrupt */
	} while ((int_stat2 & R3_FNC) == 0);
	int_stat2 &= ~R3_FNC;

	cs->sc->sc_tstatus = sc_datr;		/* get status byte */
	DMAC_WAIT0;
}

/*
 *	SCSI data in/out routine
 */
sc_dio(cs)
	register VOLATILE struct sc_chan_stat *cs;
{
	register VOLATILE struct scsi *sc;
	register struct scsi_stat *ss;
	register int i;
	register int pages;
	register u_int tag;
	register u_int pfn;
	VOLATILE int phase;

	sc = cs->sc;
	ss = &scsi_stat;

	sc_intok2 = Rb_FNC|Rb_DCNT|Rb_SRST|Rb_PHC|Rb_SPE;
	DMAC_WAIT0;

	if (cs->act_trcnt <= 0) {
		sc_dio_pad(cs);
		return;
	}

	switch (sc->sc_opcode) {

	case SCOP_READ:
	case SCOP_WRITE:
	case SCOP_EREAD:
	case SCOP_EWRITE:
		i = (cs->act_trcnt + sc->sc_bytesec -1) / sc->sc_bytesec;
		i *= sc->sc_bytesec;
		break;

	default:
		i = cs->act_trcnt;
		break;
	}

	SET_CNT(i);
	pad_cnt[cs->chan_num] = i - cs->act_trcnt;

	phase = sc_cmonr & SC_PMASK;
	DMAC_WAIT0;
	if (phase == DAT_IN) {
		if (sc_syncr == OFF) {
			DMAC_WAIT0;
			flush_fifo();
		}
	}

#if defined(mips) && defined(CPU_SINGLE)
	SET_CMD(SCMD_TR_INFO|R0_DMA|R0_TRBE);
#endif

#if defined(mips) && defined(CPU_SINGLE)
	dmac_gsel = CH_SCSI;
	dmac_ctrcl = (u_char)(cs->act_trcnt & 0xff);
	dmac_ctrcm = (u_char)((cs->act_trcnt >> 8) & 0xff);
	dmac_ctrch = (u_char)((cs->act_trcnt >> 16) & 0x0f);
	dmac_cofsh = (u_char)((cs->act_offset >> 8) & 0xf);
	dmac_cofsl = (u_char)(cs->act_offset & 0xff);
#endif
	tag = 0;

	if (sc->sc_map && (sc->sc_map->mp_pages > 0)) {
		/*
		 * Set DMAC map entry from map table
		 */
		pages = sc->sc_map->mp_pages;
		for (i = cs->act_tag; i < pages; i++) {
			if ((pfn = sc->sc_map->mp_addr[i]) == 0)
				panic("SCSI:sc_dma() zero entry");
#if defined(mips) && defined(CPU_SINGLE)
			dmac_gsel = CH_SCSI;
			dmac_ctag = (u_char)tag++;
			dmac_cmap = (u_short)pfn;
#endif
		}
#ifdef MAP_OVER_ACCESS
# if defined(mips) && defined(CPU_SINGLE)
		dmac_gsel = CH_SCSI;
		dmac_ctag = (u_char)tag++;
		dmac_cmap = (u_short)pfn;
# endif
#endif
	} else {
		/*
		 * Set DMAC map entry from logical address
		 */
		pfn = (u_int)vtophys(cs->act_point) >> PGSHIFT;
		pages = (cs->act_trcnt >> PGSHIFT) + 2;
		for (i = 0; i < pages; i++) {
#if defined(mips) && defined(CPU_SINGLE)
			dmac_gsel = CH_SCSI;
			dmac_ctag = (u_char)tag++;
			dmac_cmap = (u_short)pfn + i;
#endif
		}
	}

#if defined(mips) && defined(CPU_SINGLE)
	dmac_gsel = CH_SCSI;
	dmac_ctag = 0;
#endif

	if (phase == DAT_IN) {
		ss->dma_stat = SC_DMAC_RD;
#if defined(mips) && defined(CPU_SINGLE)
		/*
		 * auto pad flag is always on
		 */
		dmac_gsel = CH_SCSI;
		dmac_cctl = DM_MODE|DM_APAD;
		DMAC_WAIT;
		dmac_cctl = DM_MODE|DM_APAD|DM_ENABLE;
		DMAC_WAIT0;
#endif
	}
	else if (phase == DAT_OUT) {
		ss->dma_stat = SC_DMAC_WR;
#if defined(mips) && defined(CPU_SINGLE)
		dmac_gsel = CH_SCSI;
		dmac_cctl = DM_APAD;
		DMAC_WAIT;
		dmac_cctl = DM_APAD|DM_ENABLE;
		DMAC_WAIT0;
#endif
						/* DMAC start on mem->I/O */
	}
}

#define MAX_TR_CNT24	((1 << 24) -1)
sc_dio_pad(cs)
	register VOLATILE struct sc_chan_stat *cs;
{
	register VOLATILE int phase;
	register int dummy;

	if (cs->act_trcnt >= 0)
		return;
	pad_start = 1;

	SET_CNT(MAX_TR_CNT24);
	SET_CMD(SCMD_TR_PAD|R0_TRBE);
	dummy = sc_cmonr & SC_PMASK;
	DMAC_WAIT0;
	if (dummy == DAT_IN)
		dummy = sc_datr;		/* get data */
	else
		sc_datr = 0;			/* send data */
}

print_scsi_stat()
{
	register struct scsi_stat *ss;
	register VOLATILE int i;
	int dummy;

	ss = &scsi_stat;
	printf("ipc=%d wrc=%d wbc=%d\n", ss->ipc, ss->wrc, ss->wbc);
}

/*
 *	return 0 if it was done.  Or retun TRUE if it is busy.
 */
sc_busy(chan)
	register int chan;
{
	return ((int)chan_stat[chan].sc);
}


/*
 *	append channel into Waiting Bus_free queue
 */
append_wb(cs)
	register VOLATILE struct sc_chan_stat *cs;
{
	register int s;

	s = splclock();			/* inhibit process switch */
	if (wbq_actf == NULL)
		wbq_actf = cs;
	else
		wbq_actl->wb_next = cs;
	wbq_actl = cs;
	cs->sc->sc_istatus = INST_WAIT;
	scsi_stat.wbc++;
	splx(s);
}

/*
 *	get channel from Waiting Bus_free queue
 */
get_wb_chan()
{
	register int s;
	register int chan;

	s = splclock();			/* inhibit process switch */
	if (wbq_actf == NULL) {
		chan = -1;
	} else {
		chan = wbq_actf->chan_num;
		if ((chan < 0) || (chan >= NTARGET) || (chan == SC_OWNID))
			chan = -1;
	}
	splx(s);
	return (chan);
}

/*
 *	release channel from Waiting Bus_free queue
 */
release_wb()
{
	register VOLATILE struct sc_chan_stat *cs;
	register int s;
	int error;

	s = splclock();			/* inhibit process switch */
	error = 0;
	if (wbq_actf == NULL) {
		error = -1;
	} else {
		cs = wbq_actf;
		wbq_actf = cs->wb_next;
		cs->wb_next = NULL;
		if (wbq_actl == cs)
			wbq_actl = NULL;
		cs->sc->sc_istatus &= ~INST_WAIT;
		scsi_stat.wbc--;
	}
	splx(s);
	return (error);
}

adjust_transfer(cs)
	register struct sc_chan_stat *cs;
{
	register struct scsi *sc;
	register struct scsi_stat *ss;
	register VOLATILE u_int remain_cnt;
	register u_int offset;
	u_int sent_byte;

	sc = cs->sc;
	ss = &scsi_stat;

	if (pad_start) {
		pad_start = 0;
		remain_cnt = 0;
	} else {
# if defined(mips) && defined(CPU_SINGLE)
		remain_cnt = GET_CNT();
		remain_cnt -= pad_cnt[cs->chan_num];
		if (ss->dma_stat == SC_DMAC_WR) {
			/*
			 * adjust counter in the FIFO
			 */
			remain_cnt += sc_ffstr & R5_FIFOREM;
		}
# endif
	}

	sent_byte = sc->sc_ctrnscnt - remain_cnt;
	cs->act_trcnt = remain_cnt;

	offset = sc->sc_coffset + sent_byte;
	cs->act_tag += (offset >> PGSHIFT);
	cs->act_offset = offset & PGOFSET;
	if ((sc->sc_map == NULL) || (sc->sc_map->mp_pages <= 0))
		cs->act_point += sent_byte;
}
