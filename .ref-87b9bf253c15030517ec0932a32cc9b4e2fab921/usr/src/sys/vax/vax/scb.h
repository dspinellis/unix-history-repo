/*	scb.h	4.3	81/02/21	*/

/*
 * VAX System control block layout
 */

struct scb {
	int	(*scb_stray)();		/* reserved */
	int	(*scb_machchk)();	/* machine chack */
	int	(*scb_kspinval)();	/* KSP invalid */
	int	(*scb_powfail)();	/* power fail */
	int	(*scb_resinstr)();	/* reserved instruction */
	int	(*scb_custinst)();	/* XFC instr */
	int	(*scb_resopnd)();	/* reserved operand */
	int	(*scb_resaddr)();	/* reserved addr mode */
	int	(*scb_acv)();		/* access control violation */
	int	(*scb_tnv)();		/* translation not valid */
	int	(*scb_tracep)();	/* trace pending */
	int	(*scb_bpt)();		/* breakpoint instr */
	int	(*scb_compat)();	/* compatibility mode fault */
	int	(*scb_arith)();		/* arithmetic fault */
	int	(*scb_stray2)();
	int	(*scb_stray3)();
	int	(*scb_chmk)();		/* CHMK instr */
	int	(*scb_chme)();		/* CHME instr */
	int	(*scb_chms)();		/* CHMS instr */
	int	(*scb_chmu)();		/* CHMU instr */
	int	(*scb_sbisilo)();	/* SBI silo compare */
	int	(*scb_cmrd)();		/* corrected mem read data */
	int	(*scb_sbialert)();	/* SBI alert */
	int	(*scb_sbiflt)();	/* SBI fault */
	int	(*scb_wtime)();		/* memory write timeout */
	int	(*scb_stray4[8])();
	int	(*scb_soft[15])();	/* software interrupt */
	int	(*scb_timer)();		/* interval timer interrupt */
	int	(*scb_stray5[7])();
	int	(*scb_stray6[4])();
	int	(*scb_csdr)();		/* console storage receive */
	int	(*scb_csdx)();		/* console storage transmit */
	int	(*scb_ctr)();		/* console terminal receive */
	int	(*scb_ctx)();		/* console terminal transmit */
	int	(*scb_ipl14[16])();	/* device interrupts IPL 14 */
	int	(*scb_ipl15[16])();	/*   "		"    IPL 15 */
	int	(*scb_ipl16[16])();	/*   "		"    IPL 16 */
	int	(*scb_ipl17[16])();	/*   "		"    IPL 17 */
	int	(*scb_ubaint[128])();	/* Unibus device intr */
};

#ifdef KERNEL
extern	struct scb scb;
/* scb.scb_ubaint is the same as UNIvec */
#endif

#define	scbentry(f, how)		((int (*)())(((int)f)+how))

#define	SCB_KSTACK	0
#define	SCB_ISTACK	1
#define	SCB_WCS		2
#define	SCB_HALT	3
