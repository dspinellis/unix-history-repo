/*
 *	@(#)scb.h	7.1 (Berkeley) %G%
 */

/*
 * System control block.
 */
struct	scb {
	int	(*scb_vec0)();		/* 000: reserved */
	int	(*scb_powfail)();	/* 001: power failure */
	int	(*scb_doadump)();	/* 002: power restore */
	int	(*scb_vec3)();		/* 003: reserved */
	int	(*scb_vec4)();		/* 004: reserved */
	int	(*scb_vec5)();		/* 005: reserved */
	int	(*scb_vec6)();		/* 006: reserved */
	int	(*scb_hardclock)();	/* 007: interval timer (clock) */
	int	(*scb_vec8)();		/* 008: reserved */
	int	(*scb_vec9)();		/* 009: reserved */
	int	(*scb_cnrint)();	/* 00a: console receive */
	int	(*scb_cnxint)();	/* 00b: console transmit */
	int	(*scb_rmtrint)();	/* 00c: remote line receive */
	int	(*scb_rmtxint)();	/* 00d: remote line transmit */
	int	(*scb_vec14)();		/* 00e: reserved */
	int	(*scb_vec15)();		/* 00f: reserved */
	int	(*scb_softint[15])();	/* 010: software ints (ipl 1f-1) */
	int	(*scb_vec31)();		/* 01f: reserved */
	int	(*scb_buserr)();	/* 020: bus error */
	int	(*scb_vec33)();		/* 021: reserved */
	int	(*scb_vec34)();		/* 022: reserved */
	int	(*scb_vec35)();		/* 023: reserved */
	int	(*scb_vec36)();		/* 024: reserved */
	int	(*scb_vec37)();		/* 025: reserved */
	int	(*scb_vec38)();		/* 026: reserved */
	int	(*scb_vec39)();		/* 027: reserved */
	int	(*scb_vec40)();		/* 028: reserved */
	int	(*scb_vec41)();		/* 029: reserved */
	int	(*scb_vec42)();		/* 02a: reserved */
	int	(*scb_syscall)();	/* 02b: system call (kcall) */
	int	(*scb_privinflt)();	/* 02c: privileged/reserved inst */ 
	int	(*scb_respoflt)();	/* 02d: reserved operand */
	int	(*scb_resadflt)();	/* 02e: reserved addressing mode */
	int	(*scb_protflt)();	/* 02f: access control violation */
	int	(*scb_transflt)();	/* 030: translation not valid */
	int	(*scb_kspnotval)();	/* 031: kernel stack invalid */
	int	(*scb_tracep)();	/* 032: trace trap */
	int	(*scb_bptflt)();	/* 033: breakpoint */
	int	(*scb_arithtrap)();	/* 034: arithmetic exception */
	int	(*scb_alignflt)();	/* 035: alignment fault */
	int	(*scb_sfexcep)();	/* 036: system forced exception */
	int	(*scb_fpm)();		/* 037: floating point emulation */
	int	(*scb_vec56)();		/* 038: reserved */
	int	(*scb_vec57)();		/* 039: reserved */
	int	(*scb_vec58)();		/* 03a: reserved */
	int	(*scb_vec59)();		/* 03b: reserved */
	int	(*scb_vec60)();		/* 03c: reserved */
	int	(*scb_vec61)();		/* 03d: reserved */
	int	(*scb_vec62)();		/* 03e: reserved */
	int	(*scb_vec63)();		/* 03f: reserved */
	int	(*scb_devint[191])();	/* 040: device vectors */
};

#define	SCB_LASTIV	(sizeof (((struct scb *)0)->scb_devint) / \
    sizeof (((struct scb *)0)->scb_devint[0]))

#ifdef KERNEL
extern	struct scb scb;
#endif
