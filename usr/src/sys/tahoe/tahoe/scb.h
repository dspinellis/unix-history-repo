/*	scb.h	1.1	86/01/05	*/

/*
 * System control block.
 */
struct	scb {
	int	(*scb_vec0)();		/* 000: reserved */
	int	(*scb_powfail)();	/* 004: power failure */
	int	(*scb_doadump)();	/* 008: power restore */
	int	(*scb_vec3)();		/* 00c: reserved */
	int	(*scb_vec4)();		/* 010: reserved */
	int	(*scb_vec5)();		/* 014: reserved */
	int	(*scb_vec6)();		/* 018: reserved */
	int	(*scb_hardclock)();	/* 01c: interval timer (clock) */
	int	(*scb_vec8)();		/* 020: reserved */
	int	(*scb_vec9)();		/* 024: reserved */
	int	(*scb_cnrint)();	/* 028: console receive */
	int	(*scb_cnxint)();	/* 02c: console transmit */
	int	(*scb_rmtrint)();	/* 030: remote line receive */
	int	(*scb_rmtxint)();	/* 034: remote line transmit */
	int	(*scb_vec14)();		/* 038: reserved */
	int	(*scb_vec15)();		/* 03c: reserved */
	int	(*scb_softint[15])();	/* 040: software ints (ipl 1f-1) */
	int	(*scb_vec31)();		/* 07c: reserved */
	int	(*scb_buserr)();	/* 080: bus error */
	int	(*scb_vec33)();		/* 084: reserved */
	int	(*scb_vec34)();		/* 088: reserved */
	int	(*scb_vec35)();		/* 08c: reserved */
	int	(*scb_vec36)();		/* 090: reserved */
	int	(*scb_vec37)();		/* 094: reserved */
	int	(*scb_vec38)();		/* 098: reserved */
	int	(*scb_vec39)();		/* 09c: reserved */
	int	(*scb_vec40)();		/* 0a0: reserved */
	int	(*scb_vec41)();		/* 0a4: reserved */
	int	(*scb_vec42)();		/* 0a8: reserved */
	int	(*scb_syscall)();	/* 0ac: system call (kcall) */
	int	(*scb_privinflt)();	/* 0b0: privileged/reserved inst */ 
	int	(*scb_respoflt)();	/* 0b4: reserved operand */
	int	(*scb_resadflt)();	/* 0b8: reserved addressing mode */
	int	(*scb_protflt)();	/* 0bc: access control violation */
	int	(*scb_transflt)();	/* 0c0: translation not valid */
	int	(*scb_kspnotval)();	/* 0c4: kernel stack invalid */
	int	(*scb_tracep)();	/* 0c8: trace trap */
	int	(*scb_bptflt)();	/* 0cc: breakpoint */
	int	(*scb_arithtrap)();	/* 0d0: arithmetic exception */
	int	(*scb_alignflt)();	/* 0d4: alignment fault */
	int	(*scb_sfexcep)();	/* 0d8: system forced exception */
	int	(*scb_fpm)();		/* 0dc: floating point emulation */
	int	(*scb_vec56)();		/* 0e0: reserved */
	int	(*scb_vec57)();		/* 0e4: reserved */
	int	(*scb_vec58)();		/* 0e8: reserved */
	int	(*scb_vec59)();		/* 0ec: reserved */
	int	(*scb_vec60)();		/* 0f0: reserved */
	int	(*scb_vec61)();		/* 0f4: reserved */
	int	(*scb_vec62)();		/* 0f8: reserved */
	int	(*scb_vec63)();		/* 0fc: reserved */
	int	(*scb_devint[192])();	/* 100: device vectors */
};
