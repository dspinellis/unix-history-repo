/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)frame.h	5.2 (Berkeley) %G%
 */

/*
 * System stack frames.
 */

/*
 * Exception/Trap Stack Frame
 */

struct trapframe {
	int	tf_es;
	int	tf_ds;
	int	tf_edi;
	int	tf_esi;
	int	tf_ebp;
	int	tf_isp;
	int	tf_ebx;
	int	tf_edx;
	int	tf_ecx;
	int	tf_eax;
	int	tf_trapno;
	/* below portion defined in 386 hardware */
	int	tf_err;
	int	tf_eip;
	int	tf_cs;
	int	tf_eflags;
	/* below only when transitting rings (e.g. user to kernel) */
	int	tf_esp;
	int	tf_ss;
};

/* Interrupt stack frame */

struct intrframe {
	int	if_vec;
	int	if_ppl;
	int	if_es;
	int	if_ds;
	int	if_edi;
	int	if_esi;
	int	if_ebp;
	int	:32;
	int	if_ebx;
	int	if_edx;
	int	if_ecx;
	int	if_eax;
	int	:32;		/* for compat with trap frame - trapno */
	int	:32;		/* for compat with trap frame - err */
	/* below portion defined in 386 hardware */
	int	if_eip;
	int	if_cs;
	int	if_eflags;
	/* below only when transitting rings (e.g. user to kernel) */
	int	if_esp;
	int	if_ss;
};

/*
 * Call Gate/System Call Stack Frame
 */

struct syscframe {
	int	sf_edi;
	int	sf_esi;
	int	sf_ebp;
	int	:32;		/* redundant save of isp */
	int	sf_ebx;
	int	sf_edx;
	int	sf_ecx;
	int	sf_eax;
	int	sf_eflags;
	/* below portion defined in 386 hardware */
/*	int	sf_args[N]; 	/* if call gate copy args enabled!*/
	int	sf_eip;
	int	sf_cs;
	/* below only when transitting rings (e.g. user to kernel) */
	int	sf_esp;
	int	sf_ss;
};
