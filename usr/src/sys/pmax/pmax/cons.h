/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cons.h	7.1 (Berkeley) %G%
 */

struct consdev {
	int	cn_disabled;	/* If true use rom I/O routines */
	int	cn_screen;	/* True iff console is a screen/keyboard */
	dev_t	cn_dev;		/* major/minor of device */
	struct	pmax_fb *cn_fb;	/* Frame buffer struct for console screen */
	int	(*cn_getc)();	/* kernel getchar interface */
	int	(*cn_kbdgetc)(); /* kernel keyboard getchar interface */
	void	(*cn_putc)();	/* kernel putchar interface */
	struct	tty *cn_tp;	/* tty structure for console device */
};

/*
 * Major device numbers for possible console devices. XXX
 */
#define	DTOPDEV		15
#define	DCDEV		16
#define	SCCDEV		17
