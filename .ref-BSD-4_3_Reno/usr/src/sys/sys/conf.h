/*
 * Copyright (c) 1982, 1986, 1990 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)conf.h	7.5 (Berkeley) 4/4/90
 */

/*
 * Declaration of block device
 * switch. Each entry (row) is
 * the only link between the
 * main unix code and the driver.
 * The initialization of the
 * device switches is in the
 * file conf.c.
 */
struct bdevsw
{
	int	(*d_open)();
	int	(*d_close)();
	int	(*d_strategy)();
	int	(*d_ioctl)();
	int	(*d_dump)();
	int	(*d_psize)();
	int	d_flags;
};
#ifdef KERNEL
struct	bdevsw bdevsw[];
#endif

/*
 * Character device switch.
 */
struct cdevsw
{
	int	(*d_open)();
	int	(*d_close)();
	int	(*d_read)();
	int	(*d_write)();
	int	(*d_ioctl)();
	int	(*d_stop)();
	int	(*d_reset)();
	struct tty *d_ttys;
	int	(*d_select)();
	int	(*d_mmap)();
	int	(*d_strategy)();
};
#ifdef KERNEL
struct	cdevsw cdevsw[];

/* symbolic sleep message strings */
extern	 char devopn[], devio[], devwait[];
extern	 char devin[], devout[], devioc[], devcls[];
#endif

/*
 * tty line control switch.
 */
struct linesw
{
	int	(*l_open)();
	int	(*l_close)();
	int	(*l_read)();
	int	(*l_write)();
	int	(*l_ioctl)();
	int	(*l_rint)();
	int	(*l_rend)();
	int	(*l_meta)();
	int	(*l_start)();
	int	(*l_modem)();
};
#ifdef KERNEL
struct	linesw linesw[];
#endif

/*
 * Swap device information
 */
struct swdevt
{
	dev_t	sw_dev;
	int	sw_freed;
	int	sw_nblks;
	struct vnode *sw_vp;
};
#ifdef KERNEL
struct	swdevt swdevt[];
#endif
