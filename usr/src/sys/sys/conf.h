/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.h	7.6 (Berkeley) %G%
 */

struct bdevsw {
	int (*d_open)();
	int (*d_close)();
	int (*d_strategy)();
	int (*d_ioctl)();
	int (*d_dump)();
	int (*d_psize)();
	int d_flags;
};

#ifdef KERNEL
struct bdevsw bdevsw[];
#endif

struct cdevsw {
	int (*d_open)();
	int (*d_close)();
	int (*d_read)();
	int (*d_write)();
	int (*d_ioctl)();
	int (*d_stop)();
	int (*d_reset)();
	struct tty *d_ttys;
	int (*d_select)();
	int (*d_mmap)();
	int (*d_strategy)();
};

#ifdef KERNEL
struct cdevsw cdevsw[];

/* symbolic sleep message strings */
extern char devopn[], devio[], devwait[], devin[], devout[];
extern char devioc[], devcls[];
#endif

struct linesw {
	int (*l_open)();
	int (*l_close)();
	int (*l_read)();
	int (*l_write)();
	int (*l_ioctl)();
	int (*l_rint)();
	int (*l_rend)();
	int (*l_meta)();
	int (*l_start)();
	int (*l_modem)();
};

#ifdef KERNEL
struct linesw linesw[];
#endif

struct swdevt {
	dev_t sw_dev;
	int sw_freed;
	int sw_nblks;
	struct vnode *sw_vp;
#ifdef SECSIZE
	int	sw_blksize;
	int	sw_bshift;
#endif SECSIZE
};

#ifdef KERNEL
struct swdevt swdevt[];
#endif
