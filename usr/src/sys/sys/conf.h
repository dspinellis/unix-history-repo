/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.h	7.8 (Berkeley) %G%
 */

/*
 * Definitions of device driver entry switches
 */

#ifdef __STDC__
struct tty;
#endif

struct bdevsw {
	int	(*d_open)	__P((dev_t dev, int oflags, int devtype,
				     struct proc *p));
	int	(*d_close)	__P((dev_t dev, int fflag, int devtype,
				     struct proc *));
	int	(*d_strategy)	__P((struct buf *bp));
	int	(*d_ioctl)	__P((dev_t dev, int cmd, caddr_t data,
				     int fflag, struct proc *p));
	int	(*d_dump)	__P((dev_t dev));
	int	(*d_psize)	__P((dev_t dev));
	int	d_flags;
};

#ifdef KERNEL
struct bdevsw bdevsw[];
#endif

struct cdevsw {
	int	(*d_open)	__P((dev_t dev, int oflags, int devtype,
				     struct proc *p));
	int	(*d_close)	__P((dev_t dev, int fflag, int devtype,
				     struct proc *));
	int	(*d_read)	__P((dev_t dev, struct uio *uio, int ioflag,
				     struct proc *p));
	int	(*d_write)	__P((dev_t dev, struct uio *uio, int ioflag,
				     struct proc *p));
	int	(*d_ioctl)	__P((dev_t dev, int cmd, caddr_t data,
				     int fflag, struct proc *p));
	int	(*d_stop)	__P((struct tty *tp, int rw));
	int	(*d_reset)	__P((int uban));	/* XXX */
	struct	tty *d_ttys;
	int	(*d_select)	__P((dev_t dev, int which, struct proc *p));
	int	(*d_mmap)	__P(());
	int	(*d_strategy)	__P((struct buf *bp));
};

#ifdef KERNEL
struct cdevsw cdevsw[];

/* symbolic sleep message strings */
extern char devopn[], devio[], devwait[], devin[], devout[];
extern char devioc[], devcls[];
#endif

struct linesw {
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
struct linesw linesw[];
#endif

struct swdevt {
	dev_t	sw_dev;
	int	sw_freed;
	int	sw_nblks;
	struct	vnode *sw_vp;
#ifdef SECSIZE
	int	sw_blksize;
	int	sw_bshift;
#endif SECSIZE
};

#ifdef KERNEL
struct swdevt swdevt[];
#endif
