/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.h	8.2 (Berkeley) %G%
 */

/*
 * Definitions of device driver entry switches
 */

struct buf;
struct proc;
struct tty;
struct uio;
struct vnode;

struct bdevsw {
	int	(*d_open)	__P((dev_t dev, int oflags, int devtype,
				     struct proc *p));
	int	(*d_close)	__P((dev_t dev, int fflag, int devtype,
				     struct proc *p));
	int	(*d_strategy)	__P((struct buf *bp));
	int	(*d_ioctl)	__P((dev_t dev, int cmd, caddr_t data,
				     int fflag, struct proc *p));
	int	(*d_dump)	();	/* parameters vary by architecture */
	int	(*d_psize)	__P((dev_t dev));
	int	d_flags;
};

#ifdef KERNEL
extern struct bdevsw bdevsw[];
#endif

struct cdevsw {
	int	(*d_open)	__P((dev_t dev, int oflags, int devtype,
				     struct proc *p));
	int	(*d_close)	__P((dev_t dev, int fflag, int devtype,
				     struct proc *));
	int	(*d_read)	__P((dev_t dev, struct uio *uio, int ioflag));
	int	(*d_write)	__P((dev_t dev, struct uio *uio, int ioflag));
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
extern struct cdevsw cdevsw[];

/* symbolic sleep message strings */
extern char devopn[], devio[], devwait[], devin[], devout[];
extern char devioc[], devcls[];
#endif

struct linesw {
	int	(*l_open)	__P((dev_t dev, struct tty *tp));
	int	(*l_close)	__P((struct tty *tp, int flag));
	int	(*l_read)	__P((struct tty *tp, struct uio *uio,
				     int flag));
	int	(*l_write)	__P((struct tty *tp, struct uio *uio,
				     int flag));
	int	(*l_ioctl)	__P((struct tty *tp, int cmd, caddr_t data,
				     int flag, struct proc *p));
	int	(*l_rint)	__P((int c, struct tty *tp));
	int	(*l_start)	__P((struct tty *tp));
	int	(*l_modem)	__P((struct tty *tp, int flag));
};

#ifdef KERNEL
extern struct linesw linesw[];
#endif

struct swdevt {
	dev_t	sw_dev;
	int	sw_flags;
	int	sw_nblks;
	struct	vnode *sw_vp;
#ifdef SECSIZE
	int	sw_blksize;
	int	sw_bshift;
#endif SECSIZE
};
#define	SW_FREED	0x01
#define	SW_SEQUENTIAL	0x02
#define sw_freed	sw_flags	/* XXX compat */

#ifdef KERNEL
extern struct swdevt swdevt[];
#endif
