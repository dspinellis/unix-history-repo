/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.5 (Berkeley) %G%
 *
 * from: $Header: conf.c,v 1.15 93/05/05 09:43:29 torek Exp $ (LBL)
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/ioctl.h>
#include <sys/vnode.h>
#include <sys/tty.h>
#include <sys/conf.h>

int	rawread		__P((dev_t, struct uio *, int));
int	rawwrite	__P((dev_t, struct uio *, int));
int	swstrategy	__P((struct buf *));
int	ttselect	__P((dev_t, int, struct proc *));

#define	dev_type_open(n)	int n __P((dev_t, int, int, struct proc *))
#define	dev_type_close(n)	int n __P((dev_t, int, int, struct proc *))
#define	dev_type_strategy(n)	int n __P((struct buf *))
#define	dev_type_ioctl(n) \
	int n __P((dev_t, int, caddr_t, int, struct proc *))

/* bdevsw-specific types */
/*	dev_type_dump(n)	int n __P((dev_t, daddr_t, caddr_t, int))*/
#define	dev_type_dump(n)	int n ()
#define	dev_type_size(n)	int n __P((dev_t))

/* error/nullop functions */
#define	error_open	((dev_type_open((*))) enodev)
#define	error_close	((dev_type_close((*))) enodev)
#define	error_ioctl	((dev_type_ioctl((*))) enodev)
#define	error_dump	((dev_type_dump((*))) enodev)

#define	null_open	((dev_type_open((*))) nullop)
#define	null_close	((dev_type_close((*))) nullop)

#define	dev_decl(n,t)	__CONCAT(dev_type_,t)(__CONCAT(n,t))
#define	dev_init(c,n,t) \
	(c > 0 ? __CONCAT(n,t) : (__CONCAT(dev_type_,t)((*))) enxio)

/* bdevsw-specific initializations */
#define	dev_size_init(c,n)	(c > 0 ? __CONCAT(n,size) : 0)

#define	bdev_decl(n) \
	dev_decl(n,open); dev_decl(n,close); dev_decl(n,strategy); \
	dev_decl(n,ioctl); dev_decl(n,dump); dev_decl(n,size)

#define	bdev_disk_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), \
	dev_init(c,n,strategy), dev_init(c,n,ioctl), \
	dev_init(c,n,dump), dev_size_init(c,n), 0 }

#define	bdev_tape_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), \
	dev_init(c,n,strategy), dev_init(c,n,ioctl), \
	dev_init(c,n,dump), 0, B_TAPE }

#define	bdev_swap_init() { \
	error_open, error_close, swstrategy, error_ioctl, error_dump, 0, 0 }

#define	bdev_notdef()	bdev_tape_init(0,no)
bdev_decl(no);	/* dummy declarations */

#include "sd.h"

bdev_decl(sd);

struct bdevsw	bdevsw[] =
{
	bdev_notdef(),		/* 0 */
	bdev_notdef(),		/* 1 */
	bdev_notdef(),		/* 2 */
	bdev_swap_init(),	/* 3 */
	bdev_notdef(),		/* 4 */
	bdev_notdef(),		/* 5 */
	bdev_notdef(),		/* 6 */
	bdev_disk_init(NSD,sd),	/* 7: scsi disk */
};

int	nblkdev = sizeof (bdevsw) / sizeof (bdevsw[0]);

/* cdevsw-specific types */
#define	dev_type_read(n)	int n __P((dev_t, struct uio *, int))
#define	dev_type_write(n)	int n __P((dev_t, struct uio *, int))
#define	dev_type_select(n)	int n __P((dev_t, int, struct proc *))
#define	dev_type_map(n)		int n __P(())

#define	error_read	((dev_type_read((*))) enodev)
#define	error_write	((dev_type_write((*))) enodev)
#define	error_select	((dev_type_select((*))) enodev)

#define	cdev_decl(n) \
	dev_decl(n,open); dev_decl(n,close); dev_decl(n,read); \
	dev_decl(n,write); dev_decl(n,ioctl); dev_decl(n,select); \
	dev_decl(n,map); dev_decl(n,strategy); \
	extern struct tty __CONCAT(n,_tty)[]

#define	dev_tty_init(c,n)	(c > 0 ? __CONCAT(n,_tty) : 0)

/* open, close, read, write, ioctl, strategy */
#define	cdev_disk_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), rawread, rawwrite, \
	dev_init(c,n,ioctl), 0, 0, 0, seltrue, 0, dev_init(c,n,strategy) }

/* open, close, read, write, ioctl, strategy */
#define	cdev_tape_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), dev_init(c,n,read), \
	dev_init(c,n,write), dev_init(c,n,ioctl), 0, 0, 0, seltrue, 0, \
	dev_init(c,n,strategy) }

/* open, close, read, write, ioctl, tty */
#define	cdev_tty_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), dev_init(c,n,read), \
	dev_init(c,n,write), dev_init(c,n,ioctl), 0, 0, dev_tty_init(c,n), \
	ttselect, 0, 0 }

/* open, close, read, write, ioctl, select */
#define	cdev_gen_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), dev_init(c,n,read), \
	dev_init(c,n,write), dev_init(c,n,ioctl), 0, 0, 0, \
	dev_init(c,n,select), 0, 0 }

/* open, close, ioctl, mmap */
#define	cdev_fb_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), error_read, error_write, \
	dev_init(c,n,ioctl), 0, 0, 0, seltrue, dev_init(c,n,map), 0 }

#define	cdev_notdef() { \
	error_open, error_close, error_read, error_write, \
	error_ioctl, 0, 0, 0, seltrue, 0, 0 }

cdev_decl(no);			/* dummy declarations */

cdev_decl(cn);
/* open, close, read, write, ioctl, select -- XXX should be a tty */
extern struct tty cons;
#define	cdev_cn_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), dev_init(c,n,read), \
	dev_init(c,n,write), dev_init(c,n,ioctl), 0, 0, &cons, \
	dev_init(c,n,select), 0, 0 }

cdev_decl(ctty);
/* open, read, write, ioctl, select -- XXX should be a tty */
#define	cdev_ctty_init(c,n) { \
	dev_init(c,n,open), null_close, dev_init(c,n,read), \
	dev_init(c,n,write), dev_init(c,n,ioctl), 0, 0, 0, \
	dev_init(c,n,select), 0, 0 }

dev_type_read(mmrw);
/* read/write */
#define	cdev_mm_init(c,n) { \
	null_open, null_close, mmrw, mmrw, error_ioctl, 0, 0, 0, \
	seltrue, 0, 0 }

/* read, write, strategy */
#define	cdev_swap_init(c,n) { \
	null_open, null_close, rawread, rawwrite, error_ioctl, 0, 0, 0, \
	seltrue, 0, dev_init(c,n,strategy) }

#include "pty.h"
#define	pts_tty		pt_tty
#define	ptsioctl	ptyioctl
cdev_decl(pts);
#define	ptc_tty		pt_tty
#define	ptcioctl	ptyioctl
cdev_decl(ptc);

/* open, close, read, write, ioctl, tty, select */
#define	cdev_ptc_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), dev_init(c,n,read), \
	dev_init(c,n,write), dev_init(c,n,ioctl), 0, 0, dev_tty_init(c,n), \
	dev_init(c,n,select), 0, 0 }

cdev_decl(log);
/* open, close, read, ioctl, select -- XXX should be a generic device */
#define	cdev_log_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), dev_init(c,n,read), \
	error_write, dev_init(c,n,ioctl), 0, 0, 0, dev_init(c,n,select), 0, 0 }

dev_type_open(fdopen);
/* open */
#define	cdev_fd_init(c,n) { \
	dev_init(c,n,open), error_close, error_read, error_write, \
	error_ioctl, 0, 0, 0, error_select, 0, 0 }

#include "zs.h"
cdev_decl(zs);

cdev_decl(kbd);
cdev_decl(ms);
cdev_decl(fb);

#include "bwtwo.h"
cdev_decl(bwtwo);

#include "cgthree.h"
cdev_decl(cgthree);

#include "bsdaudio.h"
cdev_decl(audio);

cdev_decl(openprom);
/* open, close, ioctl */
#define	cdev_openprom_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), error_read, error_write, \
	dev_init(c,n,ioctl), 0, 0, 0, error_select, 0, 0 }

#include "bpfilter.h"
cdev_decl(bpf);
/* open, close, read, write, ioctl, select -- XXX should be generic device */
#define	cdev_bpf_init(c,n) { \
	dev_init(c,n,open), dev_init(c,n,close), dev_init(c,n,read), \
	dev_init(c,n,write), dev_init(c,n,ioctl), 0, 0, 0, \
	dev_init(c,n,select), 0, 0 }

/* prototype sun-equivalent cdevsw[] */
struct cdevsw	cdevsw[] =
{
	cdev_cn_init(1,cn),		/* 0: virtual console */
	cdev_notdef(),			/* 1 */
	cdev_ctty_init(1,ctty),		/* 2: controlling terminal */
	cdev_mm_init(1,mm),		/* 3: /dev/{null,mem,kmem,...} */
	cdev_notdef(),			/* 4 */
	cdev_notdef(),			/* 5 */
	cdev_notdef(),			/* 6 */
	cdev_swap_init(1,sw),		/* 7: /dev/drum (swap pseudo-device) */
	cdev_notdef(),			/* 8 */
	cdev_notdef(),			/* 9 */
	cdev_notdef(),			/* 10 */
	cdev_notdef(),			/* 11 */
	cdev_tty_init(NZS,zs),		/* 12: zs serial */
	cdev_gen_init(1,ms),		/* 13: /dev/mouse */
	cdev_notdef(),			/* 14 */
	cdev_notdef(),			/* 15: sun /dev/winNNN */
	cdev_log_init(1,log),		/* 16: /dev/klog */
	cdev_disk_init(NSD,sd),		/* 17: scsi disk */
	cdev_notdef(),			/* 18: should be scsi tape */
	cdev_notdef(),			/* 19 */
	cdev_ptc_init(NPTY,ptc),	/* 20: pseudo-tty master */
	cdev_tty_init(NPTY,pts),	/* 21: pseudo-tty slave */
	cdev_fb_init(1,fb),		/* 22: /dev/fb indirect driver */
	cdev_notdef(),			/* 23 */
	cdev_fd_init(1,fd),		/* 24: /dev/std{in,out,err} */
	cdev_notdef(),			/* 25 */
	cdev_notdef(),			/* 26 */
	cdev_fb_init(NBWTWO,bwtwo),	/* 27: /dev/bwtwo */
	cdev_notdef(),			/* 28 */
	cdev_gen_init(1,kbd),		/* 29: /dev/kbd */
	cdev_notdef(),			/* 30 */
	cdev_notdef(),			/* 31: should be /dev/cgtwo */
	cdev_notdef(),			/* 32: should be /dev/gpone */
	cdev_notdef(),			/* 33 */
	cdev_notdef(),			/* 34 */
	cdev_notdef(),			/* 35 */
	cdev_notdef(),			/* 36 */
	cdev_notdef(),			/* 37 */
	cdev_notdef(),			/* 38 */
	cdev_notdef(),			/* 39 */
	cdev_notdef(),			/* 40 */
	cdev_notdef(),			/* 41 */
	cdev_notdef(),			/* 42 */
	cdev_notdef(),			/* 43 */
	cdev_notdef(),			/* 44 */
	cdev_notdef(),			/* 45 */
	cdev_notdef(),			/* 46 */
	cdev_notdef(),			/* 47 */
	cdev_notdef(),			/* 48 */
	cdev_notdef(),			/* 49 */
	cdev_notdef(),			/* 50 */
	cdev_notdef(),			/* 51 */
	cdev_notdef(),			/* 52 */
	cdev_notdef(),			/* 53 */
	cdev_notdef(),			/* 54 */
	cdev_fb_init(NCGTHREE,cgthree),	/* 55: /dev/cgthree */
	cdev_notdef(),			/* 56 */
	cdev_notdef(),			/* 57 */
	cdev_notdef(),			/* 58 */
	cdev_notdef(),			/* 59 */
	cdev_notdef(),			/* 60 */
	cdev_notdef(),			/* 61 */
	cdev_notdef(),			/* 62 */
	cdev_notdef(),			/* 63 */
	cdev_notdef(),			/* 64 */
	cdev_notdef(),			/* 65 */
	cdev_notdef(),			/* 66 */
	cdev_notdef(),			/* 67 */
	cdev_notdef(),			/* 68 */
	cdev_gen_init(NBSDAUDIO,audio),	/* 69: /dev/audio */
	cdev_openprom_init(1,openprom),	/* 70: /dev/openprom */
	cdev_notdef(),			/* 71 */
	cdev_notdef(),			/* 72 */
	cdev_notdef(),			/* 73 */
	cdev_notdef(),			/* 74 */
	cdev_notdef(),			/* 75 */
	cdev_notdef(),			/* 76 */
	cdev_notdef(),			/* 77 */
	cdev_notdef(),			/* 78 */
	cdev_notdef(),			/* 79 */
	cdev_notdef(),			/* 80 */
	cdev_notdef(),			/* 81 */
	cdev_notdef(),			/* 82 */
	cdev_notdef(),			/* 83 */
	cdev_notdef(),			/* 84 */
	cdev_notdef(),			/* 85 */
	cdev_notdef(),			/* 86 */
	cdev_notdef(),			/* 87 */
	cdev_notdef(),			/* 88 */
	cdev_notdef(),			/* 89 */
	cdev_notdef(),			/* 90 */
	cdev_notdef(),			/* 91 */
	cdev_notdef(),			/* 92 */
	cdev_notdef(),			/* 93 */
	cdev_notdef(),			/* 94 */
	cdev_notdef(),			/* 95 */
	cdev_notdef(),			/* 96 */
	cdev_notdef(),			/* 97 */
	cdev_notdef(),			/* 98 */
	cdev_notdef(),			/* 99 */
	cdev_notdef(),			/* 100 */
	cdev_notdef(),			/* 101 */
	cdev_notdef(),			/* 102 */
	cdev_notdef(),			/* 103 */
	cdev_notdef(),			/* 104 */
	cdev_bpf_init(NBPFILTER,bpf),	/* 105: packet filter */
	cdev_notdef(),			/* 106 */
	cdev_notdef(),			/* 107 */
	cdev_notdef(),			/* 108 */
	cdev_notdef(),			/* 109 */
};

int	nchrdev = sizeof (cdevsw) / sizeof (cdevsw[0]);

/*
 * Swapdev is a fake device implemented
 * in sw.c used only internally to get to swstrategy.
 * It cannot be provided to the users, because the
 * swstrategy routine munches the b_dev and b_blkno entries
 * before calling the appropriate driver.  This would horribly
 * confuse, e.g. the hashing routines. Instead, /dev/drum is
 * provided as a character (raw) device.
 */
dev_t	swapdev = makedev(3, 0);

/*
 * Routine that identifies /dev/mem and /dev/kmem.
 *
 * A minimal stub routine can always return 0.
 */
iskmemdev(dev)
	dev_t dev;
{

	return (major(dev) == 3 && minor(dev) < 2);
}

/*
 * Routine to determine if a device is a disk.
 *
 * A minimal stub routine can always return 0.
 */
isdisk(dev, type)
	dev_t dev;
	int type;
{

#ifdef notyet
	/* someday, something like this, perhaps */
	dev = devtab[major(dev)];
	return (dev != NULL && dev->dv_class == DV_DISK);
#else
	switch (major(dev)) {
	case 7:
		if (type == VBLK)
			return (1);
		return (0);
	case 17:
		if (type == VCHR)
			return (1);
		/* fall through */

	default:
		return (0);
	}
#endif
}

/*
 * Routine to convert from character to block device number.
 *
 * A minimal stub routine can always return NODEV.
 */
chrtoblk(dev)
	dev_t dev;
{

	if (major(dev) != 17)
		return (NODEV);
	return (makedev(7, minor(dev)));
}
