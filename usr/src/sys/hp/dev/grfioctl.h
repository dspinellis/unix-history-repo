/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: grfioctl.h 1.15 92/01/22$
 *
 *	@(#)grfioctl.h	7.3 (Berkeley) %G%
 */

struct	grfinfo {
	int	gd_id;			/* HPUX identifier */
	caddr_t	gd_regaddr;		/* control registers physaddr */
	int	gd_regsize;		/* control registers size */
	caddr_t	gd_fbaddr;		/* frame buffer physaddr */
	int	gd_fbsize;		/* frame buffer size */
	short	gd_colors;		/* number of colors */
	short	gd_planes;		/* number of planes */
/* new stuff */
	int	gd_fbwidth;		/* frame buffer width */
	int	gd_fbheight;		/* frame buffer height */
	int	gd_dwidth;		/* displayed part width */
	int	gd_dheight;		/* displayed part height */
	int	gd_pad[6];		/* for future expansion */
};

/* types */
#define GRFGATOR	8
#define GRFBOBCAT	9
#define	GRFCATSEYE	9
#define GRFRBOX		10
#define GRFFIREEYE	11
#define GRFHYPERION	12
#define GRFDAVINCI	14

/*
 * HPUX ioctls (here for the benefit of the driver)
 */
struct	grf_slot {
	int	slot;
	u_char	*addr;
};

#ifndef _IOH
#define _IOH(x,y)	(IOC_IN|((x)<<8)|y)	/* IOC_IN is IOC_VOID */

#define	GCID		_IOR('G', 0, int)
#define	GCON		_IOH('G', 1)
#define	GCOFF		_IOH('G', 2)
#define	GCAON		_IOH('G', 3)
#define	GCAOFF		_IOH('G', 4)
#define	GCMAP		_IOWR('G', 5, int)
#define	GCUNMAP		_IOWR('G', 6, int)
#define	GCLOCK		_IOH('G', 7)
#define	GCUNLOCK	_IOH('G', 8)
#define	GCLOCK_MINIMUM	_IOH('G', 9)
#define	GCUNLOCK_MINIMUM _IOH('G', 10)
#define	GCSTATIC_CMAP	_IOH('G', 11)
#define	GCVARIABLE_CMAP _IOH('G', 12)
#define GCSLOT		_IOWR('G', 13, struct grf_slot)

/* XXX: for now */
#define	IOMAPID		_IOR('M',0,int)	/* ??? */
#define	IOMAPMAP	_IOWR('M',1,int)
#define	IOMAPUNMAP	_IOWR('M',2,int)
#endif

/*
 * BSD ioctls
 */
#define	GRFIOCGINFO	_IOR('G', 0, struct grfinfo) /* get info on device */
#define	GRFIOCON	_IO('G', 1)		/* turn graphics on */
#define	GRFIOCOFF	_IO('G', 2)		/* turn graphics off */
#define GRFIOCMAP	_IOWR('G', 5, int)	/* map in regs+framebuffer */
#define GRFIOCUNMAP	_IOW('G', 6, int)	/* unmap regs+framebuffer */
