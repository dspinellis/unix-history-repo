/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * from: Utah $Hdr: grfioctl.h 1.17 93/08/13$
 *
 *	@(#)grfioctl.h	8.2 (Berkeley) 9/9/93
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

struct	grf_fbinfo {
	int	id;
	int	mapsize;
	int	dwidth, dlength;
	int	width, length;
	int	xlen;
	int	bpp, bppu;
	int	npl, nplbytes;
	char	name[32];
	int	attr;
	caddr_t	fbbase, regbase;
	caddr_t	regions[6];
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
#define GCDESCRIBE	_IOR('G', 21, struct grf_fbinfo)

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
