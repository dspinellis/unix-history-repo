/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software developed by the Computer Systems
 * Engineering group at Lawrence Berkeley Laboratory under DARPA
 * contract BG 91-66 and contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 * from: $Header: fbio.h,v 1.3 91/12/13 22:16:32 torek Exp $ (LBL)
 *
 * from: sys/fbio.h		7.2 (Berkeley) 4/1/92
 *
 *	@(#)fbio.h	7.2 (Berkeley) %G%
 */

/*
 * Frame buffer ioctls (from Sprite, trimmed to essentials for X11).
 */

/*
 * Frame buffer type codes.
 */

#define	FBTYPE_BM		100	/* LUNA 4bit(1bit) frame buffer */

/*
 * Frame buffer descriptor as returned by FBIOGTYPE.
 */

struct fbtype {
	int	fb_type;	/* as defined above */
	int	fb_height;	/* in pixels */
	int	fb_width;	/* in pixels */
	int	fb_depth;	/* bits per pixel */
	int	fb_cmsize;	/* size of color map (entries) */
	int	fb_size;	/* total size in bytes */
};

#define	FBIOGTYPE	_IOR('F', 0, struct fbtype)
#define	FBIO_ON		_IO('F', 1)
#define	FBIO_OFF	_IO('F', 2)


/*
 * Reflesh Countor I/O
 */

struct fb_rfc {
	short	rfc_hcnt;
	short	rfc_vcnt;
};

#define	FBIOSETRFCT	_IOW('F', 3, struct fb_rfc)
#define	FBIOGETRFCT	_IOR('F', 4, struct fb_rfc)


/*
 * Color map I/O.
 */

#ifdef notyet
struct fb_palette {
	int	index;		/* first element (0 origin) */
	int	count;		/* number of elements */
	u_char	*red;		/* red color map elements */
	u_char	*green;		/* green color map elements */
	u_char	*blue;		/* blue color map elements */
};

#define	FBIOSETPALT	_IOW('F', 5, struct fb_palette)
#define	FBIOGETPALT	_IOR('F', 6, struct fb_palette)
#endif
