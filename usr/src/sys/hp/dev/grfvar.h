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
 * from: Utah $Hdr: grfvar.h 1.10 92/01/21$
 *
 *	@(#)grfvar.h	7.4 (Berkeley) %G%
 */

/* internal structure of lock page */
#define GRFMAXLCK	256
struct	grf_lockpage {
	u_char	gl_locks[GRFMAXLCK];
};
#define gl_lockslot gl_locks[0]

/*
 * Static configuration info for display types
 */
struct	grfsw {
	int	gd_hwid;	/* id returned by hardware */
	int	gd_swid;	/* id to be returned by software */
	char	*gd_desc;	/* description printed at config time */
	int	(*gd_init)();	/* boot time init routine */
	int	(*gd_mode)();	/* misc function routine */
};

/* per display info */
struct	grf_softc {
	int	g_flags;		/* software flags */
	struct  grfsw *g_sw;		/* static configuration info */
	caddr_t	g_regkva;		/* KVA of registers */
	caddr_t	g_fbkva;		/* KVA of framebuffer */
	struct	grfinfo g_display;	/* hardware description (for ioctl) */
	struct	grf_lockpage *g_lock;	/* lock page associated with device */
	struct	proc *g_lockp;		/* process holding lock */
	short	*g_pid;			/* array of pids with device open */
	int	g_lockpslot;		/* g_pid entry of g_lockp */
	caddr_t	g_data;			/* device dependent data */
};

/* flags */
#define	GF_ALIVE	0x01
#define GF_OPEN		0x02
#define GF_EXCLUDE	0x04
#define GF_WANTED	0x08
#define GF_BSDOPEN	0x10
#define GF_HPUXOPEN	0x20

/* requests to mode routine */
#define GM_GRFON	1
#define GM_GRFOFF	2
#define GM_GRFOVON	3
#define GM_GRFOVOFF	4
#define GM_DESCRIBE	5

/* minor device interpretation */
#define GRFOVDEV	0x10	/* overlay planes */
#define GRFIMDEV	0x20	/* images planes */
#define GRFUNIT(d)	((d) & 0x7)

#ifdef KERNEL
extern	struct grf_softc grf_softc[];
extern	struct grfsw grfsw[];
extern	int ngrfsw;
#endif
