/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)systm.h	7.12 (Berkeley) %G%
 */

/*
 * Random set of variables used by more than one routine.
 */
extern	char version[];		/* system version */

/*
 * Nblkdev is the number of entries (rows) in the block switch.
 * Used in bounds checking on major device numbers.
 */
int	nblkdev;

/*
 * Number of character switch entries.
 */
int	nchrdev;

int	nswdev;			/* number of swap devices */
char	kmapwnt;		/* kernel map want flag */
u_char	curpri;			/* priority of current process */

int	maxmem;			/* actual max memory per process */
int	physmem;		/* physical memory on this CPU */

int	nswap;			/* size of swap space */
extern	int intstack[];		/* stack for interrupts */
dev_t	rootdev;		/* device of the root */
struct	vnode *rootvp;		/* vnode of root filesystem */
dev_t	dumpdev;		/* device to take dumps on */
long	dumplo;			/* offset into dumpdev */
dev_t	swapdev;		/* swapping device */
struct	vnode *swapdev_vp;	/* vnode equivalent to above */
dev_t	argdev;			/* device for argument lists */
struct	vnode *argdev_vp;	/* vnode equivalent to above */

extern	int icode[];		/* user init code */
extern	int szicode;		/* its size */

int	memall();
int	vmemall();
swblk_t	vtod();

/*
 * Structure of the system-entry table
 */
extern struct sysent {
	int	sy_narg;		/* total number of arguments */
	int	(*sy_call)();		/* handler */
} sysent[];

char	*panicstr;
int	boothowto;		/* reboot flags, from console subsystem */
#ifdef	KADB
char	*bootesym;		/* end of symbol info from boot */
#endif
int	selwait;

/* casts to keep lint happy */
#define	insque(q,p)	_insque((caddr_t)q,(caddr_t)p)
#define	remque(q)	_remque((caddr_t)q)

/*
 * General function declarations
 */
int	nullop __P((void));
int	enodev __P((void));
int	enoioctl __P((void));
int	enxio __P((void));
int	eopnotsupp __P((void));
int	seltrue __P((dev_t dev, int which, struct proc *p));
