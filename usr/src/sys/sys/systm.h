/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)systm.h	7.13 (Berkeley) %G%
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

void	panic __P((char *));
void	tablefull __P((char *));
void	addlog __P((const char *, ...));
void	log __P((int, const char *, ...));
void	printf __P((const char *, ...));
void	ttyprintf __P((struct tty *, const char *, ...));

void	bcopy __P((void *from, void *to, u_int len));
void	ovbcopy __P((void *from, void *to, u_int len));
void	bzero __P((void *buf, u_int len));
int	bcmp __P((void *str1, void *str2, u_int len));
int	strlen __P((char *string));

int	copystr __P((void *kfaddr, void *kdaddr, u_int len, u_int *done));
int	copyinstr __P((void *udaddr, void *kaddr, u_int len, u_int *done));
int	copyoutstr __P((void *kaddr, void *udaddr, u_int len, u_int *done));
int	copyin __P((void *udaddr, void *kaddr, u_int len));
int	copyout __P((void *kaddr, void *udaddr, u_int len));

int	fubyte __P((void *base));
#ifdef notdef
int	fuibyte __P((void *base));
#endif
int	subyte __P((void *base, int byte));
int	suibyte __P((void *base, int byte));
int	fuword __P((void *base));
int	fuiword __P((void *base));
int	suword __P((void *base, int word));
int	suiword __P((void *base, int word));

int	scanc __P((unsigned size, u_char *cp, u_char *table, int mask));
int	skpc __P((int mask, int size, char *cp));
int	locc __P((int mask, char *cp, unsigned size));
int	ffs __P((long value));
