/*-
 * Copyright (c) 1982, 1988, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)systm.h	7.17 (Berkeley) %G%
 */

extern char *panicstr;		/* panic message */
extern char version[];		/* system version */
extern char copyright[];	/* system copyright */

extern int nblkdev;		/* number of entries in bdevsw */
extern int nchrdev;		/* number of entries in cdevsw */
extern int nswdev;		/* number of swap devices */
extern int nswap;		/* size of swap space */

extern int selwait;		/* select timeout address */

extern u_char curpri;		/* priority of current process */

extern int maxmem;		/* max memory per process */
extern int physmem;		/* physical memory */

extern dev_t dumpdev;		/* dump device */
extern long dumplo;		/* offset into dumpdev */

extern dev_t rootdev;		/* root device */
extern struct vnode *rootvp;	/* vnode equivalent to above */

extern dev_t swapdev;		/* swapping device */
extern struct vnode *swapdev_vp;/* vnode equivalent to above */

extern struct sysent {		/* system call table */
	int	sy_narg;	/* number of arguments */
	int	(*sy_call)();	/* implementing function */
} sysent[];

extern int boothowto;		/* reboot flags, from console subsystem */
#ifdef	KADB
extern char *bootesym;		/* end of symbol info from boot */
#endif

/* casts to keep lint happy */
#define	insque(q,p)	_insque((caddr_t)q,(caddr_t)p)
#define	remque(q)	_remque((caddr_t)q)

/*
 * General function declarations.
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
int	sprintf __P((char *buf, const char *, ...));
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
