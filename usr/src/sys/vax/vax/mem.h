/*	mem.h	4.2	81/02/19	*/

/*
 * Memory controller registers
 */
struct	mcr {
	int	mc_reg[3];
};

#ifdef	KERNEL
int	nmcr;
struct	mcr *mcraddr[4];
#endif

#define	M780_HIERR	0x20000000
#define	M780_ERLOG	0x10000000
#define	M750_UNCORR	0xc0000000
#define	M750_CORERR	0x40000000
#define	M750_ERLOG	(M750_UNCORR|M750_CORERR)

#define	MCR_750		((struct nexus *)0xf20000)

#define	MEMINTVL	(60*60*10)		/* 10 minutes */
