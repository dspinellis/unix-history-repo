/*	trace.h	4.2	81/02/19	*/

#define	bit(a)		(1<<(a))

#define	TR_BREAD	bit(0)
#define	TR_BWRITE	bit(1)
#define	TR_MISS		bit(2)
#define	TR_HIT		bit(3)
#define	TR_RA		bit(4)
#define	TR_XFOD		bit(5)
#define	TR_BRELSE	bit(6)
#define	TR_MALL		bit(7)

#define	TRCSIZ		4096

#if defined(KERNEL) && defined(EPAWNJ)
int	tracebuf[TRCSIZ];
unsigned tracex;
int	tracewhich;
#define	trace(a,b,c)	if (tracewhich&(a)) trace1(a,b,c)
#endif
