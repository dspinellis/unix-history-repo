/*
 *	@(#)scope.h	7.1 (Berkeley) %G%
 */

#ifdef DOSCOPE
/*  some i/o addresses used to generate pulses for scopes */
#define	OUT1	0xffffb034
#define	OUT2	0xffffb018
#define	OUT3	0xffffb020
#define	OUT4	0xffffb004
#define	OUT5	0xffffb024
#define	OUT6	0xffffb00c
#define	OUT7	0xffffb02c

#define	IOaddr(off)	(caddr_t)(&vmem[(off) & 0x0fffff])

extern	char vmem[];
extern	int cold;
#define	scope_out(x)	if (!cold) movob(IOaddr(OUT/**/x),0)
#define	scope_in(x)	if( !cold) dummy =  *IOaddr(IN/**/x)
#else
#define	scope_out(x)
#define	scope_in(x)
#endif
