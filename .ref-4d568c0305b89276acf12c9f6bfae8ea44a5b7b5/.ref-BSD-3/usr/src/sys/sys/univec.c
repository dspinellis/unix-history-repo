/*	univec.c	2.1	1/5/80	*/

#include "../h/param.h"
#include "../h/vm.h"

/* DEV puts controller no. in bits 27-31 of ISR addr */
#define	DEV(x,n)	(int (*)())((int)(x) + 0x08000000 * n)

/* DMA sets low bit to activate pseudo-dma code */
#define	DMA(x)		(int (*)())((int)(x) + 1)

/* dzxint is, in reality, a pseudo-dma entry */
#define	dzxint		DMA(dzdma)

/*
 * Definitions of functions to be placed in UNIvec.
 */
extern	ubastray();
extern	dzrint(), dzdma();
#ifdef ERNIE
extern	klrint(), klxint();
extern	uhpintr();
extern	urpintr();
extern	vpintr();
#endif

#define	___	ubastray		/* fill in the blank! */
 
int (*UNIvec[NBPG/NBPW])() = {

 /* 0x0 */	___,		___,		___,		___,
 /* 0x10 */	___,		___,		___,		___,
 /* 0x20 */	___,		___,		___,		___,
 /* 0x30 */	___,		___,		___,		___,
 /* 0x40 */	___,		___,		___,		___,
 /* 0x50 */	___,		___,		___,		___,
#ifdef ERNIE
 /* 0x60 */	___,		___,		uhpintr,	___,
 /* 0x70 */	___,		___,		___,		vpintr,
 /* 0x80 */	vpintr,		___,		___,		___,
 /* 0x90 */	___,		___,		___,		___,
 /* 0xa0 */	___,		___,		___,		urpintr,
 /* 0xb0 */	___,		___,		___,		___,
 /* 0xc0 */	DEV(dzrint,0),	DEV(dzxint,0),	DEV(dzrint,1),	DEV(dzxint,1),
 /* 0xd0 */	DEV(dzrint,2),	DEV(dzxint,2),	___,		___,
 /* 0xe0 */	___,		___,		___,		___,
 /* 0xf0 */	___,		___,		___,		___,
 /* 0x100 */	DEV(klrint,0),	DEV(klxint,0),	DEV(klrint,1),	DEV(klxint,1),
 /* 0x110 */	DEV(klrint,2),	DEV(klxint,2),	DEV(klrint,3),	DEV(klxint,3),
 /* 0x120 */	DEV(klrint,4),	DEV(klxint,4),	DEV(klrint,5),	DEV(klxint,5),
 /* 0x130 */	DEV(klrint,6),	DEV(klxint,6),	DEV(klrint,7),	DEV(klxint,7),
#else
 /* 0x60 */	___,		___,		___,		___,
 /* 0x70 */	___,		___,		___,		___,
 /* 0x80 */	___,		___,		___,		___,
 /* 0x90 */	___,		___,		___,		___,
 /* 0xa0 */	___,		___,		___,		___,
 /* 0xb0 */	___,		___,		___,		___,
 /* 0xc0 */	DEV(dzrint,0),	DEV(dzxint,0),	___,		___,
 /* 0xd0 */	___,		___,		___,		___,
 /* 0xe0 */	___,		___,		___,		___,
 /* 0xf0 */	___,		___,		___,		___,
 /* 0x100 */	___,		___,		___,		___,
 /* 0x110 */	___,		___,		___,		___,
 /* 0x120 */	___,		___,		___,		___,
 /* 0x130 */	___,		___,		___,		___,
#endif
 /* 0x140 */	___,		___,		___,		___,
 /* 0x150 */	___,		___,		___,		___,
 /* 0x160 */	___,		___,		___,		___,
 /* 0x170 */	___,		___,		___,		___,
 /* 0x180 */	___,		___,		___,		___,
 /* 0x190 */	___,		___,		___,		___,
 /* 0x1a0 */	___,		___,		___,		___,
 /* 0x1b0 */	___,		___,		___,		___,
 /* 0x1c0 */	___,		___,		___,		___,
 /* 0x1d0 */	___,		___,		___,		___,
 /* 0x1e0 */	___,		___,		___,		___,
 /* 0x1f0 */	___,		___,		___,		___,

};
 
#undef	___

ubastray()
{
	register int i;

#ifdef lint
	i = 0;
#else
	asm("movl r3,r11");		/* magic */
#endif
	printf("stray UBA interrupt: 0x%X\n", i);
}
