/*	univec.c	4.1	11/9/80	*/

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
#include "dz.h"
#if NDZ11 > 0
extern	dzrint(), dzdma();
#else
#define	dzrint	ubastray
#define	dzxint	ubastray
#endif

#include "dh.h"
#if NDH11 > 0
extern	dhrint(), dhxint();
#else
#define	dhrint	ubastray
#define	dhxint	ubastray
#endif

#include "up.h"
#if NUP > 0
extern	upintr();
#else
#define	upintr	ubastray
#endif

#include "vp.h"
#if NVP > 0
extern	vpintr();
#else
#define	vpintr	ubastray
#endif

#include "va.h"
#if NVA > 0
extern	vaintr();
#else
#define	vaintr	ubastray
#endif

#define	___	ubastray		/* fill in the blank! */
 
int (*UNIvec[NBPG/NBPW])() = {

 /* 0x0 */	___,		___,		___,		___,
 /* 0x10 */	___,		___,		___,		___,
 /* 0x20 */	___,		___,		___,		___,
 /* 0x30 */	___,		___,		___,		___,
 /* 0x40 */	___,		___,		___,		___,
 /* 0x50 */	___,		___,		___,		___,
 /* 0x60 */	___,		___,		___,		___,
 /* 0x70 */	___,		___,		vaintr,		vpintr,
 /* 0x80 */	vpintr,		___,		___,		___,
 /* 0x90 */	___,		___,		___,		___,
 /* 0xa0 */	___,		___,		___,		upintr,
 /* 0xb0 */	___,		___,		___,		___,
#if NDZ11 > 0
 /* 0xc0 */	DEV(dzrint,0),	DEV(dzxint,0),
#else
 /* 0xc0 */	___,		___,
#endif
#if NDZ11 > 1
 /* 0xc8 */					DEV(dzrint,1),	DEV(dzxint,1),
#else
 /* 0xc8 */					___,		___,
#endif
#if NDZ11 > 2
 /* 0xd0 */	DEV(dzrint,2),	DEV(dzxint,2),
#else
 /* 0xd0 */	___,		___,
#endif
#if NDZ11 > 3
 /* 0xd8 */					DEV(dzrint,3),	DEV(dzxint,3),
#else
 /* 0xd8 */					___,		___,
#endif
#if NDH11 > 0
 /* 0xe0 */	DEV(dhrint,0),	DEV(dhxint,0),
#else
 /* 0xe0 */	___,		___,
#endif
#if NDH11 > 16
 /* 0xe0 */					DEV(dhrint,1),	DEV(dhxint,1),
#else
 /* 0xf0 */					___,		___,
#endif
 /* 0xf0 */	___,		___,		___,		___,
 /* 0x100 */	___,		___,		___,		___,
 /* 0x110 */	___,		___,		___,		___,
 /* 0x120 */	___,		___,		___,		___,
 /* 0x130 */	___,		___,		___,		___,
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
