/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)isa.h	5.2 (Berkeley) %G%
 */

/*
 * AT bus specific definitions.
 */
#ifndef LOCORE

#define Rd(s) ({u_char rtn; u_short ioa; \
	ioa = (s); \
	asm volatile ("movw %1,%%dx; nop ; in %%dx,%%al ; nop ; movb %%al,%0" \
		: "=g" (rtn) \
		: "g" (ioa) \
		: "ax", "dx"); \
	rtn; \
})

#define Wr(s,n) ({u_char val; u_short ioa; \
	ioa = (s); \
	val = (n); \
	asm volatile ("movb %1,%%al; movw %0,%%dx; nop; out %%al,%%dx ; nop" \
		: /* nothing returned */ \
		: "g" (ioa), "g" (val) \
		: "ax", "dx"); \
})


#define rdw(s) ({u_short rtn; u_short ioa; \
	ioa = (s); \
	asm volatile ("movw %1,%%dx; nop ; in %%dx,%%ax ; nop ; movw %%ax,%0" \
		: "=g" (rtn) \
		: "g" (ioa) \
		: "ax", "dx"); \
	rtn; \
})

#define wrw(s,n) ({u_short val; u_short ioa; \
	ioa = (s); \
	val = (n); \
	asm volatile ("movw %1,%%ax; movw %0,%%dx; nop;  out %%ax,%%dx; nop" \
		: /* nothing returned */ \
		: "g" (ioa), "g" (val) \
		: "ax", "dx"); \
})

#define Outsw(s,a, n) ({short *addr; u_short ioa; int cnt,rtn; \
	ioa = (s); \
	addr = (a); \
	cnt = (n); \
	asm volatile ("movw %1,%%dx; movl %2,%%esi; movl %3,%%ecx; cld; nop; .byte 0x66,0xf2,0x6f; nop ; movl %%esi,%0" \
		: "=g" (rtn) \
		: "g" (ioa), "g" (addr), "g" (cnt) \
		: "si", "dx", "cx"); \
	rtn; \
})
#define Insw(s,a, n) ({short  *addr; u_short ioa; int cnt,rtn; \
	ioa = (s); \
	addr = (a); \
	cnt = (n); \
	asm volatile ("movw %1,%%dx; movl %2,%%edi; movl %3,%%ecx; cld; nop; .byte 0x66,0xf2,0x6d; nop ; movl %%edi,%0" \
		: "=g" (rtn) \
		: "g" (ioa), "g" (addr), "g" (cnt)  \
		: "di", "dx", "cx"); \
	rtn; \
})

unsigned char inb() ;
extern outb();
#endif

#define IO_KBD	0x60			/* keyboard */

#define IO_WD0	0x1f0			/* primary base i/o address */
#define IO_WD1	0x170			/* secondary base i/o address */

#define IO_FD0	0x3f2			/* primary base i/o address */
#define IO_FD1	0x372			/* secondary base i/o address */

#define IO_COM0	0x3f8			/* COM1 i/o address */
#define IO_COM1	0x2f8			/* COM2 i/o address */
