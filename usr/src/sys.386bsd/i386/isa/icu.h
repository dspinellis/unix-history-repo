/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)icu.h	5.6 (Berkeley) 5/9/91
 */

/*
 * AT/386 Interrupt Control constants
 * W. Jolitz 8/89
 */

#ifndef	__ICU__
#define	__ICU__

#ifndef	LOCORE

/*
 * Interrupt "level" mechanism variables, masks, and macros
 */
extern	unsigned short	imen;	/* interrupt mask enable */
extern	unsigned short	cpl;	/* current priority level mask */

extern	unsigned short highmask; /* group of interrupts masked with splhigh() */
extern	unsigned short ttymask; /* group of interrupts masked with spltty() */
extern	unsigned short biomask; /* group of interrupts masked with splbio() */
extern	unsigned short netmask; /* group of interrupts masked with splimp() */

#define	INTREN(s)	imen &= ~(s)
#define	INTRDIS(s)	imen |= (s)
#define	INTRMASK(msk,s)	msk |= (s)

#else

/*
 * Macro's for interrupt level priority masks (used in interrupt vector entry)
 */

/* Mask a group of interrupts atomically */
#define	INTR(unit,mask,offst) \
	pushl	$0 ; \
	pushl	$ T_ASTFLT ; \
	pushal ; \
	nop ; \
	inb	$0x84, %al ;	/* ... ASAP */ \
	movb	$0x20, %al ; 	/* next, as soon as possible send EOI ... */ \
	outb	%al, $ IO_ICU1 ; /* ... so in service bit may be cleared ...*/ \
	inb	$0x84, %al ;	/* ... ASAP */ \
	movb	$0x20, %al ;	/* likewise, the other one as well */ \
	outb	%al,$ IO_ICU2 ; \
	inb	$0x84,%al ; \
	pushl	%ds ; 		/* save our data and extra segments ... */ \
	pushl	%es ; \
	movw	$0x10, %ax ;	/* ... and reload with kernel's own */ \
	movw	%ax, %ds ; \
	movw	%ax, %es ; \
	incl	_cnt+V_INTR ;	/* tally interrupts */ \
	incl	_isa_intr + offst * 4 ; \
	inb	$0x84,%al ; \
	movzwl	_cpl,%eax ; \
	pushl	%eax ; \
	pushl	$ unit ; \
	orw	mask ,%ax ; \
	movw	%ax,_cpl ; \
	orw	_imen,%ax ; \
	outb	%al,$ IO_ICU1+1 ; \
	inb	$0x84,%al ; \
	movb	%ah,%al ; \
	outb	%al,$ IO_ICU2+1	; \
	inb	$0x84,%al ; \
	sti

/* Mask a group of interrupts atomically */
#define	INTRSTRAY(unit,mask,offst) \
	pushl	$0 ; \
	pushl	$ T_ASTFLT ; \
	pushal ; \
	nop ; \
	inb	$0x84, %al ;	/* ... ASAP */ \
	movb	$3, %al ; 	/* look at ISR ... */ \
	outb	%al, $ IO_ICU1 ; /* ... ...*/ \
	inb	$0x84, %al ;	/* ... ASAP */ \
	movb	$3, %al ; 	/* look at ISR ... */ \
	outb	%al, $ IO_ICU2 ; /* ... ...*/ \
	inb	$0x84, %al ;	/* ... ASAP */ \
	inb	$ IO_ICU1, %al ;	/* grab ISR */ \
	movb	%al, %dl ;	/* grab ISR */ \
	inb	$0x84, %al ;	/* ... ASAP */ \
	movb	$2, %al ; 	/* back to look at IRR ... */ \
	outb	%al, $ IO_ICU1 ; /* ... ...*/ \
	inb	$0x84, %al ;	/* ... ASAP */ \
	movb	$2, %al ; 	/* back to look at IRR ... */ \
	outb	%al, $ IO_ICU2 ; /* ... ...*/ \
	inb	$0x84, %al ;	/* ... ASAP */ \
	inb	$ IO_ICU2, %al ;	/* grab ISR */ \
	movb	%al, %dh ;	/* grab ISR */ \
	inb	$0x84, %al ;	/* ... ASAP */ \
	movb	$0x20, %al ; 	/* next, as soon as possible send EOI ... */ \
	outb	%al, $ IO_ICU1 ; /* ... so in service bit may be cleared ...*/ \
	inb	$0x84, %al ;	/* ... ASAP */ \
	movb	$0x20, %al ;	/* likewise, the other one as well */ \
	outb	%al,$ IO_ICU2 ; \
	inb	$0x84,%al ; \
	pushl	%ds ; 		/* save our data and extra segments ... */ \
	pushl	%es ; \
	movw	$0x10, %ax ;	/* ... and reload with kernel's own */ \
	movw	%ax, %ds ; \
	movw	%ax, %es ; \
	inb	$0x84,%al ; \
	movzwl	_cpl,%eax ; \
	pushl	%eax ; \
	movzwl	%dx,%eax ; \
	shll	$8,%eax ; \
	movb	$ unit , %al ; \
	pushl	%eax ; \
	orw	mask ,%ax ; \
	movw	%ax,_cpl ; \
	orw	_imen,%ax ; \
	outb	%al,$ IO_ICU1+1 ; \
	inb	$0x84,%al ; \
	movb	%ah,%al ; \
	outb	%al,$ IO_ICU2+1	; \
	inb	$0x84,%al ; \
	sti

/* Interrupt vector exit macros */

/* First eight interrupts (ICU1) */
#define	INTREXIT1	\
	jmp	doreti

/* Second eight interrupts (ICU2) */
#define	INTREXIT2	\
	jmp	doreti

#endif

/*
 * Interrupt enable bits -- in order of priority
 */
#define	IRQ0		0x0001		/* highest priority - timer */
#define	IRQ1		0x0002
#define	IRQ_SLAVE	0x0004
#define	IRQ8		0x0100
#define	IRQ9		0x0200
#define	IRQ2		IRQ9
#define	IRQ10		0x0400
#define	IRQ11		0x0800
#define	IRQ12		0x1000
#define	IRQ13		0x2000
#define	IRQ14		0x4000
#define	IRQ15		0x8000
#define	IRQ3		0x0008
#define	IRQ4		0x0010
#define	IRQ5		0x0020
#define	IRQ6		0x0040
#define	IRQ7		0x0080		/* lowest - parallel printer */

/*
 * Interrupt Control offset into Interrupt descriptor table (IDT)
 */
#define	ICU_OFFSET	32		/* 0-31 are processor exceptions */
#define	ICU_LEN		16		/* 32-47 are ISA interrupts */

#endif	__ICU__
