/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)isa.h	5.5 (Berkeley) %G%
 */

/*
 * ISA Bus conventions
 */

#ifndef LOCORE
unsigned char inb(), rtcin();
void outb();
#endif


/*
 * Input / Output Port Assignments
 */

#ifndef IO_BEGIN
#define	IO_ISABEGIN	0x000		/* 0x000 - Beginning of I/O Registers */

		/* CPU Board */
#define IO_DMA1		0x000		/* 8237A DMA Controller #1 */
#define IO_ICU1		0x020		/* 8259A Interrupt Controller #1 */
#define IO_TIMER1	0x040		/* 8252 Timer #1 */
#define IO_TIMER2	0x048		/* 8252 Timer #2 */
#define IO_KBD		0x060		/* 8042 Keyboard */
#define IO_RTC		0x070		/* RTC */
#define IO_NMI		IO_RTC		/* NMI Control */
#define IO_DMAPG	0x080		/* DMA Page Registers */
#define IO_ICU2		0x0A0		/* 8259A Interrupt Controller #2 */
#define IO_DMA2		0x0C0		/* 8237A DMA Controller #2 */
#define IO_NPX		0x0F0		/* Numeric Coprocessor */

		/* Cards */
					/* 0x100 - 0x16F Open */

#define IO_WD2		0x170		/* Secondary Fixed Disk Controller */

					/* 0x178 - 0x1EF Open */

#define IO_WD1		0x1f0		/* Primary Fixed Disk Controller */
#define IO_GAME		0x200		/* Game Controller */

					/* 0x208 - 0x277 Open */

#define IO_LPT2		0x278		/* Parallel Port #2 */

					/* 0x280 - 0x2F7 Open */

#define IO_COM2		0x2f8		/* COM2 i/o address */

					/* 0x300 - 0x36F Open */

#define IO_FD2		0x370		/* secondary base i/o address */
#define IO_LPT1		0x378		/* Parallel Port #1 */

					/* 0x380 - 0x3AF Open */

#define IO_MDA		0x3B0		/* Monochome Adapter */
#define IO_LPT3		0x3BC		/* Monochome Adapter Printer Port */
#define IO_VGA		0x3C0		/* E/VGA Ports */
#define IO_CGA		0x3D0		/* CGA Ports */

					/* 0x3E0 - 0x3EF Open */

#define IO_FD1		0x3f0		/* primary base i/o address */
#define IO_COM1		0x3f8		/* COM1 i/o address */

#define	IO_ISAEND	0x3FF		/* - 0x3FF End of I/O Registers */
#endif	IO_ISABEGIN

/*
 * Input / Output Memory Physical Addresses
 */

#ifndef	IOM_BEGIN
#define	IOM_BEGIN	0xa0000		/* Start of I/O Memory "hole" */
#define	IOM_END		0xFFFFF		/* End of I/O Memory "hole" */
#endif	IOM_BEGIN

/*
 * RAM Physical Address Space (ignoring the above mentioned "hole")
 */

#ifndef	RAM_BEGIN
#define	RAM_BEGIN	0x000000	/* Start of RAM Memory */
#define	RAM_END		0xFFFFFF	/* End of RAM Memory */
#endif	IOM_BEGIN

/*
 * Oddball Physical Memory Addresses
 */
#ifndef	COMPAQ_RAMRELOC
#define	COMPAQ_RAMRELOC	0x80c00000	/* Compaq RAM relocation/diag */
#define	COMPAQ_RAMSETUP	0x80c00002	/* Compaq RAM setup */
#define	WEITEK_FPU	0xC0000000	/* WTL 2167 */
#define	CYRIX_EMC	0xC0000000	/* Cyrix EMC */
#endif	COMPAQ_RAMRELOC
