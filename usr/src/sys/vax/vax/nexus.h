/*	nexus.h	4.3	81/02/26	*/

/*
 * Information about nexus's.
 */
#if VAX780
#define	NNEX780	16
#define	NEX780	((struct nexus *)0x20000000)
#endif
#if VAX750
#define	NNEX750	4
#define	NEX750	((struct nexus *)0xf28000)
#endif
#define	NEXSIZE	0x2000

#if VAX780
#define	NNEXUS	16
#else
#define	NNEXUS	6		/* 4 mba slots + mem + uba */
#endif

#ifndef LOCORE
struct	nexus {
	union nexcsr {
		long	nex_csr;
		u_char	nex_type;
	} nexcsr;
	long	nex_pad[NEXSIZE / sizeof (long) - 1];
};
#endif
#ifdef	KERNEL
extern struct nexus nexus[NNEXUS];
#endif

/*
 * Bits in high word of nexus's.
 */
#define	SBI_PARFLT	(1<<31)		/* sbi parity fault */
#define	SBI_WSQFLT	(1<<30)		/* write sequence fault */
#define	SBI_UIDFLT	(1<<29)		/* unexpected read data fault */
#define	SBI_ISQFLT	(1<<28)		/* interlock sequence fault */
#define	SBI_MXTFLT	(1<<27)		/* multiple transmitter fault */
#define	SBI_XMTFLT	(1<<26)		/* transmit fault */

#define	NEX_CFGFLT	(0xfc000000)

#define	NEX_APD		(1<<23)		/* adaptor power down */
#define	NEX_APU		(1<<22)		/* adaptor power up */

#define	MBA_OT		(1<<21)		/* overtemperature */

#define	UBA_UBINIT	(1<<18)		/* unibus init */
#define	UBA_UBPDN	(1<<17)		/* unibus power down */
#define	UBA_UBIC	(1<<16)		/* unibus initialization complete */

/*
 * Types for nex_type.
 */
#define	NEX_MEM4	0x08		/* 4K chips, non-interleaved mem */
#define	NEX_MEM4I	0x09		/* 4K chips, interleaved mem */
#define	NEX_MEM16	0x10		/* 16K chips, non-interleaved mem */
#define	NEX_MEM16I	0x11		/* 16K chips, interleaved mem */
#define	NEX_MBA		0x20		/* Massbus adaptor */
#define	NEX_UBA0	0x28		/* Unibus adaptor */
#define	NEX_UBA1	0x29		/* 4 flavours  for 4 addr spaces */
#define	NEX_UBA2	0x2a
#define	NEX_UBA3	0x2b
#define	NEX_DR32	0x30		/* DR32 user i'face to SBI */
#define	NEX_MPM0	0x40		/* Multi-port mem */
#define	NEX_MPM1	0x41		/* Who knows why 4 different ones ? */
#define	NEX_MPM2	0x42
#define	NEX_MPM3	0x43
