/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: adrsmap.h,v 4.300 91/06/09 06:34:29 root Rel41 $ SONY
 *
 *	@(#)adrsmap.h	7.3 (Berkeley) %G%
 */

/*
 * adrsmap.h
 *
 * Define all hardware address map.
 */

#ifndef __ADRSMAP__
#define	__ADRSMAP__	1

#ifdef news3400
/*----------------------------------------------------------------------
 *	news3400
 *----------------------------------------------------------------------*/
/*
 * timer
 */
#define	RTC_PORT	0xbff407f8
#define	DATA_PORT	0xbff407f9

#ifdef notdef
#define	EN_ITIMER	0xb8000004	/*XXX:???*/
#endif

#define	INTEN0	0xbfc80000
#define		INTEN0_PERR	0x80
#define		INTEN0_ABORT	0x40
#define		INTEN0_BERR	0x20
#define		INTEN0_TIMINT	0x10
#define		INTEN0_KBDINT	0x08
#define		INTEN0_MSINT	0x04
#define		INTEN0_CFLT	0x02
#define		INTEN0_CBSY	0x01

#define	INTEN1	0xbfc80001
#define		INTEN1_BEEP	0x80
#define		INTEN1_SCC	0x40
#define		INTEN1_LANCE	0x20
#define		INTEN1_DMA	0x10
#define		INTEN1_SLOT1	0x08
#define		INTEN1_SLOT3	0x04
#define		INTEN1_EXT1	0x02
#define		INTEN1_EXT3	0x01

#define	INTST0	0xbfc80002
#define		INTST0_PERR	0x80
#define		INTST0_ABORT	0x40
#define		INTST0_BERR	0x00	/* N/A */
#define		INTST0_TIMINT	0x10
#define		INTST0_KBDINT	0x08
#define		INTST0_MSINT	0x04
#define		INTST0_CFLT	0x02
#define		INTST0_CBSY	0x01
#define			INTST0_PERR_BIT		7
#define			INTST0_ABORT_BIT	6
#define			INTST0_BERR_BIT		5	/* N/A */
#define			INTST0_TIMINT_BIT	4
#define			INTST0_KBDINT_BIT	3
#define			INTST0_MSINT_BIT	2
#define			INTST0_CFLT_BIT		1
#define			INTST0_CBSY_BIT		0

#define	INTST1	0xbfc80003
#define		INTST1_BEEP	0x80
#define		INTST1_SCC	0x40
#define		INTST1_LANCE	0x20
#define		INTST1_DMA	0x10
#define		INTST1_SLOT1	0x08
#define		INTST1_SLOT3	0x04
#define		INTST1_EXT1	0x02
#define		INTST1_EXT3	0x01
#define			INTST1_BEEP_BIT		7
#define			INTST1_SCC_BIT		6
#define			INTST1_LANCE_BIT	5
#define			INTST1_DMA_BIT		4
#define			INTST1_SLOT1_BIT	3
#define			INTST1_SLOT3_BIT	2
#define			INTST1_EXT1_BIT		1
#define			INTST1_EXT3_BIT		0

#define	INTCLR0	0xbfc80004
#define		INTCLR0_PERR	0x80
#define		INTCLR0_ABORT	0x40
#define		INTCLR0_BERR	0x20
#define		INTCLR0_TIMINT	0x10
#define		INTCLR0_KBDINT	0x00	/* N/A */
#define		INTCLR0_MSINT	0x00	/* N/A */
#define		INTCLR0_CFLT	0x02
#define		INTCLR0_CBSY	0x01

#define	INTCLR1	0xbfc80005
#define		INTCLR1_BEEP	0x80
#define		INTCLR1_SCC	0x00	/* N/A */
#define		INTCLR1_LANCE	0x00	/* N/A */
#define		INTCLR1_DMA	0x00	/* N/A */
#define		INTCLR1_SLOT1	0x00	/* N/A */
#define		INTCLR1_SLOT3	0x00	/* N/A */
#define		INTCLR1_EXT1	0x00	/* N/A */
#define		INTCLR1_EXT3	0x00	/* N/A */

#define	ITIMER		0xbfc80006
#define	IOCLOCK		4915200

#define	DIP_SWITCH	0xbfe40000
#define	IDROM		0xbfe80000

#define	DEBUG_PORT	0xbfcc0003
#define		DP_READ		0x00
#define		DP_WRITE	0xf0
#define		DP_LED0		0x01
#define		DP_LED1		0x02
#define		DP_LED2		0x04
#define		DP_LED3		0x08


#define	LANCE_PORT	0xbff80000
#define	LANCE_MEMORY	0xbffc0000
#define	ETHER_ID	IDROM_PORT

#define	LANCE_PORT1	0xb8c30000	/* expansion lance #1 */
#define	LANCE_MEMORY1	0xb8c20000
#define	ETHER_ID1	0xb8c38000

#define	LANCE_PORT2	0xb8c70000	/* expansion lance #2 */
#define	LANCE_MEMORY2	0xb8c60000
#define	ETHER_ID2	0xb8c78000

#define	IDROM_PORT	0xbfe80000

#define	SCCPORT0B	0xbfec0000
#define	SCCPORT0A	0xbfec0002
#define SCCPORT1B	0xb8c40100
#define SCCPORT1A	0xb8c40102
#define SCCPORT2B	0xb8c40104
#define SCCPORT2A	0xb8c40106
#define SCCPORT3B	0xb8c40110
#define SCCPORT3A	0xb8c40112
#define SCCPORT4B	0xb8c40114
#define SCCPORT4A	0xb8c40116

#define	SCC_STATUS0	0xbfcc0002
#define	SCC_STATUS1	0xb8c40108
#define	SCC_STATUS2	0xb8c40118

#define	SCCVECT		(0x1fcc0007 | MACH_UNCACHED_MEMORY_ADDR)
#define	SCC_RECV	2
#define	SCC_XMIT	0
#define	SCC_CTRL	3
#define	SCC_STAT	1
#define	SCC_INT_MASK	0x6

/*XXX: SHOULD BE FIX*/
#define	KEYB_DATA	0xbfd00000	/* keyboard data port */
#define KEYB_STAT	0xbfd00001	/* keyboard status port */
#define	KEYB_INTE	INTEN0		/* keyboard interrupt enable */
#define	KEYB_RESET	0xbfd00002	/* keyboard reset port*/
#define	KEYB_INIT1	0xbfd00003	/* keyboard speed */
#define	KEYB_INIT2	KEYB_INIT1	/* keyboard clock */
#define	KEYB_BUZZ	0xbfd40001	/* keyboard buzzer (length) */
#define	KEYB_BUZZF	0xbfd40000	/* keyboard buzzer frequency */
#define	MOUSE_DATA	0xbfd00004	/* mouse data port */
#define MOUSE_STAT	0xbfd00005	/* mouse status port */
#define	MOUSE_INTE	INTEN0		/* mouse interrupt enable */
#define	MOUSE_RESET	0xbfd00006	/* mouse reset port */
#define	MOUSE_INIT1	0xbfd00007	/* mouse speed */
#define	MOUSE_INIT2	MOUSE_INIT1	/* mouse clock */

#define	RX_MSINTE	0x04		/* Mouse Interrupt Enable */
#define RX_KBINTE	0x08		/* Keyboard Intr. Enable */
#define	RX_MSINT	0x04		/* Mouse Interrupted */
#define	RX_KBINT	0x08		/* Keyboard Interrupted */
#define	RX_MSBUF	0x01		/* Mouse data buffer Full */
#define	RX_KBBUF	0x01		/* Keyboard data Full */
#define	RX_MSRDY	0x02		/* Mouse data ready */
#define	RX_KBRDY	0x02		/* Keyboard data ready */
/*XXX: SHOULD BE FIX*/

#define	ABEINT_BADDR	0xbfdc0038
#endif /* news3400 */

#endif /* !__ADRSMAP__ */
