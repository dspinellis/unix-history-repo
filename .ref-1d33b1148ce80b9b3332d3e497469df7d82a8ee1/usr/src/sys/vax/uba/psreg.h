/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)psreg.h	7.3 (Berkeley) %G%
 */


/*
 *	The Real Nitty Gritty Device Registers
 */

struct psdevice {
	short int ps_data;		/* data register */
	short int ps_addr;		/* address register */
	short int ps_wcount;		/* word count register */
	short int ps_busaddr;		/* unibus address register */
	short int ps_iostat;		/* io status register */
};

/*
 *	Possible ioctl's
 */
#define PSIOAUTOREFRESH		_IO('p', 0)		/* auto refresh */
#define PSIOSINGLEREFRESH	_IO('p', 1)		/* single refresh */
#define PSIOAUTOMAP		_IO('p', 2)		/* auto map */
#define PSIOSINGLEMAP		_IO('p', 3)		/* single map */
#define PSIODOUBLEBUFFER	_IO('p', 4)		/* double buffer */
#define PSIOSINGLEBUFFER	_IO('p', 5)		/* single buffer */
#define PSIOWAITREFRESH		_IO('p', 6)		/* await refresh */
#define PSIOWAITMAP		_IO('p', 7)		/* await map */
#define PSIOWAITHIT		_IO('p', 8)		/* await hit */
#define PSIOSTOPREFRESH		_IO('p', 9)		/* stop refresh */
#define PSIOSTOPMAP		_IO('p',10)		/* stop map */
#define PSIOGETADDR		_IOR('p',11, int)	/* get Unibus address */
#define PSIOTIMEREFRESH		_IO('p',12)		/* time refresh */

/*
 *	Picture system io status register bits
 */

#define DIOREADY	0100000
#define PSAHOLD		040000
#define PSRESET		020000
#define DIORESET	010000
#define DMARESET	04000
#define PSIE		0400
#define DMAREADY	0200
#define DMAIE		0100
#define PASSIVE		010
#define DMAIN		04
#define NEXEM		02
#define GO		01

/*
 *	Picture system memory mapping control registers: SCB 0177400-0177410
 */

#define EXMMR_DMA	0177400
#define EXMMR_DIO	0177404
#define EXMMR_RC	0177405
#define EXMMR_MAPOUT	0177406
#define EXMMR_MAPIN	0177407
#define EXMSR		0177410

/*
 *	Extended memory status register bits
 */

#define DBERROR		0100000
#define SBERROR		040000
#define MEMREADY	0200
#define DBIE		0100
#define MMENBL		02
#define INITMEM		01

/*
 *	Size of extended memory
 */

#define NEXMPAGES	(256*2)
#define WORDSPERPAGE	(256)

/*
 *	MAP picture processor registers: SCB 0177750-0177753
 */

#define MAOL		0177750
#define MAOA		0177751
#define MAIA		0177752
#define MASR		0177753
#define MAMSR		0177754

/*
 *	MAP status register bits
 */

#define PPDONE		0100000
#define FIFOFULL	040000
#define FIFOEMPTY	020000
#define HIT		010000
#define IB		04000
#define TAKE		02000
#define MMODE		01400
#define MOSTOPPED	0200
#define IOUT		0100
#define MAO		040
#define MAI		020
#define HIT_HOLD	010
#define RSR_HOLD	04
#define VEC_HOLD	02
#define MAP_RESET	01

/*
 *	Refresh controller registers: SCB 0177730-0177737
 */

#define RFCSN		0177730
#define RFSN		0177731
#define RFAWA		0177732
#define RFAWL		0177733
#define RFAIA		0177734
#define RFASA		0177735
#define RFAIL		0177736
#define RFSR		0177737

/*
 *	Refresh controller status register bits
 */

#define RFSTOPPED	0100000
#define RFHOLD		040000
#define RFSTART		020000
#define AUTOREF		010000
#define RFBLANK		04000
#define RIGHT		02000
#define LGFIFO_FULL	01000
#define NOT_EXEC	0200
#define SKIPSEG		0100
#define WRITEBACK	040
#define SEARCH		020
#define MATCH_HOLD	010
#define MATCH_DEC	04
#define SEARCH_MODE	03

/*
 *	Interrupt control
 */

#define RTCREQ		0177760
#define RTCIE		0177761
#define SYSREQ		0177762
#define SYSIE		0177763
#define DEVREQ		0177764
#define DEVIE		0177765

/*
 *	System interrupt request bits
 */

#define LPEN_REQ	0200
#define MATCH_REQ	0100
#define WBSTOP_REQ	040
#define RFSTOP_REQ	020
#define MOSTOP_REQ	010
#define JUMP_REQ	04
#define HIT_REQ		02
#define HALT_REQ	01

/*
 *	Real-Time Clock registers
 */

#define RTCCNT		0177744
#define RTCSR		0177745

/*
 *	Real-Time Clock status register bits
 */

#define HZ120		040
#define EXT		020
#define SYNC		010
#define EXTSEL2		04
#define EXTSEL1		02
#define RUN		01

/*
 *	Control dials a/d registers
 */

#define ADDR0		0177500
#define ADDR1		0177501
#define ADDR2		0177502
#define ADDR3		0177503
#define ADDR4		0177504
#define ADDR5		0177505
#define ADDR6		0177506
#define ADDR7		0177507

/*
 *	Function switches and lights
 */

#define FSWR		0177626
#define FSLR		0177627
