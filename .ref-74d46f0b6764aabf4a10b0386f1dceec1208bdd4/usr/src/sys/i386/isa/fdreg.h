/*
 * AT floppy controller registers and bitfields
 *	@(#)fdreg.h	1.1 (Berkeley) %G%
 */

/* uses NEC765 controller */
#include "../i386/isa/ic/nec765.h"

/* registers */
#define	fdout	2	/* Digital Output Register (W) */
#define	FDO_FDSEL	0x01	/*  floppy device select */
#define	FDO_FRST	0x04	/*  floppy controller reset */
#define	FDO_FDMAEN	0x08	/*  enable floppy DMA and Interrupt */
#define	FDO_MOEN0	0x10	/*  motor enable drive 0 */
#define	FDO_MOEN1	0x20	/*  motor enable drive 1 */

#define	fdsts	4	/* NEC 765 Main Status Register (R) */
#define	fddata	5	/* NEC 765 Data Register (R/W) */

#define	fdctl	7	/* Control Register (W) */
#define	FDC_500KBPS	0x00	/* 500KBPS MFM drive transfer rate */
#define	FDC_300KBPS	0x01	/* 300KBPS MFM drive transfer rate */
#define	FDC_250KBPS	0x02	/* 250KBPS MFM drive transfer rate */
#define	FDC_125KBPS	0x03	/* 125KBPS FM drive transfer rate */

#define	fdin	7	/* Digital Input Register (R) */
#define	FDI_DCHG	0x80	/* diskette has been changed */


