/*	va.c	3.4	%H%	*/

#ifdef ERNIE
#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/uba.h"
#include "../h/vcmd.h"

/*
 * Benson-Varian matrix printer/plotter.  Device "va", for "varian".
 * dma interface driver
 */
int	vabdp = 1;	/* Used with ubasetup. */

unsigned minvaph();	/* Maximum amount transferred by physio. */

#define	VAPRI	(PZERO-1)

struct	varegs {	/* Unibus registers provided by va. */
	unsigned short vabufaddr;	/* DMA buffer address. */
	short vawcount;			/* Negative of number of 16-bit
					   words to transfer by DMA. */
	union {
		short vacsrword;	/* csr addressed as a word (for R). */
		struct {
			char Vacsrlo;
			char Vacsrhi;	/* High byte (command bytes go here). */
		} vacsrbytes;		/* csr addressed as bytes (for W). */
	} vacsr;			/* Control/Status Register (csr). */
	short	vadata;
};

#define	vacsrhi	vacsr.vacsrbytes.Vacsrhi
#define	vacsrlo	vacsr.vacsrbytes.Vacsrlo
#define	VAADDR	((struct varegs *)(UBA0_DEV + 0164000))

/* vacsr.vacsrword bits: */
#define	ERROR		0100000		/* R	Some error has occurred */
#define NPRTIMO		01000		/* R    DMA timeout error */
#define NOTREADY	0400		/* R	Something besides NPRTIMO */
#define DONE		0200		/* R	*/
#define	IENABLE		0100		/* R/W	Interrupt enable */
#define	SUPPLIESLOW	04		/* R	*/
#define BOTOFFORM	02		/* R	*/
#define BYTEREVERSE	01		/* R/W	Reverse byte order in words */

/* Command bytes sent to vacsrhi */
#define VAPLOT		0340
#define VAPRINT		0100
#define VAPRINTPLOT	0160
#define VAAUTOSTEP	0244
/* The following commands are not used in this driver: */
#define VANOAUTOSTEP	0045
#define	VAFORMFEED	0263
#define	VASLEW		0265
#define	VASTEP		0064

struct {
	char	va_is_open;
	char	va_busy;
	int	va_state;	/* State: bits are commands in vcmd.h. */
	int	va_wcount;
	int	va_bufp;
} vainfo;
int	va_ubinfo;

struct	buf rvabuf;		/* Used by physio for a buffer. */

vaopen()
{

	if (vainfo.va_is_open) {	/* Can't open if it's already open. */
		u.u_error = ENXIO;
		return;
	}
	vainfo.va_is_open = 1;		/* NOW it's open! */
	VAADDR->vawcount = 0;		/* Clear residual errors */
	vainfo.va_wcount = 0;		/* No DMA to do now. */
	vainfo.va_state = 0;
	VAADDR->vacsrlo = IENABLE;
					/* Enable interrupts. */
	vatimo();

	vacmd(VPRINT);			/* Start in print mode. */
	if (u.u_error)
		vaclose();
}

vastrategy(bp)
	register struct buf *bp;
{
	register int e;

	(void) spl4();
	while (vainfo.va_busy)		/* Wait till not busy. */
		sleep((caddr_t)&vainfo, VAPRI);
	vainfo.va_busy = 1;		/* Grab it. */
	(void) spl0();

	va_ubinfo = ubasetup(bp, vabdp);	/* Set up uba mapper. */
	vainfo.va_bufp = va_ubinfo & 0x3ffff;

	(void) spl4();
	if (e = vaerror(DONE))
		goto brkout;
	vainfo.va_wcount = -(bp->b_bcount/2);
		/* va uses a word count, 
		   so user had better supply an even number of bytes. */
	vastart();
	e = vaerror(DONE);	/* Wait for DMA to complete. */
	vainfo.va_wcount = 0;	/* Reset state info. */
	vainfo.va_bufp = 0;

	/* After printing a line of characters, VPRINTPLOT mode essentially
	   reverts to VPLOT mode, plotting things until a new mode is set.
	   This change is indicated by sending a VAAUTOSTEP command to
	   the va.  We also change va_state to reflect this effective
	   mode change.
	 */
	if (vainfo.va_state & VPRINTPLOT) {
		vainfo.va_state = (vainfo.va_state & ~VPRINTPLOT) | VPLOT;
		VAADDR->vacsrhi = VAAUTOSTEP;
		e |= vaerror(DONE);
	}
	(void) spl0();
brkout:
	ubafree(va_ubinfo), va_ubinfo = 0;
	vainfo.va_busy = 0;
	iodone(bp);
	if (e)
		u.u_error = EIO;
	wakeup((caddr_t)&vainfo);
}

int	vablock = 16384;

unsigned
minvaph(bp)
struct buf *bp;
{
	if (bp->b_bcount > vablock)
		bp->b_bcount = vablock;
}

/*ARGSUSED*/
vawrite(dev)
{
	physio(vastrategy, &rvabuf, dev, B_WRITE, minvaph);
}

/*
 * Vaerror waits until bit or ERROR gets set, then returns non-zero if
 * if it was ERROR that was set.
 */
vaerror(bit)
{
	register int e;

	while ((e = VAADDR->vacsr.vacsrword & (bit|ERROR)) == 0)
		sleep((caddr_t)&vainfo, VAPRI);
	return (e & ERROR);
}

/* vastart starts up the DMA by setting the buffer pointer and the word count. */
vastart()
{
	if (vainfo.va_wcount) {
		VAADDR->vabufaddr = vainfo.va_bufp;
		VAADDR->vawcount = vainfo.va_wcount;
		return;
	}
}

/*ARGSUSED*/
vaioctl(dev, cmd, addr, flag)
	register caddr_t addr;
{
	register int vcmd;

	switch (cmd) {

	case VGETSTATE:
		(void) suword(addr, vainfo.va_state);
		return;

	case VSETSTATE:
		vcmd = fuword(addr);
		if (vcmd == -1) {	
			u.u_error = EFAULT;
			return;
		}
		vacmd(vcmd);
		return;

	default:
		u.u_error = ENOTTY;	/* Not a legal ioctl cmd. */
		return;
	}
}

/* vacmd sends a command code to the va, and waits for it to complete.
   If an error occurs, u.u_error is set to EIO.
   vacmd also updates vainfo.va_state.
 */

vacmd(vcmd)
{
	(void) spl4();
	(void) vaerror(DONE);	/* Wait for va to be ready. */
	switch (vcmd) {

	case VPLOT:
		/* Must turn on plot AND autostep modes. */
		VAADDR->vacsrhi = VAPLOT;
		if (vaerror(DONE))
			u.u_error = EIO;
		VAADDR->vacsrhi = VAAUTOSTEP;
		break;

	case VPRINT:
		VAADDR->vacsrhi = VAPRINT;
		break;

	case VPRINTPLOT:
		VAADDR->vacsrhi = VAPRINTPLOT;
		break;
	}
	vainfo.va_state =
		(vainfo.va_state & ~(VPLOT | VPRINT | VPRINTPLOT)) | vcmd;

	if (vaerror(DONE))	/* Wait for command to complete. */
		u.u_error = EIO;
	(void) spl0();
}

vatimo()
{
	if (vainfo.va_is_open)
		timeout(vatimo, (caddr_t)0, HZ/10);
	vaintr(0);
}

/*ARGSUSED*/
vaintr(dev)
{
	wakeup((caddr_t)&vainfo);
}

vaclose()
{

	vainfo.va_is_open = 0;
	vainfo.va_busy = 0;
	vainfo.va_state = 0;
	vainfo.va_wcount = 0;
	vainfo.va_bufp = 0;
	VAADDR->vacsrlo = 0;
}
#endif
