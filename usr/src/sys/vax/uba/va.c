/*	va.c	3.6	%G%	*/

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
 * Benson-Varian matrix printer/plotter
 * dma interface driver
 */
int	vabdp = 1;

unsigned minvaph();

#define	VAPRI	(PZERO-1)

#define	ushort	unsigned short
struct	varegs {
	ushort	vaba;
	short	vawc;
	union {
		short	Vacsw;
		struct {
			char Vacsl;
			char Vacsh;
		} vacsr;
	} vacs;
	short	vadata;
};

#define	vacsw	vacs.Vacsw
#define	vacsh	vacs.vacsr.Vacsh
#define	vacsl	vacs.vacsr.Vacsl

#define	VAADDR	((struct varegs *)(UBA0_DEV + 0164000))

/* vacsw bits */
#define	ERROR		0100000		/* Some error has occurred */
#define NPRTIMO		01000		/* DMA timeout error */
#define NOTREADY	0400		/* Something besides NPRTIMO */
#define DONE		0200
#define	IENABLE		0100		/* Interrupt enable */
#define	SUPPLIESLOW	04
#define BOTOFFORM	02
#define BYTEREVERSE	01		/* Reverse byte order in words */

/* vacsh command bytes */
#define VAPLOT		0340
#define VAPRINT		0100
#define VAPRINTPLOT	0160
#define VAAUTOSTEP	0244
#define VANOAUTOSTEP	0045		/* unused */
#define	VAFORMFEED	0263		/* unused */
#define	VASLEW		0265		/* unused */
#define	VASTEP		0064		/* unused */

struct {
	char	va_open;
	char	va_busy;
	int	va_state;	/* State: bits are commands in vcmd.h. */
	int	va_wc;
	int	va_bufp;
	struct	buf *va_bp;
} va11;
int	va_ubinfo;

struct	buf rvabuf;		/* Used by physio for a buffer. */

vaopen()
{

	if (va11.va_open) {
		u.u_error = ENXIO;
		return;
	}
	va11.va_open = 1;
	VAADDR->vawc = 0;
	va11.va_wc = 0;
	va11.va_state = 0;
	VAADDR->vacsl = IENABLE;
	vatimo();
	vacmd(VPRINT);
	if (u.u_error)
		vaclose();
}

vastrategy(bp)
	register struct buf *bp;
{
	register int e;

	(void) spl4();
	while (va11.va_busy)
		sleep((caddr_t)&va11, VAPRI);
	va11.va_busy = 1;
	va11.va_bp = bp;
	va_ubinfo = ubasetup(bp, vabdp);
	va11.va_bufp = va_ubinfo & 0x3ffff;
	if (e = vaerror(DONE))
		goto brkout;
	va11.va_wc = -(bp->b_bcount/2);
	vastart();
	e = vaerror(DONE);	/* Wait for DMA to complete */
	va11.va_wc = 0;
	va11.va_bufp = 0;

	/*
	 * After printing a line of characters, VPRINTPLOT mode essentially
	 * reverts to VPLOT mode, plotting things until a new mode is set.
	 * This change is indicated by sending a VAAUTOSTEP command to
	 * the va.  We also change va_state to reflect this effective
	 * mode change.
	 */
	if (va11.va_state & VPRINTPLOT) {
		va11.va_state = (va11.va_state & ~VPRINTPLOT) | VPLOT;
		VAADDR->vacsh = VAAUTOSTEP;
		e |= vaerror(DONE);
	}
	(void) spl0();
brkout:
	ubafree(va_ubinfo), va_ubinfo = 0;
	va11.va_bp = 0;
	va11.va_busy = 0;
	iodone(bp);
	if (e)
		u.u_error = EIO;
	wakeup((caddr_t)&va11);
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

	while ((e = VAADDR->vacsw & (bit|ERROR)) == 0)
		sleep((caddr_t)&va11, VAPRI);
	return (e & ERROR);
}

vastart()
{
	if (va11.va_wc) {
		VAADDR->vaba = va11.va_bufp;
		VAADDR->vawc = va11.va_wc;
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
		(void) suword(addr, va11.va_state);
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

/*
 * Send a command code to the va, and wait for it to complete.
 * If an error occurs, u.u_error is set to EIO.
 * In any case, update va11.va_state.
 */
vacmd(vcmd)
{
	(void) spl4();
	(void) vaerror(DONE);		/* Wait for va to be ready */
	switch (vcmd) {

	case VPLOT:
		/* Must turn on plot AND autostep modes. */
		VAADDR->vacsh = VAPLOT;
		if (vaerror(DONE))
			u.u_error = EIO;
		VAADDR->vacsh = VAAUTOSTEP;
		break;

	case VPRINT:
		VAADDR->vacsh = VAPRINT;
		break;

	case VPRINTPLOT:
		VAADDR->vacsh = VAPRINTPLOT;
		break;
	}
	va11.va_state =
		(va11.va_state & ~(VPLOT | VPRINT | VPRINTPLOT)) | vcmd;

	if (vaerror(DONE))	/* Wait for command to complete. */
		u.u_error = EIO;
	(void) spl0();
}

vatimo()
{
	if (va11.va_open)
		timeout(vatimo, (caddr_t)0, HZ/10);
	vaintr(0);
}

/*ARGSUSED*/
vaintr(dev)
{
	wakeup((caddr_t)&va11);
}

vaclose()
{

	va11.va_open = 0;
	va11.va_busy = 0;
	va11.va_state = 0;
	va11.va_wc = 0;
	va11.va_bufp = 0;
	VAADDR->vacsl = 0;
}

#define	DELAY(N)	{ register int d; d = N; while (--d > 0); }

vareset()
{

	if (va11.va_open == 0)
		return;
	printf(" va");
	VAADDR->vacsl = IENABLE;
	if (va11.va_state & VPLOT) {
		VAADDR->vacsh = VAPLOT;
		DELAY(10000);
		VAADDR->vacsh = VAAUTOSTEP;
	} else if (va11.va_state & VPRINTPLOT)
		VAADDR->vacsh = VPRINTPLOT;
	else
		VAADDR->vacsh = VAPRINTPLOT;
	DELAY(10000);
	if (va11.va_busy == 0)
		return;
	if (va_ubinfo) {
		printf("<%d>", (va_ubinfo>>28)&0xf);
		ubafree(va_ubinfo), va_ubinfo = 0;
	}
	/* This code belongs in vastart() */
	va_ubinfo = ubasetup(va11.va_bp, vabdp);
	va11.va_bufp = va_ubinfo & 0x3ffff;
	va11.va_wc = (-va11.va_bp->b_bcount/2);
	/* End badly placed code */
	vastart();
}
