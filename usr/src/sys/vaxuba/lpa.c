/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)lpa.c	7.1 (Berkeley) 6/5/86
 */

#include "lpa.h"
#if NLPA > 0

#include "param.h"
#include "dir.h"
#include "user.h"
#include "buf.h"
#include "proc.h"
#include "ioctl.h"
#include "uio.h"

#include "ubavar.h"

/*
 * LPA driver for -- Asa Romberger
 *
 *	open
 *	write microcode
 *	write dedicated mode dispatch table
 *	ioctl TIOCSETP to set parameters
 *		struct iocb {
 *			short *baddr;	buffer address
 *			short rate;	- 1,000,000 / frequency in Hz
 *			short wc;	15-13 = number of buffers - 1
 *					12-0 = buffer size in words
 *		} iocb;
 *	read - 1 character indicating buffer index
 *		fill or empty buffer
 * minor device number = DDCCCCCC where:
 *	DD	= 00 for analog input
 *		= 01 for analog output
 *	CCCCCC	= channel number
 */
 *	define NOMCODE to eliminate the microcode download check
 */
/* #define TRACELPA */
/* #define NOMCODE */

#ifdef TRACELPA
#	define TRACER(x)	printf(x)
#	define TRACERN(x, d)	printf(x, d)
#else
#	define TRACER(x)
#	define TRACERN(x, d)
#endif

	/* PRIORITY AT WHICH PROGRAM SHOULD RUN */
	/* THIS SHOULD EVENTUALLY  TELL UNIX THIS IS A REAL-TIME DEVICE */

#define NICE	0

#define inc(v)		(sc->v = ((sc->v + 1) % sc->sc_nbuf))

#define LPAPRI		(PZERO + 0)
#define LPAUNIT(dev)	0
#define LPADEVICE(dev)	(((dev) >> 6) & 03)
#define LPACHANNEL(dev)	((dev) & 077)

int	lpaprobe(), lpaattach(), lpaiintr(), lpaointr();
u_short	lpastd[] = {0170460, 0};
struct	uba_device *lpadinfo[NLPA];
struct uba_driver lpadriver =
  {lpaprobe, 0, lpaattach, 0, lpastd, "lpa", lpadinfo, 0, 0, 0 };

struct lpa_softc {
	int	sc_flag;	/* flags, as defined below */
	int	sc_device;	/* device: 0 = analog in, 1 = analog out */
	int	sc_channel;	/* device channel number */
	struct buf sc_ubuffer;	/* user buffer header */
	int	sc_ubabuf;	/* uba allocation pointer for buffer */
	int	sc_ubufn;	/* present buffer that user is accessing */
	int	sc_lbufn;	/* present buffer that lpa is accessing */
	int	sc_lbufnx;	/* next buffer for lpa (value in ustat) */
	int	sc_nbuf;	/* number of buffers */
	int	sc_count;	/* buffer size in words */
	short	sc_ustat;	/* user status word */
	struct buf sc_ustatbuf;	/* dummy user status word buffer for ubasetup */
	int	sc_ubaustat;	/* uba allocation pointer for ustat */
	struct buf *sc_buffer;	/* scratch buffer header */
	int	sc_start;	/* 0 if lpa operation has been started */
} lpa_softc[NLPA];

/* flags for sc_flag */
#define OPEN	01		/* device is open */
#define MCODE	02		/* microcode has been loaded */
#define DMDT	04		/* dedicated mode dispatch table loaded */
#define STTY	010		/* stty call and device initialized */
#define SLEEP	020		/* sleeping */

/* bits for ustat */
#define DONE	0100000		/* done */
#define STOP	0040000		/* stop data transfer */
#define NBI	0003400		/* next buffer index */
#define LBI	0000003		/* last buffer index */

struct lpadevice {
	short	lcim;		/* control in and maintenance */
	short	lcos;		/* control and status out */
	short	lrda;		/* request description array address word */
	short	lms;		/* maintenance status */
};

/* control in and maintenance register bits */
#define	READYI	0000200		/* ready in */
#define IIE	0000100		/* in interrupt enable */
#define RDAEXT	0000014		/* rda address extension */
#define RDAEXTOFFSET	2	/* offset of RDAEXT from right side */
#define GO	0000001		/* go */
#define RUN	0100000		/* run */
#define RESET	0040000		/* reset */
#define CWRITE	0020000		/* cram write */
#define EA	0004000		/* enable arbitration */
#define ROMO	0002000		/* rom O */
#define ROMI	0001000		/* rom I */
#define SMICRO	0000400		/* step microprocessor */

/* control and status out register bits */
#define READYO	0200		/* ready out */
#define OIE	0100		/* out interrupt enable */
#define UINDEX	0007		/* user index */
#define ERROR	0100000		/* error */
#define ESTAT	0060000		/* error status */
#define ESCODE	0017400		/* error sub code */
#define ECODE	0077400		/* error status + error sub code */
#define OVERRUN	0243		/* overrun error */

/* LPA COMMAND DESCRIPTION AREA */

/* INIT COMMAND */
#define INIT	0		/* mode */
#define MCVERS	4		/* microcode version */
#define ACLOCKA	0170404		/* LPA bus addresses */
#define ACLOCKB	0170432
#define AAD1	0170400
#define AAD2	1		/* 0170440 - DOES NOT EXIST */
#define ADA	0170420
#define ADIO1	1		/* 0167770 - DOES NOT EXIST */
#define ADIO2	1		/* 0167760 - DOES NOT EXIST */
#define ADIO3	1		/* 0167750 - DOES NOT EXIST */
#define ADIO4	1		/* 0167740 - DOES NOT EXIST */
#define ADIO5	1		/* 0167730 - DOES NOT EXIST */

/* CLOCK START COMMAND */
#define CLOCK	1		/* mode */
#define CLOCKA	0<<4		/* clock A */
	/* clock status word */
#define ENACTR	1		/* enable counter */
#define R1M	1<<1		/* 1 MHz rate */
#define R100K	2<<1		/* 100 KHz rate */
#define R10K	3<<1		/* 10 KHz rate */
#define R1K	4<<1		/* 1 KHz rate */
#define R100	5<<1		/* 100 Hz rate */
#define REXT	6<<1		/* external rate (from st1 input) */
#define R60	7<<1		/* line frequency rate */
#define MFIE	0100		/* mode flag interrupt enable */
#define MSI	0<<8		/* single interval mode */
#define MRI	1<<8		/* repeat interval mode */
#define MEET	2<<8		/* external event time mode */
#define MEETZ	3<<8		/* external event time mode from zero base */
#define ST1EC	020000		/* st1 enable counter */
#define ST1IE	040000		/* st1 interrupt enable */

/* DATA TRANSFER START COMMAND */
#define DTS	2		/* mode */
#define SCHAN	1<<8		/* single channel */

lpaprobe(reg)
	caddr_t reg;
{
	register int br, cvec;	/* value result */
	register struct lpadevice *lpaaddr = (struct lpadevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
#endif
	/* this should force an interrupt, stall, clear the lpa */
	br = 0x15;
	cvec = 0330;
TRACER("PROBE\n");
	return (sizeof (struct lpadevice));
}

lpaattach(ui)
	register struct upa_device *ui;
{

}

lpaopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit = LPAUNIT(dev);
	register struct lpa_softc *sc = &lpa_softc[unit];
	register struct uba_device *ui = lpadinfo[unit];
	register struct lpadevice *lpaaddr = (struct lpadevice *) ui->ui_addr;

TRACER("OPEN\n");
	if (unit >= NLPA || sc->sc_flag & OPEN || ui == 0 ||
	    ui->ui_alive == 0)
		return (ENXIO);
	(void) splhigh();
	lpaaddr->lcim = RESET;
	lpaaddr->lcim = 0;
	(void) spl0();
	lpaaddr->lcos = 0;	/* clear the registers as a precaution */
	lpaaddr->lrda = 0;
	lpaaddr->lms = 0;
	sc->sc_flag = OPEN;
	sc->sc_device = LPADEVICE(dev);
	sc->sc_channel = LPACHANNEL(dev);
	sc->sc_buffer = geteblk();
	sc->sc_buffer->b_error = 0;
	sc->sc_buffer->b_proc = u.u_procp;
	sc->sc_ubufn = -1;
	/* THIS SHOULD EVENTUALLY SPECIFY "REAL-TIME" */
	u.u_procp->p_nice = NICE;
	return (0);
}

lpaclose(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit = LPAUNIT(dev);
	register struct lpa_softc *sc = &lpa_softc[unit];
	register struct uba_device *ui = lpadinfo[unit];
	register struct lpadevice *lpaaddr = (struct lpadevice *) ui->ui_addr;

	if (sc->sc_device && sc->sc_ubufn >= 0 && (sc->sc_flag & ERROR) == 0) {
		if (sc->sc_start)
			lpacmd(sc->sc_buffer, lpaaddr, sc, ui->ui_ubanum);
		sc->sc_flag |= STOP;
		(void) spl5();
		while (sc->sc_flag & STOP) {
TRACER("SLEEP\n");
			sc->sc_flag |= SLEEP;
			sleep((caddr_t)sc, LPAPRI);
		}
	}
	(void) splhigh();
	lpaaddr->lcim = RESET;
	lpaaddr->lcim = 0;
	(void) spl0();
	if (sc->sc_ubabuf) {
		ubarelse(ui->ui_ubanum, &sc->sc_ubabuf);
		sc->sc_ubabuf = 0;
		(void) splclock();
		vsunlock(sc->sc_ubuffer.b_un.b_addr, sc->sc_ubuffer.b_bcount,
			(sc->sc_device)? B_READ : B_WRITE);
		u.u_procp->p_flag &= ~SPHYSIO;
		(void) spl0();
	}
	if (sc->sc_ubaustat) {
		ubarelse(ui->ui_ubanum, &sc->sc_ubaustat);
		sc->sc_ubaustat = 0;
	}
	if (sc->sc_buffer) {
		brelse(sc->sc_buffer);
		sc->sc_buffer = 0;
	}
	sc->sc_flag = 0;
TRACER("CLOSE\n");
}

lpawrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = LPAUNIT(dev);
	register struct lpa_softc *sc = &lpa_softc[unit];
	register struct uba_device *ui = lpadinfo[unit];
	register struct lpadevice *lpaaddr = (struct lpadevice *) ui->ui_addr;
	register int f;

TRACER("WRITE\n");
	f = sc->sc_flag;
	if ((f & OPEN) == 0)
		return (ENXIO);
	if ((f & MCODE) == 0)		/* first write is the microcode */
		return (lpamcode(lpaaddr, sc, uio));
	if ((f & DMDT) == 0)		/* second write is the dispatch table */
		return (lpadmdt(lpaaddr, sc, ui->ui_ubanum, uio));
	return (ENXIO);
}

lpamcode(lpaaddr, sc, uio)
	register struct lpadevice *lpaaddr;
	register struct lpa_softc *sc;
	struct uio *uio;
{
	short v, r;
	register int mcaddr;
	int error;

	mcaddr = 0;
	while (uio->uio_resid) {
		error = uiomove(&v, 2, UIO_WRITE, uio);
		if (error)
			break;
		lpaaddr->lcim = 0;		/* load microcode word */
		lpaaddr->lrda = mcaddr;
		lpaaddr->lms = v;
		lpaaddr->lcim = ROMO;
		lpaaddr->lcim |= CWRITE;
		lpaaddr->lcim = 0;		/* verify microcode word */
		lpaaddr->lrda = mcaddr;
		lpaaddr->lcim = ROMO;
		if ((r = lpaaddr->lms) != v) {
			/* download failure */
			printf("LPA MICROCODE FAIL: exp:%o got:%o\n", v, r);
			return (ENXIO);
		}
		mcaddr++;
	}
	lpaaddr->lcim = RUN | EA;	/* turn it on */
	sc->sc_flag |= MCODE;
	lpaaddr->lcim |= IIE;
	lpaaddr->lcos |= OIE;
	return (error);
TRACER("MCODE\n");
}

lpadmdt(lpaaddr, sc, ubanum, uio)
	register struct lpadevice *lpaaddr;
	register struct lpa_softc *sc;
	register short ubanum;
	struct uio *uio;
{
	register short *p;
	register int n;
	int error;

	p = (short *) sc->sc_buffer->b_un.b_addr;		/* INIT */
	*p++ = (MCVERS << 8) | INIT;	/* mode */
	*p++ = ACLOCKA;		/* LPA bus device addresses */
	*p++ = ACLOCKB;
	*p++ = AAD1;
	*p++ = AAD2;
	*p++ = ADA;
	*p++ = ADIO1;
	*p++ = ADIO2;
	*p++ = ADIO3;
	*p++ = ADIO4;
	*p++ = ADIO5;
	n = MIN(uio->uio_resid, 256);	/* dedicated mode dispatch table */
	error = uiomove((char *)p, n, UIO_WRITE, uio);
	if (error)
		return (error);
	n >>= 1;
	p += n;
	while (n++ < 128)
		*p++ = 0;
	lpacmd(sc->sc_buffer, lpaaddr, sc, ubanum);
	sc->sc_flag |= DMDT;
	return (0);
TRACER("DMDT\n");
}

lpaioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register int unit = LPAUNIT(dev);
	register struct lpa_softc *sc = &lpa_softc[unit];
	register struct uba_device *ui = lpadinfo[unit];
	register struct lpadevice *lpaaddr = (struct lpadevice *) ui->ui_addr;
	register short *p;
	register int i;
	register int v;
	struct iocb {
		short *baddr;
		short rate;
		short wc;
	} *iocb;

TRACER("IOCTL IN\n");
	if (cmd != TIOCSETP || (sc->sc_flag & DMDT) == 0)
		return (ENXIO);
	iocb = (struct iocb *)data;
	p = (short *) sc->sc_buffer->b_un.b_addr;	/* CLOCK START */
	*p++ = CLOCK | CLOCKA;			/* mode */
	*p++ = ENACTR | R1M | MFIE | MRI;	/* clock status */
	*p = iocb->rate;			/* clock preset */
	lpacmd(sc->sc_buffer, lpaaddr, sc, ui->ui_ubanum);
TRACER("CLOCK STARTED\n");
	p = (short *) sc->sc_buffer->b_un.b_addr;	/* DATA TRANSFER START*/
	*p++ = (sc->sc_device << 7) | DTS | SCHAN;	/* mode */
	sc->sc_count = iocb->wc & 017777;	/* word count per buffer */
	*p++ = sc->sc_count;
							/* user status word */
	sc->sc_ustatbuf.b_un.b_addr = (caddr_t) &sc->sc_ustat;
	sc->sc_ustatbuf.b_flags = 0;
	sc->sc_ustatbuf.b_bcount = 2;
	sc->sc_ustatbuf.b_proc = u.u_procp;
	sc->sc_ubaustat = ubasetup(ui->ui_ubanum, &sc->sc_ustatbuf, 0);
	v = sc->sc_ubaustat;
	*p++ = v;
	*p = (v >> 16) & 03;		/* into low portion of word */
	sc->sc_nbuf = (iocb->wc >> 13) & 07;	/* number of buffers */
	*p++ |= sc->sc_nbuf++ << 8;		/* into high portion of word */
					/* buffer addresses */
	if (useracc(sc->sc_ubuffer.b_un.b_addr = (caddr_t) iocb->baddr,
	    sc->sc_ubuffer.b_bcount = sc->sc_count * sc->sc_nbuf * 2,
	    (i = (sc->sc_device)? B_READ : B_WRITE) ) == NULL) {
TRACER("USER BUFFER FAULT\n");
		return (EFAULT);
	}
	sc->sc_ubuffer.b_flags = B_PHYS | B_BUSY | i;
	sc->sc_ubuffer.b_proc = u.u_procp;
	u.u_procp->p_flag |= SPHYSIO;
	vslock(sc->sc_ubuffer.b_un.b_addr, sc->sc_ubuffer.b_bcount);
	sc->sc_ubabuf = ubasetup(ui->ui_ubanum, &sc->sc_ubuffer, 0);
	v = sc->sc_ubabuf;
	for (i = 0; i < sc->sc_nbuf; i++) {
		*p++ = v;
		*p++ = (v >> 16) & 03;
		v += sc->sc_count * 2;
	}
	for ( ; i <= 7; i++) {
		*p++ = 0;
		*p++ = 0;
	}
	*p++ = 0; *p++ = 0;		/* random channel list address */
	*p++ = 0;			/* delay */
	*p++ = sc->sc_channel;		/* start channel, channel inc */
	*p++ = 1;			/* number of samples in a sequence */
	*p++ = 0;			/* dwell */
	*p++ = 0;			/* start word no., event mark word */
	*p++ = 0;			/* start word mask */
	*p = 0;				/* event mark mask */
	sc->sc_ustat = 0;
	sc->sc_start = (sc->sc_device)? sc->sc_nbuf+1 : 1;
	sc->sc_lbufn = 0;
	sc->sc_lbufnx = 0;
	sc->sc_flag |= STTY;
TRACER("IOCTL OUT\n");
	return (0);
}

lparead(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = LPAUNIT(dev);
	register struct lpa_softc *sc = &lpa_softc[unit];
	register struct uba_device *ui = lpadinfo[unit];
	register struct lpadevice *lpaaddr = (struct lpadevice *) ui->ui_addr;

TRACER("READ\n");
	if ((sc->sc_flag & STTY) == 0)
		return (ENXIO);
	if (sc->sc_flag & ERROR)
		return (ENXIO);
	if (sc->sc_start)
		if (--sc->sc_start == 0) {
			lpacmd(sc->sc_buffer, lpaaddr, sc, ui->ui_ubanum);
TRACER("START\n");
		}
	inc(sc_ubufn);
	if (sc->sc_start == 0) {
		(void) spl5();
		while (sc->sc_ubufn == sc->sc_lbufn) {
			if (sc->sc_flag & ERROR)
				return (ENXIO);
TRACER("SLEEP\n");
			sc->sc_flag |= SLEEP;
			sleep(sc, LPAPRI);
		}
		(void) spl0();
	}
TRACERN("READ %d\n", sc->sc_ubufn);
	return (uiomove(&sc->sc_ubufn, 1, UIO_READ, uio));
}

lpacmd(bp, lpaaddr, sc, ubanum)
	register struct buf *bp;
	register struct lpadevice *lpaaddr;
	register struct lpa_softc *sc;
	register short ubanum;
{
	int ubareg;

TRACER("CMD\n");
	ubareg = ubasetup(ubanum, bp, UBA_NEEDBDP);
	lpawait(lpaaddr, sc);
	lpaaddr->lrda = ubareg;
	lpaaddr->lcim &= ~RDAEXT;
	lpaaddr->lcim |= ((ubareg >> (16-RDAEXTOFFSET)) & RDAEXT) | GO;
	lpawait(lpaaddr, sc);
	ubarelse(ubanum, &ubareg);
}

lpawait(lpaaddr, sc)
	register struct lpadevice *lpaaddr;
	register struct lpa_softc *sc;
{

	(void) spl5();
	while ((lpaaddr->lcim & READYI) == 0) {
TRACER("SLEEP\n");
		sc->sc_flag |= SLEEP;
		sleep((caddr_t)sc, LPAPRI);
	}
	(void) spl0();
}

lpaiintr(unit)
	int unit;
{
	register struct lpa_softc *sc = &lpa_softc[unit];

TRACER("{I");
	if (sc->sc_flag & SLEEP) {
TRACER("<WAKEUP>");
		wakeup((caddr_t)sc);
		sc->sc_flag &= ~SLEEP;
	}
TRACER("}");
}

lpaointr(unit)
	int unit;
{
	register int c, m;
	register struct lpa_softc *sc = &lpa_softc[unit];
	register struct uba_device *ui = lpadinfo[unit];
	register struct lpadevice *lpaaddr = (struct lpadevice *) ui->ui_addr;
	int spx;

TRACER("{O");
	if (sc->sc_flag & SLEEP) {
TRACER("<WAKEUP>");
		wakeup(sc);
		sc->sc_flag &= ~SLEEP;
	}
	c = lpaaddr->lcos;
	m = lpaaddr->lms;
	lpaaddr->lcos &= ~READYO;
	if (c & ERROR) {
TRACER("<ERROR>");
		c = (c >> 8) & 0377;
		if ((sc->sc_flag & STOP) == 0 || (c != OVERRUN)) {
			printf("LPA ERROR %o %o\n", c, m&0177777);
			sc->sc_flag |= ERROR;
		}
		sc->sc_flag &= ~STOP;
TRACER("}\n");
		return;
	}
TRACERN("<LPA %d>", sc->sc_lbufnx);
	sc->sc_lbufn = sc->sc_lbufnx;
	if (sc->sc_ubufn == sc->sc_lbufnx && c & ECODE) {
TRACER("<STOP?>");
		if (sc->sc_flag & STOP)
			return;
		printf("LPA OVERRUN\n");
		sc->sc_flag |= ERROR;
	}
	inc(sc_lbufnx);
TRACERN("<USTAT %o>", sc->sc_ustat);
	spx = splhigh();
	sc->sc_ustat &= ~NBI;
	sc->sc_ustat |= sc->sc_lbufnx << 8;
	sc->sc_ustat &= ~DONE;
	splx(spx);
TRACERN("<LPAN %d>}", sc->sc_lbufnx);
}

lpareset(uban)
	int uban;
{
	register struct uba_device *ui;
	register struct lpadevice *lpaaddr;
	register struct lpa_softc *sc;
	register int unit;

TRACER("LPA RESET\n");
	for (unit = 0; unit < NLPA; unit++) {
		if ((ui = lpadinfo[unit]) == 0 ||
		    ui->ui_ubanum != uban || ui->ui_alive == 0)
			continue;
		printf(" lpa%d", unit);
		lpaaddr = (struct lpadevice *)ui->ui_addr;
		sc = &lpa_softc[unit];
		sc->sc_flag |= ERROR;
		(void) splhigh();
		lpaaddr->lcim = RESET;
		lpaaddr->lcim = 0;
		(void) spl0();
		if (sc->sc_flag & SLEEP) {
			wakeup((caddr_t)sc);
			sc->sc_flag &= ~SLEEP;
		}
	}
}
#endif NLPA
