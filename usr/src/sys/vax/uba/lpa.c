/*	lpa.c	4.1	82/05/27	*/
#include "lpa.h"
#if NLPA > 0

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/ubavar.h"
#include "../h/proc.h"
#include "../h/ioctl.h"

/*
 *	LPA driver for 4.1BSD
 *	Asa Romberger
 * method of usage:
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
/*
 *	define TRACELPA to get trace printouts on the console
 *	define NOMCODE to eliminate the microcode download check
 */
/*	#define	NOMCODE		*/

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

	/* WAKEUP PRIORITY */

#define LPAPRI	(PZERO + 0)

	/* MACRO DEFINITIONS */
#define inc(v)	(sc->v = ((sc->v + 1) % sc->sc_nbuf))
#define LPAUNIT(dev)	0
#define LPADEVICE(dev)	(((dev) >> 6) & 03)
#define LPACHANNEL(dev)	((dev) & 077)

	/* DEFINITIONS FOR INTERACTION WITH UNIX I/O */

int lpaprobe(), /*lpaslave(),*/ lpaattach() /*,lpadgo()*/;
int lpaiintr(), lpaointr();
u_short lpastd[] = {0170460, 0};
struct uba_device *lpadinfo[NLPA];
/*struct uba_ctlr *lpaminfo[Ndevice name];*/
struct uba_driver lpadriver =
	{lpaprobe, 0/*lpaslave*/, lpaattach, 0/*lpadgo*/, lpastd,
	"lpa", lpadinfo, 0/*"device name"*/, 0/*lpaminfo*/, 0/*exclusive use*/};


	/* LPA SOFTWARE OPERATION FLAGS */

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
    /* flag bits */
#define OPEN	01		/* device is open */
#define MCODE	02		/* microcode has been loaded */
#define DMDT	04		/* dedicated mode dispatch table loaded */
#define STTY	010		/* stty call and device initialized */
#define SLEEP	020		/* sleeping */
    /* ustat bits */
#define DONE	0100000		/* done */
#define STOP	0040000		/* stop data transfer */
#define NBI	0003400		/* next buffer index */
#define LBI	0000003		/* last buffer index */

	/* DEVICE REGISTER DESCRIPTION AREA */

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

	/* THE ROUTINES THEMSELVES */

/*
 *	probe lpa to get br level and interrupt vector
 */
lpaprobe(reg)
caddr_t reg;
{
	register int br, cvec;	/* value result (required for UNIX) */
	register struct lpadevice *lpaaddr = (struct lpadevice *) reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
#endif
	/* this should force an interrupt, stall, clear the lpa */
	br = 0x15;
	cvec = 0330;
TRACER("PROBE\n");
	return (1);
}

/*
 *	attach the specified controller
 */
lpaattach(ui)
register struct upa_device *ui;
{
	/* any stuff necessary for initialization can go here */
}

/*
 *	open the device
 */
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
	    ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	(void) spl7();
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
}

/*
 *	close the device
 */
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
	(void) spl7();
	lpaaddr->lcim = RESET;
	lpaaddr->lcim = 0;
	(void) spl0();
	if (sc->sc_ubabuf) {
		ubarelse(ui->ui_ubanum, &sc->sc_ubabuf);
		sc->sc_ubabuf = 0;
		(void) spl6();
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

/*
 *	write
 *		first write is the microcode
 *		second write is the dispatch table
 */
lpawrite(dev)
dev_t dev;
{
	register int unit = LPAUNIT(dev);
	register struct lpa_softc *sc = &lpa_softc[unit];
	register struct uba_device *ui = lpadinfo[unit];
	register struct lpadevice *lpaaddr = (struct lpadevice *) ui->ui_addr;
	register int f;

TRACER("WRITE\n");
	f = sc->sc_flag;
	if ((f & OPEN) == 0) {
		u.u_error = ENXIO;
		return;
	}
	if ((f & MCODE) == 0) {
		lpamcode(lpaaddr, sc);
		return;
	}
	if ((f & DMDT) == 0) {
		lpadmdt(lpaaddr, sc, ui->ui_ubanum);
		return;
	}
	/* writes are only for microcode and dedicated mode dispatch table */
	u.u_error = ENXIO;
}

lpamcode(lpaaddr, sc)
register struct lpadevice *lpaaddr;
register struct lpa_softc *sc;
{
	short v, r;
	register int mcaddr;

	mcaddr = 0;
	while (u.u_count) {
		iomove(&v, 2, B_WRITE);		/* get next microcode word */
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
			u.u_error = ENXIO;
			return;
		}
		mcaddr++;
	}
	lpaaddr->lcim = RUN | EA;	/* turn it on */
	sc->sc_flag |= MCODE;
	lpaaddr->lcim |= IIE;
	lpaaddr->lcos |= OIE;
TRACER("MCODE\n");
}

lpadmdt(lpaaddr, sc, ubanum)
register struct lpadevice *lpaaddr;
register struct lpa_softc *sc;
register short ubanum;
{
	register short *p;
	register int n;

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
	n = min(u.u_count, 256);	/* dedicated mode dispatch table */
	iomove((char *) p, n, B_WRITE);
	n >>= 1;
	p += n;
	while (n++ < 128)
		*p++ = 0;
	lpacmd(sc->sc_buffer, lpaaddr, sc, ubanum);
	sc->sc_flag |= DMDT;
TRACER("DMDT\n");
}

lpaioctl(dev, cmd, addr, flag)
dev_t dev;
caddr_t *addr;
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
	} iocb;

TRACER("IOCTL IN\n");
	if (cmd != TIOCSETP) {
TRACER("NOT TIOCSETP\n");
		/* not valid */
		u.u_error = ENXIO;
		return;
	}
#ifndef NOMCODE
	if ((sc->sc_flag & DMDT) == 0) {
TRACER("NO DMDT\n");
		u.u_error = ENXIO;
		return;
	}
#endif
	if (copyin(addr, (caddr_t)&iocb, sizeof(iocb))) {
TRACER("COPYIN FAULT\n");
		u.u_error = EFAULT;
		return;
	}
	p = (short *) sc->sc_buffer->b_un.b_addr;	/* CLOCK START */
	*p++ = CLOCK | CLOCKA;			/* mode */
	*p++ = ENACTR | R1M | MFIE | MRI;	/* clock status */
	*p = iocb.rate;				/* clock preset */
	lpacmd(sc->sc_buffer, lpaaddr, sc, ui->ui_ubanum);
TRACER("CLOCK STARTED\n");
	p = (short *) sc->sc_buffer->b_un.b_addr;	/* DATA TRANSFER START*/
	*p++ = (sc->sc_device << 7) | DTS | SCHAN;	/* mode */
	sc->sc_count = iocb.wc & 017777;	/* word count per buffer */
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
	sc->sc_nbuf = (iocb.wc >> 13) & 07;	/* number of buffers */
	*p++ |= sc->sc_nbuf++ << 8;		/* into high portion of word */
					/* buffer addresses */
	if (useracc(sc->sc_ubuffer.b_un.b_addr = (caddr_t) iocb.baddr,
		    sc->sc_ubuffer.b_bcount = sc->sc_count * sc->sc_nbuf * 2,
		    (i = (sc->sc_device)? B_READ : B_WRITE) ) == NULL) {
TRACER("USER BUFFER FAULT\n");
			u.u_error = EFAULT;
			return;
	}
	sc->sc_ubuffer.b_flags = B_PHYS | B_BUSY | i;
	sc->sc_ubuffer.b_proc = u.u_procp;
	u.u_procp->p_flag |= SPHYSIO;
	vslock(sc->sc_ubuffer.b_un.b_addr, sc->sc_ubuffer.b_bcount);
/*	sc->sc_ubabuf = ubasetup(ui->ui_ubanum, &sc->sc_ubuffer, UBA_NEEDBDP);*/
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
}

/*
 *	read
 *		read 1 character only - the next available buffer number
 */
lparead(dev)
dev_t dev;
{
	register int unit = LPAUNIT(dev);
	register struct lpa_softc *sc = &lpa_softc[unit];
	register struct uba_device *ui = lpadinfo[unit];
	register struct lpadevice *lpaaddr = (struct lpadevice *) ui->ui_addr;

TRACER("READ\n");
	if ((sc->sc_flag & STTY) == 0) {
		u.u_error = ENXIO;
		return;
	}
	if (sc->sc_flag & ERROR) {
		u.u_error = ENXIO;
		return;
	}
	if (sc->sc_start)
		if (--sc->sc_start == 0) {
			lpacmd(sc->sc_buffer, lpaaddr, sc, ui->ui_ubanum);
TRACER("START\n");
		}
	inc(sc_ubufn);
	if (sc->sc_start == 0) {
		(void) spl5();
		while (sc->sc_ubufn == sc->sc_lbufn) {
			if (sc->sc_flag & ERROR) {
				u.u_error = ENXIO;
				return;
			}
TRACER("SLEEP\n");
			sc->sc_flag |= SLEEP;
			sleep(sc, LPAPRI);
		}
		(void) spl0();
	}
TRACERN("READ %d\n", sc->sc_ubufn);
	iomove(&sc->sc_ubufn, 1, B_READ);
}

/*
 *	execute a command and wait for completion
 */
lpacmd(bp, lpaaddr, sc, ubanum)
register struct buf *bp;
register struct lpadevice *lpaaddr;
register struct lpa_softc *sc;
register short ubanum;
{
	int ubareg;

TRACER("CMD\n");
/*	bp->b_flags |= B_BUSY|B_WRITE;		*/
	ubareg = ubasetup(ubanum, bp, UBA_NEEDBDP);
	lpawait(lpaaddr, sc);
	lpaaddr->lrda = ubareg;
	lpaaddr->lcim &= ~RDAEXT;
	lpaaddr->lcim |= ((ubareg >> (16-RDAEXTOFFSET)) & RDAEXT) | GO;
	lpawait(lpaaddr, sc);
	ubarelse(ubanum, &ubareg);
/*	bp->b_flags &= ~(B_BUSY|B_WRITE);		*/
}

/*
 *	wait for completion (ready input)
 */
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

/*
 *	lpaiintr
 *		in interrupt
 *		LPA is now ready to accept a user request
 */
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

/*
 *	lpaointr
 *		out interrupt
 *		LPA has status information
 */
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
	spx = spl7();
	sc->sc_ustat &= ~NBI;
	sc->sc_ustat |= sc->sc_lbufnx << 8;
	sc->sc_ustat &= ~DONE;
	(void) splx(spx);
TRACERN("<LPAN %d>}", sc->sc_lbufnx);
}

/*
 *	reset called for a unibus reset
 */
lpareset(uban)
int uban;
{
	register struct uba_device *ui;
	register struct lpadevice *lpaaddr;
	register struct lpa_softc *sc;
	register int unit;

TRACER("LPA RESET\n");
	for (unit = 0; unit < NLPA; unit++) {
		if (	(ui = lpadinfo[unit]) == 0 ||
			ui->ui_ubanum != uban ||
			ui->ui_alive == 0)
				continue;
		printf(" lpa%d", unit);
		lpaaddr = (struct lpadevice *)ui->ui_addr;
		sc = &lpa_softc[unit];
		sc->sc_flag |= ERROR;
		(void) spl7();
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
