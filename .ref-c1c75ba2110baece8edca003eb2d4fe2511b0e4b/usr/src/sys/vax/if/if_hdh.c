/*	@(#)if_hdh.c	7.5 (Berkeley) %G% */


/************************************************************************\

     ________________________________________________________
    /                                                        \
   |          AAA          CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |
   |         AAAAA        CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |
   |        AAAAAAA       CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |
   |       AAAA AAAA      CCCC              CCCC              |
   |      AAAA   AAAA     CCCC              CCCC              |
   |     AAAA     AAAA    CCCC              CCCC              |
   |    AAAA       AAAA   CCCC              CCCC              |
   |   AAAA  AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |
   |  AAAA    AAAAAAAAAAA CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |
   | AAAA      AAAAAAAAA   CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |
    \________________________________________________________/

	Copyright (c) 1984 by Advanced Computer Communications
	720 Santa Barbara Street, Santa Barbara, California  93101
	(805) 963-9431

	This software may be duplicated and used on systems
	which are licensed to run U.C. Berkeley versions of
	the UNIX operating system.  Any duplication of any
	part of this software must include a copy of ACC's
	copyright notice.


File:
		if_hdh.c

Author:
		Art Berggreen

Project:
		4.2BSD HDH

Function:
		Device specific driver for IF-11/HDH under 4.2BSD
    		networking code.

Revision History:
		Converted to 4.3, updated, UCB.
		31-Aug-1984: V1.0 - First Implementation. A.B.
		 6-Nov-1984: V1.1 - Supress extra "LINE DOWN" msgs. A.B.
		13-Jan-1984: V1.2 - Add conditionals for TWG. A.B.

\************************************************************************/




/* $Header$ */

#include "hdh.h"
#ifdef NHDH > 0

/*
 *
 * ACC IF-11/HDH interface
 *
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "vmmac.h"

#include "../machine/pte.h"

#include "../net/if.h"
#include "../netimp/if_imp.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

#include "if_hdhreg.h"
#include "if_uba.h"

int     hdhprobe(), hdhattach(), hdhintr();
struct  uba_device *hdhinfo[NHDH];
u_short hdhstd[] = { 0 };
struct  uba_driver hdhdriver =
	{ hdhprobe, 0, hdhattach, 0, hdhstd, "hdh", hdhinfo };

#define	HDHUNIT(x)	minor(x)

int	hdhinit(), hdhoutput(), hdhreset();

/*
 * "Lower half" of IMP interface driver.
 *
 * Each IMP interface is handled by a common module which handles
 * the IMP-host protocol and a hardware driver which manages the
 * hardware specific details of talking with the IMP.
 *
 * The hardware portion of the IMP driver handles DMA and related
 * management of UNIBUS resources.  The IMP protocol module interprets
 * contents of these messages and "controls" the actions of the
 * hardware module during IMP resets, but not, for instance, during
 * UNIBUS resets.
 *
 * The two modules are coupled at "attach time", and ever after,
 * through the imp interface structure.  Higher level protocols,
 * e.g. IP, interact with the IMP driver, rather than the HDH.
 */

#define NHDHCH	2		/* no. of FDX channels for HDH */
#define SUPR	0		/* supervisor channel */
#define	DATA	1		/* data channel */
#define HDHSUPR	0		/* supervisor read */
#define HDHSUPW	1		/* supervisor write */
#define HDHDATR	2		/* data read */
#define HDHDATW	3		/* data write */

#define HDH_UP		2	/* HDH protocol is up */
#define HDH_STARTED	1	/* HDH has been initialized */

#define HCBUSY	1		/* HDH HDX channel busy flag */

/*
/* The IF-11/HDH has four independent dath flow channels between the
/* front-end and the host.  Two are used for reading and writing
/* control messages and two are used for data flow.  Each IF-11/HDH
/* has a device dependent data structure (hdh_softc) which contains
/* an array of four channel dependent structures (hdh_chan) to maintain
/* the context of each channel.  Channel structures can be linked into
/* a queue of I/O requests pending for the hardware interface.
/* UNIBUS mapping resources are allocated for each channel pair.
*/

struct	hdh_chan {		/* HDH HDX channel structure */
	struct hdh_chan	*hc_next;	/* link for Start I/O queuing */
	char		hc_chan;	/* HDX chan number */
	char		hc_adx;		/* extended UNIBUS address bits */
	short		hc_addr;	/* lower UNIBUS address bits */
	short		hc_cnt;		/* byte count */
	char		hc_func;	/* UMC I/O function */
	char		hc_sbfc;	/* UMC I/O subfunction */
	short		hc_flags;	/* status flags */
};

struct	hdh_sioq {		/* Start I/O queue head structure */
	struct hdh_chan *sioq_head;	/* pointer to queue head */
	struct hdh_chan *sioq_tail;	/* pointer to queue tail */
};

struct	hdh_softc {		/* HDH device dependent structure */
	struct imp_softc *hdh_imp;	/* pointer to IMP's imp_softc struct */
	struct ifuba	hdh_ifuba[NHDHCH]; /* UNIBUS resources */
	struct hdh_chan hdh_chan[2*NHDHCH]; /* HDX HDH channels */
	struct hdh_sioq hdh_sioq;	/* start i/o queue */
	short		hdh_flags;	/* various status conditions */
} hdh_softc[NHDH];


/*
 * Normally, code goes here to cause the device to interrupt to determine its
 * interrupt vector.  However, since the UMC must be told its vector in order
 * to interrupt, we allocate and return an unused vector and initialize the
 * UMC.
 */
hdhprobe(reg)
caddr_t reg;
{
	register int br, cvec;
	struct hdhregs *addr = (struct hdhregs *)reg;
#ifdef lint
	br = 0; cvec = br; br = cvec;
	hdhintr(0);
#endif

	br = 0x15;			/* priority 21 (5 on UNIBUS) */

#ifdef HDHDEBUG
	cvec = 0270;			/* use constant for now ... */
#else

#ifdef VAXVMS				/* if VMS */
	cvec = 0270;			/*   we can't allocate vectors */
#else
	cvec = (uba_hd[numuba].uh_lastiv -= 4);  /* available vector */
#endif VAXVMS

#endif HDHDEBUG

	addr->ioini = (char) 0;		/* init UMC regs */
	addr->staack = (char) 0;	/*   pass vector */
	addr->ionmi = (char) 0;		/*     and kick UMC */
	addr->iochn = (char) (cvec >> 2);
	addr->csr = (short) HDH_RST;
	addr->csr = (short) (HDH_IEN|HDH_DMA|HDH_WRT); /* set enables */
	DELAY(5000);			/* give the UMC some time */
	return(1);
}

/*
 * Call the IMP module to allow it to set up its internal
 * state, then tie the two modules together by setting up
 * the back pointers to common data structures.
 */
hdhattach(ui)
	register struct uba_device *ui;
{
	register struct hdh_softc *sc = &hdh_softc[ui->ui_unit];
	register struct impcb *ip;

	if ((sc->hdh_imp = impattach(ui->ui_driver->ud_dname, ui->ui_unit,
	    hdhreset)) == 0)
		return;
	ip = &sc->hdh_imp->imp_cb;
	ip->ic_init = hdhinit;
	ip->ic_output = hdhoutput;
	sc->hdh_ifuba[ui->ui_unit].ifu_flags = UBA_CANTWAIT;
}

/*
 * Reset interface after UNIBUS reset.
 */
hdhreset(unit, uban)
int unit, uban;
{
	register struct uba_device *ui = hdhinfo[unit];
	register struct hdh_softc *sc = &hdh_softc[unit];

#ifdef HDHDEBUG
	printf("HDH RESET\n");
#endif HDHDEBUG

	if ((unit >= NHDH) || (ui == 0) || (ui->ui_alive == 0)
	    || (ui->ui_ubanum != uban))
		return;
	printf(" hdh%d", unit);
	sc->hdh_imp->imp_if.if_flags &= ~IFF_RUNNING;
	sc->hdh_imp->imp_cb.ic_oactive = 0;
	sc->hdh_flags = 0;
	(*sc->hdh_imp->imp_if.if_init)(sc->hdh_imp->imp_if.if_unit);
}

/*
 * Initialize the imp interface.
 */

static char init_blk[] = 
    {
	HDHINIT,		/* SYSINIT opcode			*/
	HDHRQUP & 0xff,		/* control code (LSB)			*/
	(HDHRQUP>>8) & 0xff,	/* control code (MSB)			*/
	10,			/* command extension len		*/
	0,			/* loopback mode (off)			*/
	3,			/* our address (3=DTE)			*/
	1,			/* their address (1=DCE)		*/
	3,			/* frame ack t1 timeout			*/
	3,			/* poll ack timeout			*/
	30,			/* adm wait timeout			*/
	3,			/* rej wait timeout			*/
	10,			/* max retries				*/
	3,			/* watchdog timeout			*/
	0xaa			/* baud rate (0xaa=38.4KB)		*/
				/*   (output on RS-232 pin 24,		*/
				/*    send/receive timing is always	*/
				/*    taken from pins 15/17)		*/
    };

hdhinit(unit)
int unit;
{	
	register struct hdh_softc *sc;
	register struct uba_device *ui;
	int i;

#ifdef HDHDEBUG
	printf("HDH INIT\n");
#endif HDHDEBUG

	if (unit >= NHDH || (ui = hdhinfo[unit]) == NULL
	    || ui->ui_alive == 0) {
		printf("hdh%d: not alive\n", unit);
		return(0);
	}
	sc = &hdh_softc[unit];

	if (sc->hdh_flags & HDH_STARTED)
		return(1);

	/*
	 * Alloc uba resources
	 */
	if ((sc->hdh_imp->imp_if.if_flags & IFF_RUNNING) == 0)
	    for(i=0;i<NHDHCH;i++) {
		if (if_ubainit(&sc->hdh_ifuba[i], ui->ui_ubanum, 0,
		    (int)btoc(IMP_RCVBUF)) == 0) {
			printf("hdh%d: cannot get chan %d uba resources\n",
				unit, i);
			ui->ui_alive = 0;
			return(0);
		}
	}

	sc->hdh_imp->imp_if.if_flags |= IFF_RUNNING;
	sc->hdh_flags = HDH_STARTED;

	/*
	 * hang a supervisor read (for line status)
	 */
	hdh_iorq(unit, HDHSUPR, IMP_RCVBUF, HDHRDB);

	/*
	 * hang a data read
	 */
	hdh_iorq(unit, HDHDATR, IMP_RCVBUF, HDHRDB+HDHSTR);

	/*
	 * bring up line to IMP
	 */

	snd_supr(unit, init_blk, sizeof(init_blk));

	return(1);
}

/*
 * Start an output operation on an mbuf.
 */
hdhoutput(unit, m)
	int unit;
	struct mbuf *m;
{
	register struct hdh_softc *sc = &hdh_softc[unit];
        int len;

	/*
	 * If output isn't active, attempt to
	 * start sending a new packet.
	 */

	if (sc->hdh_imp->imp_cb.ic_oactive) {
		printf("hdh%d: start on active unit\n", unit);
		return;
	}

	if ((sc->hdh_flags & HDH_UP) == 0) {
		/* Link not up, can't xmit */
		return;
	}

	len = if_wubaput(&sc->hdh_ifuba[DATA], m);	/* copy data to mapped mem */
	sc->hdh_imp->imp_cb.ic_oactive = 1;

	hdh_iorq(unit, HDHDATW, len, HDHWRT+HDHEOS);
}

/*
 * Start i/o operation on a UMC logical channel
 */
hdh_iorq(unit, lcn, len, func)
int unit, lcn, len, func;
{
	register struct hdh_softc *sc = &hdh_softc[unit];
	register struct hdh_chan *hc = &sc->hdh_chan[lcn];
	register int info, s;

	/*
	 * If channel is busy (shouldn't be), drop.
	 */
	if  (hc->hc_flags & HCBUSY) {
		printf("hdh%d: channel busy lcn=%d\n", unit, lcn);
		return;
	}

 	/* get appropriate UNIBUS mapping info */

	if (lcn & 1)		/* read or write? */
		info = sc->hdh_ifuba[lcn>>1].ifu_w.ifrw_info;
	else
		info = sc->hdh_ifuba[lcn>>1].ifu_r.ifrw_info;

	/* set channel info */

	hc->hc_flags |= HCBUSY;
	hc->hc_chan = lcn;
	hc->hc_adx = (char)((info & 0x30000) >> 12);
	hc->hc_addr = (unsigned short)(info & 0xffff);
	hc->hc_cnt = len;
	hc->hc_func = (char)func;
	hc->hc_sbfc = 0;

	s = splimp();
	/*
	 * If UMC comm regs busy, queue start i/o for later.
	 */
	if (sc->hdh_sioq.sioq_head) {
		(sc->hdh_sioq.sioq_tail)->hc_next = hc;
		sc->hdh_sioq.sioq_tail = hc;
		hc->hc_next = 0;
		splx(s);
		return;
	}

	/* start i/o on channel now */

	sc->hdh_sioq.sioq_head = hc;
	sc->hdh_sioq.sioq_tail = hc;
	hc->hc_next = 0;
	start_chn(unit);
	splx(s);
}

start_chn(unit)
int unit;
{
	register struct hdh_softc *sc = &hdh_softc[unit];
	register struct hdh_chan *hc = sc->hdh_sioq.sioq_head;
	register struct hdhregs *addr = (struct hdhregs *)hdhinfo[unit]->ui_addr;

	/*
	 * Set up comm regs.
	 */
	addr->iochn = hc->hc_chan;
	addr->ioadx = hc->hc_adx;
	addr->ioadl = hc->hc_addr;
	addr->iocnt = hc->hc_cnt;
	addr->iofcn = hc->hc_func;
	addr->iosbf = hc->hc_sbfc;
	addr->ioini = 1;

	/* signal UMC if necessary */

	if (!(addr->ionmi)) {
		addr->ionmi = 1;
		addr->csr = HDH_DMA|HDH_WRT|HDH_IEN|HDH_NMI;
	}
}

/*
 * IF-11/HDH interrupt handler
 */
hdhintr(unit)
int unit;
{
	register struct hdh_softc *sc = &hdh_softc[unit];
	register struct hdh_chan *hc;
	register struct hdhregs *addr = (struct hdhregs *)hdhinfo[unit]->ui_addr;
	int lcn, type, cc, cnt;

	/*
	 * Check for hardware errors.
	 */
	if (addr->csr & HDH_UER) {
		printf("hdh%d: hard error csr=%b\n", unit, addr->csr, HDH_BITS);
		addr->csr = 0;		/* disable i/f */
		return;
	}
	/*
	 * Get logical channel info.
	 */
	if ((lcn = addr->stachn) >= (NHDHCH*2)) {
		printf("hdh%d: unknown channel lcn=%d\n", unit, lcn);
		return;
	}

	hc = &sc->hdh_chan[lcn];

	type = addr->statyp;
	cc = addr->stacc;
	cnt = hc->hc_cnt - addr->stacnt;

	/* Figure out what kind of interrupt it was */

	switch(type) {

	case HDHSACK:		/* start i/o accepted */
		if (hc != sc->hdh_sioq.sioq_head) {
			printf("hdh%d: STARTIO error lcn=%d hc=%x sq=%x\n",
				unit, lcn, hc, sc->hdh_sioq.sioq_head);
			return;
		}

		/* try to start any queued i/o request */

		if (sc->hdh_sioq.sioq_head = sc->hdh_sioq.sioq_head->hc_next) {
			start_chn(unit);
		}
		break;

	case HDHDONE:		/* i/o completion */
		switch (cc) {

		case HDHIOCABT:
			printf("hdh%d: I/O abort ", unit);
			goto daterr;

		case HDHIOCERR:
			printf("hdh%d: program error ", unit);
			goto daterr;

		case HDHIOCOVR:
			printf("hdh%d: overrun error ", unit);
			goto daterr;

		case HDHIOCUBE:
			printf("hdh%d: NXM timeout or UB parity error ", unit);
		
		daterr:
			printf("lcn=%d func=%x\n", lcn, hc->hc_func);
			if (hc->hc_func & HDHRDB)
				sc->hdh_imp->imp_if.if_ierrors++;
			else
				sc->hdh_imp->imp_if.if_oerrors++;
		}

		hc->hc_flags &= ~HCBUSY;

		/* was it supervisor or data traffic? */

		if (lcn > HDHSUPW)
			hdh_data(unit, lcn, cc, cnt);
		else
			hdh_supr(unit, lcn, cc);

	}

	/*
	 * Ack the interrupt
	 */
	addr->staack = 1;
	if (!(addr->ionmi)) {
		addr->ionmi = 1;
		addr->csr = HDH_DMA|HDH_WRT|HDH_IEN|HDH_NMI;
	}	
}

/*
 * data channel interrupt completion handler
 */
hdh_data(unit, lcn, cc, rcnt)
int unit, lcn, cc, rcnt;
{
	register struct hdh_softc *sc = &hdh_softc[unit];
	register struct hdh_chan *hc = &sc->hdh_chan[lcn];
	register struct mbuf *m;


	/* was it read or write? */

	if (hc->hc_func & HDHRDB) {
		if (cc == HDHIOCOK) {
			/*
			 * Queue good packet for input 
			 */
			sc->hdh_imp->imp_if.if_ipackets++;
			m = if_rubaget(&sc->hdh_ifuba[lcn>>1], rcnt, 0,
				&sc->hdh_imp->imp_if);
			impinput(unit, m);
		}

		/* hang a new data read */

		hdh_iorq(unit, lcn, IMP_RCVBUF, HDHRDB+HDHSTR);

	} else {
		/*
		 * fire up next output
		 */
		sc->hdh_imp->imp_if.if_opackets++;
		sc->hdh_imp->imp_cb.ic_oactive = 0;
		impstart(sc->hdh_imp);
	}
}

/*
 * supervisor channel interrupt completion handler
 */
hdh_supr(unit, lcn, cc)
int unit, lcn, cc;
{
	register struct hdh_softc *sc = &hdh_softc[unit];
	register struct hdh_chan *hc = &sc->hdh_chan[lcn];
	short *p;
	

	/* was it read or write? */

	if (hc->hc_func & HDHRDB) {	
		if (cc == HDHIOCOK) {
			p = (short *)(sc->hdh_ifuba[lcn>>1].ifu_r.ifrw_addr);

			/* figure out what kind of supervisor message */

			switch (*p) {

			case HDHIACK:
			case HDHLNACK:
				break;
	
			case HDHLNUP:
				printf("hdh%d: LINE UP\n", unit);
				sc->hdh_flags |= HDH_UP;
				impstart(sc->hdh_imp);
				break;
	
			case HDHLNDN:
				if (sc->hdh_flags & HDH_UP)
					printf("hdh%d: LINE DOWN\n", unit);
				sc->hdh_flags &= ~HDH_UP;
				break;
	
			case HDHLOOP:
				break;
	
			case HDHSQERR:
				printf("hdh%d: HOST SEQUENCE ERROR\n", unit);
				break;
	
			case HDHSQRCV:
				printf("hdh%d: IMP SEQUENCE ERROR\n", unit);
				break;
	
			case HDHDTERR:
				printf("hdh%d: HOST DATA ERROR\n", unit);
				break;
	
			case HDHTIMO:
				printf("hdh%d: TIMEOUT\n", unit);
				break;
	
			default:
				printf("hdh%d: supervisor error, code=%x\n",
					unit, *p);
			}
		}

		/* hang a new supr read */

		hdh_iorq(unit, HDHSUPR, IMP_RCVBUF, HDHRDB+HDHSTR);
	} 
}

snd_supr(unit, msg, len)
int unit, len;
char *msg;
{
	register struct hdh_softc *sc = &hdh_softc[unit];
	register struct mbuf *m;
	register char *p;
	register int cnt;

	if ((m = m_get(M_DONTWAIT, MT_DATA)) == NULL) {
		printf("hdh%d: cannot get supervisor cmnd buffer\n", unit);
			return;
	}

	cnt = len;
	m->m_len = len;
	p = mtod(m, char *);

	while(cnt--) *p++ = *msg++;

	cnt = if_wubaput(&sc->hdh_ifuba[SUPR], m);

	hdh_iorq(unit, HDHSUPW, cnt, HDHWRT+HDHEOS);
}
#endif NHDH
