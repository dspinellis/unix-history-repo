/*	@(#)if_ddn.c	7.3 (Berkeley) 4/25/89 */


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

	Copyright (c) 1985 by Advanced Computer Communications
	720 Santa Barbara Street, Santa Barbara, California  93101
	(805) 963-9431

	This software may be duplicated and used on systems
	which are licensed to run U.C. Berkeley versions of
	the UNIX operating system.  Any duplication of any
	part of this software must include a copy of ACC's
	copyright notice.


File:
		if_ddn.c

Author:
		Art Berggreen

Project:
		4.2 DDN X.25 network driver

Function:
		This is a network device driver for BSD 4.2 UNIX which
		provides an interface between IP and ACC's ACP625
		(IF-11/X25) for connecting to the Defense Data Network.

Components:

Revision History:
		16-May-1985:	V1.0 - First release.
				Art Berggreen.

\************************************************************************/


/*	if_ddn.c	 V1.0	5/16/85	*/

/*
 * ACC ACP625 DDN/X.25 Network device driver
 */

/* #define DDNDEBUG 1		/* Enable definition for Debug code */

#include "ddn.h"
#if NDDN > 0
#include "machine/pte.h"

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "vmmac.h"
#include "errno.h"
#include "time.h"
#include "kernel.h"
#include "ioctl.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#ifdef	INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#endif

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "if_ddnreg.h"
#include "if_ddnvar.h"
#include "if_uba.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"



/* declare global functions */

int ddnprobe();
int ddnattach();
int ddnreset();
int ddninit();
int ddnoutput();
int ddntimer();
int ddnioctl();
int ddnintr();

/* declare local functions */

static void x25_init();
static struct ddn_cb *locate_x25_lcn();
static boolean convert_ip_addr();
static int convert_x25_addr();
static boolean make_x25_call();
static void ddn_start();
static void ddn_iorq();
static void start_chn();
static void ddn_data();
static void ddn_supr();
static void supr_msg();
static boolean decode_ring();
static void clear_lcn();
static void send_restart();
static void send_supr();
#ifdef DDNDEBUG
static void prt_addr();
static void prt_bytes();
#endif DDNDEBUG


struct	uba_device *ddninfo[NDDN];	/* ptrs to device info */
u_short	ddnstd[] = { 0766740, 0 };	/* standard addresses */
struct	uba_driver ddndriver =		/* device driver info */
  {
    ddnprobe,				/* device probe routine */
    0,					/* slave probe routine */
    ddnattach,				/* device attach routine */
    0,					/* "dmago" routine */
    ddnstd,				/* device address */
    "ddn",				/* device name */
    ddninfo				/* ptr to device info ptrs */
  };

static u_char init_msg[] =
  {
    LINE_CNTL,				/* set command code */
    0x00,				/* not used */
    0x00,				/* not used */
    0x00,				/* extension length (set at runtime) */
    LINK_DISABLE,			/* link disable */
/*    LINK_LOOPBACK,			/* loopback mode */
/*    LOOP_INTERNAL,			/*   = internal loopback */
    PKT_SIZE,				/* packet size */
    0x80,				/*   128 - LSB */
    0x00,				/*   128 - MSB */
    PKT_WINDOW,				/* packet window */
    0x02,				/*   = 2 */
    LINK_ENABLE				/* link enable */
  };

u_char cb_cmnd[4] =
  {
    CALL,
    0,
    0,
    0
  };

u_char cb_called_addr[16] = {0};

u_char cb_calling_addr[16] = {0};

u_char cb_facilities[64] = {0};

u_char cb_protocol[5] = {0};

u_char cb_user_data[1] = {0};

#ifdef DDNDEBUG
int ddn_debug = 1;		/* values 0-8 cause increasing verbosity */
#endif DDNDEBUG


/***********************************************************************\
*									*
*	Information for each device unit is maintained in an array	*
*	of structures named ddn_softc[].  The array is indexed by	*
*	unit number.  Each entry includes the network interface		*
*	structure (ddn_if) used by the routing code to locate the	*
*	interface,  an array of Logical	Channel control blocks which	*
*	maintain information about each of the Logical Channels (LCNs)	*
*	through which X.25 virtual calls are established, a queue of	*
*	I/O requests pending for the UMC, the UNIBUS interrupt vector	*
*	for the unit and misc flags.  The Logical Channel Control	*
*	blocks maintain information about the state of each LCN,	*
*	a queue of outbound data, Half Duplex Channel (HDX) blocks	*
*	used for queuing I/O requests to the UMC and an ifuba		*
*	structure which records the UNIBUS resources being held by	*
*	the LCN.							*
*									*
\***********************************************************************/

struct sioq		/* Start I/O queue head */
  {
    struct hdx_chan	*sq_head;	/* queue head */
    struct hdx_chan	*sq_tail;	/* queue tail */
  };

struct hdx_chan		/* HDX channel block */
  {
    struct hdx_chan	*hc_next;	/* link to next HDX channel */
    u_char		hc_chan;	/* HDX channel number */
    u_char		hc_adx;		/* address bits 17-16 */
    u_short		hc_addr;	/* address bits 15-00 */
    u_short		hc_cnt;		/* byte count */
    u_char		hc_func;	/* I/O function */
    u_char		hc_sbfc;	/* I/O subfunction */
  };

struct ddn_cb		/* Logical Channel control block */
  {
    struct in_addr	dc_inaddr;	/* remote Internet address */
    u_char		dc_lcn;		/* LCN number */
    u_char		dc_state;	/* LCN state */
    u_short		dc_timer;	/* LCN timer */
    struct ifqueue	dc_oq;		/* LCN output queue */
    struct hdx_chan	dc_rchan;	/* LCN read HDX channel */
    struct hdx_chan	dc_wchan;	/* LCN write HDX channel */
    struct ifuba	dc_ifuba;	/* UNIBUS resources */
    u_short		dc_flags;	/* misc flags */
  };

struct ddn_softc	/* device control structure */
  {
    struct ifnet	ddn_if;		/* network-visible interface */
    struct ddn_cb	ddn_cb[NDDNCH+1]; /* Logical Channel cntl blks */
    struct sioq		ddn_sioq;	/* start I/O queue */
    int			ddn_vector;	/* UNIBUS interrupt vector */
    u_short		ddn_flags;	/* misc flags */
    struct in_addr	ddn_ipaddr;	/* local IP address */
  } ddn_softc[NDDN];


/***********************************************************************\
*				ddnprobe()				*
*************************************************************************
*									*
*	This routine probes the device to obtain the UNIBUS interrupt	*
*	vector.  Since the UMC is a soft vector device, we obtain	*
*	an unused vector from the uba structure and return that.	*
*	The UMC is given the vector and the board is reset.		*
*	In order to save the vector in the device info structure, we	*
*	place it in a static temporary where the attach routine can	*
*	find it and save it in the device info structure.  This is	*
*	necessary because probe only provides a pointer to the device	*
*	and we have no idea which unit is being referenced.  This	*
*	works in 4.2 because the attach routine is called immediately	*
*	after a successful probe.					*
*									*
\***********************************************************************/

#define INIT_DELAY	(100 * 2)	/* time for board initialization */
					/*   ( in 10 millisecond ticks) */

static int savevec;			/* static variable for vector */

ddnprobe(reg)
caddr_t reg;
  {
    register int br, cvec;		/* r11, r10 value-result */
    register struct ddnregs *addr = (struct ddnregs *)reg;
    register int delay_time;

#ifdef lint
    br = 0; cvec = br; br = cvec; ddnintr(0);
#endif

    cvec = savevec = (uba_hd[numuba].uh_lastiv -= 4);	/* return vector */
    br = 0x15;				/* return bus level */

    addr->ioini = 0;			/* clear handshake flags */
    addr->ionmi = 0;
    addr->staack = 0;
    addr->xfrgnt = 0;
    addr->iovect = cvec >> 2;		/* pass vector to UMC */
    addr->csr = DDN_RST;		/* reset the board */
    delay_time = mfpr(TODR) + INIT_DELAY;
    while(delay_time > mfpr(TODR)) /* wait */ ;

    return (sizeof(struct ddnregs));
  }


/***********************************************************************\
*				ddnattach				*
*************************************************************************
*									*
*	This routine attaches the device to the network software.	*
*	The network interface structure is filled in.  The device	*
*	will be initialized when the system is ready to accept packets.	*
*									*
\***********************************************************************/

ddnattach(ui)
struct uba_device *ui;
  {
    register struct ddn_softc *ds = &ddn_softc[ui->ui_unit];

    ds->ddn_vector = savevec;		/* save vector from probe() */
    ds->ddn_if.if_unit = ui->ui_unit;	/* set unit number */
    ds->ddn_if.if_name = "ddn";		/* set device name */
    ds->ddn_if.if_mtu = DDNMTU;		/* set max msg size */
    ds->ddn_if.if_init = ddninit;	/* set init routine addr */
    ds->ddn_if.if_ioctl = ddnioctl;	/* set ioctl routine addr */
    ds->ddn_if.if_output = ddnoutput;	/* set output routine addr */
    ds->ddn_if.if_reset = ddnreset;	/* set reset routine addr */
    ds->ddn_if.if_watchdog = ddntimer;	/* set timer routine addr */
    if_attach(&ds->ddn_if);
  }


/***********************************************************************\
*				ddnreset()				*
*************************************************************************
*									*
*	Reset of interface after UNIBUS reset.				*
*	If interface is on specified uba, reset its state.		*
*									*
\***********************************************************************/

ddnreset(unit, uban)
int unit, uban;
  {
    register struct uba_device *ui;
    register struct ddnregs *addr;
    register int delay_time;

    if (unit >= NDDN || (ui = ddninfo[unit]) == 0 || ui->ui_alive == 0 ||
      ui->ui_ubanum != uban)
	return;

    printf(" ddn%d", unit);

    addr = (struct ddnregs *)ui->ui_addr;
    addr->ioini = 0;			/* clear handshake flags */
    addr->ionmi = 0;
    addr->staack = 0;
    addr->xfrgnt = 0;
    addr->iovect = ddn_softc[unit].ddn_vector >> 2;  /* pass vector to UMC */
    addr->csr = DDN_RST;		/* reset the board */
    delay_time = mfpr(TODR) + INIT_DELAY;
    while(delay_time > mfpr(TODR)) /* wait */ ;

    ddninit(unit);
  }


/***********************************************************************\
*				ddninit()				*
*************************************************************************
*									*
*	This routine initializes the interface for operation.  The	*
*	device control blocks are initialized, UNIBUS resources are	*
*	allocated and an X.25 initialization message is sent to the	*
*	UMC.								*
*									*
\***********************************************************************/

ddninit(unit)
int unit;
  {
    register struct ddn_softc *ds = &ddn_softc[unit];
    register struct ddn_cb *dc;
    register struct uba_device *ui = ddninfo[unit];
    int lcn, s;

#ifdef DDNDEBUG
if (ddn_debug > 0)
  {
printf("ddn%d: ddninit()\n", unit);
  }
#endif DDNDEBUG

    if (ds->ddn_if.if_addrlist == 0)	/* if we have no internet addr */
	return;				/*   don't init yet */

    dc = ds->ddn_cb;			/* setup ptr to first LCN cntl block */

    for(lcn = 0; lcn <= NDDNCH; lcn++)	/* for all LCN's ... */
      {
    	dc->dc_lcn = lcn;		/* record LCN */
    	dc->dc_inaddr.s_addr = 0;	/* clear remote internet addr */
    	dc->dc_state = LC_DOWN;		/* init LCN state */
    	dc->dc_timer = TMO_OFF;		/* turn LCN timer off */

		/* init LCN output queue */

    	dc->dc_oq.ifq_head = (struct mbuf *)0;
    	dc->dc_oq.ifq_tail = (struct mbuf *)0;
    	dc->dc_oq.ifq_len = 0;
    	dc->dc_oq.ifq_maxlen = DDN_OQMAX;
    	dc->dc_oq.ifq_drops = 0;

    		/* init HDX channels */

    	dc->dc_rchan.hc_next = (struct hdx_chan *)0;
    	dc->dc_rchan.hc_chan = lcn * 2;
    	dc->dc_wchan.hc_next = (struct hdx_chan *)0;
    	dc->dc_wchan.hc_chan = (lcn * 2) + 1;

    		/* init UNIBUS resources */

    	if (if_ubainit(&dc->dc_ifuba, ui->ui_ubanum,
    	    0, (int)btoc(DDNMTU)) == 0)
    	  {
    	    printf("ddn%d: failed getting UBA resources for lcn %d\n",
    		unit, lcn);
    	    ds->ddn_if.if_flags &= ~(IFF_RUNNING | IFF_UP);
    	    return;
    	  }

    	dc->dc_flags = 0;		/* initialize flags */

	dc++;				/* point at next cntl blk */
      }

    ds->ddn_sioq.sq_head = (struct hdx_chan *)0;
    ds->ddn_sioq.sq_tail = (struct hdx_chan *)0;
    ds->ddn_if.if_flags |= IFF_RUNNING;

    s = splimp();

    dc = ds->ddn_cb;			/* setup ptr to first LCN cntl block */

    for(lcn = 0; lcn <= NDDNCH; lcn++)	/* issue reads on all LCNs */
      {
    	ddn_iorq(ds, dc, DDNMTU, DDNRDB+DDNSTR);
	dc++;
      }

    x25_init(ds);			/* init the X.25 board */

    splx(s);

    ddntimer(unit);			/* start timers */
  }


/***********************************************************************\
*				ddnoutput()				*
*************************************************************************
*									*
*	This routine is called by the network software when it has	*
*	an IP datagram to send out this interface.  An attempt is	*
*	made to find a LCN which has a virtual circuit open to the	*
*	indicated host.  If an LCN is found the packet is queued for	*
*	output on that LCN.						*
*									*
\***********************************************************************/

ddnoutput(ifp, m0, dst)
struct ifnet *ifp;
struct mbuf *m0;
struct sockaddr_in *dst;
  {
    register struct mbuf *m = m0;
    register struct ddn_softc *ds = &ddn_softc[ifp->if_unit];
    register struct ddn_cb *dc;
    register struct ifqueue *oq;
    int s;

    if ((ds->ddn_if.if_flags & IFF_UP) == 0)
	return (ENETDOWN);

    switch (dst->sin_family)
      {

#ifdef INET
    case AF_INET:
	break;
#endif INET

    default:
	printf("ddn%d: can't handle af%d\n", ifp->if_unit,
	    dst->sin_family);
	m_freem(m0);
	return (EAFNOSUPPORT);
      }


#ifdef DDNDEBUG
if (ddn_debug > 6)
  {
printf("ddnoutput(): dst = ");
prt_addr(dst->sin_addr.s_addr);
printf("\n");
  }
#endif DDNDEBUG

    s = splimp();

    /* try to find an LCN */

    if (dc = locate_x25_lcn(ds, dst->sin_addr))
      {						/* if found */
	oq = &(dc->dc_oq);			/*   point to output queue */
	dc->dc_state = LC_DATA_IDLE;
	dc->dc_timer = TMO_DATA_IDLE;
	if (IF_QFULL(oq))			/*   if q full */
    	  {
	    IF_DROP(oq);			/*     drop the data */
	    m_freem(m);
	    splx(s);
	    return (ENOBUFS);
	  }
	IF_ENQUEUE(oq, m);			/*   otherwise queue it */
	ddn_start(ds, dc);			/*   and try to output */
	splx(s);
	return (0);
      }
    else					/* if no circuit available */
      {
    	IF_DROP(&ifp->if_snd);			/*   drop the data */
    	m_freem(m);
	splx(s);
	return (EHOSTUNREACH);
      }

  }


/***********************************************************************\
*				ddntimer()				*
*************************************************************************
*									*
*	This routine is entered once a second to perform timer		*
*	managment.  The LCN table is scanned for active timers,		*
*	(nonzero) which are decremented.  If a timer expires		*
*	(becomes zero), the proper action is taken.			*
*									*
\***********************************************************************/

int ddntimer(unit)
int unit;
  {
    register struct ddn_softc *ds = &ddn_softc[unit];
    register struct ddn_cb *dc;
    register int s, lcn;

#ifdef DDNDEBUG
if (ddn_debug > 7)
  {
printf("ddntimer()\n");
  }
#endif DDNDEBUG

    ds->ddn_if.if_timer = DDN_TIMEOUT;		/* restart timer */

    dc = ds->ddn_cb;

    s = splimp();

    for(lcn = 0; lcn <= NDDNCH; lcn++)		/* scan all LCN's */
      {
    	if (dc->dc_timer && (--(dc->dc_timer) == 0))
    	  {					/* if a timer expired */
    	    if (dc->dc_state == LC_RESTART)
    	      {					/*   if a restart was out */
    		send_restart(ds);		/*     send another one */
    		break;
    	      }
    	    else				/*   otherwise */
    	      {
    		clear_lcn(ds, dc);		/*     clear the LCN */
    	      }
    	  }
	dc++;
      }
    splx(s);
  }


/***********************************************************************\
*				ddnioctl()				*
*************************************************************************
*									*
*	This routine processes device dependent ioctl's.  Currently,	*
*	the only ioctl supported is used to set the host's internet	*
*	address for this network interface.				*
*									*
\***********************************************************************/

ddnioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	struct ifaddr *ifa = (struct ifaddr *) data;
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		if (ifa->ifa_addr->sa_family != AF_INET)
			return(EINVAL);
		ifp->if_flags |= IFF_UP;
		if ((ifp->if_flags & IFF_RUNNING) == 0)
			ddninit(ifp->if_unit);
		ddn_softc[ifp->if_unit].ddn_ipaddr = IA_SIN(ifa)->sin_addr;
		break;

	default:
		error = EINVAL;
		break;
	}
	splx(s);
	return (error);
}


/***********************************************************************\
*				ddnintr()				*
*************************************************************************
*									*
*	This is the interrupt handler for UNIBUS interrupts from the	*
*	UMC.  The interrupting HDX channel and interrupt type are	*
*	obtained from the completion comm regs.  If the interrupt is	*
*	an I/O request acknowledge, the next I/O request is passed	*
*	to the UMC.  If the interrupt is an I/O completion, the		*
*	completion is processed depending on whether it is for the	*
*	supervisor or a data channel.					*
*									*
\***********************************************************************/

ddnintr(unit)
int unit;
  {
    register struct ddn_softc *ds = &ddn_softc[unit];
    register struct hdx_chan *hc;
    register struct ddnregs *addr = (struct ddnregs *)ddninfo[unit]->ui_addr;
    int chan, type, cc, cnt;

    /*
     * Check for hardware errors.
     */
    if (addr->csr & DDN_UER)
      {
	printf("ddn%d: hard error csr=%b\n", unit, addr->csr, DDN_BITS);
	addr->csr = 0;		/* disable i/f */
    	return;
      }

    /*
     * Get logical channel info.
     */
    if ((chan = addr->stachn) >= ((NDDNCH+1)*2))
      {
    	printf("ddn%d: unknown channel, chan=%d\n", unit, chan);
    	return;
      }

    if (chan & 0x01)
    	hc = &(ds->ddn_cb[chan/2].dc_wchan);
    else
    	hc = &(ds->ddn_cb[chan/2].dc_rchan);

    type = addr->statyp;
    cc = addr->stacc;
    cnt = hc->hc_cnt - addr->stacnt;

    /* Figure out what kind of interrupt it was */

    switch(type)
      {
    case DDNSACK:		/* start i/o accepted */
    	if (hc != ds->ddn_sioq.sq_head)  /* does ack match waiting req? */
    	  {
    	    printf("ddn%d: STARTIO error chan=%d hc=%x sq=%x\n",
    		unit, chan, hc, ds->ddn_sioq.sq_head);
    	    addr->csr = 0;		/* disable UMC */
    	    return;
    	  }

	/* dequeue old request by copying link to queue head */
	/*   and start next I/O request if queue has not gone empty */

    	if (ds->ddn_sioq.sq_head = ds->ddn_sioq.sq_head->hc_next)
    	  {
    	    start_chn(ds);
    	  }
    	break;

    case DDNDONE:		/* i/o completion */
    	switch (cc)
    	  {
    	case DDNIOCABT:		/* probably VCN flush */
	    break;

    	case DDNIOCERR:
    	    printf("ddn%d: program error ", unit);
    	    goto daterr;

    	case DDNIOCOVR:
    	    printf("ddn%d: overrun error ", unit);
    	    goto daterr;

    	case DDNIOCUBE:
    	    printf("ddn%d: NXM timeout or UB parity error ", unit);

    	daterr:
    	    printf("chan=%d func=%x\n", chan, hc->hc_func);
    	    if (hc->hc_func & DDNRDB)
    		ds->ddn_if.if_ierrors++;
    	    else
    		ds->ddn_if.if_oerrors++;
    	  }

    	/* was it supervisor or data traffic? */

    	if (chan > 1)
    	    ddn_data(unit, chan, cc, cnt);
    	else
    	    ddn_supr(unit, chan, cc);

      }

    /*
     * Ack the interrupt
     */
    addr->staack = 1;
    if (!(addr->ionmi))
      {
    	addr->ionmi = 1;
    	addr->csr = DDN_DMA|DDN_WRT|DDN_IEN|DDN_NMI;
      }
  }


/***********************************************************************\
*				x25_init()				*
*************************************************************************
*									*
*	This routine builds and sends an X.25 initialization msg	*
*	to the UMC.							*
*									*
\***********************************************************************/

static void x25_init(ds)
struct ddn_softc *ds;
  {
    struct mbuf *m;

#ifdef DDNDEBUG
if (ddn_debug > 0)
  {
printf("ddn%d: x25_init()\n", ds->ddn_if.if_unit);
  }
#endif DDNDEBUG

    MGET(m, M_DONTWAIT, MT_DATA);	/* try to get X25 init buffer */
    if (m == 0)
      {
    	printf("ddn%d: couldn't get X25 init buffer\n", ds->ddn_if.if_unit);
    	return;
      }

    init_msg[3] = sizeof(init_msg) - 4;	/* set cmnd ext length */

    bcopy((caddr_t)init_msg, mtod(m, caddr_t), sizeof(init_msg));

    m->m_len = sizeof(init_msg);	/* set msg length */

    IF_ENQUEUE(&(ds->ddn_cb[0].dc_oq), m);
    ddn_start(ds, &(ds->ddn_cb[0]));
  }


/***********************************************************************\
*			locate_x25_lcn()				*
*************************************************************************
*									*
*	This routine tries to locate an X25 LCN associated with a	*
*	remote internet address.  A linear search of the LCN table	*
*	is made for a matching address.  If the search succeeds, the	*
*	LCN is returned.  If the search fails, the LCN table is		*
*	searched for an unused table entry.  If an unused table entry	*
*	is found, an X25 call is generated to the host specified in	*
*	the destination internet address.  If no LCN is available,	*
*	zero is returned.						*
*									*
\***********************************************************************/

static struct ddn_cb *locate_x25_lcn(ds, ip_addr)
struct ddn_softc *ds;
struct in_addr ip_addr;
  {
    register int lcn;
    register struct ddn_cb *dc;

#ifdef DDNDEBUG
if (ddn_debug > 6)
  {
printf("locate_x25_lcn()\n");
  }
#endif DDNDEBUG

    dc = &(ds->ddn_cb[1]);
    for(lcn = 1; lcn <= NDDNCH; lcn++)	/* scan LCN table for addr match */
      {
    	if (dc->dc_inaddr.s_addr == ip_addr.s_addr)	/* if found */
    	    return(dc);		     			/*   return LCN */
	dc++;
      }

    dc = &(ds->ddn_cb[1]);
    for(lcn = 1; lcn <= NDDNCH; lcn++)	/* scan LCN table for free entry */
      {
    	if (dc->dc_state == LC_IDLE)
    	    break;
	dc++;
      }

    if (lcn > NDDNCH)			/* if we didn't find a free entry */
        return(0);			/*   return empty handed */


    if (convert_ip_addr(ip_addr, cb_called_addr) && make_x25_call(ds, dc))
      {					/*  addr can be converted */
	dc->dc_inaddr.s_addr = ip_addr.s_addr;
    	return(dc);			/*   and return the LCN */
      }
    else
      {
	return(0);				/* give up */
      }
  }


/***********************************************************************\
*			convert_ip_addr()				*
*************************************************************************
*									*
*	This routine accepts an internet address and attempts to	*
*	translate to an equivalent X25 address.  For DDN this follows	*
*	the guidelines in the DDN X25 interface spec.  The resultant	*
*	X25 address is stored in the X25 called addr buffer.  The	*
*	routine returns TRUE if successfull, FALSE otherwise.		*
*									*
*	NOTE: Although IF-11/X25 was designed to accept ASCII coded	*
*	digits for the address fields, we only supply the binary	*
*	values.  The front-end only uses the low four bits to extract	*
*	the binary value from the ASCII digits, so this works out.	*
*									*
\***********************************************************************/

static boolean convert_ip_addr(ip_addr, x25addr)
struct in_addr ip_addr;
u_char x25addr[];
  {
    register int temp;
    union {
	struct in_addr ip;
	struct {	 /*   (assumes Class A network number) */
	    u_char s_net;
	    u_char s_host;
	    u_char s_lh;
	    u_char s_impno;
	} imp;
    } imp_addr;

    imp_addr.ip = ip_addr;
    x25addr[0] = 14;		/* set addr length */

    x25addr[1] = 0;		/* clear DNIC */
    x25addr[2] = 0;
    x25addr[3] = 0;
    x25addr[4] = 0;

    if (imp_addr.imp.s_host < 64)	/* Physical:  0000 0 IIIHH00 [SS] */
      {					/*   s_impno -> III, s_host -> HH */
    	x25addr[5] = 0;		/* set flag bit */
    	x25addr[6] = imp_addr.imp.s_impno / 100;
    	x25addr[7] = (imp_addr.imp.s_impno % 100) / 10;
    	x25addr[8] = imp_addr.imp.s_impno % 10;
    	x25addr[9] = imp_addr.imp.s_host / 10;
    	x25addr[10] = imp_addr.imp.s_host % 10;
      }
    else			/* Logical:   0000 1 RRRRR00 [SS]	*/
      {				/*   s_host * 256 + s_impno -> RRRRR	*/
    	temp = (imp_addr.imp.s_host << 8) + imp_addr.imp.s_impno;
    	x25addr[5] = 1;
    	x25addr[6] = temp / 10000;
    	x25addr[7] = (temp % 10000) / 1000;
    	x25addr[8] = (temp % 1000) / 100;
    	x25addr[9] = (temp % 100) / 10;
    	x25addr[10] = temp % 10;
      }

    x25addr[11] = 0;		/* clear rest of addr */
    x25addr[12] = 0;
    x25addr[13] = 0;
    x25addr[14] = 0;

#ifdef DDNDEBUG
if (ddn_debug > 4)
  {
printf("convert_ip_addr():  ");
prt_addr(ip_addr);
printf(" ==> ");
prt_bytes(x25addr, 14);
printf("\n");
  }
#endif DDNDEBUG

    return(1);
  }


/***********************************************************************\
*			convert_x25_addr()				*
*************************************************************************
*									*
*	This routine accepts an X25 address and attempts to translate	*
*	to an equivalent internet address.  For DDN this follows the	*
*	guidelines in the DDN X25 interface spec.  The resultant	*
*	internet address is returned to the caller.			*
*									*
\***********************************************************************/

static int convert_x25_addr(x25addr)
u_char x25addr[];
  {
    register int cnt, temp;
    union {
	struct in_addr ip;
	struct {	 /*   (assumes Class A network number) */
	    u_char s_net;
	    u_char s_host;
	    u_char s_lh;
	    u_char s_impno;
	} imp;
    } imp_addr;

    if (((cnt = x25addr[0]) < 12) || (cnt > 14))
      {
    	printf("DDN: illegal X25 address length!\n");
    	return(0);
      }

    switch(x25addr[5] & 0x0f)
      {
    case 0:			/* Physical:  0000 0 IIIHH00 [SS]	*/
	imp_addr.imp.s_impno =
		((int)(x25addr[6] & 0x0f) * 100) +
		((int)(x25addr[7] & 0x0f) * 10)  +
		((int)(x25addr[8] & 0x0f));
    	

    	imp_addr.imp.s_host =
    		((int)(x25addr[9] & 0x0f) * 10) +
		((int)(x25addr[10] & 0x0f));
        break;
    case 1:			/* Logical:   0000 1 RRRRR00 [SS]	*/
    	temp =    ((int)(x25addr[6] & 0x0f) * 10000)
		+ ((int)(x25addr[7] & 0x0f) * 1000)
		+ ((int)(x25addr[8] & 0x0f) * 100)
    		+ ((int)(x25addr[9] & 0x0f) * 10)
		+ ((int)(x25addr[10] & 0x0f));

    	imp_addr.imp.s_host = temp >> 8;
    	imp_addr.imp.s_impno = temp & 0xff;
    	break;
    default:
    	printf("DDN: illegal X25 address format!\n");
    	return(0);
      }

    imp_addr.imp.s_lh = 0;
    imp_addr.imp.s_net = 0;

#ifdef DDNDEBUG
if (ddn_debug > 4)
  {
printf("convert_x25_addr():  ");
prt_bytes(&x25addr[1], cnt);
printf(" ==> ");
prt_addr(imp_addr.ip);
printf("\n");
  }
#endif DDNDEBUG

    return(imp_addr.ip.s_addr);
  }


/***********************************************************************\
*			make_x25_call()					*
*************************************************************************
*									*
*	This routine places an X25 call using the X25 Call Msg		*
*	buffer.  The calling LCN is placed in the appropriate state	*
*	and a timer is started.						*
*									*
\***********************************************************************/

static boolean make_x25_call(ds, dc)
register struct ddn_softc *ds;
register struct ddn_cb *dc;
  {
    register struct mbuf *m_callbfr;
    register caddr_t cb;

    MGET(m_callbfr, M_DONTWAIT, MT_DATA);  /* try to get call cmnd buffer */
    if (m_callbfr == 0)
	return(0);

    cb = mtod(m_callbfr, caddr_t);

    (void)convert_ip_addr(ds->ddn_ipaddr, cb_calling_addr);

    cb_protocol[0] = 4;
    cb_protocol[1] = X25_PROTO_IP;	/* protocol = IP */
    cb_protocol[2] = 0;
    cb_protocol[3] = 0;
    cb_protocol[4] = 0;

    cb_facilities[0] = 4;		/* number facility bytes */
    cb_facilities[1] = 0;		/*  options marker */
    cb_facilities[2] = 0;
    cb_facilities[3] = X25_FACIL_DDN;	/*  DDN standard mode */
    cb_facilities[4] = FAC_DDNSTD;

    cb_user_data[0] = 0;		/* no user data */

    cb_cmnd[0] = CALL;			/* set command code */
    cb_cmnd[1] = dc->dc_lcn << 1;	/* set channel id */
    cb_cmnd[2] = 0;
    cb_cmnd[3] = (cb_called_addr[0] + 1) +	/* tally up cmnd ext length */
		 (cb_calling_addr[0] + 1) +
		 (cb_protocol[0] + 1) +
		 (cb_facilities[0] + 1) +
		 (cb_user_data[0] + 1);

    m_callbfr->m_len = cb_cmnd[3] + 4;

    /* copy command header */
    bcopy((caddr_t)cb_cmnd, cb, 4);
    cb += 4;

    /* copy called address */
    bcopy((caddr_t)cb_called_addr, cb, cb_called_addr[0] + 1);
    cb += (cb_called_addr[0] + 1);

    /* copy calling address */
    bcopy((caddr_t)cb_calling_addr, cb, cb_calling_addr[0] + 1);
    cb += (cb_calling_addr[0] + 1);

    /* copy protocol */
    bcopy((caddr_t)cb_protocol, cb, cb_protocol[0] + 1);
    cb += (cb_protocol[0] + 1);

    /* copy facilities */
    bcopy((caddr_t)cb_facilities, cb, cb_facilities[0] + 1);
    cb += (cb_facilities[0] + 1);

    /* copy user data */
    bcopy((caddr_t)cb_user_data, cb, cb_user_data[0] + 1);
    cb += (cb_user_data[0] + 1);

    dc->dc_state = LC_CALL_PENDING;		/* set state */
    dc->dc_timer = TMO_CALL_PENDING;		/* start call timeout */

#ifdef DDNDEBUG
if (ddn_debug > 3)
  {
printf("make_x25_call(): call_bfr = ");
prt_bytes(mtod(m_callbfr, u_char *), m_callbfr->m_len);
printf("\n");
  }
#endif DDNDEBUG

    IF_ENQUEUE(&(ds->ddn_cb[0].dc_oq), m_callbfr);
    ddn_start(ds, &(ds->ddn_cb[0]));

    return(1);
  }


/***********************************************************************\
*				ddn_start()				*
*************************************************************************
*									*
*	This routine attempts to start output of data queued on	a	*
*	specific LCN.  If the LCN was not already busy and data is	*
*	available for output, the data is copied into the LCN's I/O	*
*	buffer and an I/O request queued to the UMC.			*
*									*
\***********************************************************************/

static void ddn_start(ds, dc)
register struct ddn_softc *ds;
register struct ddn_cb *dc;
  {
    register struct mbuf *m;
    int len;

    /*
     * If output isn't active, attempt to
     * start sending a new packet.
     */

    if ((dc->dc_flags & DC_OBUSY) ||
    	(dc->dc_oq.ifq_len == 0) ||
    	((dc->dc_lcn != 0) && (dc->dc_state != LC_DATA_IDLE)))
      {
    	return;
      }

    IF_DEQUEUE(&dc->dc_oq, m);

    len = if_wubaput(&dc->dc_ifuba, m);	/* copy data to mapped mem */
    dc->dc_flags |= DC_OBUSY;

    ddn_iorq(ds, dc, len, DDNWRT+DDNEOS);
  }


/***********************************************************************\
*				ddn_iorq()				*
*************************************************************************
*									*
*	This routine builds UMC I/O requests and queues them for	*
*	delivery to the UMC. If the UMC I/O request comm regs are	*
*	not busy, the I/O request is passed to the UMC.			*
*									*
\***********************************************************************/

static void ddn_iorq(ds, dc, len, func)
struct ddn_softc *ds;
struct ddn_cb *dc;
int len, func;
  {
    register struct hdx_chan *hc;
    register int info;


    /* get appropriate UNIBUS mapping info */

    if (func & DDNRDB)		/* read or write? */
      {
    	hc = &dc->dc_rchan;
    	info = dc->dc_ifuba.ifu_r.ifrw_info;
      }
    else
      {
    	hc = &dc->dc_wchan;
    	info = dc->dc_ifuba.ifu_w.ifrw_info;
      }

    /* set channel info */

    hc->hc_adx = (u_char)((info & 0x30000) >> 12);
    hc->hc_addr = (u_short)(info & 0xffff);
    hc->hc_cnt = len;
    hc->hc_func = (u_char)func;
    hc->hc_sbfc = 0;

    /*
     * If UMC comm regs busy, queue start i/o for later.
     */
    if (ds->ddn_sioq.sq_head)
      {
    	(ds->ddn_sioq.sq_tail)->hc_next = hc;
    	ds->ddn_sioq.sq_tail = hc;
    	hc->hc_next = 0;
    	return;
      }

    /* start i/o on channel now */

    ds->ddn_sioq.sq_head = hc;
    ds->ddn_sioq.sq_tail = hc;
    hc->hc_next = 0;
    start_chn(ds);
  }


/***********************************************************************\
*				start_chn()				*
*************************************************************************
*									*
*	This routine copies UMC I/O requests into the UMC comm regs	*
*	and notifies the UMC.						*
*									*
\***********************************************************************/

static void start_chn(ds)
struct ddn_softc *ds;
  {
    register struct hdx_chan *hc = ds->ddn_sioq.sq_head;
    register struct ddnregs *addr =
    	(struct ddnregs *)ddninfo[ds->ddn_if.if_unit]->ui_addr;

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

    if (!(addr->ionmi))
      {
    	addr->ionmi = 1;
    	addr->csr = DDN_DMA|DDN_WRT|DDN_IEN|DDN_NMI;
      }
  }


/***********************************************************************\
*				ddn_data()				*
*************************************************************************
*									*
*	This routine is called when a data channel I/O completes.	*
*	If the completion was for a write, an attempt is made to	*
*	start output on the next packet waiting for output on that	*
*	LCN.  If the completion was for a read, the received packet	*
*	is sent to the IP input queue (if no error) and another read	*
*	is started on the LCN.						*
*									*
\***********************************************************************/

static void ddn_data(unit, chan, cc, rcnt)
int unit, chan, cc, rcnt;
{
    register struct ddn_softc *ds = &ddn_softc[unit];
    register struct ddn_cb *dc = &(ds->ddn_cb[chan/2]);
    register struct ifqueue *inq = &ipintrq;
    register struct mbuf *m;

    if (chan & 0x01)			/* was it read or write? */	
      {					/*   write, fire up next output */
    	ds->ddn_if.if_opackets++;
    	dc->dc_flags &= ~DC_OBUSY;
    	ddn_start(ds, dc);
      }
    else				/*   read, process rcvd packet */
      {
    	if (cc == DDNIOCOK)
    	  {				/* Queue good packet for input */
    	    ds->ddn_if.if_ipackets++;
	    dc->dc_state = LC_DATA_IDLE;
	    dc->dc_timer = TMO_DATA_IDLE;
    	    m = if_rubaget(&(dc->dc_ifuba), rcnt, 0, &ds->ddn_if);
    	    if (m)
    	      {
    		if (IF_QFULL(inq))
    		  {
    		    IF_DROP(inq);
    		    m_freem(m);
    		  }
    		else
    		  {
    		    IF_ENQUEUE(inq, m);
    		    schednetisr(NETISR_IP);
    		  }
    	      }
    	  }

    	/* hang a new data read */

    	ddn_iorq(ds, dc, DDNMTU, DDNRDB+DDNSTR);

      }
  }


/***********************************************************************\
*				ddn_supr()				*
*************************************************************************
*									*
*	This routine is called when a supervisor I/O completes.		*
*	If the completion was for a write, an attempt is made to	*
*	start output on the next supervisor command waiting for		*
*	output.  If the completion was for a read, the received		*
*	supervisor message is processed and another read is started.	*
*									*
\***********************************************************************/

static void ddn_supr(unit, chan, cc)
int unit, chan, cc;
{
    register struct ddn_softc *ds = &ddn_softc[unit];
    u_char *p;

    /* was it read or write? */

    if (chan & 0x01)
      {
    	ds->ddn_cb[0].dc_flags &= ~DC_OBUSY;
    	ddn_start(ds, &(ds->ddn_cb[0]));
      }
    else
      {
    	if (cc == DDNIOCOK)
    	  {
    	    p = (u_char *)(ds->ddn_cb[0].dc_ifuba.ifu_r.ifrw_addr);

    	    /* process supervisor message */

    	    supr_msg(ds, p);

    	  }

    	/* hang a new supr read */

    	ddn_iorq(ds, &(ds->ddn_cb[0]), DDNMTU, DDNRDB+DDNSTR);
      }
  }


/***********************************************************************\
*				supr_msg()				*
*************************************************************************
*									*
*	This routine processes received supervisor messages.		*
*	Depending on the message type, the appropriate action is	*
*	taken.
*									*
\***********************************************************************/

static void supr_msg(ds, p)
struct ddn_softc *ds;
u_char p[];
  {
    register struct ddn_cb *dc;
    register int lcn;
    register struct mbuf *m;

#ifdef DDNDEBUG
if (ddn_debug > 5)
  {
printf("supr_msg():  ");
prt_bytes(p, 4+p[3]);
printf("\n");
  }
#endif DDNDEBUG

    switch (p[0])
      {
    case LINE_STATUS:			/*   link status msg */
	if (p[2] == LINK_UP)		/*   if link came up */
	  {
    	    send_restart(ds);		/*     send restart msg */
	  }
	else				/*   if link went down */
	  {
    	    ds->ddn_if.if_flags &= ~IFF_UP;
    	    dc = ds->ddn_cb;
    	    for(lcn = 0; lcn <= NDDNCH; lcn++) /*    for all LCN's */
    	      {
    		dc->dc_state = LC_DOWN;  /* set state */
    		dc->dc_timer = TMO_OFF;  /* stop timer */
		dc++;
    	      }
	  }
    	break;

    case RESTART:			/* restart received */
    	if (ds->ddn_cb[0].dc_state != LC_RESTART) /* if not restarting */
    	    send_supr(ds, RSTRT_ACK, 0, 0);    /*   send restart ack */
	/* fall thru */
    case RSTRT_ACK:			/* restart ack */
    	ds->ddn_if.if_flags |= IFF_UP;
    	dc = ds->ddn_cb;
    	for(lcn = 0; lcn <= NDDNCH; lcn++)	/* for all LCN's */
    	  {
    	    dc->dc_state = LC_IDLE;   /* set state */
    	    dc->dc_timer = TMO_OFF;   /* stop timer */
    	    dc->dc_inaddr.s_addr = 0; /* forget address */
    	    while (dc->dc_oq.ifq_len) /* drop pending data */
    	      {
    		IF_DEQUEUE(&dc->dc_oq, m);
    		m_freem(m);
    	      }
	    dc++;
    	  }
    	break;

    case ANSWER:			/* call answered */
    	lcn = p[1] / 2;
    	dc = &(ds->ddn_cb[lcn]);
    	if (dc->dc_state == LC_CALL_PENDING) /* if a call pending */
    	  {
    	    dc->dc_state = LC_DATA_IDLE;  /* set state */
    	    dc->dc_timer = TMO_DATA_IDLE; /* start timer */
    	    ddn_start(ds, dc);		  /* try to send data */
    	  }
    	break;

    case RING:				/* incoming call */
    	for(lcn = NDDNCH; lcn > 0; lcn--)	/* search LCN's */
    	  {
    	    if (ds->ddn_cb[lcn].dc_state == LC_IDLE) /* unused? */
    	    	break;
    	  }

    	if (lcn && decode_ring(p))	/* if a free LCN found */
					/*   and ring looks ok */
    	  {
    	    dc = &(ds->ddn_cb[lcn]);
    	    dc->dc_inaddr.s_addr = convert_x25_addr(cb_calling_addr);
    	    dc->dc_state = LC_DATA_IDLE;  /* set state */
    	    dc->dc_timer = TMO_DATA_IDLE; /* start timer */
    	    send_supr(ds, ANSWER, lcn * 2, (int)p[2]); /* send answer */
    	  }
    	else				/* if no free LCN's */
    	  {
    	    send_supr(ds, CLEARVC, (int)p[2], 0); /* clear call */
    	  }
    	break;

    case CLEARLC:			/* clear by LCN */
    	lcn = p[1] / 2;			/* get LCN */
    	dc = &(ds->ddn_cb[lcn]);
    	if (dc->dc_state != LC_CLR_PENDING) /* if no clear pending */
    	  {
    	    send_supr(ds, CLEARLC, (int)p[1], 0);      /*   ack the clear */
    	  }
    	dc->dc_state = LC_IDLE; /* set state */
    	dc->dc_timer = TMO_OFF; /* stop timer */
    	dc->dc_inaddr.s_addr = 0; /* forget address */
    	while (dc->dc_oq.ifq_len) /* drop pending data */
    	  {
    	    IF_DEQUEUE(&dc->dc_oq, m);
    	    m_freem(m);
    	  }
    	break;

    case CLEARVC:			/* clear by VCN */
    	send_supr(ds, CLEARVC, (int)p[1], 0); /* send clear ack */
    	break;

    case RESET:				/* X25 reset */
	send_supr(ds, RESET_ACK, (int)p[1], 0); /* send reset ack */
    	printf("X25 RESET on lcn = %d\n", p[1] / 2); /* log it */
	break;

    case INTERRUPT:			/* X25 interrupt */
    	printf("X25 INTERRUPT on lcn = %d, code = %d\n",	/* log it */
    	    p[1] / 2, p[2]);
	break;

    default:
    	printf("ddn%d: supervisor error, code=%x\n",
	   ds->ddn_if.if_unit, p[0]);
      }
  }


/***********************************************************************\
*				decode_ring()				*
*************************************************************************
*									*
*	This routine parses and validates the incoming call msg.	*
*									*
\***********************************************************************/

static boolean decode_ring(p)
register u_char *p;
  {
    register int cnt;

#ifdef DDNDEBUG
if (ddn_debug > 3)
  {
printf("decode_ring()\n");
  }
#endif DDNDEBUG


    p += 3;			/* skip to cmnd ext length */
    if (*p++ < 5)		/* is count appropriate */
	return(0);		/*   return false if not */

    /* called address */
    if ((cnt = *p + 1) > 16)	/* is called addr len legal? */
	return(0);		/*   return false if not */
    bcopy((caddr_t)p, (caddr_t)cb_called_addr, (unsigned)cnt); /* copy field */
    p += cnt;

    /* calling address */
    if ((cnt = *p + 1) > 16)	/* is calling addr len legal? */
	return(0);		/*   return false if not */
    bcopy((caddr_t)p, (caddr_t)cb_calling_addr, (unsigned)cnt); /* copy field */
    p += cnt;

    /* protocol part of user data */
    if ((cnt = *p + 1) > 5)	/* is protocol len legal? */
	return(0);		/*   return false if not */
    bcopy((caddr_t)p, (caddr_t)cb_protocol, (unsigned)cnt); /* copy field */
    p += cnt;

    /* facilities */
    if ((cnt = *p + 1) > 64)	/* is facilities len legal? */
	return(0);		/*   return false if not */
    bcopy((caddr_t)p, (caddr_t)cb_facilities, (unsigned)cnt); /* copy field */
    p += cnt;

    /* ignore rest of user data for now */

    if ((cb_protocol[0] == 0) || (cb_protocol[1] != X25_PROTO_IP))
	return(0);		/* bad if not IP */

    return(1);			/* looks ok */
  }


/***********************************************************************\
*				clear_lcn()				*
*************************************************************************
*									*
*	This routine clears an X25 circuit and releases any buffers	*
*	queued for transmission.					*
*									*
\***********************************************************************/

static void clear_lcn(ds, dc)
struct ddn_softc *ds;
struct ddn_cb *dc;
  {
    register struct mbuf *m;

#ifdef DDNDEBUG
if (ddn_debug > 3)
  {
printf("clear_lcn(%d)\n", dc->dc_lcn);
  }
#endif DDNDEBUG

    dc->dc_state = LC_CLR_PENDING;  /* set state */
    dc->dc_timer = TMO_CLR_PENDING; /* start clear timer */
    dc->dc_inaddr.s_addr = 0;	    /* clear associated address */
    while (dc->dc_oq.ifq_len)	    /* drop any pending data */
      {
    	IF_DEQUEUE(&dc->dc_oq, m);
    	m_freem(m);
      }
    send_supr(ds, CLEARLC, (int)dc->dc_lcn * 2, 0);    /* send clear msg */
  }


/***********************************************************************\
*				send_restart()				*
*************************************************************************
*									*
*	This routine marks all LCNs as being in a restarting state	*
*	and sends a restart command to X25.				*
*									*
\***********************************************************************/

static void send_restart(ds)
struct ddn_softc *ds;
  {
    register struct ddn_cb *dc;
    register int lcn;
    struct mbuf *m;

#ifdef DDNDEBUG
if (ddn_debug > 1)
  {
printf("send_restart()\n");
  }
#endif DDNDEBUG
    dc = ds->ddn_cb;
    for(lcn = 0; lcn <= NDDNCH; lcn++)	    /* for all LCN's */
      {
    	dc->dc_state = LC_RESTART;  /* set state */
    	dc->dc_timer = TMO_RESTART; /* start restart timeout */
    	dc->dc_inaddr.s_addr = 0;     /* forget address */
    	while (dc->dc_oq.ifq_len)	/* drop any pending data */
    	  {
    	    IF_DEQUEUE(&dc->dc_oq, m);
    	    m_freem(m);
    	  }
	dc++;
      }

    send_supr(ds, RESTART, 0, 0);	    /* send restart msg */
  }


/***********************************************************************\
*				send_supr()				*
*************************************************************************
*									*
*	This routine is used to send short (4 bytes only) supervisor	*
*	commands.							*
*									*
\***********************************************************************/

static void send_supr(ds, cmd, p1, p2)
struct ddn_softc *ds;
int cmd, p1, p2;
  {
    struct mbuf *m;
    register u_char *cp;

#ifdef DDNDEBUG
if (ddn_debug > 6)
  {
printf("send_supr():  %x %x %x\n", cmd, p1, p2);
  }
#endif DDNDEBUG

    MGET(m, M_DONTWAIT, MT_DATA);

    if (m == 0)
      {
    	printf("ddn%d: failed to get supr msg bfr!\n", ds->ddn_if.if_unit);
    	return;
      }

    cp = mtod(m, u_char *);

    /* build supervisor message */

    *cp++ = (byte)cmd;
    *cp++ = (byte)p1;
    *cp++ = (byte)p2;
    *cp++ = 0;

    m->m_len = 4;

    IF_ENQUEUE(&(ds->ddn_cb[0].dc_oq), m);
    ddn_start(ds, &(ds->ddn_cb[0]));

  }


#ifdef DDNDEBUG

/***********************************************************************\
*				prt_addr()				*
*************************************************************************
*									*
*	This routine is used to print internet addresses in the		*
*	standard A.B.C.D format.					*
*									*
\***********************************************************************/

static void prt_addr(addr)
struct in_addr addr;
  {
    printf("%d.%d.%d.%d", addr.s_net, addr.s_host, addr.s_lh, addr.s_impno);
  }

/***********************************************************************\
*				prt_bytes()				*
*************************************************************************
*									*
*	This routine is used to print a string of bytes in hex.		*
*									*
\***********************************************************************/

static void prt_bytes(bp, cnt)
u_char *bp;
int cnt;
  {
    while(cnt--)
      {
	printf(" %x", *bp++ & 0xff);
      }
  }

#endif DDNDEBUG

#endif NDDN
