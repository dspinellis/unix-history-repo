/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*       ________________________________________________________        */
/*      /                                                        \       */
/*     |          AAA          CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |      */
/*     |         AAAAA        CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |      */
/*     |        AAAAAAA       CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |      */
/*     |       AAAA AAAA      CCCC              CCCC              |      */
/*     |      AAAA   AAAA     CCCC              CCCC              |      */
/*     |     AAAA     AAAA    CCCC              CCCC              |      */
/*     |    AAAA       AAAA   CCCC              CCCC              |      */
/*     |   AAAA  AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |      */
/*     |  AAAA    AAAAAAAAAAA CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |      */
/*     | AAAA      AAAAAAAAA   CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |      */
/*      \________________________________________________________/       */
/*                                                                       */
/*  	Copyright (c) 1989 by Advanced Computer Communications           */
/*  	720 Santa Barbara Street, Santa Barbara, California  93101       */
/*  	(805) 963-9431                                                   */
/*                                                                       */
/*                                                                       */
/*  Files:		if_pi.c, if_pivar.c                              */
/*			ACP_PI (Programmer Interface) for 4.3bsd and	 */
/*			Ultrix 2.0 (and newer)				 */
/*                                                                       */
/*  Author:		??? (Steve or Charles)				 */
/*                                                                       */
/*  Project:		Programmers Interface for 6250 software		 */
/*                                                                       */
/*  Function:		To enable network connections on ACP_PI to	 */
/*			communicate with UNIX.				 */
/*									 */
/* Configuration Entry:							 */
/*									 */
/*      device dda0 at uba? csr 0166740 vector ddainta ddaintb		 */
/*									 */
/* 									 */
/*  Revision History at end of file					 */
/*************************************************************************/

/*************************************************************************/
/*									 */
/* Usage Notes:								 */
/*									 */
/*      - make devices in /dev for those pi				 */
/*        devices which you want in your configuration			 */
/* 									 */
/* System Notes:							 */
/* 									 */
/*       Refer to the installation instructions, readme.txt, which	 */
/*       are included on the pi driver distribution medium.		 */
/*									 */
/* Design Overview:							 */
/*									 */
/* Data flows in two directions through the PI interface.  This section	 */
/* covers how data gets in and out of the driver.			 */
/*									 */
/* OUTBOUND DATA (user process -> FE):					 */
/*									 */
/* 1) The user process issues an ioctl(2) call with pi_dblock  structure */
/*    which gives the address and length of the user data as well as	 */
/*    the lcn that the data is to go out on.				 */
/* 2) The piioctl routine in the case XIOWRITE checks the validity of    */
/*    the specified lcn the calls pi_write.				 */
/* 3) The pi_write routine allocates a small mbuf.  If the user data	 */
/*    is longer than MLEN (112) bytes then a page cluster is allocated	 */
/*    to convert the small mbuf into a large mbuf.  The user data is	 */
/*    then copied to the mbuf.  The mbuf is put on the output queue for  */
/*    the lcn (dc->dc_oq) using the IF_ENQUEUE macros.  If the queue is  */
/*    full then the process will sleep on dc->dc_oq.  The wakeup will    */
/*    come from the pi_data routine when it sees a write completion.     */
/*    After the data is queue the dda_start is called to try to send the */
/*    data.								 */
/* 4) The dda_start routine dequeues the data from the per lcn ouput     */
/*    data queue and hooks the mbuf onto the write side of the hdx_chan  */
/*    structure for the lcn. The routine dda_wrq is called passing this  */
/*    structure.							 */
/* 5) The routine dda_wrq takes the hc structure and puts it on the      */
/*     global output queue ds->dda_sioq. It then calls start_chn().	 */
/* 6) start_chn() set up the comregs and request an A interrupt.  The    */
/*    A interrupt routine will handle the transfer grant and write 	 */
/*    completion.							 */
/*									 */
/* INBOUND DATA (FE -> user process):					 */
/*									 */
/* A) interrupt side----						 */
/*    1) Initially a read is posted for every logical circuit.  When     */
/*	 data comes in the interrupt A routine will handle doing the	 */
/*	 transfer grant. When the read completes then the type of	 */
/*	 channel is determined (IP,X.29 or PI).  For PI data the routine */
/*	 pi_data() is called with the mbuf and read completion status.   */
/*    2) In the routine pi_data() if the read completion status is       */
/*	 DDAIOCOK then the the packet is complete so the routine	 */
/*	 pi_queue_data is called to queue the data for the user process  */
/*	 to read.  If the status is DDAIOCOKP then the packet has not	 */
/*	 been completely read.  In this case the routine dda_rrq is	 */
/*	 called to allocate a new mbuf and chain it to the end of the	 */
/*	 mbufs that have already been filled from this packet.  When the */
/*	 packet is exhausted then the whole chain of mbufs will be 	 */
/*	 passed to pi_queue_data.					 */
/*    3) The routine pi_queue_data uses a small array of pi_qmem	 */
/*	 structures to form a queue of input data.  Each of these	 */
/*	 structures has a pointer to an mbuf.  This may point to a	 */
/*	 single mbuf or a chain of mbufs depending on how many mbufs it	 */
/*	 took to read the packet.  Pi_queue_data will copy the important */
/*	 information to the pi_qmem structure and then wakeup any	 */
/*	 processes that have slept waiting for data to come in.		 */
/*									 */
/* B) system call side----						 */
/*    1) The user process issues the XIOREAD ioctl with a pi_dblock	 */
/*	structure describing the target buffer.				 */
/*    2) the piioctl routine using the XIOREAD case calls the routine	 */
/*       pi_rem_qdata to get the next data item.  If there are no data	 */
/*       items ready then pi_rem_qdata will return -1  and the process	 */
/*       will sleep waiting for data to come in.  The wakeup will be done*/
/*       in pi_queue_data.  When a data item is dequeued from a channel	 */
/*       that is not the supervisor channel then a new read is issued on */
/*       that logical circuit.  This means that at most 1 data item will */
/*       be present in the input queue for a particular data circuit.    */
/*	 This excludes the supervisory circuit 0.			 */
/*    3) the pi_rem_qdata routine will dequeue  an mbuf and copy the	 */
/*	 data to the user buffer along with other information such as	 */
/*	 the lcn and read completions statuses.  If several mbufs were	 */
/*	 used to hold a single packet then the entire chain will be	 */
/*	 scanned and all the data placed in the user buffer.		 */
/*************************************************************************/

#ifdef SIMULATION
#include "if_pivar.h"
#else
#include "../vaxif/if_pivar.h"
#endif

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                             %%*/
/*%%                   LOCAL  FUNCTIONS                          %%*/
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void    pi_init ();
PRIVATE void    pi_init_single ();
PRIVATE void    pi_clear_piq ();
PRIVATE void    pi_clear_single ();
PRIVATE void    pi_free_all_lcns ();
PRIVATE void    pi_queue_data ();
PRIVATE int     pi_rem_qdata ();
PRIVATE int     pi_find_chan_for_ring ();
PRIVATE int     pi_write ();

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                             %%*/
/*%%                   LOCAL  VARIABLES                          %%*/
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/* PILINES: number of channels (minor device numbers) allocated per board */
/* PIQLEN: max number of messages that can be queued for a channel */
#define PILINES		32
#define PIQLEN		8

#define NPILINES        (NDDA * PILINES)

#ifdef DDADEBUG
int pi_placemark = 0;
#define PI_PLACEMARK(n)	(pi_placemark = (n))
#else
#define PI_PLACEMARK(n)
#endif

/* the pi_qmem structure is used to hold essential information about
 * incomming data until the user process has a chance to issue a read.
 * These structures form a queue of input data. All fields should be 0
 * when the structure is not actively part of the queue.
 */
struct pi_qmem
{
    struct mbuf    *mb;
    u_char          stat;
    u_char          substat;
    u_char          lcn;
};


/* the pi_info structure describes a data channel which corresponds to a
 * minor device.  The values in the protocols fields are only valid when
 * the PI_ACCEPT_RING bit is set in flags.  The 0 element is the lowest
 * protocol, The 1 element is the highest.
 */

struct pi_info
{
    u_short         flags;
    u_int           pgrp;	/* process group */
    u_int           signal;	/* signal to use upon data ready */
    struct mbuf    *msav;	/* place to hang mbuf waiting to go out */
    struct pi_qmem  pi_q[PIQLEN];	/* queue of pending input data */
    u_char          firstq, lastq;	/* index of first and last queue
					 * members */
    u_char          pi_qlen;	/* length of the input queue */
    u_char          protocols[2];	/* low and high protocol range */
};

/* bits for the flags field */
#define PI_ACCEPT_RING	0x1
#define PI_ACCEPT_ANY   0x2

struct pi_info  pi_info[NPILINES];	/* tty structures */

/* this macro gives the channel (minor device number) associated with
 * an active lcn.
 */
#define CHANNEL(lcn,ds)		((ds)->dda_cb[(lcn)].dc_line)

/*
 * macro to translate a device number to the unit (i.e. ACP_PI)
 * with which it is associated,  M001 use V7 major and minor macros
 */

#define PI_UNIT(x) (minor(x) / PILINES)	/* ACP_PI controlling this line */
#define PI_LINE(x) (minor(x) % PILINES)	/* Line number */

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                             %%*/
/*%%                   GLOBAL ROUTINES                           %%*/
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                       PIIOCTL()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Process ioctl request.                                         */
/*                                                                 */
/*  Call:           piioctl(dev, cmd, data, flag)                  */
/*  Argument:       dev:   device                                  */
/*                  cmd:   ioctl command                           */
/*                  data:  pointer to data                         */
/*                  flag:  ignored                                 */
/*  Returns:        0 for sucess, else nonzero error code          */
/*  Called by:      kernel software software,  this routine is in  */
/*                  the cdevsw table                               */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

piioctl (dev, cmd, data, flag)
dev_t           dev;
caddr_t         data;
{
    struct dda_cb  *dc;
    struct dda_softc *ds;
    int             l, unit, maxlcn;

    l = PI_LINE (dev);
    unit = PI_UNIT (dev);
    maxlcn = nddach[unit];
    ds = &dda_softc[unit];
    switch (cmd)
    {
    case XIOWRITE:
	{
	    struct pi_dblock *pd;

	    pd = (struct pi_dblock *) data;
	    if (pd->lcn > maxlcn)
	    {
		DMESG(unit, 66,
		    (DDALOG(LOG_ERR)
			 "XIOWRITE: invalid lcn %d\n", pd->lcn
		     DDAELOG));
		return (EINVAL);
	    }
	    if (pd->lcn && (CHANNEL (pd->lcn, ds) != l))
	    {
		DMESG (unit, 67, 
		    (DDALOG(LOG_ERR)
			"XIOWRITE: lcn %d not associated with channel %d.\n",
			 pd->lcn, l
		    DDAELOG));
		return (EINVAL);
	    }
	    return (pi_write (ds, l, pd));
	}
    case XIOREAD:
	{
	    int             ret;
	    struct pi_dblock *pd;

	    pd = (struct pi_dblock *) data;
	    while (ret = pi_rem_qdata (l, pd))
	    {
		if (ret > 0)
		    goto newread;
		else
		if (pd->flags & DB_NONBLOCK)
		    return (EWOULDBLOCK);	/* read already posted */
		else
		{
#ifdef DDA_MSGQ
		    dda_mqstr ("(r sl) ");
#endif
		    sleep (pi_info[l].pi_q, TTIPRI);
#ifdef DDA_MSGQ
		    dda_mqstr ("(r wo) ");
#endif
		}
	    }
    newread:
#ifdef DDA_MSGQ
	    dda_mqstr ("(r go) ");
#endif
	    if (pd->lcn)	/* issue new read for the lcn */
		dda_rrq (ds, &ds->dda_cb[pd->lcn].dc_rchan);
	    return (ret);
	}
    case XIORPEND:
	{
	    int             s;

	    s = splimp ();
	    *((int *) data) = pi_info[l].pi_qlen;
	    splx (s);
	    break;
	}
    case XIOACCRING:
	{
	    struct proto_range *pr;
	    int             s;

	    pr = (proto_range *) data;
	    s = splimp ();
	    pi_info[l].flags |= PI_ACCEPT_RING;
	    pi_info[l].protocols[0] = pr->lower;
	    pi_info[l].protocols[1] = pr->upper;
	    splx (s);
	    break;
	}
    case XIOANYPROTO:
	pi_info[l].flags |= PI_ACCEPT_ANY;
	break;
    case XIOFREELCN:
	{
	    u_char          lcn;

	    lcn = *((u_char *) data);
	    if (lcn > maxlcn)
		return EINVAL;
	    dc = &ds->dda_cb[lcn];
	    if (CHANNEL (lcn, ds) != l)
		return EINVAL;
	    else
	    {
		abort_io (unit, lcn);
		dc->dc_state = LC_IDLE;
		dc->dc_line = -1;
		dc->dc_inaddr.s_addr = 0;	/* forget address */
		dc->dc_key.key_addr.s_addr = 0;
		dc->dc_wsizein = dc->dc_wsizeout = 0;
		dc->dc_pktsizein = dc->dc_pktsizeout = 0;
		dc->dc_flags = 0;
	    }
	    break;
	}
    case XIOABORT:
	{
	    u_char          lcn;

	    lcn = *((u_char *) data);
	    if (lcn > maxlcn)
		return EINVAL;
	    dc = &ds->dda_cb[lcn];
	    if (CHANNEL (lcn, ds) != l)
		return EINVAL;
	    else
		abort_io (ds->dda_if.if_unit, lcn);
	    break;
	}
    case XIOGETLCN:

	/*
	 * find a free lcn.  Init the line field for this minor device. stuff
	 * lcn number in struct. init the circuit state. 
	 */
	if (dc = find_free_lcn (ds))
	{
	    dc->dc_flags = DC_RAW;
	    dc->dc_state = LC_DATA_IDLE;
	    dc->dc_line = l;
	    *data = dc->dc_lcn;
	}
	else
	    *data = 0;
	break;
    case XIOCLRCHAN:
	pi_clear_piq (ds, &pi_info[l]);
	break;
    case XIONORING:
	pi_info[l].flags &= ~(PI_ACCEPT_ANY | PI_ACCEPT_RING);
	break;
    case XIORSIG:
	pi_info[l].signal = *((int *) data);
	break;
	/* case XIOABORTIO: do an abort-io on the channel */
    default:
	return (ENOTTY);
    }
    return (0);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                       PIOPEN()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Open a line.                                                   */
/*                                                                 */
/*  Call:           piopen(dev, flag)                              */
/*  Argument:       dev:   device                                  */
/*                  flag:  indicates type of open, "nonblocking"   */
/*                         "or block if in use"                    */
/*  Returns:        0 for success, else nonzero error code         */
/*  Called by:      kernel software software,  this routine is in  */
/*                  the cdevsw table                               */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

piopen (dev, flag)
dev_t           dev;
int             flag;
{
    int             unit, d;

    unit = PI_UNIT (dev);
    d = PI_LINE (dev);

    if (d >= NPILINES)
	return (ENXIO);

    /* wait for interface to come up */
    while (dda_softc[unit].dda_state != S_LINK_UP)
	sleep (&dda_softc[unit].dda_state, TTIPRI);

    if (pi_info[d].pgrp == 0)
	pi_info[d].pgrp = u.u_procp->p_pid;
    return (0);
}


/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                       PICLOSE()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Close a line.                                                  */
/*                                                                 */
/*  Call:           piclose(dev, flag)                             */
/*  Argument:       dev:   device                                  */
/*                  flag:  unused                                  */
/*  Returns:        nothing                                        */
/*  Called by:      kernel software software,  this routine is in  */
/*                  the cdevsw table                               */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

piclose (dev, flag)
dev_t           dev;
int             flag;
{
    pi_clear_single (&dda_softc[PI_UNIT (dev)], &pi_info[PI_LINE (dev)], 0);
    pi_free_all_lcns (PI_UNIT (dev), PI_LINE (dev));
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                             %%*/
/*%%                   LOCAL  FUNCTIONS                          %%*/
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      PI_SUPR()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*       This routine processes received supervisor messages.      */
/*       Depending on the message type, the appropriate action is  */
/*       taken.                                                    */
/*                                                                 */
/*  Call:              pi_supr(ds, mb)                             */
/*  Arguments:         ds:  pointer to dev control block struct    */
/*                     mb:  pointer to mbuf 			   */
/*                              containing the supervisor message  */
/*  Returns:           nothing                                     */
/*  Called by:         dda_supr()                                  */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
pi_supr (ds, mb)
struct dda_softc *ds;
struct mbuf    *mb;

{
    u_char         *p;
    register int    lcn;
    register int    chan;
    int             unit;

    unit = ds->dda_if.if_unit;
#ifdef DDADEBUG
    if (DDADBCH (21, unit))
	DDALOG(LOG_ERR) "dda%d:(pi) pi_supr()\n", unit DDAELOG;
#endif DDADEBUG
    p = mtod (mb, u_char *);
    switch (p[0])
    {
    case LINE_STATUS:		/* link status msg */
    case RESTART:		/* restart received */
    case RSTRT_ACK:		/* restart ack */
    case STATRESP:		/* Statistics Response from FEP */
    case CLEARVC:		/* clear by VCN */
	DMESG (unit, 64,
	    (DDALOG(LOG_ERR)
		"dda%d:(pi) pi_supr: unexpected msg type 0x%x\n",
		unit, p[0]
	    DDAELOG));
	break;

    case RING:			/* incoming call */
	if ((chan = pi_find_chan_for_ring (p)) >= 0)
	    pi_queue_data (unit, chan, 0, 0, 0, mb);
	else			/* if no willing channel's */
	{
	    /* reject the call */
	    send_supr (ds, CLEARVC, p[2], 0);	/* clear call */
	    if (LOG_CALLS)
		DDALOG(LOG_ERR)
		    "dda%d:(pi) Call REJECTED VC 0x%x\n", unit, p[1]
		DDAELOG;
	}
	break;
    case ANSWER:		/* call answered */
    case CLEARLC:		/* clear by LCN */
    case RESET:		/* X25 reset */
    case INTERRUPT:		/* X25 interrupt */
    case INTR_ACK:
	lcn = p[1] / 2;		/* get LCN */
	pi_queue_data (unit, CHANNEL(lcn, ds), 0, 0, 0, mb);
	break;
    default:
	DMESG(unit, 65, 
	    (DDALOG(LOG_ERR)
		"dda%d:(pi) supervisor error (%x %x %x %x)\n",
		unit, p[0], p[1], p[2], p[3]
	    DDAELOG));
    }
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                       PI_DATA()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine is called when a data channel I/O completes.    */
/*    If the completion was for a write, an attempt is made to     */
/*    start output on the next packet waiting for output on that   */
/*    LCN.  If the completion was for a read, the received packet  */
/*    is sent to the pi input queue (if no error).                 */
/*                                                                 */
/*  Call:              pi_data(ds, hc, cc, cnt, subcc)             */
/*  Argument:          ds:  device control block                   */
/*                     hc:  half duplex channel control block      */
/*                     cc:   Mailbox I/O completion status         */
/*                     cnt:  byte count                            */
/*		       subcc: Mailbox I/O completion substatus     */
/*  Returns:           nothing                                     */
/*  Called by:         ddainta()                                   */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
pi_data (ds, hc, cc, cnt, subcc)
register struct dda_softc *ds;
register struct hdx_chan *hc;
int             cc, cnt, subcc;
{
    register struct dda_cb *dc = &(ds->dda_cb[hc->hc_chan / 2]);
    int             unit;

    unit = ds->dda_if.if_unit;
#ifdef DDADEBUG
    if (DDADBCH (18, unit))
	DDALOG(LOG_ERR)
	    "dda%d:(pi) pi_data: chan=0x%x  cc=0x%x  cnt=0x%x subcc=0x%x\n",
	    unit, hc->hc_chan, cc, cnt, subcc
	DDAELOG;
#endif DDADEBUG

#ifdef DDA_MSGQ
	dda_mqstr("(dt");
	dda_mqnum(hc->hc_chan,MQHEX); dda_mqstr(" ");
	dda_mqnum(cc,MQHEX); dda_mqstr(" ");
	dda_mqnum(cnt,MQHEX); dda_mqstr(" ");
	dda_mqstr(")");
#endif
    if (hc->hc_chan & 0x01)	/* was it read or write? */
    {				/* write, fire up next output */
#ifdef DDADEBUG
	dc->dc_out_t = TMO_OFF;	/* turn off output completion timer */
#endif
	if ((hc->hc_func != DDAABT) && (hc->hc_curr = hc->hc_curr->m_next))
	    dda_wrq (ds, hc, 0);
	else
	{
	    m_freem (hc->hc_mbuf);
	    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	    if (hc->hc_func == DDAABT)
	    {
		hc->hc_func &= ~DDAABT;
		hc->hc_sbfc &= ~INVALID_MBUF;
	    }
	    else
		ds->dda_if.if_opackets++;
	    dc->dc_flags &= ~DC_OBUSY;
#ifdef DDA_MSGQ
	    dda_mqstr ("(w wa) ");
#endif
	    wakeup (&(dc->dc_oq));	/* wake up anyone sleeping on output */
	    dda_start (ds, dc);	/* and try to output */
	    dc->dc_timer = TMO_OFF;	/* reset timer */
	}
    }
    else			/* read, process rcvd packet */
    {
#ifdef DDADEBUG
#ifdef notdef
	if (DDADBCH (19, unit))
	{
	    u_char         *p;

	    DDALOG(LOG_ERR) "dda%d:(pi) ", unit DDAELOG;
	    p = mtod (hc->hc_curr, u_char *);
	    prt_bytes (p, (cnt < 56 ? cnt : 56));
	    DDALOG(LOG_ERR) "\n" DDAELOG;
	}
#endif
#endif DDADEBUG

	if (cc == DDAIOCOK)
	{			/* Queue good packet for input */
	    hc->hc_curr->m_len += cnt;	/* update byte count */

	    ds->dda_if.if_ipackets++;
	    pi_queue_data (unit, dc->dc_line, dc->dc_lcn, cc, subcc, hc->hc_mbuf);
	    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	}
	else
	if (cc == DDAIOCOKP)	/* more data pending in this packet */
	{
	    hc->hc_curr->m_len += cnt;	/* update byte count */
	    dda_rrq (ds, hc);
	}
	else
	{
	    m_freem (hc->hc_mbuf);
	    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	    dda_rrq (ds, hc);
	}
	/* don't hang new data read.  This is done in the user read ioctl */
    }
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                       PI_INIT()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*	This function initializes the pi_info structure.  The      */
/*	active flag is non-zero if the interface is active and got */
/*	a reset.  In this case pi_clear_single is called which     */
/*	free any resources held and send a hangup to the           */
/*	controlling process.                                       */
/*                                                                 */
/*  Call:           pi_init(unit, active)                          */
/*  Argument:       unit: unit to be initialized.                  */
/*                  active: non-zero if the interface is active    */
/*                          when the reset is issued.              */
/*  Returns:        nothing.                                       */
/*  Called by:      ddareset(), ddainit(), bufreset().             */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
pi_init (unit, active)
int             unit;
int             active;
{
    struct pi_info *pi, *end;
    struct dda_softc *ds;

    ds = &dda_softc[unit];
    end = pi_info + ((unit + 1) * PILINES);
    for (pi = end - PILINES; pi < end; pi++)
	if (active)
	    pi_clear_single (ds, pi, 1);
	else
	    pi_init_single (pi);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                    PI_INIT_SINGLE()                         %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*	Initialize a pi_info structure.  For now all this means is */
/*	zeroing out everything.	This routine is here to make future*/
/*	enhancements easier.				           */
/*                                                                 */
/*  Call:           pi_init_single (pi)                            */
/*  Argument:       pi: pointer to pi_info structure               */
/*  Returns:        nothing.                                       */
/*  Called by:      pi_init(), pi_clear_single().                  */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
pi_init_single (pi)
struct pi_info *pi;
{
    bzero ((char *) pi, sizeof (struct pi_info));
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                 PI_CLEAR_PIQ()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*	This routine will free up all the mbufs that are queued on */
/*	the pi_q.  Note that this routine assumes that any pi_q    */
/*	element is zeroed when not in use.  			   */
/*                                                                 */
/*  IMPORTANT: this routine reissues reads for all the lcn         */
/*	whose data is removed from the queue.  This parallels what */
/*	is done when the data is dequeue normally by XIOREAD       */
/*                                                                 */
/*  Call:           pi_clear_piq (ds, pi)                          */
/*  Argument:       ds: pointer to softc used to call dda_rrq.     */
/*                  pi: pointer to pi_info structure.              */
/*  Returns:        nothing                                        */
/*  Called by:      pi_clear_single(), piioctl() case XIOCLRCHAN   */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
pi_clear_piq (ds, pi)
struct dda_softc *ds;
struct pi_info *pi;
{
    register int    i;

    for (i = 0; i < PIQLEN; i++)
	if (pi->pi_q[i].mb)
	{
	    m_freem (pi->pi_q[i].mb);
	    if (pi->pi_q[i].lcn)/* issue new read for the lcn */
		dda_rrq (ds, &ds->dda_cb[pi->pi_q[i].lcn].dc_rchan);
	}
    bzero ((char *) pi->pi_q, sizeof (pi->pi_q));
    pi->pi_qlen = 0;
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                  PI_CLEAR_SINGLE()                          %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*	this routine will re-initialize a single pi_info structure.*/
/*	If hangup is non-zero then a hangup signal will be sent to */
/*	the controlling process.                                   */
/*                                                                 */
/*  Call:           pi_clear_single (ds, pi, hangup)               */
/*  Argument:       ds: pointer to dda_softc structure             */
/*                  pi: pointer to pi_info structure               */
/*                  hangup: non-zero if hangup is to be sent.      */
/*  Returns:        nothing.                                       */
/*  Called by:      piclose(), pi_init().                          */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/* reset a pi_info structure that is active.  This includes freeing any
 * queue mbufs and sending a hangup to the controlling process
 */
PRIVATE void
pi_clear_single (ds, pi, hangup)
struct dda_softc *ds;
struct pi_info *pi;
int             hangup;		/* non-zero if a hangup is to be sent to
				 * controlling process */
{
    pi_clear_piq (ds, pi);
    if (hangup && pi->pgrp)
    {
	gsignal (pi->pgrp, SIGHUP);
	gsignal (pi->pgrp, SIGCONT);
    }
    if (pi->msav)
	m_freem (pi->msav);
    pi_init_single (pi);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                 PI_FREE_ALL_LCNS ()                         %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*	this routine will free all logical circuits associated with*/
/*	a particular channel.  This should return all mbufs that   */
/*	Are used by the channel.                                   */
/*                                                                 */
/*  Call:           pi_free_all_lcns (unit,chan)                   */
/*  Argument:       chan: channel number.                          */
/*		    unit: unit number.                             */
/*  Returns:        nothing.                                       */
/*  Called by:      piclose()                                      */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
pi_free_all_lcns (unit, chan)
int             unit;
int             chan;
{
    register int    i, maxlcn;
    struct dda_cb  *dc;

    maxlcn = nddach[unit];
    dc = &dda_softc[unit].dda_cb[1];
    for (i = 1; i <= maxlcn; i++, dc++)
	if ((dc->dc_flags & DC_RAW) && dc->dc_line == chan)
	{
	    abort_io (unit, dc->dc_lcn);
	    dc->dc_state = LC_IDLE;
	    dc->dc_line = -1;
	    dc->dc_inaddr.s_addr = 0;	/* forget address */
	    dc->dc_key.key_addr.s_addr = 0;
	    dc->dc_wsizein = dc->dc_wsizeout = 0;
	    dc->dc_pktsizein = dc->dc_pktsizeout = 0;
	    dc->dc_flags = 0;
	}
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                   PI_QUEUE_DATA()                           %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*	This routine queue up supervisory and data messages.  This */
/*	messages will be dequeued when the user does a read using  */
/*	the XIOREAD ioctl.  If the user tried to do a read and no  */
/*	data was ready then the process sleeps on pi_info[chan]pi_q*/
/*      This routine wakes up any of the sleeping processes.       */
/*                                                                 */
/*  Call:           pi_queue_data( unit, chan, lcn, cc, subcc, mb) */
/*  Argument:       unit: unit number (board)                      */
/*                  chan: channel for the data to be queue on      */
/*		    lcn: the lcn that the data came in on          */
/*		    cc: the read completion status from the mailbox*/
/*		    subcc: the read completion substatus           */
/*		    mb: pointer to the mbuf (or mbuf chain) that   */
/*		        contains the data packet.                  */
/*  Returns:        0 for success, else nonzero error code         */
/*  Called by:      pi_data(), pi_supr().                          */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
pi_queue_data (unit, chan, lcn, cc, subcc, mb)
int             unit;
int             chan;
int             lcn;
int             cc;
int             subcc;
struct mbuf    *mb;
{
    int             s;
    struct pi_info *pi;
    struct pi_qmem *pq;

    if (chan == -1) {		/* invalid channel */
	DMESG (unit, 68, 
	    (DDALOG(LOG_ERR)
		"dda%d:(pi) data queue on dead channel: lcn %d\n",
		unit, lcn
	    DDAELOG));
	return;			/* don't do it, otherwise panic! */
    }

    pi = &pi_info[chan];
    s = splimp ();
    if (pi->pi_qlen == PIQLEN)	/* overflow */
    {
	DMESG(unit, 68,
	    (DDALOG(LOG_ERR)
		"dda%d:(pi) Data queue full, message dropped  chan %d lcn %d\n",
		 unit, chan, lcn
	    DDAELOG));
	m_freem (mb);
	splx (s);
	return;
    }
    else
	pi->pi_qlen++;
    if (pi->lastq == (PIQLEN - 1))
	pi->lastq = 0;
    else
	pi->lastq++;
    pq = &pi->pi_q[pi->lastq];
    pq->mb = mb;
    pq->lcn = lcn;
    pq->stat = cc;
    pq->substat = subcc;
    splx (s);
#ifdef DDA_MSGQ
    dda_mqstr ("(r wa) ");
#endif
    wakeup (pi->pi_q);		/* wakeup any sleepers */
    if (pi->signal)
	gsignal (pi->pgrp, pi->signal);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                     PI_REM_QDATA()                          %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*	Remove a data item from the input queue and copy the       */
/*	information to the user pi_dblock structure that was       */
/*	passed to the XIOREAD case in piioctl.                     */
/*                                                                 */
/*  Call:           pi_rem_qdata(chan, dblk)                       */
/*  Argument:       chan: index of the channel to read.            */
/*                  dblk: pointer to user pi_dblock.               */
/*  Returns:        -1 if the input queue is empty.                */
/*                  0 if pi_dblock was filled in successfully.     */
/*		    error code if there was an error filling in    */
/*		       the pi_dblock.				   */
/*  Called by:      piioctl()                                      */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE int
pi_rem_qdata (chan, dblk)
int             chan;
struct pi_dblock *dblk;
{
    int             s, cnt = 0;
    struct pi_info *pi;
    struct pi_qmem *pq;
    struct mbuf    *mb, *m;
    caddr_t         usrdata;
    int             error = 0;

    pi = &pi_info[chan];
    s = splimp ();
    if (pi->pi_qlen == 0)
	return (-1);
    else
	pi->pi_qlen--;
    if (pi->firstq == (PIQLEN - 1))
	pi->firstq = 0;
    else
	pi->firstq++;
    pq = &pi->pi_q[pi->firstq];
    mb = pq->mb;
    dblk->func = pq->stat;
    dblk->subfunc = pq->substat;
    dblk->lcn = pq->lcn;
    bzero ((char *) pq, sizeof (*pq));	/* zero the structure out */
    splx (s);
    for (m = mb, usrdata = dblk->dataptr; m; m = m->m_next)
    {
	if (dblk->length < cnt + m->m_len)
	{
	    error = EINVAL;
	    goto out;
	}
	if (copyout (mtod (m, char *), usrdata, m->m_len))
	{
	    error = EFAULT;
	    goto out;
	}
	cnt += m->m_len;
	usrdata += m->m_len;
    }
out:
    dblk->length = cnt;
    m_freem (mb);
    return (error);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%               PI_FIND_CHAN_FOR_RING()                       %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*	This routine picks a channel to handle a ring.  It uses    */
/*	a static int (lastchan) to remember where it left off      */
/*	during the last search.  This will prevent it from always  */
/*	picking the same channel if there is more than 1 channel   */
/*	willing to accept the call.  If no channel is willing to   */
/*	accept the protocol type then it will go to a channel that */
/*	has indicated that it will accept any protocol type.       */
/* 	Note that the ring will not be given to a channel that     */
/*	has a full input queue.					   */
/*      This routine should be called a interrupt level.           */
/*                                                                 */
/*  Call:           pi_find_chan_for_ring (p)                      */
/*  Argument:       p: pointer to data containg the ring packet.   */
/*  Returns:        index of a channel willing to accept the call. */
/*                  -1 is return if no channel can accept          */
/*  Called by:      pi_supr()                                      */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE int
pi_find_chan_for_ring (p)
u_char          p[];		/* p is a pointer to a ring packet */

{
    register u_char *cp;
    int             i, proto, anychan = -1;
    static int      lastchan = 0;
    struct pi_info *pi;

    cp = p + 4;			/* skip over code, lcn, vcn and count in
				 * answer message */
    /* cp now points to length of called address */
    cp += *cp + 1;		/* skip over called address and length byte */
    /* cp now points to length of calling address */
    cp += *cp + 1;		/* skip over calling address and length byte */
    /* cp now points to length of protocol */
    if (*cp++ == 0)
	return (-1);
    proto = *cp;
    i = lastchan;
    do
    {
	i = (i + 1) % NPILINES;
	pi = &pi_info[i];
	if (pi->flags & PI_ACCEPT_RING)
	{
	    if (proto >= pi->protocols[0] && proto <= pi->protocols[1] && pi->pi_qlen < PIQLEN)
	    {
		lastchan = i;
		return (i);
	    }
	}
	if ((pi->flags & PI_ACCEPT_ANY) && anychan == -1 && pi->pi_qlen < PIQLEN)
	    anychan = i;
    } while (i != lastchan);
    if (anychan >= 0)
	lastchan = anychan;
    return (anychan);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%          PI_CIRCUIT_TO_HANDLE_PROTOCOL()                    %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*	This routine find out if there is a pi channel that is     */
/*	willing to accept a particular protocol.  This is needed   */
/*	because the PI interface has precedence over IP and X.29   */
/*	interfaces.  This routine is called before the data is     */
/*	passed to one of the interfaces.			   */
/*                                                                 */
/*  Call:           pi_circuit_to_handle_protocol(proto)           */
/*  Argument:       proto: protocol byte to check.                 */
/*  Returns:        1 if PI interface will handle the call.        */
/*		    0 otherwise.				   */
/*  Called by:      dda_decode_type().                             */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE int
pi_circuit_to_handle_protocol (proto)
u_char          proto;
{
    register int    i;
    struct pi_info *pi;

    for (i = 0, pi = pi_info; i < NPILINES; i++, pi++)
    {
	if (pi->flags & PI_ACCEPT_RING)
	    if (proto >= pi->protocols[0] && proto <= pi->protocols[1])
		return (1);
    }
    return (0);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      PI_WRITE()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*	This routine copies user data to an mbuf and queue the     */
/*	mbuf to go out.  The process will sleep if the output      */
/*	queue is full.   If the non-block bit is set then the      */
/*	process will not sleep and EWOULDBLOCK is returned.        */
/*      The last  byte of the built in data area for the mbuf      */
/*      is used to store the subfunction byte.  This byte is then  */
/*	put in the comregs by start_chn when it sees that this is  */
/*	a write for a RAW circuit.                                 */
/*								   */
/*  Call:           pi_write (ds, l, dblk)                         */
/*  Argument:       ds : pointer to dda_softc structure            */
/*                  l  : the channel (minor device) number         */
/*		    dblk : pointer to the pi_dblock for user data  */
/*  Returns:        0 for success, else nonzero error code         */
/*  Called by:      piioctl.                                       */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE int
pi_write (ds, l, dblk)
struct dda_softc *ds;
int             l;
struct pi_dblock *dblk;
{
    register int    length;
    struct dda_cb  *dc;
    struct mbuf    *m;
    char           *cp;
    struct ifqueue *oq;
    int             s, unit;

    length = dblk->length;
    unit = ds->dda_if.if_unit;
    if (length <= 0 || length > CLBYTES)
    {
	DMESG(unit, 69,
	    (DDALOG(LOG_ERR) "XIOWRITE: invalid length %d.\n", length DDAELOG));
	return (EINVAL);
    }
    m = 0;
    MGET (m, M_WAIT, MT_DATA);
    if (m == 0)
    {
	DMESG(unit, 70,
	    (DDALOG(LOG_ERR) "XIOWRITE: could not get small mbuf\n" DDAELOG));
	return (ENOBUFS);
    }
    if (length > (MLEN - 1))
    {
#if ACC_ULTRIX > 12
	struct mbuf    *p;
	MCLGET (m, p);
	if (p == 0)
#else
	MCLGET (m);
	if (m->m_len != CLBYTES)
#endif
	{
	    DMESG(unit, 71,
		(DDALOG(LOG_ERR)
		    "XIOWRITE: could not get page cluster\n"
		DDAELOG));
	    m_freem (m);
	    return (ENOBUFS);
	}
    }
    cp = mtod (m, char *);
    if (copyin (dblk->dataptr, cp, length))
    {
	m_freem (m);
	return (EFAULT);
    }
    m->m_len = length;
    if (length < (MLEN - 1))
	m->m_dat[MLEN-1] = dblk->subfunc;  /* store subfunction in last byte */
    dc = &ds->dda_cb[dblk->lcn];
    oq = &(dc->dc_oq);		/* point to output queue */
    s = splimp ();
    while (IF_QFULL (oq))	/* if q full */
    {
	if (dblk->flags & DB_NONBLOCK)
	{
	    m_freem (m);
	    splx (s);
	    return (EWOULDBLOCK);
	}
	else
	{
	    /*
	     * note that we save the mbuf here.  It may happen that the
	     * process dies in its sleep and we need a way to recover the
	     * allocated mbuf 
	     */
	    pi_info[l].msav = m;
#ifdef DDA_MSGQ
	    dda_mqstr ("(w sl) ");
#endif
	    sleep (&(dc->dc_oq), TTIPRI);
#ifdef DDA_MSGQ
	    dda_mqstr ("(w wo) ");
#endif
	}
    }
#ifdef DDA_MSGQ
    dda_mqstr ("(w go) ");
#endif
    IF_ENQUEUE (oq, m);		/* otherwise queue it */
    pi_info[l].msav = (struct mbuf *) 0;
    dda_start (ds, dc);		/* and try to output */
    dc->dc_timer = TMO_OFF;	/* reset timer */
    splx (s);
    return (0);
}

/*

Revision History:

13-Jul-89 PST	Modified LOCAL_VOIDs and statics to PRIVATE.

18-Jul-89 PST	Moved dc_key.ttyline out of union, creating dc_line.

26-Jul-89 PST	Replaced kernel printfs with DDALOG calls.

16-Aug-89 PST	Fixed DMESG macros.

29-Oct-89 PST	Fixed off-by-one errors where user could allocate but not
		use last LCN.  (Fix from brad@invector)

*/
