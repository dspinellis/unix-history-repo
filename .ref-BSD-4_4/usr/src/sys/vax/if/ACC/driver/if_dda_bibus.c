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
/*  	Copyright (c) 1986 by Advanced Computer Communications           */
/*  	720 Santa Barbara Street, Santa Barbara, California  93101       */
/*  	(805) 963-9431                                                   */
/*                                                                       */
/*                                                                       */
/*  File:		if_dda_bibus.c                                   */
/*			BI bus support routines for dda			 */
/*                                                                       */
/*  Project:		DDN-X.25 Network Interface Driver for ACP 7250   */
/*                                                                       */
/*  revision history at the end of if_dda.c				 */
/*************************************************************************/

#include "../vaxif/if_uba.h"
#include "../vaxuba/ubavar.h"
#include "../vaxbi/bireg.h"
#include "../vaxif/if_ddabique.h"

#ifdef	SIMULATION
#define	KM_CLUSTER	28
#define	KM_NOWAIT	1
#define	KM_ALLOC(space, cast, size, type, flags) { \
	(space) = (cast)malloc(size); \
}
#define	KM_FREE(addr, type) { \
	(void)free((caddr_t)(addr)); \
}
#undef svtophy
#define svtophy(x)	((int)(x))
#endif

/*
 *	private functions in this module
 */

PRIVATE void	dda_shm_setup();
PRIVATE void	dda_disable();
PRIVATE void	dda_dump_biic_regs();
PRIVATE void	dda_dump_shm();
PRIVATE void	dda_unit_reset();
PRIVATE int	dda_dload();

extern struct bidata bidata[];

/* previous_unload contains this value if the request queue is *not* blocked */
#define	UL_NOT_BLOCKED	(RQSIZE+1)

typedef struct {
    byte	fe_state;
    byte	fe_soft_id;
    byte	fe_soft_vers;
    byte	fe_diag_status;
} GP_REG3_USAGE;

PRIVATE u_short	ddastd[] = { 0 };	/* standard addresses */

/* ddainfo is setup by OS */
struct uba_device *ddainfo[NDDA];	/* ptrs to device info */

struct uba_driver ddadriver =	/* device driver info */
{
 ddaprobe,			/* device probe routine */
 0,				/* slave probe routine */
 ddaattach,			/* device attach routine */
 0,				/* "dmago" routine */
 ddastd,			/* device address */
 "dda",				/* device name */
 ddainfo,			/* ptr to device info ptrs */
 "dda",				/* device name */
 0
};

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDAPROBE()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/* Purpose:                                                        */
/*                                                                 */
/*  This routine is pretty much a dummy.  The real probe is done   */
/*  the operating system.					   */
/*                                                                 */
/*  Call:          ddaprobe(ui)	                                   */
/*  Argument:	   ui:   pointer to device data structure, used    */
/*                       for BI environment                        */
/*  Returns:       length of register structure for ACP device     */
/*  Called by:     network software, part of autoconfiguration on  */
/*                 the VAX, the address of this routine is one of  */
/*                 the fields of the uba_driver structure          */
/*  Calls to:      none                                            */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE int savefirmrev;	/* save between probe & attach */

ddaprobe(nxv, nxp, cpup, binumber, binode, ui)
struct bi_nodespace *nxv;
char                *nxp;
struct cpusw        *cpup;
int                  binumber;
int                  binode;
struct uba_device   *ui;
{
    GP_REG3_USAGE   *gpreg3;

    /* check to see if this board was in system configuration */

    if (ui->ui_unit > NDDA)
	return 0;		/* extra board? punt */

    gpreg3 = (GP_REG3_USAGE *) &nxv->biic.biic_gpr3;

    /* check that the device is really a 7250 and save away */
    /* the firmware revision level for version dependent processing */
    /* If we just booted, the diagnostics may still be running as we */
    /* probe the device - it's still OK to read the ID and VERSION */
    /* numbers, which come valid within milliseconds after power is */
    /* applied to the board - so say the firmware gurus */

    dda_hasmaint = gpreg3->fe_soft_vers & 0x80;
    savefirmrev  = gpreg3->fe_soft_vers & 0x7f;

    switch (gpreg3->fe_soft_id) {
    case 0x68:
	dda_product = "ACP7250";
	break;
    default:
#ifdef	notdef
	if (dda_hasmaint == 0)	/* assume we know what we're doing */
	    return 0;
	savefirmrev = 255;	/* assume high version */
	dda_product = "ACP7250";
	break;
#else
	return 0;
#endif
    }

    return 1;	/* return that we have found a board and are happy */
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDAATTACH()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  This routine attaches the device to the network software.  The */
/*  network interface structure is filled in.  The device will be  */
/*  initialized when the system is ready to accept packets.  The   */
/*  dda_init initialization/service flag is zeroed, DDN standard   */
/*  X.25 service is implemented by default unless otherwise        */
/*  specified by the user via the acpconfig program.               */
/*--
	Various other types of one-time initialization:
	    - allocate memory and setup queues
								 --*/
/*                                                                 */
/*  Call:           ddaattach(ui)                                  */
/*  Argument:       ui:  ptr to the uba_device data structure      */
/*  Returns:        nothing                                        */
/*  Called by:      network software, part of network system       */
/*                  configuration, identification to the network   */
/*                  software,  the address of this routine is one  */
/*                  of the fields of the uba_driver structure      */
/*  Calls to:       if_attach()                                    */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

ddaattach(ui)
struct uba_device *ui;
{
    register struct dda_softc *ds = &dda_softc[ui->ui_unit];
    SYSGEN_BLOCK   *SysGenPtr;
    static long     initmsgs[] = {0, 0, 0, 0, -1, -1, -1, -1, -2};

    ds->dda_init = DDA_STANDARD;	/* init/service flag <- default  */
    ds->dda_firmrev = savefirmrev;	/* saved firmware rev level */
    ds->dda_net_id = 0;			/* default */
    ds->dda_if.if_unit = ui->ui_unit;	/* set unit number */
    ds->dda_if.if_name = "dda";		/* set device name */
    ds->dda_if.if_mtu = DDAMTU;		/* set max msg size */
    ds->dda_if.if_init = ddainit;	/* set init routine addr */
    ds->dda_if.if_ioctl = ddaioctl;	/* set ioctl routine addr */
    ds->dda_if.if_output = ddaoutput;	/* set output routine addr */
    ds->dda_if.if_reset = ddareset;	/* set reset routine addr */
    ds->dda_if.if_watchdog = ddatimer;	/* set timer routine addr */

    bcopy(initmsgs, ddamsgs[ui->ui_unit], sizeof(initmsgs)); 

    /* allocate shared memory segment */

    KM_ALLOC(SysGenPtr, SYSGEN_BLOCK *, 1024, KM_CLUSTER, KM_NOWAIT);
    if (SysGenPtr == NULL) {
	DMESG(ui->ui_unit, 0, (DDALOG(LOG_ERR)
		 "dda%d: unable to get shared memory segment\n", ui->ui_unit
		 DDAELOG));
	return;
    }
	
    ds->dda_mapreg = (int) SysGenPtr;	/* save it in ds structure */

    /* hook the interrupt vector to scb */

#ifndef	SIMULATION
    bidev_vec(ui->ui_adpt, ui->ui_nexus, LEVEL14, ui);
#endif

    dda_shm_setup(ui->ui_unit, (struct biic_regs *) ui->ui_addr, SYSGEN_VALID);

    if_attach(&ds->dda_if);	/* attach new network device */
}

PRIVATE void
dda_shm_setup(unit, nxv, drvr_state)
int               unit;
struct biic_regs *nxv;
int		  drvr_state;
{
    register struct dda_softc *ds  = &dda_softc[unit];
    register struct uba_device *ui = ddainfo[unit];
    SYSGEN_BLOCK   *SysGenPtr;
    char           *mem;

    SysGenPtr = (SYSGEN_BLOCK *) ds->dda_mapreg;
    mem       = (char *)         ds->dda_mapreg;

    SysGenPtr->request     = (RQUEUE *) &mem[512 - 4]; /* get entries on page */
    SysGenPtr->completion  = (CQUEUE *) &mem[sizeof(SYSGEN_BLOCK) + 7 & 0xff7c];

    SysGenPtr->prequest    = svtophy(SysGenPtr->request);
    SysGenPtr->pcompletion = svtophy(SysGenPtr->completion);

    SysGenPtr->req_size   = RQSIZE;
    SysGenPtr->comp_size  = CQSIZE;
    SysGenPtr->intr_level = 0;		/* level 0 is lowest level */
    SysGenPtr->pwr_action = 0;		/* boot default code */

    SysGenPtr->previous_unload = UL_NOT_BLOCKED; /* request queue not blocked */

    SysGenPtr->request->load = 0;
    SysGenPtr->request->unload = 0;
    SysGenPtr->completion->load = 0;
    SysGenPtr->completion->unload = 0;
    bzero(SysGenPtr->completion->entry, sizeof(CENTRY) * CQSIZE);
    bzero(SysGenPtr->request->entry, sizeof(RENTRY) * RQSIZE);

#ifndef	SIMULATION
						/* set interrupt destination */
    nxv->biic_int_dst = bidata[ui->ui_adpt].biintr_dst;
#endif

    nxv->biic_err  = nxv->biic_err;		/* actually clears error reg */
    nxv->biic_gpr1 = svtophy(SysGenPtr);	/* get BI physical address */
    nxv->biic_gpr0 = drvr_state;		/* tell FE our state */
}

#ifdef DDADEBUG
PRIVATE void
dda_dump_biic_regs(nxv)
struct biic_regs *nxv;
{
    uprintf("biic register dump (nxv=0x%x 0x%x(p))\n",
	nxv, svtophy(nxv));
    uprintf("typ=0x%x ctrl=0x%x err=0x%x err_int=0x%x\n",
	nxv->biic_typ, nxv->biic_ctrl, nxv->biic_err, nxv->biic_err_int);
    uprintf("int_dst=0x%x ip_msk=0x%x ip_dst=0x%x ip_src=0x%x\n",
	nxv->biic_int_dst, nxv->biic_ip_dst, nxv->biic_ip_dst, nxv->biic_ip_src);
    uprintf("strt=0x%x end=0x%x bci_ctrl=0x%x wrt_stat=0x%x\n",
	nxv->biic_strt, nxv->biic_end, nxv->biic_bci_ctrl, nxv->biic_wrt_stat);
    uprintf("int_ctrl=0x%x\n", nxv->biic_int_ctrl);
    uprintf("gpr0=0x%x gpr1=0x%x gpr2=0x%x gpr3=0x%x\n",
	nxv->biic_gpr0, nxv->biic_gpr1, nxv->biic_gpr2, nxv->biic_gpr3);
}

PRIVATE void dda_dump_shm(shm)
SYSGEN_BLOCK *shm;
{
    uprintf("request=0x%x 0x%x(p) completion=0x%x 0x%x(p)\n",
	shm->request, shm->prequest, shm->completion, shm->pcompletion);
    uprintf("req_size=%d comp_size=%d intr_level=%d pwr_act=%d\n",
	shm->req_size, shm->comp_size, shm->intr_level, shm->pwr_action);
    uprintf("req_load=%d req_un=%d comp_load=%d comp_ul=%d prev=%d\n",
	shm->request->load, shm->request->unload,
	shm->completion->load, shm->completion->unload,
	shm->previous_unload);
    uprintf("request queue is%s blocked\n",
	(shm->previous_unload == UL_NOT_BLOCKED ? " not" : ""));
}
#endif

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDARESET()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*      Reset of interface.  Free mbufs if there is		   */
/*      queued output data.                                        */
/*                                                                 */
/*  Call:              ddareset(unit, uban)                        */
/*  Arguments:         unit:   ACP device unit number              */
/*		       uban:   Unibus adapter # (unused, but the   */
/*				kernel placed it on the stack)	   */
/*  Returns:           nothing                                     */
/*  Called by:         network software, address of routine is     */
/*                     defined in dda_if network interface struct  */
/*  Calls to:          DDALOG()                                    */
/*		       hist_link_state()			   */
/*		       hist_all_lcns()				   */
/*		       dda_shm_setup()			           */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*ARGSUSED*/
ddareset(unit, uban)
int             unit;
int             uban;
{
    DMESG(unit, 0, (DDALOG(LOG_ERR) "dda%d: reset\n", unit DDAELOG));
    dda_unit_reset(unit, 1, SYSGEN_VALID);
}

PRIVATE void
bufreset(unit)
int		unit;
{
    DMESG(unit, 0, (DDALOG(LOG_ERR) "dda%d: buffer reset\n", unit DDAELOG));
    dda_unit_reset(unit, 0, SYSGEN_VALID);
}

PRIVATE void
dda_unit_reset(unit, doreset, drvr_state)
int		unit;
int		doreset;
int		drvr_state;
{
    register struct uba_device *ui = ddainfo[unit];
    register struct dda_softc  *ds = &dda_softc[unit];
    register struct dda_cb     *dc;
    register int                lcn;
    struct biic_regs	       *nxv;

    if (unit >= NDDA || (ui == 0 || ui->ui_alive == 0))
	return;

    nxv = (struct biic_regs *) ui->ui_addr;

    ds->dda_if.if_flags &= ~IFF_UP;
    hist_link_state(unit, ds->dda_state, S_DISABLED);
    ds->dda_state = S_DISABLED;

    /* reset the board */
    if (doreset)
	nxv->biic_ctrl = BICTRL_STS | BICTRL_SST;

    DELAY(50000);			/* delay 50ms for BIIC to run tests */

    ds->dda_flags = 0;			/* clear ACP operational flag  */
					/* which will get us ready for B int */

    ds->dda_init &= ~DDA_INTCLOCK;	/* reset internal-clocking-set bit */

    nddach[unit] = NDDACH_DEFAULT;	/* reset SVC limit */

    dc = ds->dda_cb;			/* flush any queued output data */
    /* LCNLINK */
    for (lcn = 0; lcn <= NDDACH; lcn++) {	/* for all LCN's ... */
	dc->dc_inaddr.s_addr = 0;	/* clear remote internet addr */
        dc->dc_key.key_addr.s_addr = 0;
	dc->dc_wsizein = dc->dc_wsizeout = 0;
	dc->dc_pktsizein = dc->dc_pktsizeout = 0;
	dc->dc_state = LC_IDLE;	/* init LCN state */
	dc->dc_timer = TMO_OFF;	/* turn LCN timer off */
	dc->dc_flags = 0;
#ifdef DDADEBUG
	dc->dc_out_t = TMO_OFF;	/* turn FE completion timer off */
#endif
	dc++;
    }

    hist_all_lcns(unit, LC_IDLE);
#ifdef DDA_RAWOPT
    pi_init(unit, 1);
#endif
#ifdef DDA_PADOPT
    x29_init(unit, 1);
#endif
    dda_shm_setup(unit, nxv, drvr_state);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDAINIT()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine initializes the interface for operation.  The   */
/*    device control blocks are initialized, UNIBUS resources are  */
/*    allocated and an initialization message is sent to the ACP.  */
/*                                                                 */
/*    Note that interrupt "b" is enabled here to avoid a possible  */
/*    race condition at power up time - it was previously done in  */
/*    the probe and reset routines.                                */
/*                                                                 */
/*  Call:             ddainit(unit)                                */
/*  Argument:         unit:  ACP device unit number                */
/*  Returns:          nothing                                      */
/*  Called by:        network software, address of this routine is */
/*                    defined in dda_if network interface struct   */
/*                    ddaioctl()                                   */
/*                    ddaintb()                                    */
/*  Calls to:         ddatimer()                                   */
/*                    splimp()                                     */
/*                    dda_rrq()                                    */
/*                    splx()                                       */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

ddainit(unit)
int             unit;
{
    register struct dda_softc *ds;
    register struct dda_cb *dc;
    register struct uba_device *ui;

    int             lcn,
                    s;

    ds = &dda_softc[unit];
    ui = (struct uba_device *) ddainfo[unit];
#ifdef DDADEBUG
    if (DDADBCH(0, unit)) {
	DDALOG(LOG_DEBUG) "dda%d: ddainit()\n", unit DDAELOG;
	DDALOG(LOG_DEBUG) "dda%d: ds->dda_if.if_addrlist=0x%x ds->dda_flags=0x%x\n",
			  unit, ds->dda_if.if_addrlist, ds->dda_flags DDAELOG;
    }
#endif DDADEBUG

    /* if we have no internet addr  if device not operational don't init yet */
    if (ds->dda_if.if_addrlist == (struct ifaddr *) 0 ||
	((ds->dda_flags & DDAF_OK) == 0))
	return;

    /* set default options */
    ds->dda_init &= ~DDA_INTCLOCK;	/* reset internal-clocking-set bit */
    nddach[unit] = NDDACH_DEFAULT;	/* reset SVC limit */

    if ((ds->dda_if.if_flags & IFF_RUNNING) == 0) {
	dc = ds->dda_cb;	/* setup ptr to first LCN cntl block */

	/* LCNLINK */
	for (lcn = 0; lcn <= NDDACH; lcn++) {	/* for all LCN's ... */
	    dc->dc_lcn = lcn;	/* record LCN */
	    dc->dc_inaddr.s_addr = 0;	/* clear remote internet addr */
	    dc->dc_key.key_addr.s_addr = 0;
	    dc->dc_wsizein = dc->dc_wsizeout = 0;
	    dc->dc_pktsizein = dc->dc_pktsizeout = 0;
	    dc->dc_state = LC_DOWN;	/* init LCN state */
	    dc->dc_timer = TMO_OFF;	/* turn LCN timer off */
#ifdef DDADEBUG
	    dc->dc_out_t = TMO_OFF;	/* turn FE completion timer off */
#endif

	    /* init LCN output queue */

	    s = splimp();
	    dc->dc_oq.ifq_head = (struct mbuf *) 0;
	    dc->dc_oq.ifq_tail = (struct mbuf *) 0;
	    splx(s);
	    dc->dc_oq.ifq_len = 0;
	    dc->dc_oq.ifq_maxlen = DDA_OQMAX;
	    dc->dc_oq.ifq_drops = 0;

	    /* init HDX channels */

	    dc->dc_rchan.hc_next = (struct hdx_chan *) 0;
	    dc->dc_rchan.hc_chan = lcn * 2;
	    dc->dc_wchan.hc_next = (struct hdx_chan *) 0;
	    dc->dc_wchan.hc_chan = (lcn * 2) + 1;

	    dc->dc_rchan.hc_mbuf = (struct mbuf *) 0;
	    dc->dc_rchan.hc_curr = (struct mbuf *) 0;
	    dc->dc_wchan.hc_mbuf = (struct mbuf *) 0;
	    dc->dc_wchan.hc_curr = (struct mbuf *) 0;

	    dc->dc_flags = 0;	/* initialize flags */

	    dc++;		/* point at next cntl blk */
	}
	hist_all_lcns(unit, LC_DOWN);

	ds->dda_sioq.sq_head = (struct hdx_chan *) 0;
	ds->dda_sioq.sq_tail = (struct hdx_chan *) 0;

	ds->dda_if.if_flags |= IFF_RUNNING;
    }
    s = splimp();
    dc = ds->dda_cb;		/* setup ptr to first LCN cntl block */
    for (lcn = 0; lcn <= nddach[unit]; lcn++) {	/* issue reads on all LCNs */
	dda_rrq(ds, &(dc->dc_rchan));
	dc++;
    }
    splx(s);

    ddatimer(unit);		/* start timers */

#ifdef DDA_RAWOPT
    pi_init(unit, 0);		/* initialize progammer interface */
#endif
#ifdef DDA_PADOPT
    x29_init(unit, 0);
#endif
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDAINTA()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This is the interrupt handler for I/O interrupts (interrupt  */
/*    "a") from the ACP device.  The I/O mailboxes are scanned for */
/*    handshake events to process. Two types of interrupts are	   */
/*    processed:  I/O request acknowledge, and I/O completion.     */
/*    The interrupting HDX channel and interrupt type are	   */
/*    obtained. If interrupt is an I/O request acknowledge the	   */
/*    next I/O request is passed to the device.  If the interrupt  */
/*    is an I/O completion, check for errors, if ok process	   */
/*    according to whether supervisory or data channel.		   */
/*                                                                 */
/*  Call:              ddainta(unit)                               */
/*  Arguments:         unit:  ACP device unit number               */
/*  Returns:           nothing                                     */
/*  Called by:         network software, address of this routine   */
/*                     is defined in af_inet network interface     */
/*                     data structure                              */
/*  Calls to:          DDALOG()                                    */
/*                     start_chn()                                 */
/*                     dda_data()                                  */
/*                     dda_supr()                                  */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

ddainta(unit)
int             unit;
{
    register struct dda_softc *ds;
    register struct hdx_chan *hc;
    register struct uba_device *ui;
    CQUEUE         *complq;
    CENTRY         *centry_ptr;
    byte            flags;
    int             chan,
                    cc,
                    subcc,
                    cnt;
    SYSGEN_BLOCK   *SysGenPtr;

    ds = &dda_softc[unit];
    SysGenPtr = (SYSGEN_BLOCK *) ds->dda_mapreg;
    ui = (struct uba_device *) ddainfo[unit];

#ifdef DDADEBUG
    if (DDADBCH(5, unit)) {
	DDALOG(LOG_DEBUG) "dda%d: ddainta()\n", unit DDAELOG;
    }
#endif DDADEBUG

    /***********************************************************************/
    /* Check Request Queue if xmt was blocked				   */
    /***********************************************************************/

    /* was request queue blocked before? (previous unload = a blocked value) */
    if (SysGenPtr->previous_unload != UL_NOT_BLOCKED) {
	/* yes, so check to see if unload pointer has changed */
	if (SysGenPtr->previous_unload != SysGenPtr->request->unload) {
	    /* if the unload pointer changed, we're no longer blocked */
	    SysGenPtr->previous_unload = UL_NOT_BLOCKED;
	    start_chn(ds);			/* re-start I/O */
	}
    }

    /***********************************************************************/
    /* Check I/O Completion Queue                                          */
    /***********************************************************************/

    complq = SysGenPtr->completion;	/* point to completion q */
    centry_ptr = complq->entry + complq->unload;

    /* new entry valid? I/O Completion Mailbox */
    while ((flags = centry_ptr->flags) & CENTRY_VALID) {

#ifdef DDA_MSGQ
	dda_mqstr("(cx)");
#endif

	centry_ptr->flags = 0;	/* clear valid bit */
	/* Get logical channel information. */
	chan = centry_ptr->dpn;
	if (chan > nddach[unit]) {
	    DMESG(unit, 6,
		 (DDALOG(LOG_ERR) "dda%d: unknown completion channel, lcn=%d\n",
		  unit, chan DDAELOG));
	    goto bump_unload;
	}
	if (flags & FLAGS_DIR)
	    hc = &(ds->dda_cb[chan].dc_wchan);	/* write channel */
	else
	    hc = &(ds->dda_cb[chan].dc_rchan);	/* read channel */

	cc = centry_ptr->stat;		/* Mailbox I/O completion status */
	subcc = centry_ptr->sbstat;	/* Mailbox I/O completion substatus */
	cnt = centry_ptr->count;	/* Mailbox I/O completion byte count */

#ifdef	DDADEBUG
	if (DDADBCH(33, ds->dda_if.if_unit)) 
	    prt_bytes(ds->dda_if.if_unit, "incoming data", hc->hc_curr, cnt);
#endif

	switch (cc) {	/* check for unsuccessful I/O completion status */
	case DDAIOCABT:	/* probably VCN flush */
	    if (LOG_ABT)
		DDALOG(LOG_ERR) "dda%d: abort completed on chan %d\n",
		    unit, hc->hc_chan DDAELOG;
	    break;

	case DDAIOCERR:
	    DMESG(unit, 7,
		  (DDALOG(LOG_ERR) "dda%d: program error\n", unit DDAELOG));
	    goto daterr;

	case DDAIOCOVR:
	    DMESG(unit, 8,
		  (DDALOG(LOG_ERR) "dda%d: overrun error\n", unit DDAELOG));
	    goto daterr;

	case DDAIOCUBE:
	    DMESG(unit, 9,
	      (DDALOG(LOG_ERR) "dda%d: transfer count = 0\n", unit DDAELOG));
	    goto daterr;

	case DDAIODMAE:
	    DMESG(unit, 10,
		  (DDALOG(LOG_ERR) "dda%d: DMA completion error (%x)\n",
		   unit, subcc DDAELOG));
	    goto daterr;

	case DDAIOLCOL:
	    DMESG(unit, 11,
		(DDALOG(LOG_ERR) "dda%d: listen collision\n", unit DDAELOG));
	    goto daterr;

	case DDAIOFUNC:
	    DMESG(unit, 12,
		(DDALOG(LOG_ERR) "dda%d: invalid function\n", unit DDAELOG));
	    goto daterr;

	case DDAIODPN:
	    DMESG(unit, 13,
		  (DDALOG(LOG_ERR) "dda%d: invalid dpn\n", unit DDAELOG));
	    goto daterr;

    daterr:
	    DMESG(unit, 14,
	    (DDALOG(LOG_ERR) "lcn=%d func=%x\n", chan, hc->hc_func DDAELOG));
	    if (hc->hc_func & DDARDB)
		ds->dda_if.if_ierrors++;
	    else
		ds->dda_if.if_oerrors++;
	}

	/* was it supervisor or data traffic? */

	if (chan > DDA_SUPR) {
#ifdef DDA_PADOPT
	    if (ds->dda_cb[chan].dc_flags & DC_X29)
		x29_data(ds, hc, cc, cnt, subcc);
	    else
#endif
#ifdef DDA_RAWOPT
	    if (ds->dda_cb[chan].dc_flags & DC_RAW)
		pi_data(ds, hc, cc, cnt, subcc);
	    else
#endif
		dda_data(ds, hc, cc, cnt);
	} else
	    dda_supr(ds, hc, cc, cnt);

bump_unload:
	/* Ack the interrupt.  Fix the Mailbox Ready and Done bits:  set DON
	   bits, and clear RDY bits so mailbox may be reused. */

	complq->unload = (complq->unload + 1) % CQSIZE;
	centry_ptr = complq->entry + complq->unload;
    }
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDAINTB()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*   Service interrupt "b", system interrupt, from the ACP device. */
/*   If the ACP device is operational, interrupt is unexpected,    */
/*   disable the device.  If the interrupt indicates a powerup     */
/*   diagnostic failure, disable the device.  Otherwise, set ACP   */
/*   flag for device operational, enable interrupt a, enable DMA,  */
/*   and perform initialization tasks.                             */
/*                                                                 */
/*  Call:             ddaintb(unit)                                */
/*  Argument:         unit: DDA device unit number                 */
/*  Returns:          nothing                                      */
/*  Called by:        network software, address of this routine is */
/*                    defined in af_inet network interface struct  */
/*  Calls to:         DDALOG()                                     */
/*                    ddainit()                                    */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

ddaintb(unit)
int unit;
{
    register struct dda_softc *ds = &dda_softc[unit];
    register int    lcn;
    register struct mbuf *m;
    register struct hdx_chan *hc;
    register struct dda_cb *dc;
    struct biic_regs *nxv = (struct biic_regs *) ddainfo[unit]->ui_addr;
    int               stat_val;

    stat_val = ((GP_REG3_USAGE *) &nxv->biic_gpr3)->fe_diag_status;
   
#ifdef DDADEBUG
    if (DDADBCH(6, unit)) {
	DDALOG(LOG_DEBUG) "dda%d: ddaintb()\n", unit DDAELOG;
    }
#endif DDADEBUG

    if (ds->dda_flags & DDAF_OK) {
	DMESG(unit, 15,
	      (DDALOG(LOG_ERR) "dda%d: asynchronous restart, status = %d\n",
	       unit, stat_val DDAELOG));
	ds->dda_flags = 0;
	ds->dda_if.if_flags &= ~(IFF_RUNNING | IFF_UP);
	hist_link_state(unit, ds->dda_state, S_DISABLED);
	ds->dda_state = S_DISABLED;
	ddareset(unit, 0);
	return;
    }

    if ((stat_val & DDASTAT_ERR) != 0) {
	DMESG(unit, 16,
	      (DDALOG(LOG_ERR) "dda%d: Diagnostic failure = %d\n",
	       unit, stat_val DDAELOG));
	dda_disable(unit);
    } else if (stat_val == DDASTAT_NMC) {
	DMESG(unit, 17,
	  (DDALOG(LOG_ERR) "dda%d: No Microcode Present!\n", unit DDAELOG));
	dda_disable(unit);
    } else if (stat_val == DDASTAT_OK) {
	SYSGEN_BLOCK *SysGenPtr = (SYSGEN_BLOCK *) ds->dda_mapreg;
	ds->dda_flags |= DDAF_OK;	/* no longer expecting a B interrupt */

	for (lcn = 0; lcn <= NDDACH; lcn++) {	/* for all LCNs */
	    dc = &dda_softc[unit].dda_cb[lcn];
	    while (dc->dc_oq.ifq_len) {		/* clear output queue */
		IF_DEQUEUE(&dc->dc_oq, m);
		if (m)
		    m_freem(m);
	    }
	    hc = &dc->dc_rchan;
	    if (hc->hc_mbuf) {
		m_freem(hc->hc_mbuf);		/* free read mbufs */
		hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	    }
	    hc = &dc->dc_wchan;
	    if (hc->hc_mbuf) {
		m_freem(hc->hc_mbuf);		/* free write mbufs */
		hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	    }
	}

	SysGenPtr->request->load = 0;
	SysGenPtr->request->unload = 0;
	SysGenPtr->completion->load = 0;
	SysGenPtr->completion->unload = 0;
	bzero(SysGenPtr->completion->entry, sizeof(CENTRY) * CQSIZE);
	bzero(SysGenPtr->request->entry, sizeof(RENTRY) * RQSIZE);
	SysGenPtr->pwr_action = 0xff;	/* we are operational */

	/* this 10ms delay is here to give the FEP enough time to re-program
	   its interrupt vector to point to the A interrupt routine and
	   reset the MFP interrupt counter before the driver posts the
	   NDDACH reads in ddainit.  It's a kludge, but keep it for safety */

	DELAY(10000);			/* delay 10 miliseconds */
	ddainit(unit);
    }
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDA_WRQ()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    Process write requests.  Put I/O request values in           */
/*    half-duplex control channel structure:  set function code    */
/*    for write to ACP with Transfer Grant set.  If there are no   */
/*    more mbufs in chain, mark DDAEOS for end of stream.  Set     */
/*    count from data length (byte count) in mbuf, and subfunction */
/*    is zero.  If the COMREGs are busy, queue for start later.    */
/*                                                                 */
/*  Call:            dda_wrq(ds, hc, abt)                          */
/*  Argument:        ds:   pointer to device control block struct  */
/*                   hc:   pointer to half-duplex channel cntl blk */
/*		     abt:  indication of whether request is an     */
/*			   abort request.			   */
/*  Returns:         nothing                                       */
/*  Called by:         dda_start()                                 */
/*                     dda_data()                                  */
/*  Calls to:          mtod()                                      */
/*                     start_chn()                                 */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
dda_wrq(ds, hc, abt)
struct dda_softc *ds;
register struct hdx_chan *hc;
u_char          abt;
{
    register struct mbuf *m;
    register int    s;

    if (abt)
	hc->hc_func = DDAABT;
    else
	hc->hc_func = DDAWRT;

#ifdef DDADEBUG
    if (DDADBCH(15, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: dda_wrq: chan=%d func=%x\n",
	    ds->dda_if.if_unit, hc->hc_chan, hc->hc_func DDAELOG;
    }
#endif DDADEBUG

    s = splimp();

    /*
     * If ACP comm regs busy, queue start i/o for later. 
     */
    if (ds->dda_sioq.sq_head) {
	(ds->dda_sioq.sq_tail)->hc_next = hc;
	ds->dda_sioq.sq_tail = hc;
	hc->hc_next = 0;
	splx(s);
	return;
    }
    /* start i/o on channel now */

    ds->dda_sioq.sq_head = hc;
    ds->dda_sioq.sq_tail = hc;
    hc->hc_next = 0;
    splx(s);
    (void) start_chn(ds);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDA_RRQ()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    Process read requests.  Quit if attempt to get an mbuf is    */
/*    unsuccessful.  Put I/O request values in half-duplex control */
/*    channel structure:  set function code for read from ACP with */
/*    Transfer Grant set, set count from data length (byte count)  */
/*    in mbuf, and subfunction is zero.                            */
/*                                                                 */
/*  Call:            dda_rrq(ds, hc)                               */
/*  Argument:        ds:   pointer to device control block struct  */
/*                   hc:   pointer to half-duplex control chan     */
/*  Returns:         nothing                                       */
/*  Called by:       ddainit()                                     */
/*                   dda_data()                                    */
/*                   dda_supr()                                    */
/*  Calls to:        MGET()                                        */
/*                   MCLGET()                                      */
/*                   DDALOG()                                      */
/*                   start_chn()                                   */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
dda_rrq(ds, hc)
struct dda_softc *ds;
register struct hdx_chan *hc;
{
    register struct mbuf *m;
    register int    s;
#if ACC_ULTRIX > 12
    struct mbuf    *p;
#endif

#ifdef DDADEBUG
    if (DDADBCH(16, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: dda_rrq()\n", ds->dda_if.if_unit DDAELOG;
    }
#endif DDADEBUG

    MGET(m, M_DONTWAIT, MT_DATA);
    if (m == 0) {
	DMESG(ds->dda_if.if_unit, 25, (DDALOG(LOG_ERR) 
	    "dda%d:  couldn't get buffer for read\n",
	    ds->dda_if.if_unit DDAELOG));
	return;
    }

    s = splimp();

    /* hc_mbuf set to zero during initialization */

    /* if hc->hc_mbuf is zero, then this is the first mbuf in the chain,
       so be conservative and only queue up a small mbuf */

    if (hc->hc_mbuf == 0) {
	m->m_len = MLEN;	/* set the size to a small mbuf */
	hc->hc_mbuf = m;
	hc->hc_curr = m;

    /* if it's not the first mbuf in the chain, we may be running a FTP
       or something that deserves higher performance, so queue a cluster */

    } else {
#if	ACC_ULTRIX > 12
	MCLGET(m, p);		/* associate a page cluster with this mbuf */
#else
	MCLGET(m);
#endif
	hc->hc_curr->m_next = m;
	hc->hc_curr = m;
	m->m_next = 0;
    }
    splx(s);

    hc->hc_func = DDARDB + DDASTR;
    hc->hc_sbfc = 0;

    s = splimp();

    /*
     * If ACP comm regs busy, queue start i/o for later. 
     */
    if (ds->dda_sioq.sq_head) {
	(ds->dda_sioq.sq_tail)->hc_next = hc;
	ds->dda_sioq.sq_tail = hc;
	hc->hc_next = 0;
	splx(s);
	return;
    }
    /* start i/o on channel now */

    ds->dda_sioq.sq_head = hc;
    ds->dda_sioq.sq_tail = hc;
    hc->hc_next = 0;
    splx(s);
    (void) start_chn(ds);
}





/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      START_CHN()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine copies ACP I/O requests into the ACP            */
/*    Communications Registers (COMREGs) and notifies the ACP.     */
/*    If the channel number is odd, indicating write, then the     */
/*    direction flag is set to indicate a transfer from the host   */
/*    to the front end.                                            */
/*                                                                 */
/*  Call:              start_chn(ds)                               */
/*  Argument:          ds:  pointer to device control block struct */
/*  Returns:           nothing                                     */
/*  Called by:         ddainta()                                   */
/*                     dda_rrq()                                   */
/*                     dda_wrq()                                   */
/*  Calls to:          none                                        */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE int
start_chn(ds)
struct dda_softc *ds;
{
    register struct hdx_chan *hc;
    register int    s;
    struct uba_device *ui;
    struct biic_regs *nxv;
    RQUEUE         *requestq;
    short           next_load;
    SYSGEN_BLOCK   *SysGenPtr;

    SysGenPtr = (SYSGEN_BLOCK *) (ds->dda_mapreg);
    ui = (struct uba_device *) (ddainfo[ds->dda_if.if_unit]);
    nxv = (struct biic_regs *) ui->ui_addr;

#ifdef DDADEBUG
    if (DDADBCH(17, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: start_chn()\n", ds->dda_if.if_unit DDAELOG;
    }
#endif DDADEBUG

    s = splimp();

    while (hc = (struct hdx_chan *) (ds->dda_sioq.sq_head)) {
	requestq  = SysGenPtr->request;
	next_load = (requestq->load + 1) % RQSIZE;

	if (next_load == requestq->unload) {	/* any room left in q? */
	    /* no, say we're blocked */
	    SysGenPtr->previous_unload = requestq->unload;
	    break;
	}
/*
 *  Check sioq for invalid writes
 */

	/* if write channel, but not supervisor, and it's marked invalid,
	   then either drop or abort it */
	if ((hc->hc_chan & 0x01) && (hc->hc_chan != 1) &&
	    (hc->hc_inv & INVALID_MBUF)) {
	    if (ds->dda_cb[hc->hc_chan >> 1].dc_flags & DC_OBUSY) {
#ifdef DDADEBUG
		if (DDADBCH(28, ds->dda_if.if_unit)) {
		    DDALOG(LOG_DEBUG) "dda%d: start_chn: aborting chan %d\n",
			ds->dda_if.if_unit, hc->hc_chan DDAELOG;
		}
#endif DDADEBUG
		goto send;	/* send an abort if output not complete */
	    } else {		/* otherwise free it */
		if (hc->hc_mbuf) {	/* free pending request */
		    m_freem(hc->hc_mbuf);
		    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
		}
/*
 * Restart Output
 */
		hc->hc_inv &= ~INVALID_MBUF;
		ds->dda_sioq.sq_head = ds->dda_sioq.sq_head->hc_next;
		/* The following call may cause a re-entrant call to
		   start_chn, but only if the sioq is currently empty. In
		   that case, the re-entrant call is ok */
		dda_start(ds, &ds->dda_cb[hc->hc_chan >> 1]);
		continue;
	    }
	}
send:
	{			/* Read or Write request is valid */
	    RENTRY         *rentry_ptr;
	    int             index;

	    /* Set up queue element */
	    rentry_ptr = requestq->entry + requestq->load;

	    /* Fill the slots starting at hc->hc_curr */
	    rentry_ptr->count  = 0;
	    index              = 0;

#ifdef DDADEBUG
    if (DDADBCH(33, ds->dda_if.if_unit)) {
	struct mbuf *m;
	int    i;
	DDALOG(LOG_DEBUG)
	    "hc->hc_mbuf=0x%x hc->hc_curr=0x%x\n", hc->hc_mbuf, hc->hc_curr
	DDAELOG;
	for (i = 0, m = hc->hc_mbuf; m && m != hc->hc_curr; i++, m = m->m_next)
	    DDALOG(LOG_DEBUG)
		"previous(%d): 0x%x\n", i, m
	    DDAELOG;
	for (i = 0, m = hc->hc_curr; m; i++, m = m->m_next) 
	    DDALOG(LOG_DEBUG)
	        "chain(%d): 0x%x\n", i, m
	    DDAELOG;
    }
#endif DDADEBUG

	    /* if it's a write, map as many mbufs on as we can */
	    if (hc->hc_chan & 1) {
		register struct mbuf *last_mapped;
		while (hc->hc_curr) {
		    register int   len, seglen, pages;
		    register char *addr;

		    addr  = mtod(hc->hc_curr, char *);
		    len   = hc->hc_curr->m_len;
		    pages = (len - 1 >> PGSHIFT) + 1;

		    if (len == 0) 
			printf("start_chn: len was zero?!?\n");

		    /* If we have enough slots left in this entry to map this
		       mbuf,  then do so */

		    if (NUM_BI_ADDR - index > pages) {
			while ((seglen = MIN(NBPG, len)) > 0) {
			    rentry_ptr->BI_address[index] = svtophy(addr);
			    rentry_ptr->BI_count[index]   = seglen;
			    rentry_ptr->count            += seglen;
			    len  -= seglen;
			    addr += seglen;
			    index++;
			}
			/* we've mapped this mbuf onto the queue, deal with next */
			last_mapped = hc->hc_curr;
			hc->hc_curr = hc->hc_curr->m_next;

		    } else {
			/* if this buffer can't ever fit, there is a major
			   problem (someone changed the size of a large mbuf
			   so it can't fit in NUM_BI_ADDR memory pages).  This
			   should never happen. */
		       if (index == 0)
			    panic("dda driver: mbuf too large to map\n");
		       break;
		    }
		}

		/* if more mbufs to map, we're not at the end of stream */
		if (hc->hc_curr)
		    hc->hc_func |= DDASTR;
		else
		    hc->hc_func |= DDAEOS;

		/* higher up routines expect hc->hc_curr to be the last
		   mapped mbuf */
		hc->hc_curr = last_mapped;

	    } else {	/* otherwise it's a read, so process the one mbuf */

		register int   len, seglen, pages;
		register char *addr;

		addr  = mtod(hc->hc_curr, char *);
		len   = hc->hc_curr->m_len;
		pages = (len - 1 >> PGSHIFT) + 1;

		/* if it's a read, zero out the mbuf's m_len field because
		   the mbuf currently contains no valid data */

		hc->hc_curr->m_len = 0;

		if (len == 0) 
		    printf("start_chn: len was zero?!?\n");

		/* If we have enough slots left in this entry to map this
		   mbuf,  then do so */

		if (NUM_BI_ADDR >= pages) 
		    while ((seglen = MIN(NBPG, len)) > 0) {
			rentry_ptr->BI_address[index] = svtophy(addr);
			rentry_ptr->BI_count[index]   = seglen;
			rentry_ptr->count            += seglen;
			len  -= seglen;
			addr += seglen;
			index++;
		    }

		/* if this buffer can't ever fit, there is a major problem
		   (someone changed the size of a large mbuf so it can't
		   fit in NUM_BI_ADDR memory pages).  This should never
		   happen. */
		else 
		   panic("dda driver: mbuf too large to map\n");
	    }

	    rentry_ptr->opcode = CMD_MPCPRQS;
	    rentry_ptr->dpn    = hc->hc_chan >> 1;
	    rentry_ptr->func   = hc->hc_func;
	    rentry_ptr->sbfunc = hc->hc_sbfc;
		
#ifdef DDADEBUG
	    if (DDADBCH(34, ds->dda_if.if_unit)) {
		int i;
		DDALOG(LOG_DEBUG)
		    "dpn=0x%x func=0x%x sbfunc=0x%x count=0x%x index=%d\n",
		    rentry_ptr->dpn, rentry_ptr->func, rentry_ptr->sbfunc,
		    rentry_ptr->count, index
		DDAELOG;
		for (i = 0; i < NUM_BI_ADDR; i++)
		    DDALOG(LOG_DEBUG)
			"addr[%d]=0x%x count[%d]=0x%x\n",
			rentry_ptr->BI_address[i], rentry_ptr->BI_count[i]
		    DDAELOG;
	    }
#endif DDADEBUG

#ifdef DDADEBUG
	    if (hc->hc_chan & 1) {	/* write */
		struct dda_cb  *dc;
		dc = &ds->dda_cb[hc->hc_chan >> 1];
		if (DDADBCH(17, ds->dda_if.if_unit))
		    DDALOG(LOG_DEBUG)
			"dda%d: start_chn: WRITE on lcn %d func %x\n",
			ds->dda_if.if_unit, hc->hc_chan >> 1,
			hc->hc_func DDAELOG;

		if (dc->dc_lcn)			/* don't start timer on lcn 0 */
		    dc->dc_out_t = TMO_RESTART;	/* Wait 90 sec for completion */
	    }
	    if (DDADBCH(30, ds->dda_if.if_unit))
		DDALOG(LOG_DEBUG) "dda%d: interrupting FE\n",
				  ds->dda_if.if_unit
		DDAELOG;
#endif DDADEBUG

	    /* TELL 7000 about new entry in request queue */
	    requestq->load = next_load;		/* boom! */
	    nxv->biic_gpr0 = SYSGEN_VALID;	/* interrupt FE */

#ifdef SIMULATION
	    MFP_COUNTER_SIM();	/* simulate hardware action */
#endif

	}
next_sioq_element:
	ds->dda_sioq.sq_head = ds->dda_sioq.sq_head->hc_next;
    }
    splx(s);
    return;
}

/*
 * disable interrupts and forget about unit
 */

PRIVATE void
dda_disable(unit)
int             unit;
{
    struct uba_device *ui = ddainfo[unit];
    struct biic_regs *nxv;

    nxv = (struct biic_regs *) ui->ui_addr;

    nxv->biic_int_dst = 0;
}

PRIVATE int
dda_dload(unit, dl)
int		unit;
struct dda_dnload *dl;
{
    register struct uba_device *ui = ddainfo[unit];
    register int i;
    static unsigned char *dlbuf[RQSIZE];

    if (unit >= NDDA)
	return EFAULT;

    switch (dl->lcommand) {
	case DN_LCMD_SETUP:
	    /* allocate buffers, reset board/setup shared memory */
	    for (i = 0; i < RQSIZE; i++) {
		KM_ALLOC(dlbuf[i], unsigned char *, 512, KM_CLUSTER, KM_NOWAIT);
		if (dlbuf[i] == NULL)
		    return ENOMEM;
	    }
 
	    dda_unit_reset(unit, 1, SYSGEN_DLOAD);
	    DMESG(unit, 0,
	      (DDALOG(LOG_ERR) "dda%d: download mode\n", unit DDAELOG));
	    return 0;

        case DN_LCMD_CLEANUP:
	    /* deallocate buffers and restore shm to normal */
	    for (i = 0; i < RQSIZE; i++) 
		if (dlbuf[i])
		    KM_FREE(dlbuf[i], KM_CLUSTER);

	    dda_shm_setup(unit, (struct biic_regs *) ui->ui_addr, SYSGEN_VALID);
	    DMESG(unit, 0,
	      (DDALOG(LOG_ERR) "dda%d: operational mode\n", unit DDAELOG));

	    /* now we will expect a B interrupt from the unit which will
	       cause ddainit to be called and set up the driver */
	    return 0;

	case DN_LCMD_FEOP:
        {
		/* perform a front end operation */
   		struct dda_softc *ds = &dda_softc[unit];
    		struct biic_regs *nxv = (struct biic_regs *) ui->ui_addr;
	        register RQUEUE         *requestq;
		register RENTRY         *rentry_ptr;
		register unsigned	 checksum;
		register unsigned char	*buffer;
		short	                 next_load;

		requestq  = ((SYSGEN_BLOCK *) ds->dda_mapreg)->request;
		next_load = (requestq->load + 1) % RQSIZE;

		if (next_load == requestq->unload) { /* any room left in q? */
		    /* no, say we're blocked */
		    return ENOMEM;
		}

		rentry_ptr = requestq->entry + requestq->load;

		switch (dl->type) {
		    case DN_TYPE_DATA:
		    case DN_TYPE_ID:
		    case DN_TYPE_VER:
		    case DN_TYPE_COPY:
#ifdef DDADEBUG
			if (DDADBCH(29, unit))
			    DDALOG(LOG_DEBUG)
				"dda%d: xfr type=%d buf=%d len=%d dest=0x%x\n",
				unit, dl->type, requestq->load,
				dl->len, dl->dest
			    DDAELOG;
#endif DDADEBUG
			buffer   = dlbuf[requestq->load];	

			if (copyin(dl->data, buffer, dl->len)) 
			    return EFAULT;
			/* Set up queue element */
			rentry_ptr->opcode        = dl->type;
			rentry_ptr->count         = dl->len;
			rentry_ptr->BI_address[0] = svtophy(buffer);
			rentry_ptr->BI_address[1] = dl->dest;

			for (checksum = 0, i = dl->len; i--; ) 		
			    checksum += *buffer++;

			rentry_ptr->BI_address[2] = checksum;
			rentry_ptr->BI_address[2] = 0;
			break;

		    case DN_TYPE_XFR:
#ifdef DDADEBUG
			if (DDADBCH(29, unit))
			    DDALOG(LOG_DEBUG)
				"dda%d: execute buf=%d addr=0x%x\n",
				unit, requestq->load, dl->dest
			    DDAELOG;
#endif DDADEBUG

			rentry_ptr->opcode	  = dl->type;
			rentry_ptr->BI_address[1] = dl->dest;
			break;
		}			

		/* TELL 7000 about new entry in request queue */
		requestq->load = next_load;	/* boom! */
		nxv->biic_gpr0 = SYSGEN_DLOAD;	/* interrupt FE */
#ifdef SIMULATION
		MFP_COUNTER_SIM();	/* simulate hardware action */
#endif

		return 0;
	}
	default:
	    return EFAULT;
    }
}

/*

Revision history:

30-Nov-1989	Paul Traina
	First cut for beta release.

*/
