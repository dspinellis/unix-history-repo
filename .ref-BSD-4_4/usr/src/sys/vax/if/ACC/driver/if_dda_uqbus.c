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
/*  File:		if_dda_uqbus.c                                   */
/*			Unibus & Q22bus support routines for dda	 */
/*                                                                       */
/*  Project:		DDN-X.25 Network Interface Driver for ACP 5250   */
/*			and ACP 6250                                     */
/*                                                                       */
/*  revision history at the end of if_dda.c				 */
/*************************************************************************/


#ifndef	SIMULATION
#include "../vaxif/if_uba.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"
#else
#include "vaxif/if_uba.h"
#include "vaxuba/ubareg.h"
#include "vaxuba/ubavar.h"
#endif

PRIVATE u_short	ddastd[] = {0767000, 0}; /* standard addresses */

struct uba_device *ddainfo[NDDA];	/* ptrs to device info */

struct uba_driver ddadriver =	/* device driver info */
{
 ddaprobe,			/* device probe routine */
 0,				/* slave probe routine */
 ddaattach,			/* device attach routine */
 0,				/* "dmago" routine */
 ddastd,			/* device address */
 "dda",				/* device name */
 ddainfo			/* ptr to device info ptrs */
};

/* figure out if this machine is a QBUS machine */
#if defined(MVAX) || defined(VAX3400) || defined(VAX3600) || defined(DS5400)
#define	QBUS
#endif

/*
 * The following definitions declare the structure of the UNIBUS/QBUS
 * mapped pages (which are 512 bytes long).  In previous versions we
 * assumed that the bus pages were the same size as the memory pages,
 * but the mips PMAX cpu uses 4k memory pages but 512 byte bus pages.
 */

#define	BUS_NBPG	512		/* bytes/unibus mapped page */
#define	BUS_PGOFSET	(BUS_NBPG-1)	/* offset into bus page */
#define	BUS_PGSHIFT	9		/* number of bits to shift for page */

/*
 * If we're not running on a PMAX cpu, we don't need to do write buffer
 * pipeline flushes nor do we need to declare I/O space pointers to
 * be volatile.
 */

#ifndef	mips
#define	wbflush()
#define	volatile	register
#endif

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDAPROBE()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/* Purpose:                                                        */
/*                                                                 */
/*  This routine probes the device to obtain the UNIBUS interrupt  */
/*  vector.  Since the ACP is a soft vector device, we obtain an   */
/*  unused vector from the uba structure and return that.  The ACP */
/*  is given the vector and the board is reset.  In order to save  */
/*  the vector in the device info structure, we place it in a      */
/*  static temporary where the attach routine can find it and save */
/*  it in the device info structure.  This is necessary because    */
/*  probe only provides a pointer to the device and we have no     */
/*  idea which unit is being referenced.  This works in 4.2BSD     */
/*  because the attach routine is called immediately after a       */
/*  successful probe.                                              */
/*                                                                 */
/*  Call:          ddaprobe(reg, ui)                               */
/*  Argument:      reg:  caddr_t address in virtual memory of the  */
/*                        control-status register                  */
/*                 ui:   pointer to device data structure, used    */
/*                       for TWG environment                       */
/*  Returns:       length of register structure for ACP device     */
/*  Called by:     network software, part of autoconfiguration on  */
/*                 the VAX, the address of this routine is one of  */
/*                 the fields of the uba_driver structure          */
/*  Calls to:      none                                            */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static int      savevec;	/* static variable for vector */
static int      savefirmrev;	/* firmware rev has same problem as vector */

ddaprobe(reg, ui)
caddr_t         reg;
struct uba_device *ui;		/* TWG VAX/VMS ONLY! */
{
	/* In 2.0 ULTRIX and newer, br and cvec are global (ubavar.h) */
	/* while multinet needs them external */

#if !defined(MULTINET) && ACC_ULTRIX < 20
    register int    br, cvec;	/* r11, r10 value-result */
#endif

#ifdef MULTINET
    extern int      br, cvec;	/* must define external */
#endif

    volatile struct ddaregs *addr = (struct ddaregs *) reg;

#ifdef lint
    br = 0; cvec = br; br = cvec;
#  if ACC_VMS == 00
    ui = ui;
#  endif
#endif

#if ACC_VMS > 00
    cvec = savevec = ui->ui_flags & 0x1f8;	/* flags from config file */
#else
    cvec = savevec = ((uba_hd[numuba].uh_lastiv - 8) & ~7);
    uba_hd[numuba].uh_lastiv = cvec;
#endif

    /* return a vector aligned on a */
    /* QUADWORD boundary */
    /* cvec is the interrupt vector */
    /* address on the UNIBUS */
#ifdef QBUS
    br = 0x17;			/* bus level for MicroVAX */
#else
    br = 0x15;			/* bus level for VAX */
#endif

    /* check that the device is really a 5250/6250 and save away */
    /* the firmware revision level for version dependent processing */
    /* If we just booted, the diagnostics may still be running as we */
    /* probe the device - it's still OK to read the ID and VERSION */
    /* numbers, which come valid within milliseconds after power is */
    /* applied to the board - so say the firmware gurus */

    dda_hasmaint = !(addr->csr & CSR_MAINT);
    savefirmrev = addr->sys_vers;

    switch (addr->sys_id) {
      case 0x8:
	dda_product = "ACP6250";
	break;
      case 0x28:
	dda_product = "ACP5250";
	break;
      case 0x2C:
	dda_product = "ACP5250-W";
	break;
      default:
	if (!dda_hasmaint)
	    return 0;
	savefirmrev = 0xff;
	dda_product = "ACPx250";
	break;
    }

    /* clear communications registers */

    addr->req_flags = 0;	/* I/O request flags            */
    addr->cmp_flags = 0;	/* I/O completion flags         */
    addr->xfr_flags = 0;	/* transfer request/grant flags */
    addr->req_chan = 0;		/* FDX channel number           */
    addr->req_adx = 0;		/* address bits 17-16           */
    addr->req_addr = 0;		/* address bits 15-00           */
    addr->req_cnt = 0;		/* byte count                   */
    addr->req_fcn = 0;		/* I/O function                 */
    addr->req_sbf = 0;		/* I/O subfunction              */
    addr->cmp_chan = 0;		/* FDX channel number           */
    addr->cmp_unused = 0;	/* address bits 17-16           */
    addr->cmp_cnt = 0;		/* byte count                   */
    addr->cmp_stat = 0;		/* I/O status                   */
    addr->cmp_sbst = 0;		/* I/O substatus                */
    addr->xfr_chan = 0;		/* FDX channel number           */
    addr->xfr_adx = 0;		/* address bits 17-16           */
    addr->xfr_addr = 0;		/* address bits 15-00           */
    addr->xfr_cnt = 0;		/* byte count                   */
    addr->sys_stat = 0;		/* system status                */

    addr->sys_vect = cvec >> 2;	/* pass interrupt vector to ACP */
    addr->csr = CSR_RESET;	/* reset the board */
    addr->csr |= CSR_IENB;	/* enable status intr */
    return (sizeof(struct ddaregs));
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
    static long initmsgs[] = { 0, 0, 0, 0, -1, -1, -1, -1, -2 };

    ds->dda_init = DDA_STANDARD;/* init/service flag <- default  */
    ds->dda_vector = savevec;	/* save vector from probe() */
    ds->dda_firmrev = savefirmrev;	/* save firmware rev level */
    ds->dda_net_id = 0;		/* default */
    ds->dda_if.if_unit = ui->ui_unit;	/* set unit number */
    ds->dda_if.if_name = "dda";	/* set device name */
    ds->dda_if.if_mtu = DDAMTU;	/* set max msg size */
    ds->dda_if.if_init = ddainit;	/* set init routine addr */
    ds->dda_if.if_ioctl = ddaioctl;	/* set ioctl routine addr */
    ds->dda_if.if_output = ddaoutput;	/* set output routine addr */
    ds->dda_if.if_reset = ddareset;	/* set reset routine addr */
    ds->dda_if.if_watchdog = ddatimer;	/* set timer routine addr */

    bcopy((char *) initmsgs, (char *) ddamsgs[ui->ui_unit], sizeof(initmsgs));

    if_attach(&ds->dda_if);	/* attach new network device */
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDARESET()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*      Reset of interface after UNIBUS reset.  If interface is on */
/*      specified uba, reset its state.  Free mbufs if there is    */
/*      queued output data.                                        */
/*                                                                 */
/*  Call:              ddareset(unit, uban)                        */
/*  Arguments:         unit:   ACP device unit number              */
/*		       uban:   Unibus Adapter # (do not use!)	   */
/*  Returns:           nothing                                     */
/*  Called by:         network software, address of routine is     */
/*                     defined in dda_if network interface struct  */
/*  Calls to:          DDALOG()                                    */
/*                     IF_DEQUEUE()                                */
/*                     m_freem()                                   */
/*                                                                 */
/*  NOTE: the uban parameter is NOT USED, and may be garbage under */
/*	  some circumnstances.  It is there because the kerenel    */
/*	  expects to pass two parameters to ddareset()		   */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

ddareset(unit, uban)
int             unit, uban;
{
    volatile struct ddaregs *addr;
    register struct uba_device *ui;
    register struct dda_cb *dc;
    register struct dda_softc *ds = &dda_softc[unit];
    register int    lcn;

    ui = (struct uba_device *) ddainfo[unit];
    if (unit >= NDDA || (ui == 0 || ui->ui_alive == 0)) {
        DMESG(unit, 0, (DDALOG(LOG_ERR) "dda%d: ddareset: invalid unit\n", unit DDAELOG));
	return;
    }

    DMESG(unit, 0, (DDALOG(LOG_ERR) "dda%d: reset\n", unit DDAELOG));

    ds->dda_if.if_flags &= ~IFF_UP;
    hist_link_state(unit, ds->dda_state, S_DISABLED);
    ds->dda_state = S_DISABLED;

    addr = (struct ddaregs *) ui->ui_addr;

    addr->cmp_flags = 0;	/* I/O completion flags         */
    addr->xfr_flags = 0;	/* transfer request/grant flags */
    addr->req_chan = 0;		/* FDX channel number           */
    addr->req_adx = 0;		/* address bits 17-16           */
    addr->req_addr = 0;		/* address bits 15-00           */
    addr->req_cnt = 0;		/* byte count                   */
    addr->req_fcn = 0;		/* I/O function                 */
    addr->req_sbf = 0;		/* I/O subfunction              */
    addr->cmp_chan = 0;		/* FDX channel number           */
    addr->cmp_unused = 0;	/* address bits 17-16           */
    addr->cmp_cnt = 0;		/* byte count                   */
    addr->cmp_stat = 0;		/* I/O status                   */
    addr->cmp_sbst = 0;		/* I/O substatus                */
    addr->xfr_chan = 0;		/* FDX channel number           */
    addr->xfr_adx = 0;		/* address bits 17-16           */
    addr->xfr_addr = 0;		/* address bits 15-00           */
    addr->xfr_cnt = 0;		/* byte count                   */
    addr->sys_stat = 0;		/* system status                */
    addr->req_flags = 0;	/* clear handshake flags, mailbox     */

    /* pass interrupt vector to ACP */
    addr->sys_vect = dda_softc[unit].dda_vector >> 2;

    addr->csr = CSR_RESET;	/* reset the board                    */
    dda_softc[unit].dda_flags = 0;	/* clear ACP operational flag  */

    ds->dda_init &= ~DDA_INTCLOCK;	/* reset internal-clocking-set bit */
    nddach[unit] = NDDACH_DEFAULT;	/* reset SVC limit */
    dc = dda_softc[unit].dda_cb;/* flush any queued output data */
    /* LCNLINK */
    for (lcn = 0; lcn <= NDDACH; lcn++) {	/* for all LCN's ... */
	dc->dc_inaddr.s_addr = 0;	/* clear remote internet addr */
	dc->dc_key.key_addr.s_addr = 0;	/* must save for x29 */
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
    addr->csr |= CSR_IENB;
    wbflush();			/* flush write cache pipeline */
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
/*  Calls to:         in_netof() return the network number from    */
/*                               internet address                  */
/*                    spl6()                                       */
/*                    uballoc()                                    */
/*                    ddatimer()                                   */
/*                    btoc()                                       */
/*                    splimp()                                     */
/*                    dda_rrq()                                    */
/*                    splx()                                       */
/*                    if_rtinit()                                  */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

ddainit(unit)
int             unit;
{
    volatile struct ddaregs *addr;
    register struct dda_softc *ds = &dda_softc[unit];
    register struct dda_cb *dc;
    register struct uba_device *ui;

#if ACC_BSD == 42 || ACC_ULTRIX == 12
    struct sockaddr_in *sin;
#endif

    int             lcn, s;

    ui = (struct uba_device *) ddainfo[unit];
    addr = (struct ddaregs *) ui->ui_addr;

#ifdef DDADEBUG
    if (DDADBCH(0, unit)) {
	DDALOG(LOG_DEBUG) "dda%d: ddainit()\n", unit DDAELOG;
    }
#endif DDADEBUG

    /* if we have no internet addr  if device not operational don't init yet */

#if ACC_BSD == 42 || ACC_ULTRIX == 12
    sin = (struct sockaddr_in *) & ds->dda_if.if_addr;
    if ((in_netof(sin->sin_addr) == 0) || ((ds->dda_flags & DDAF_OK) == 0))
#else
    if (ds->dda_if.if_addrlist == (struct ifaddr *) 0 ||
       ((ds->dda_flags & DDAF_OK) == 0))
#endif
	return;

    /* enable intrpt "b" */
    addr->csr |= CSR_IENB;

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


	/* allocate UNIBUS mapping registers, uballoc() */
	/* returns ubinfo, CLBYTES = (CLSIZE * NBPG)    */
	/* rev 2.0 and above: we are now attempting to handle interrupt */
	/* B by resetting the board and bringing it back on line.  So   */
	/* have to deal with the possibility that we already did this.  */
	/* It's more serious to let mapping registers "leak" than mbufs */
	/* (see above) and it's easy to handle too so THIS we do check. */

#ifndef MULTINET
	s = spl6();
#endif
	if (ds->dda_mapreg == 0)	/* don't have one yet, allocate */
	    ds->dda_mapreg = uballoc(ui->ui_ubanum,
				     (char *) &dda_softc[unit], CLBYTES, 0);
#ifndef MULTINET
	splx(s);
#endif

	if (ds->dda_mapreg == 0) {
	    DMESG(unit, 1,
		  (DDALOG(LOG_ERR)
		   "dda%d: failed getting UBA resources for lcn %d\n",
		   unit, lcn DDAELOG));
	    ds->dda_if.if_flags &= ~(IFF_RUNNING | IFF_UP);
	    hist_link_state(unit, ds->dda_state, S_DISABLED);
	    ds->dda_state = S_DISABLED;
	    return;
	}
	/* leave the UNIBUS mapping register */
#ifdef	UBAI_MR	 /* if the macro exists, use it, because it's always right */
	ds->dda_mapreg = UBAI_MR(ds->dda_mapreg);
#else
	ds->dda_mapreg = (ds->dda_mapreg >> BUS_PGSHIFT) & BUS_PGOFSET;
#endif

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

#if ACC_BSD == 42 || ACC_ULTRIX == 12
    if_rtinit(&ds->dda_if, RTF_UP);	/* initialize the routing table */
    /* for network, give address of ifnet structure and RTF_UP   */
    /* which means route is useable */
#endif

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
/*    handshake events to process. Three types of interrupts are   */
/*    processed:  Transfer Request, I/O request acknowledge, and   */
/*    I/O completion.  The interrupting HDX channel and interrupt  */
/*    type are obtained.  If the interrupt is Transfer Request,    */
/*    quit if there is no data, otherwise supply values for the    */
/*    Transfer Request Mailbox.  Note the mapping of the system    */
/*    page table entry, pte, and the UNIBUS Mapping Register,      */
/*    ddamapreg.  The mapping algorithm is different for the TWG   */
/*    Eunice/IPTCP environments because Sysmap is not present as   */
/*    it is in UNIX 4.2 BSD.  If interrupt is an I/O request       */
/*    acknowledge the next I/O request is passed to the ACP        */
/*    device.  If the interrupt is an I/O completion, check for    */
/*    errors, if ok process according to whether supervisory or    */
/*    data channel.                                                */
/*                                                                 */
/*  Call:              ddainta(unit)                               */
/*  Arguments:         unit:  ACP device unit number               */
/*  Returns:           nothing                                     */
/*  Called by:         network software, address of this routine   */
/*                     is defined in af_inet network interface     */
/*                     data structure                              */
/*  Calls to:          DDALOG()                                    */
/*                     btop() - byte to page w/o rounding ( >> 9 ) */
/*                     btoc()                                      */
/*                     start_chn()                                 */
/*                     dda_data()                                  */
/*                     dda_supr()                                  */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifdef WINS
extern struct pte *mmg$gl_sptbase;	/* base of system page table */
extern struct ADP *ioc$gl_adplist;	/* Adapter control blocks */
#endif WINS

#ifdef	MULTINET
extern struct pte *vms_sptbase;
#define mmg$gl_sptbase vms_sptbase
#endif	MULTINET

ddainta(unit)
int             unit;
{
    volatile struct ddaregs *addr = (struct ddaregs *) ddainfo[unit]->ui_addr;
    register struct dda_softc *ds = &dda_softc[unit];
    register struct hdx_chan *hc;
    register struct uba_device *ui;
    register struct uba_hd *uh = &uba_hd[ddainfo[unit]->ui_ubanum];
    struct pte     *pte;	/* page table entry pointers */
    int             chan, cc, subcc, cnt, npf, uadr, pgoff;
    register int    page_to_map;

#if ACC_VMS > 00
    int             temp;
    register struct ADP *adp;	/* TWG or TGV, UNIBUS adapter */
    int            *io;
#else
    struct pte     *io;		/* page table entry pointers */
#endif

#ifdef DDADEBUG
    if (DDADBCH(5, unit)) {
	DDALOG(LOG_DEBUG) "dda%d: ddainta()\n", unit DDAELOG;
    }
#endif DDADEBUG

    ui = (struct uba_device *) ddainfo[unit];
    addr = (struct ddaregs *) ui->ui_addr;

/***********************************************************************/
/*   Check Transfer Request Mailbox  (note VAXVMS dependent code)      */
/***********************************************************************/

    if (addr->xfr_flags & FLAGS_RDY) {	/* Transfer Request Mailbox */
#ifdef DDA_MSGQ
	dda_mqstr("(tx)");
#endif

	/*
	 * Get logical channel information. 
	 */
	if ((chan = addr->xfr_chan) > nddach[unit]) {
	    DMESG(unit, 4,
	       (DDALOG(LOG_ERR) "dda%d: unknown transfer channel, lcn=%d\n",
		unit, chan DDAELOG));
	    return;
	}
	if (addr->xfr_flags & FLAGS_DIR)
	    hc = &(ds->dda_cb[chan].dc_wchan);	/* write channel */
	else
	    hc = &(ds->dda_cb[chan].dc_rchan);	/* read channel */

	cnt = addr->xfr_cnt;	/* Transfer Request byte count */

	if (hc->hc_mbuf == 0) {
	    DMESG(unit, 5,
	       (DDALOG(LOG_ERR) "dda%d: transfer request lcn %d: no mbuf\n",
		unit, chan DDAELOG));
	    addr->xfr_cnt = 0;	/* Abort the transfer */
	    return;
	}
	pgoff    = (int) hc->hc_addr & PGOFSET;

#if ACC_VMS > 00
	/* System page table entry for VMS */
	temp = (((int) hc->hc_addr & (~PG_V & ~(NBPG - 1))) / NBPG);
	pte = (struct pte *) ((int) mmg$gl_sptbase + (4 * temp));

	/*
	 * Find the adapter 
	 */
#  ifdef WINS
	adp = ioc$gl_adplist;
	while (adp) {
	    if ((adp->adp$w_adptype == AT$_UBA) &&
		(adp->adp$b_number == ui->ui_ubanum))
		break;
	    adp = adp->adp$l_link;
	}
	if (!adp)
	    return;		/* adp not found */
	io = (int *) ((int) adp->adp$l_csr + 0x800 + (4 * (ds->dda_mapreg)));
#  endif
#  ifdef MULTINET
	adp = (struct ADP *) 0;
	while (1) {
	    char          **vector;
	    int             number, csr;

	    /*
	     * Get the next adapter 
	     */
	    adp = (struct ADP *) vms_next_uba(adp, &vector, &number, &csr, 0);
	    if (!adp)
		return;

	    /*
	     * Is this the one we are looking for? 
	     */
	    if (number == ui->ui_ubanum) { /* yes */
		io = (int *) ((int) csr + 0x800 + (4 * (ds->dda_mapreg)));
		break;
	    }
	}
#  endif
#else	/* not VMS */
#  ifdef kvtopte	/* if the macro exists, use it for compatibility */
			/* this will be in 4.3tahoe and newer */
	pte = kvtopte(hc->hc_addr);
	io  = &uh->uh_mr[ds->dda_mapreg];
#  else
#    ifdef svtopte	/* under ultrix, kvtopte is svtopte */
			/* but the map registers are in the right place */
	pte = svtopte(hc->hc_addr);
	io  = &uh->uh_uba->uba_map[ds->dda_mapreg];
#    else  /* not 4.3tahoe or a new ultrix flavor */
	pte = &Sysmap[btop((int) hc->hc_addr & ~PG_V)];
	io = &uh->uh_uba->uba_map[ds->dda_mapreg];
#    endif /* not 4.3tahoe or a new ultrix flavor */
#  endif
#endif

	/* pte now points to system memory page table entry */
	/* io points to UNIBUS/QBUS mapping register */

#ifndef SIMULATION
	/* calculate number of page frames */
	npf = (hc->hc_cnt + BUS_PGOFSET) >> BUS_PGSHIFT;

	/* page_to_map is all but nine bits of the physical address
	   in other words, the 512-byte page the bus wants us to map */

	page_to_map = ((pte++)->pg_pfnum << (PGSHIFT - BUS_PGSHIFT)) |
		      (pgoff >> BUS_PGSHIFT);

	while (npf--) {
	    /* UBAMR_MRV = mapping register valid */
	    *(int *) io++ = page_to_map++ | UBAMR_MRV;

	    /* when page_to_map crosses a memory page boundary, we
	       need to get the new physical address from the next pte
	       (on a vax, this happens every time, on a mips, only when
	       the lower 3 bits go from 111 to 000) */	

	   if (page_to_map & (PGOFSET >> BUS_PGSHIFT) == 0)
		/* we know page offset is now going to be zero, so don't
		   bother adding it in (like we had to do above) */
		page_to_map = (pte++)->pg_pfnum << (PGSHIFT - BUS_PGSHIFT);
	}
	*(int *) io++ = 0;	/* invalidate last UMR */

	/* supply values for Transfer Request Mailbox */

	addr->xfr_chan = chan;	/* data path number */

	uadr = (ds->dda_mapreg << BUS_PGSHIFT) + (pgoff & BUS_PGOFSET);

#ifdef	QBUS
	addr->xfr_adx = (uadr & 0x3f0000) >> 16; /* ext address bits 23-16  */
#else	QBUS
	addr->xfr_adx = (uadr &  0x30000) >> 16; /* ext address bits 18-16  */
#endif	QBUS
#else	SIMULATION
	uadr = (int) hc->hc_addr;	/* just use the direct address */
	addr->xfr_adx = (uadr & 0xFF0000) >> 16; /* ext ms address bits */
#endif	SIMULATION

	addr->xfr_addr = uadr & 0xffff;	/* grnt transfer address bits 15-0 */

	hc->hc_cnt -= cnt;
	hc->hc_addr += cnt;

	addr->xfr_flags = (addr->xfr_flags & ~FLAGS_RDY) | FLAGS_DON;
	addr->csr |= CSR_INTRA;	/* enable interrupt "a" */
    }

    /***********************************************************************/
    /*   Check I/O Request Mailbox                                         */
    /***********************************************************************/

    if (addr->req_flags & FLAGS_DON) {	/* I/O Request Mailbox */
#ifdef DDA_MSGQ
	dda_mqstr("(rx)");
#endif

	addr->req_flags &= ~FLAGS_DON;

	/* dequeue old request by copying link to queue head */
	/* and start next I/O request if queue is not empty */

	if (ds->dda_sioq.sq_head = ds->dda_sioq.sq_head->hc_next)
	    start_chn(ds);
    }
    /***********************************************************************/
    /*   Check I/O Completion Mailbox                                      */
    /***********************************************************************/

    if (addr->cmp_flags & FLAGS_RDY) {	/* I/O Completion Mailbox */
#ifdef DDA_MSGQ
	dda_mqstr("(cx)");
#endif

	/*
	 * Get logical channel information. 
	 */
	if ((chan = addr->cmp_chan) > nddach[unit]) {
	    DMESG(unit, 6,
	     (DDALOG(LOG_ERR) "dda%d: unknown completion channel, lcn=%d\n",
	      unit, chan DDAELOG));
	    return;
	}
	if (addr->cmp_flags & FLAGS_DIR)
	    hc = &(ds->dda_cb[chan].dc_wchan);	/* write channel */
	else
	    hc = &(ds->dda_cb[chan].dc_rchan);	/* read channel */

	cc = addr->cmp_stat;	/* Mailbox I/O completion status */
	subcc = addr->cmp_sbst;	/* Mailbox I/O completion substatus */
	cnt = addr->cmp_cnt;	/* Mailbox I/O completion byte count */

#ifdef	mips
	/* if it was a read completion, invalidate the mbuf data portion */
	/* NOTE: the direction check has been commented out because it 
	   seemed like we got it backwards and forgot to check it later.
	   This is now believed to be correct, but is left commented out until
	   a beta site checks it for us. */
	/* if (!(hc->hc_chan & 1)) */ {
		int phys_begin = svtophy(mtod(hc->hc_cur, u_char *));
		clean_dcache(PHYS_TO_K0(phys_begin), cnt);
	}
#endif

	switch (cc) { 	/* check for unsuccessful I/O completion status */
	  case DDAIOCABT:	/* probably VCN flush */
	    if (LOG_ABT)
		DDALOG(LOG_ERR) "dda%d: abort completed on chan %d\n",
		    unit, hc->hc_chan DDAELOG;
	    break;

	  case DDAIOCERR:
	    DMESG(unit, 7,
		  (DDALOG(LOG_ERR) "dda%d: program error ", unit DDAELOG));
	    goto daterr;

	  case DDAIOCOVR:
	    DMESG(unit, 8,
		  (DDALOG(LOG_ERR) "dda%d: overrun error ", unit DDAELOG));
	    goto daterr;

	  case DDAIOCUBE:
	    DMESG(unit, 9,
	      (DDALOG(LOG_ERR) "dda%d: transfer count = 0 ", unit DDAELOG));
	    goto daterr;

	  case DDAIODMAE:
	    DMESG(unit, 10,
		  (DDALOG(LOG_ERR) "dda%d: DMA completion error (%x) ",
		   unit, addr->cmp_sbst DDAELOG));
	    goto daterr;

	  case DDAIOLCOL:
	    DMESG(unit, 11,
		(DDALOG(LOG_ERR) "dda%d: listen collision ", unit DDAELOG));
	    goto daterr;

	  case DDAIOFUNC:
	    DMESG(unit, 12,
		(DDALOG(LOG_ERR) "dda%d: invalid function ", unit DDAELOG));
	    goto daterr;

	  case DDAIODPN:
	    DMESG(unit, 13,
		  (DDALOG(LOG_ERR) "dda%d: invalid dpn ", unit DDAELOG));
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

	/*
	 * Ack the interrupt.  Fix the Mailbox Ready and Done bits:  set DON
	 * bits, and clear RDY bits so mailbox may be reused. 
	 */
	addr->cmp_flags = (addr->cmp_flags & ~FLAGS_RDY) | FLAGS_DON;
	addr->csr |= CSR_INTRA;	/* enable interrupt "a" */
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
int             unit;
{
    volatile struct ddaregs *addr;
    struct uba_device *ui;
    register struct dda_softc *ds = &dda_softc[unit];
    register struct dda_cb *dc;
    register u_char diag_stat;
    register int    lcn;
    register struct mbuf *m;
    register struct hdx_chan *hc;

    ui = (struct uba_device *) ddainfo[unit];
    addr = (struct ddaregs *) ui->ui_addr;
#ifdef DDADEBUG
    if (DDADBCH(6, unit)) {
	DDALOG(LOG_DEBUG) "dda%d: ddaintb()\n", unit DDAELOG;
    }
#endif DDADEBUG

    if (ds->dda_flags & DDAF_OK) {

	/*
	 * Change to rev2.0 and above driver: attempt to handle an unexpected
	 * B interrupt sanely by reinitializing the world. We turn off all
	 * the flavors of UP and OK flags and call ddareset().  This will
	 * restart the diagnostics and enable interrupt B.  After the
	 * diagnostics run, we'll come back here to the B interrupt handler
	 * and with DDAF_OK off will enter the "else" below.  With luck the
	 * board will report success and we'll call ddainit() to bring the
	 * link back on line. 
	 */

	DMESG(unit, 15,
	      (DDALOG(LOG_ERR) "dda%d: asynchronous restart, status = %d\n",
	       unit, addr->sys_stat DDAELOG));
	ds->dda_flags = 0;
	ds->dda_if.if_flags &= ~(IFF_RUNNING | IFF_UP);
	hist_link_state(unit, ds->dda_state, S_DISABLED);
	ds->dda_state = S_DISABLED;
	ddareset(unit, ui->ui_ubanum);
    } else {
	diag_stat = addr->sys_stat;
	if ((diag_stat & DDASTAT_ERR) != 0) {
	    DMESG(unit, 16,
		  (DDALOG(LOG_ERR) "dda%d: Diagnostic failure = %d\n",
		   unit, addr->sys_stat DDAELOG));
	    addr->csr = 0;
	} else if (diag_stat == DDASTAT_NMC) {
	    DMESG(unit, 17,
		  (DDALOG(LOG_ERR) "dda%d: No Microcode Present!\n", unit DDAELOG));
	    addr->csr = 0;
	} else if (diag_stat == DDASTAT_OK) {

/*
 * For each lcn, clear the output queue and free hdx read and write mbufs
 */
	    for (lcn = 0; lcn <= NDDACH; lcn++) {	/* for all LCNs */
		dc = &dda_softc[unit].dda_cb[lcn];
		while (dc->dc_oq.ifq_len) {
		    IF_DEQUEUE(&dc->dc_oq, m);
		    if (m)
			m_freem(m);
		}

		hc = &dc->dc_rchan;
		if (hc->hc_mbuf) {
		    m_freem(hc->hc_mbuf);
		    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
		}
		hc = &dc->dc_wchan;
		if (hc->hc_mbuf) {
		    m_freem(hc->hc_mbuf);
		    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
		}
	    }
	    ds->dda_flags |= DDAF_OK;
	    addr->csr |= (CSR_IENA | CSR_DMAEN);
	    ddainit(unit);
	}
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


    /* set channel info */
    s = splimp();
    m = hc->hc_curr;
    hc->hc_addr = mtod(m, u_char *);	/* point to data in mbuf */
    hc->hc_cnt  = m->m_len;		/* byte count            */
    splx(s);

    /* set Transfer Request Write, mark DDAEOS if end of stream */
    /* note: non-unibus products ignore the address invalid (DDAXFR) bit */

    if (abt)
	hc->hc_func = DDAABT;
    else if (m->m_next == 0)
	hc->hc_func = DDAWRT + DDAEOS + DDAXFR;
    else
	hc->hc_func = DDAWRT + DDASTR + DDAXFR;

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

#ifdef DDADEBUG
    if (DDADBCH(16, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: dda_rrq()\n", ds->dda_if.if_unit DDAELOG;
    }
#endif DDADEBUG

    MGET(m, M_DONTWAIT, MT_DATA);
    if (m == 0) {
	DMESG(ds->dda_if.if_unit, 25,
	      (DDALOG(LOG_ERR) "dda%d:  couldn't get buffer for read\n",
	       ds->dda_if.if_unit DDAELOG));
	return;
    }
    m->m_len = 0;		/* new mbuf, doesn't contain data yet */

    /* hc_mbuf set to zero during initialization */

    s = splimp();
    if (hc->hc_mbuf == 0) {
	hc->hc_mbuf = m;
	hc->hc_curr = m;
    } else {
	hc->hc_curr->m_next = m;
	hc->hc_curr = m;
	m->m_next = 0;
    }
    splx(s);

    hc->hc_func = DDARDB + DDASTR + DDAXFR;
    hc->hc_sbfc = 0;

    /* hc_cnt determines size of read request, addr->req_cnt */
    /* MLEN is 112 bytes, the data portion of a small mbuf   */

    hc->hc_cnt = MLEN;		/* just got the mbuf */

    hc->hc_addr = mtod(m, u_char *);	/* point to mbuf data */

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
    volatile struct ddaregs *addr;
    register struct hdx_chan *hc;
    register int    s;
    struct uba_device *ui;

    ui = (struct uba_device *) (ddainfo[ds->dda_if.if_unit]);
    addr = (struct ddaregs *) ui->ui_addr;

#ifdef DDADEBUG
    if (DDADBCH(17, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: start_chn()\n", ds->dda_if.if_unit DDAELOG;
    }
#endif DDADEBUG

/*
 *  Scan sioq for invalid writes
 */

    s = splimp();

    hc = (struct hdx_chan *) (ds->dda_sioq.sq_head);
    if (hc && ((addr->req_flags & FLAGS_RDY) == 0)) {
	for (; hc; hc = hc->hc_next) {	/* scan sioq */
	    if ((hc->hc_chan & 0x01) && (hc->hc_chan != 1)
		&& (hc->hc_inv & INVALID_MBUF)) {
		if (ds->dda_cb[hc->hc_chan >> 1].dc_flags & DC_OBUSY)
		    goto send;	/* send an abort */
		else {
		    if (hc->hc_mbuf) {	/* free pending request */
			m_freem(hc->hc_mbuf);
			hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
			hc->hc_inv &= ~INVALID_MBUF;
			ds->dda_sioq.sq_head = ds->dda_sioq.sq_head->hc_next;
		    }
/*
 * Restart Output
 */

		    /*
		     * not needed ds->dda_cb[hc->hc_chan>>1].dc_flags &=
		     * ~DC_OBUSY; 
		     */

		    /*
		     * this should be changed to drop ipl before calling
		     * dda_start 
		     */
		    dda_start(ds, &ds->dda_cb[hc->hc_chan >> 1]);
		}
	    } else {		/* Read or Write request is valid */

		/*
		 * Set up comm regs. 
		 */


	send:
		addr->req_chan = hc->hc_chan >> 1;
		addr->req_cnt = hc->hc_cnt;
		addr->req_fcn = hc->hc_func;
		addr->req_sbf = hc->hc_sbfc;

		if (hc->hc_chan & 1) {	/* write */
#ifdef DDADEBUG
		    struct dda_cb  *dc = &ds->dda_cb[hc->hc_chan >> 1];

		    if (DDADBCH(17, ds->dda_if.if_unit))
			DDALOG(LOG_DEBUG)
			    "dda%d: start_chn: WRITE on lcn %d func %x\n",
			    ds->dda_if.if_unit, hc->hc_chan >> 1,
			    hc->hc_func DDAELOG;

		    if (dc->dc_lcn)	/* don't start timer on lcn 0 */
			dc->dc_out_t = TMO_RESTART;	/* Wait 90 sec for
							 * completion */
#endif
		    addr->req_flags = FLAGS_RDY | FLAGS_DIR;
		} else
		    addr->req_flags = FLAGS_RDY;
#ifdef DDADEBUG
		if (DDADBCH(28, ds->dda_if.if_unit)) {
		    if (hc->hc_func == DDAABT)
			DDALOG(LOG_DEBUG) "dda%d: start_chn: aborting chan %d\n",
			    ds->dda_if.if_unit, hc->hc_chan DDAELOG;
		}
#  ifdef DDA_MSGQ
		dda_mqstr("(sc ");
		dda_mqnum(hc->hc_chan >> 1, MQHEX);
		dda_mqstr(" ");
		dda_mqnum(addr->req_flags, MQHEX);
		dda_mqstr(" ");
		dda_mqnum(hc->hc_cnt, MQHEX);
		dda_mqstr(" ");
		dda_mqnum(hc->hc_func, MQHEX);
		dda_mqstr(" ");
		dda_mqstr(")");
#  endif DDA_MSGQ
#endif DDADEBUG
		addr->csr |= CSR_INTRA;	/* interrupt FE */
		splx(s);
		return;
	    }
	}
    }
    splx(s);
    return (1);			/* no valid requests found */
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      BUFRESET()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*      Reset of interface after UNIBUS reset.  If interface is on */
/*      specified uba, reset its state.  Free mbufs if there is    */
/*      queued output data.                                        */
/*                                                                 */
/*  Call:              bufreset(unit)                              */
/*  Arguments:         unit:   ACP device unit number              */
/*  Returns:           nothing                                     */
/*  Called by:         network software, address of routine is     */
/*                     defined in dda_if network interface struct  */
/*  Calls to:          DDALOG()                                    */
/*                     IF_DEQUEUE()                                */
/*                     m_freem()                                   */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
bufreset(unit)
int             unit;
{
    volatile struct ddaregs *addr;
    register struct uba_device *ui;
    register struct dda_cb *dc;
    register struct dda_softc *ds = &dda_softc[unit];
    register int    lcn;

    ui = (struct uba_device *) ddainfo[unit];
    if (unit >= NDDA || (ui == 0 || ui->ui_alive == 0))
	return;

    DMESG(unit, 38, (DDALOG(LOG_ERR) "dda%d: buffer size reset\n", unit DDAELOG));

    ds->dda_if.if_flags &= ~IFF_UP;
    hist_link_state(unit, ds->dda_state, S_DISABLED);
    ds->dda_state = S_DISABLED;

    addr = (struct ddaregs *) ui->ui_addr;

    addr->cmp_flags = 0;	/* I/O completion flags         */
    addr->xfr_flags = 0;	/* transfer request/grant flags */
    addr->req_chan = 0;		/* FDX channel number           */
    addr->req_adx = 0;		/* address bits 17-16           */
    addr->req_addr = 0;		/* address bits 15-00           */
    addr->req_cnt = 0;		/* byte count                   */
    addr->req_fcn = 0;		/* I/O function                 */
    addr->req_sbf = 0;		/* I/O subfunction              */
    addr->cmp_chan = 0;		/* FDX channel number           */
    addr->cmp_unused = 0;	/* address bits 17-16           */
    addr->cmp_cnt = 0;		/* byte count                   */
    addr->cmp_stat = 0;		/* I/O status                   */
    addr->cmp_sbst = 0;		/* I/O substatus                */
    addr->xfr_chan = 0;		/* FDX channel number           */
    addr->xfr_adx = 0;		/* address bits 17-16           */
    addr->xfr_addr = 0;		/* address bits 15-00           */
    addr->xfr_cnt = 0;		/* byte count                   */
    addr->sys_stat = 0;		/* system status                */
    addr->req_flags = 0;	/* clear handshake flags, mailbox     */

    /* pass interrupt vector to ACP */
    addr->sys_vect = dda_softc[unit].dda_vector >> 2;

    dda_softc[unit].dda_flags = 0;	/* clear ACP operational flag  */

    ds->dda_init &= ~DDA_INTCLOCK;	/* reset internal-clocking-set bit */
    nddach[unit] = NDDACH_DEFAULT;	/* reset SVC limit */
    /* LCNLINK */
    dc = dda_softc[unit].dda_cb;/* flush any queued output data */
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
    addr->csr |= CSR_IENB;
    wbflush();				/* flush write pipeline cache */
}

/*
 * Disable all interrrupts and forget about board
 */

PRIVATE void
dda_disable(unit)
{
    volatile struct ddaregs *addr;
    register struct uba_device *ui;

    ui = (struct uba_device *) ddainfo[unit];
    addr = (struct ddaregs *) (ui->ui_addr);
    addr->csr = 0;
    wbflush();				/* flush write pipeline cache */
}

/*

Revision History:

13-Jul-1989: PST	Used PRIVATE convention on functions
01-Aug-1989: PST	Added initialization of ddamsgs in attach routine.
01-Sep-1989: PST	Ignore uban parameter to ddareset.
14-Nov-1989: PST	Added support for MIPS PMAX architecture and
			fixed a QBUS bug (we were not using all 22 addr bits).
			Moved rrq and wrq routines here.
30-Nov-1989: PST	Moved unibus defines here.
01-Dec-1989: PST	Fixed typos.
*/
