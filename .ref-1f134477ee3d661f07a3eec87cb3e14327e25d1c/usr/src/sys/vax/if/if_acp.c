/*-
 *	@(#)if_acp.c	7.3 (Berkeley) %G%
 */

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
/*  	Copyright (c) 1985 by Advanced Computer Communications           */
/*  	720 Santa Barbara Street, Santa Barbara, California  93101       */
/*  	(805) 963-9431                                                   */
/*                                                                       */
/*                                                                       */
/*  File:		if_acp.c                                         */
/*                                                                       */
/*  Author:		Arthur Berggreen                                 */
/*                                                                       */
/*  Project:		ACP6100 (UPB with HDLC firmware)                 */
/*                                                                       */
/*  Function:		4.2BSD UNIX Network Interface Driver for ACP6100 */
/*                                                                       */
/*  Components:		if_acp.c, if_acpreg.h, if_acpvar.h               */
/*                                                                       */
/*  Revision History:                                                    */
/*                                                                       */
/*    16-AUG-1985  Clare Russ:  add fileheader and comments              */
/*    24-SEP-1985  Clare Russ:  modify for socket ioctl user interface   */
/*    06-NOV-1985  Clare Russ:  modify for socket ioctl under TWG        */
/*    11-NOV-1985  Clare Russ:  Add a call to acpreset() in acpioctl()   */
/*         before processing socket ioctl to clear COMREGs.  In the      */
/*         acpinit() routine, avoid redundant allocation of UMRs by      */
/*         doing so only if the front end is not RUNNING.                */
/*    14-NOV-1985  Clare Russ:  Trace if_ubainit failure:  happens with  */
/*         TWG, not 4.2BSD.                                              */
/*    21-NOV-1985  Clare Russ:  Modify for compliance with the new       */
/*         Control Interface (CIF) and Access Path Allocation Protocol   */
/*         (APAP).  The CIF requires that Control Interface Messages     */
/*         (CIMs) are exchanged between the host and front end in        */
/*         command/response pairs.  The APAP requires that the control   */
/*         and data paths be established (via exchange of CIMs between   */
/*         the host and the front end) prior to use.                     */
/*    26-NOV-1985  Clare Russ:  Add ability to bring down line in        */
/*         response to 'acpconfig' command.                              */
/*    27-NOV-1985  Clare Russ:  Add ability to specify DTE or DCE mode   */
/*         in response to 'acpconfig' command.                           */
/*    02-DEC-1985  Clare Russ:  Add ability to set baud rate (external   */
/*         clock) or set internal clock.                                 */
/*    14-JAN-1986  Clare Russ:  Add acpinit call to acpioctl under       */
/*         SIOCSIFADDR processing                                        */
/*    21-JAN-1986  Clare Russ:  Flush pending I/O in acpreset, free the  */
/*         mbufs                                                         */
/*    30-MAY-1986  Clare Russ:  Update MPCP host request subfunction     */
/*         values, fix baud rate values in baud_rate[], change default   */
/*         clock source from internal to external (in ssp_msg[])         */
/*    24-JUL-1986  Clare Russ:  In supr_msg() print out RSF field when   */
/*         path allocation or deallocation fails                         */
/*    23-FEB-1987  Jeff Berkowitz: port to 4.3BSD by adding #ifdefs for  */
/*	   new interface address formats, trapping 0 length mbufs, etc.  */
/*    08-JAN-1988  Brad Engstrom:  port to ULTRIX 2.0 by using the       */
/*	   UBAUVII (ultrix 2.0) and MVAX (microvax) defines to handle    */
/*         special cases.  These cases are:                              */
/*         1) not declaring br, cvec as value-result in the probe routine*/
/*         2) using 0x17 as the ipl for a microvax                       */
/*         3) in all other cases the ULTRIX drivers behaves like a 4.3   */
/*            driver.                                                    */
/*                                                                       */
/*  Usage Notes:                                                         */
/*                                                                       */
/*    device acp0 at uba0 csr 016700 flags 0 vector acpinta acpintb      */
/*                                                                       */
/*         The 'flags' value is nonzero in the configuration file        */
/*         for TWG, and may be left as zero in the configuration         */
/*         file for UNIX 4.2 BSD.                                        */
/*                                                                       */
/*  Application Notes:	                                                 */
/*                                                                       */
/*    Refer to the Installation Instructions and the UNIX Programmer's   */
/*    Manual page which are on the driver distribution medium.           */
/*                                                                       */
/*                                                                       */
/*************************************************************************/


/* #define ACPDEBUG 1	/* define for debug printf statements */

#ifdef ACPDEBUG
int acp_debug = 0;	/* acp_debug is 1-8 for increasing verbosity */
#endif ACPDEBUG


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                                   %%*/
/*%%                          INCLUDE FILES                            %%*/
/*%%                                                                   %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/* The number of ACP 6100s in the system is defined in the configuration */
/* file in /sys/conf.  When 'config' is run, the file acp.h is created   */
/* with the definition of NACP, the number of ACP 6100s in the system.   */

#include "acp.h"
#if NACP > 0
#include "../include/pte.h"

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/mbuf.h"
#include "sys/buf.h"
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/vmmac.h"
#include "sys/errno.h"
#include "sys/time.h"
#include "sys/kernel.h"
#include "sys/ioctl.h"

#include "net/if.h"
#include "net/netisr.h"
#include "net/route.h"
#include "netinet/in.h"
#include "netinet/in_systm.h"
#ifndef FOURTWO
# include "netinet/in_var.h"
#endif
#include "netinet/ip.h"
#include "netinet/ip_var.h"

#include "../include/cpu.h"
#include "../include/mtpr.h"
#include "../if/if_acpreg.h"
#include "../if/if_acpvar.h"
#include "../if/if_uba.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                                   %%*/
/*%%                        GLOBAL FUNCTIONS                           %%*/
/*%%                                                                   %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

int acpprobe();
int acpattach();
int acpreset();
int acpinit();
int acpoutput();
int acptimer();		/* currently no timer routine exists   */
int acpioctl();
int acpinta();
int acpintb();
int acpstart();

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                                   %%*/
/*%%                         LOCAL FUNCTIONS                           %%*/
/*%%                                                                   %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void acp_alloc();	/* allocate control and data paths       */
static void acp_init();		/* send Set System Parameters Message    */
static void acp_iorq();
static void start_chn();
static void acp_data();
static void acp_response();	/* send CIM response to the front end    */
static void acp_supr();
static void supr_msg();
static void send_supr();

#ifdef ACPDEBUG
static void prt_addr();
static void prt_bytes();
#endif ACPDEBUG

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                                   %%*/
/*%%                         LOCAL VARIABLES                           %%*/
/*%%                                                                   %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

struct	uba_device *acpinfo[NACP];	/* ptrs to device info           */
u_short	acpstd[] = { 0767000, 0 };	/* standard UNIBUS CSR addresses */
struct	uba_driver acpdriver =		/* device driver info            */
  {
    acpprobe,				/* device probe routine */
    0,					/* slave probe routine */
    acpattach,				/* device attach routine */
    0,					/* "dmago" routine */
    acpstd,				/* device address */
    "acp",				/* device name */
    acpinfo				/* ptr to device info ptrs */
  };

/* The alloc_msg array contains the Command Interface Message (CIM)    */
/* for path allocation.  There are 12 bytes of header followed by 6    */
/* bytes of command information                                        */

static u_char alloc_msg[] =
  {
    0x00,				/* reserved, must be zero      */
    FAC_ALLOC,				/* front end ALLOC facility    */
    0x00,				/* reserved, must be zero      */
    CMD_ALLOC,				/* allocate path command       */
    0x0f, 0x0a, 0x0c, 0x0e,		/* Command ID (CID)            */
    0x00, 0x00, 0x00, 0x00,		/* Response/Status Field (RSF) */
    0x00, ACP_SUPR,			/* Data Path Number (DPN)      */
    0x00, FAC_HDLC,			/* front end HDLC facility     */
    0x00, TYPE_CNTL			/* type of path:  control      */
  };


/* The dealloc_msg array contains the Command Interface Message (CIM)  */
/* for path deallocation.  There are 12 bytes of header followed by 2  */
/* bytes of command information                                        */

static u_char dealloc_msg[] =
  {
    0x00,				/* reserved, must be zero      */
    FAC_ALLOC,				/* front end ALLOC facility    */
    0x00,				/* reserved, must be zero      */
    CMD_DEALLOC,			/* allocate path command       */
    0x0c, 0x0a, 0x0f, 0x0e,		/* Command ID (CID)            */
    0x00, 0x00, 0x00, 0x00,		/* Response/Status Field (RSF) */
    0x00, ACP_SUPR,			/* Data Path Number (DPN)      */
  };


/* Table of baud rate values and the associated parameter for the Set  */
/* System Parameters message, ssp_msg.  The second byte is nonzero for */
/* valid baud rate divisors.                                           */

struct	baud	{
	char	b_value;
	u_char	parameter1;	/* first byte of baud rate setting  */
	u_char	parameter2;	/* second byte of baud rate setting */
}	baud_rate[] =	{
	{ 1,	0x00, 	0x02 },
	{ 2,	0x00, 	0x03 },
	{ 3,	0x00, 	0x04 },
	{ 4,	0x00, 	0x08 },
	{ 5,	0x00, 	0x10 },
	{ 6,	0x00, 	0x28 },
	{ 7,	0x00, 	0x3e },
	{ 8,	0x00, 	0x47 },
	{ 9,	0x00, 	0x85 },
	{ 10,	0x00, 	0xd0 },
	{ 11,	0x01, 	0xa1 },
	{ 12,	0x03, 	0x41 },
	{ 13,	0x06,	0x83 },
	{ 14,	0x0d, 	0x05 },
	{ 0,	0,	0 },
};

/* The ssp_msg array contains the Command Interface Message (CIM) for  */
/* Setting HDLC System Paramters.  There are 12 bytes of header        */
/* followed by the line number and parameter modification commands     */
/* (PMCs).  The driver sends this CIM to the front end when kicked by  */
/* the acpconfig program (via socket ioctl).  In future versions, the  */
/* CIM won't be here in the driver, it will be passed to the driver.   */

u_char ssp_msg[] =
  {
    0x00,				/* reserved, must be zero      */
    FAC_HDLC,				/* front end HDLC facility     */
    0x00,				/* reserved, must be zero      */
    CMD_SSP,				/* set HDCL system parameters  */
    0x0b, 0x0e, 0x0e, 0x0f,		/* Command ID (CID)            */
    0x00, 0x00, 0x00, 0x00,		/* Response/Status Field (RSF) */
    0x00, 0x00,				/* HDLC Line Number (0)        */
    LINK_DISABLE,			/* link disable                */
    LINK_LOOPBACK,			/* loopback mode               */
    LOOP_EXTERNAL,			/*   external loopback         */
    DCE_OR_DTE,				/* specify DTE or DCE mode     */
    DTE_MODE,				/*   DTE mode                  */
    BAUD_CNTL,				/* baud rate divisor           */
    0x00,				/*                             */
    0x03,				/*  3 = 1.333 Mb/sec           */
    IDLE_POLL,				/* idle poll selection         */
    0x01,				/*  1 = on                     */ 
    CLOCK_CNTL,				/* xmit clock selection        */
    0x00,				/*  0 = external source        */
    LINK_ENABLE				/* link enable                 */
  };

/* The response_msg array contains the Command Interface Message (CIM) */
/* response to be sent back to the front end in response to a CIM      */
/* command for Frame Level Status from the front end.   The front end  */
/* sends the Frame Level Status CIM command to the host when the frame */
/* level status changes from up to down or vice versa.  In keeping     */
/* with the philosophy with CIMs, they are always exchanged in command */
/* response pairs.                                                     */

static u_char response_msg[] =
  {
    0x00,				/* reserved, must be zero      */
    FAC_HDLC,				/* front end HDLC facility     */
    0x00,				/* reserved, must be zero      */
    RSP_FLUP,				/* Frame Level Status          */
    0x00, 0x00, 0x00, 0x00,		/* Command ID (CID)            */
    0x00, 0x00, 0x00, 0x00,		/* RSF is 0 for success        */
    0x00, 0x00				/* HDLC Line Number  (0)       */
  };


/***********************************************************************\
*									*
*	Information for each device unit is maintained in an array	*
*	of structures named acp_softc[].  The array is indexed by	*
*	unit number.  Each entry includes the network interface		*
*	structure (acp_if) used by the routing code to locate the	*
*	interface,  an array of Logical	Channel control blocks which	*
*	maintain information about each of the Logical Channels (LCNs)	*
*	through which communication with the ACP is maintained, a queue *
*	of I/O requests pending for the ACP, the UNIBUS interrupt	*
*	vector for the unit and misc flags.  The Logical Channel	*
*	Control blocks maintain information about the state of each	*
*	LCN, a queue of outbound data, Half Duplex Channel (HDX) blocks	*
*	used for queuing I/O requests to the ACP and an ifuba		*
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

struct acp_cb		/* Logical Channel control block */
  {
    u_char		dc_lcn;		/* LCN number */
    struct ifqueue	dc_oq;		/* LCN output queue */
    struct hdx_chan	dc_rchan;	/* LCN read HDX channel */
    struct hdx_chan	dc_wchan;	/* LCN write HDX channel */
    struct ifuba	dc_ifuba;	/* UNIBUS resources */
    u_short		dc_flags;	/* misc flags */
  };

struct acp_softc	/* device control structure */
  {
    struct ifnet	acp_if;		/* network-visible interface   */
    struct acp_cb	acp_cb[NACPCH+1]; /* Logical Channel cntl blks */
    struct sioq		acp_sioq;	/* start I/O queue             */
    u_short		acp_vector;	/* UNIBUS interrupt vector     */
    u_short		acp_flags;	/* ACP operational flag        */
    u_char		acp_path;	/* path allocation flag        */
    u_short		acp_maxout;	/* maximum IP message sent     */
    u_short 		acp_maxin;	/* maximum IP message rcvd     */
#ifndef FOURTWO
    struct in_addr	acp_ipaddr;	/* local IP address */
#endif
  } acp_softc[NACP];

/* The acp_path flag indicates whether or not a path has been allocated */
/* and also whether or not to call acp_init to send an ssp_msg to the   */
/* front end:  acp_path = 1    indicates supervisory path is allocated  */
/*             acp_path = 2    indicates data path is allocated         */
/*             acp_path = 0x10 indicates acp_init should be called      */
/*                             to send CIM ssp_msg to the front end     */

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                                                             %%*/ 
/*%%                   GLOBAL ROUTINES                           %%*/ 
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                      ACPPROBE()                             %%*/ 
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
/*  Call:          acpprobe(reg)                                   */
/*  Argument:      reg:  caddr_t address in virtual memory of the  */
/*                        control-status register                  */
/*  Returns:       length of register structure for ACP device     */
/*  Called by:     network software, part of autoconfiguration on  */
/*                 the VAX, the address of this routine is one of  */
/*                 the fields of the uba_driver structure          */ 
/*  Calls to:      nothing                                         */ 
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static int savevec;			/* static variable for vector */

acpprobe(reg, ui)
caddr_t reg;
struct uba_device *ui;			/* TWG VAX/VMS ONLY! */
  {
#ifndef UBAUVII                         /* not for Ultrix 2.0 */
    register int br, cvec;		/* r11, r10 value-result */
#endif UBAUVII
    register struct acpregs *addr = (struct acpregs *)reg;

#ifdef lint
    br = 0; cvec = br; br = cvec;	/* these variables are value-result */
#endif

#ifdef VAXVMS
    cvec = savevec = ui->ui_flags & 0x1f8;	/* use flags from config file */
#else
    cvec = savevec = (uba_hd[numuba].uh_lastiv - 8) & ~7;
    uba_hd[numuba].uh_lastiv = cvec;
#endif VAXVMS

					/* return a vector pair */
					/*   aligned on QUADWORD boundary */

					/* cvec is the interrupt vector   */
					/*   address on the UNIBUS        */

					/* br is the IPL of the device    */
					/*   when it interrupts           */

#ifdef MVAX
    br = 0x17;				/* return bus level for a uVAX */
#else
    br = 0x15;				/* return bus level */
#endif MVAX

    addr->req_flags = 0;		/* clear handshake flags */
    addr->cmp_flags = 0;
    addr->xfr_flags = 0;
    addr->sys_stat = 0;
    addr->sys_vect = cvec >> 2;		/* pass vector to ACP */
    addr->csr = CSR_RESET;		/* reset the board */
    addr->csr |= CSR_IENB;		/* enable status intr */

    return (sizeof(struct acpregs));
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                      ACPATTACH()                            %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */ 
/*  Purpose:                                                       */ 
/*                                                                 */
/*  This routine attaches the device to the network software.  The */
/*  network interface structure is filled in.  The device will be  */
/*  initialized when the system is ready to accept packets.        */
/*                                                                 */ 
/*  Call:           acpattach(ui)                                  */ 
/*  Argument:       ui:  ptr to the uba_device data structure      */
/*  Returns:        nothing                                        */ 
/*  Called by:      network software, part of network system       */
/*                  configuration, identification to the network   */
/*                  software,  the address of this routine is one  */
/*                  of the fields of the uba_driver sturcture      */ 
/*  Calls to:       if_attach()                                    */ 
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

acpattach(ui)
struct uba_device *ui;
  {
    register struct acp_softc *ds = &acp_softc[ui->ui_unit];

    ds->acp_vector = savevec;		/* save vector from probe() */
    ds->acp_if.if_unit = ui->ui_unit;	/* set unit number */
    ds->acp_if.if_name = "acp";		/* set device name */
    ds->acp_if.if_mtu = ACPMTU;		/* set max msg size */
    ds->acp_if.if_init = acpinit;	/* set init routine addr */
    ds->acp_if.if_ioctl = acpioctl;	/* set ioctl routine addr */
    ds->acp_if.if_output = acpoutput;	/* set output routine addr */
    ds->acp_if.if_start = acpstart;	/* set start routine addr */
    ds->acp_if.if_reset = acpreset;	/* set reset routine addr */
    if_attach(&ds->acp_if);		/* attach new network device */
					/*  add to list of "active"  */
					/*  interfaces, the argument */
					/*  passed locates the ifnet */
					/*  data structure           */
  }
  

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                      ACPRESET()                             %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */ 
/*  Purpose:                                                       */ 
/*                                                                 */
/*      Reset of interface after UNIBUS reset.  If interface is on */
/*      specified uba, reset its state.                            */
/*                                                                 */
/*  Call:              acpreset(unit, uban)                        */ 
/*  Arguments:         unit:   ACP device unit number              */
/*                     uban:   UNIBUS adapter number               */
/*  Returns:           nothing                                     */ 
/*  Called by:         network software, address of routine is     */
/*                     defined in acp_if network interface struct  */
/*  Calls to:          printf()                                    */ 
/*                     IF_DEQUEUE()                                */ 
/*                     m_freem()                                   */ 
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

acpreset(unit, uban)
int unit, uban;
  {
    register struct uba_device *ui;	/* per "device" structure        */
    register struct acpregs *addr;	/* ACP device register struct    */
    register struct acp_cb *dc;
    register struct mbuf *m;
    register int lcn;

    if (unit >= NACP || (ui = acpinfo[unit]) == 0 || ui->ui_alive == 0 ||
      ui->ui_ubanum != uban)
	return;

    printf("acp%d\n", unit);

    addr = (struct acpregs *)ui->ui_addr;  /* address of device in I/O space  */

    addr->req_flags = 0;		/* clear handshake flags, mailbox     */
					/*  flags for I/O requests            */
    addr->cmp_flags = 0;		/* mailbox flags for I/O completion   */
    addr->xfr_flags = 0;		/* mailbox flags for I/O transfer     */
					/*  requests                          */
    addr->sys_stat = 0;			/* mailbox flags for system status    */
    addr->sys_vect = acp_softc[unit].acp_vector >> 2;  /* pass base interrupt */
					/*  vector to ACP                     */
    addr->csr = CSR_RESET;		/* reset the board                    */
    addr->csr |= CSR_IENB;		/* enable status intr                 */
    acp_softc[unit].acp_flags = 0;	/* clear ACP operational flag         */
    acp_softc[unit].acp_path = 0;	/* clear path allocation flag         */

    dc = acp_softc[unit].acp_cb;	/* flush any queued output data */
    for(lcn = 0; lcn <= NACPCH; lcn++)	/* for all LCN's ... */
    {
        while(dc->dc_oq.ifq_len)
            {
                IF_DEQUEUE(&dc->dc_oq, m);
                m_freem(m);
            }
        dc++;
    }

  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                      ACPINIT()                              %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */ 
/*  Purpose:                                                       */ 
/*                                                                 */
/*    This routine initializes the interface for operation.  The   */
/*    device control blocks are initialized, UNIBUS resources are  */
/*    allocated and an initialization message is sent to the ACP.  */
/*                                                                 */
/*  Call:             acpinit(unit)                                */ 
/*  Argument:         unit:  ACP device unit number                */
/*  Returns:          nothing                                      */ 
/*  Called by:        network software, address of this routine is */
/*                    defined in acp_if network interface struct   */
/*                    acpioctl()                                   */ 
/*                    acpintb()                                    */ 
/*  Calls to:         in_netof() return the network number from    */ 
/*                               internet address                  */ 
/*                    if_ubainit()                                 */ 
/*                    btoc()                                       */ 
/*                    splimp()                                     */ 
/*                    acp_ioreq()                                  */ 
/*                    acp_alloc()                                  */ 
/*                    acp_init()                                   */ 
/*                    splx()                                       */ 
/*                    if_rtinit()                                  */ 
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

acpinit(unit)
int unit;
  {
    register struct acp_softc *ds = &acp_softc[unit];
    register struct acp_cb *dc;
    register struct uba_device *ui = acpinfo[unit];
#ifdef FOURTWO
    struct sockaddr_in *sin;
#else
    struct ifaddr *ifa = ds->acp_if.if_addrlist;
#endif
    int lcn, s;

#ifdef ACPDEBUG
if (acp_debug > 0)
  {
      printf("acp%d: acpinit()\n", unit);
  }
#endif ACPDEBUG

#ifdef FOURTWO
    sin = (struct sockaddr_in *)&ds->acp_if.if_addr;
    if (in_netof(sin->sin_addr) == 0)
#else
ifa = ds->acp_if.ifaddrlist;
#ifdef AF_LINK
for (; ifa; ifa = ifa->ifa_next)
    if (ifa->ifa_addr->sa_family != AF_LINK)
	break;
#endif
if (    ifa == 0)    /* if we have no internet addr */
#endif
	return;
    if ((ds->acp_flags & ACPF_OK) == 0)	/* or if ACP not operational */
	return;				/*   don't init */


    dc = ds->acp_cb;			/* setup ptr to first LCN cntl block */

    for(lcn=ACP_ALLOC;lcn<=NACPCH;lcn++)	/* for all LCN's ... */
      {
    	dc->dc_lcn = lcn;		/* record LCN */

		/* init LCN output queue */

    	dc->dc_oq.ifq_head = (struct mbuf *)0;
    	dc->dc_oq.ifq_tail = (struct mbuf *)0;
    	dc->dc_oq.ifq_len = 0;
    	dc->dc_oq.ifq_maxlen = ACP_OQMAX;
    	dc->dc_oq.ifq_drops = 0;

    		/* init HDX channels */

    	dc->dc_rchan.hc_next = (struct hdx_chan *)0;
    	dc->dc_rchan.hc_chan = lcn * 2;
    	dc->dc_wchan.hc_next = (struct hdx_chan *)0;
    	dc->dc_wchan.hc_chan = (lcn * 2) + 1;

    		/* init UNIBUS resources, allocate UNIBUS map registers */

    	if ((ds->acp_if.if_flags & IFF_RUNNING) == 0)
          {
    	    if (if_ubainit(&dc->dc_ifuba, ui->ui_ubanum,
    	      0, (int)btoc(ACPMTU)) == 0)
    	      {
    	        printf("acp%d: failed getting UBA resources for lcn %d\n",
    		  unit, lcn);
    	        ds->acp_if.if_flags &= ~(IFF_RUNNING | IFF_UP);
    	        return;
    	      }
	  }
    	dc->dc_flags = 0;		/* initialize flags */

	dc++;				/* point at next cntl blk */
      }

    ds->acp_sioq.sq_head = (struct hdx_chan *)0;
    ds->acp_sioq.sq_tail = (struct hdx_chan *)0;
    ds->acp_if.if_flags |= IFF_RUNNING;

    s = splimp();			/* disable interrupts        */

    dc = ds->acp_cb;			/* setup ptr to first LCN cntl block */

    for(lcn=ACP_ALLOC;lcn<=NACPCH;lcn++)	/* issue reads on all LCNs */
      {
    	acp_iorq(ds, dc, ACPMTU, ACPRED+ACPSTR);
	dc++;
      }

	/* if not already established, allocate control and data paths  */

    if ((ds->acp_path & ACP_SUPR) == 0)
        acp_alloc(ds, TYPE_CNTL);	/* allocate control path  */
    if ((ds->acp_path & ACP_DATA) == 0)
        acp_alloc(ds, TYPE_DATA);	/* allocate data path     */

    if ((ds->acp_path & INIT_OK) == INIT_OK)
      {
        acp_init(ds);			/* init the ACP, if ioctl to do so */
    	ds->acp_path &= ~INIT_OK;	/* turn off flag for acpinit() */
      }

    splx(s);				/* enable interrupts        */

#ifdef FOURTWO
    if_rtinit(&ds->acp_if, RTF_UP);	/* initialize the routing table entry */
					/*  according to the network, args    */
					/*  are the addr of the ifnet struct  */
					/*  and RTF_UP means the route is     */
					/*  useable                           */
#endif
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      ACPOUTPUT()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*   This routine is called by the network software when it has an */
/*   IP datagram to send out this interface.  The datagtram is     */
/*   queued for output on that LCN.                                */
/*                                                                 */
/*  Call:            acpoutput(ifp, m0, dst)                       */
/*  Arguments:       ifp:  locates the network interface, ifnet    */
/*                   m0:   locates an mbuf buffer                  */
/*                   dst:  is the socket destination address       */
/*  Returns:         0 for success, or one of following nonzero    */
/*                        error indications:                       */
/*                               ENETDOWN                          */
/*                               EAFNOSUPPORT                      */
/*                               ENOBUFS                           */
/*  Called by:     network software, address of this routine is    */
/*                 defined in the acp_if network interface struct  */
/*  Calls to:      printf()                                        */
/*                 mfreem()                                        */
/*                 splimp()                                        */
/*                 IF_QFULL()                                      */
/*                 IF_DROP()                                       */
/*                 splx()                                          */
/*                 IF_ENQUEUE()                                    */
/*                 m_freem()                                       */
/*                 acp_start()                                     */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

acpoutput(ifp, m0, dst)
struct ifnet *ifp;		/* network interface          */
struct mbuf *m0;		/* buffer                     */
struct sockaddr_in *dst;	/* socket destination address */
  {
    register struct mbuf *m = m0;
    register struct acp_softc *ds = &acp_softc[ifp->if_unit];
    register struct acp_cb *dc;
    register struct ifqueue *oq;
    struct mbuf *prev;
    int s;

    if ((ds->acp_if.if_flags & IFF_UP) == 0)
	return (ENETDOWN);

    switch (dst->sin_family)
      {

#ifdef INET
    case AF_INET:		/* address format of protocol family */
				/*  this is the internet:  TCP, UDP, */
				/*  ICMP, IP, etc.                   */
	break;
#endif INET

    default:
	printf("acp%d: can't handle af%d\n", ifp->if_unit,
	    dst->sin_family);
	m_freem(m0);
	return (EAFNOSUPPORT);
      }


#ifdef ACPDEBUG
if (acp_debug > 6)
  {
      printf("acpoutput(): dst = ");
      prt_addr(dst->sin_addr);
      printf("\n");
  }
#endif ACPDEBUG

    /* In 4.3, the IP code may pass mbuf chains with 0-length mbufs */
    /* This causes "transfer count = 0" messages and might even     */
    /* cause actual garbage data transmission if the mbuf is at the */
    /* end of the chain (we don't think it ever will be, but one    */
    /* can't be too sure...so we scan the chain first).		    */
    /* WE DO ASSUME that there is at least one nonempty mbuf!	    */

    while (m0->m_len == 0)
    {
	m = m0;
	m0 = m0->m_next;
	m->m_next = 0;
	m_freem (m);
    }
    /* Now we know the first mbuf (at m0)  is not zero length	    */
    prev = m0;
    m = m0->m_next;
    while (m)
    {
	if (m->m_len == 0)
	{
	    prev->m_next = m->m_next;
	    m->m_next = 0;
	    m_freem (m);
	    m = prev->m_next;
	}
	else
	{
	    prev = m;
	    m = m->m_next;
	}
    }
    m = m0;			/* reset m to beginning of modified chain */

    s = splimp();		/* disable interrupts  */

    dc = &(ds->acp_cb[ACP_DATA]);	/*   data channel          */
    oq = &(dc->dc_oq);			/*   point to output queue */
    if (IF_QFULL(oq))			/*   if q full */
      {
	IF_DROP(oq);			/*     drop the data */
	m_freem(m);
	ds->acp_if.if_collisions++;
	splx(s);
	return (ENOBUFS);
      }
    IF_ENQUEUE(oq, m);			/*   otherwise queue it */
    acp_start(ds, dc);			/*   and try to output  */
    splx(s);				/*   enable interrupts  */
    return (0);				/*   successful return  */
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      ACPIOCTL()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*   This routine processes device dependent ioctl's.  Supported   */
/*   ioctls set the host's internet address for this network       */
/*   interface, or send CIMs (Command Interface Messages) to the   */
/*   ACP (ie to bring up the line).  The logic for setting the     */
/*   interface address must remain compatible with both ifconfig   */
/*   and acpconfig programs.                                       */
/*                                                                 */
/*  Call:            acpioctl(ifp, cmd, data)                      */
/*  Argument:        ifp:   pointer to the network interface data  */
/*                               structure, ifnet                  */
/*                   cmd:   identifies the type of ioctl           */
/*                   data:  information for the ioctl              */
/*  Returns:         0 for success, or the nonzero error value:    */
/*                                EINVAL invalid ioctl request     */
/*  Called by:        network software, address of this routine is */
/*                    defined in af_inet network interface struct  */
/*  Calls to:         splimp()                                     */
/*                    if_rtinit()                                  */
/*                    in_netof()                                   */
/*                    in_lnaof()                                   */
/*                    acpinit()                                    */
/*                    acpreset()                                   */
/*                    printf()                                     */
/*                    splx()                                       */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

acpioctl(ifp, cmd, data)
register struct ifnet *ifp;	/* network interface data structure      */
int cmd;			/* type of ioctl request                 */
caddr_t data;			/* address of data for ioctl request     */
  {
    register struct uba_device *ui = acpinfo[ifp->if_unit];
    struct ifreq *ifr = (struct ifreq *)data;	/* ifreq is the interface */
				/* request struct used for socket ioctls  */
    struct acp_softc *ds = &acp_softc[ifp->if_unit];
    int s = splimp(), error = 0;	/* disable interrupts    */
#ifdef FOURTWO
    struct sockaddr_in *sin = (struct sockaddr_in *)&ifr->ifr_addr;
#else
    struct ifaddr *ifa = ds->acp_if.if_addrlist;
#endif

#ifdef ACPDEBUG
if (acp_debug > 2)
  {
      printf("acp%d:  acpioctl()\n", ifp->if_unit);
  }
#endif ACPDEBUG

    switch (cmd)
      {
    case SIOCSIFADDR:			/* set ifnet address    */
#ifdef FOURTWO
	if (ifp->if_flags & IFF_RUNNING)
	    if_rtinit(ifp, -1);		/* delete previous route */
	ifp->if_addr = *(struct sockaddr *)sin;
	ifp->if_net = in_netof(sin->sin_addr);
	ifp->if_host[0] = in_lnaof(sin->sin_addr);
	if (ifp->if_flags & IFF_RUNNING)
	    if_rtinit(ifp, RTF_UP);	/* RTF_UP means route useable */
	else
	    acpinit(ifp->if_unit);
#else
        if (ifa->ifa_addr.sa_family != AF_INET)
                return(EINVAL);
        if ((ifp->if_flags & IFF_RUNNING) == 0)
                acpinit(ifp->if_unit);
        ds->acp_ipaddr = IA_SIN(ifa)->sin_addr;
#endif
	break;

    case SIOCACPCONFIG:
	/* if not trying to bring down link (case '0') then trying to bring */
	/* it up, or reconfigure it -- don't do cmd unless internet address */
	/* has already been set                                             */

    	if (*(ifr->ifr_data) != '0' )
	{
#ifdef FOURTWO
            sin = (struct sockaddr_in *)&ds->acp_if.if_addr;  
            if (in_netof(sin->sin_addr) == 0)
#else
	    if (ds->acp_if.if_addrlist == 0)
#endif
	    {
	        printf("acp%d:  no internet address is set,", ifp->if_unit);
	        printf(" acpconfig command ignored\n");
	        goto exit;
	    }
  	}
  	acpreset(ifp->if_unit, ui->ui_ubanum); /* reset device */
    	ds->acp_path |= INIT_OK;	/* set flag for acpinit() */

	/* if command is set the baud rate, then set clocking for  */
	/* internal generation, and look up the value for the baud */
	/* rate divisor in the baud_rate table, put this value in  */
	/* the Set System Parameters message, ssp_msg              */

	if ( (*(ifr->ifr_data) >= 1) && (*(ifr->ifr_data) <= 14) )
	{
	    register struct baud *p;

  	    ssp_msg[CLOCK_OFFSET] = INTERNAL_CLOCK;
		
	    for (p = baud_rate; p->b_value; p++)
		{
	        if ((*(ifr->ifr_data) - p->b_value) == 0)
		    break;
		}
	    ssp_msg[BAUD_OFFSET] = p->parameter1;
	    ssp_msg[BAUD_OFFSET + 1] = p->parameter2;
	    if (p->b_value == 0)	
	    { 	
		printf("acp%d: invalid value for baud rate\n", ifp->if_unit);
		goto exit;
	    } 	
	}
	else
	{
    	    switch (*(ifr->ifr_data))
            {
    	        case '0':
  		    ssp_msg[DOWN_OFFSET] = LINK_DISABLE;
        	    break;
    	        case '1':
  		    ssp_msg[LOOP_OFFSET] = LOOP_NONE;
  		    ssp_msg[DTE_OFFSET] = DTE_MODE;
  		    ssp_msg[DOWN_OFFSET] = LINK_ENABLE;
		    break;
    	        case '2':
  		    ssp_msg[LOOP_OFFSET] = LOOP_NONE;
  		    ssp_msg[DTE_OFFSET] = DCE_MODE;
  		    ssp_msg[DOWN_OFFSET] = LINK_ENABLE;
		    break;
    	        case '3': 
  		    ssp_msg[LOOP_OFFSET] = LOOP_EXTERNAL;
  		    ssp_msg[DOWN_OFFSET] = LINK_ENABLE;
        	    break;
    	        case '4':
  		    ssp_msg[LOOP_OFFSET] = LOOP_INTERNAL;
  		    ssp_msg[DOWN_OFFSET] = LINK_ENABLE;
        	    break;
    	        case 'b':
  		    ssp_msg[CLOCK_OFFSET] = EXTERNAL_CLOCK;
        	    break;
                default:
		    error = EINVAL;
		    goto exit;
              }
	 }
  	 acpinit(ifp->if_unit);		/* send ssp_msg to frontend */
         break;

    default:
	error = EINVAL;
    }

exit:
    splx(s);				/* enable interrupts   */
    return (error);
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      ACPINTA()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  This is the interrupt handler for I/O interrupts from the ACP  */
/*  The I/O mailboxes are scanned for handshake events to process. */
/*  The events are Transfer Request, I/O Request done and I/O      */
/*  Completion ready.  Note that the Transfer Request is not yet   */
/*  supported; an error message is printed if one is received.     */
/*                                                                 */
/*  Call:           acpinta(unit)                                  */
/*  Argument:       unit:  ACP device unit number                  */
/*  Returns:        nothing                                        */
/*  Called by:      network software, address of this routine is   */
/*                  defined in af_inet network interface struct    */
/*  Calls to:       printf()                                       */
/*                  start_chn()                                    */
/*                  acp_data()                                     */
/*                  acp_supr()                                     */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

acpinta(unit)
int unit;
  {
    register struct acpregs *addr = (struct acpregs *)acpinfo[unit]->ui_addr;
    register struct acp_softc *ds = &acp_softc[unit];
    register struct hdx_chan *hc;
    int chan, cc, cnt;

#ifdef ACPDEBUG
if (acp_debug > 7)
  {
      printf("acp%d: acpinta()\n", unit);
  }
#endif ACPDEBUG

    /* Figure out what kind of interrupt it was */

    if (addr->xfr_flags & FLAGS_RDY)	/* Transfer Request Mailbox */
      {
	printf("acp%d: UNEXPECTED TRANSFER REQUEST!\n", unit);
	addr->xfr_cnt = 0;
	addr->xfr_flags = (addr->xfr_flags & ~FLAGS_RDY) | FLAGS_DON;
	addr->csr |= CSR_INTRA;
      }

    if (addr->req_flags & FLAGS_DON)	/* I/O Request Mailbox  */
      {
    	/* try to start any queued i/o request */

    	if (ds->acp_sioq.sq_head = ds->acp_sioq.sq_head->hc_next)
    	  {
    	    start_chn(ds);
    	  }
	else
	  {
	    addr->req_flags &= ~FLAGS_DON;
	  }
      }

    if (addr->cmp_flags & FLAGS_RDY)	/* I/O Completion  Mailbox */
      {

	/*
	 * Get logical channel info.
	 */
	if ((chan = addr->cmp_chan) > NACPCH)
          {
	    printf("acp%d: unknown channel, chan=%d\n", unit, chan);
	    return;
	  }

	if (addr->cmp_flags & FLAGS_DIR)
    	    hc = &(ds->acp_cb[chan].dc_wchan);
	else
    	    hc = &(ds->acp_cb[chan].dc_rchan);
	cc = addr->cmp_stat;	/* Mailbox I/O completion status      */
	cnt = addr->cmp_cnt;	/* Mailbox I/O completion byte count  */

    	switch (cc)	/* check for unsuccessful I/O completion status */
    	  {
    	case ACPIOCABT:
    	    printf("acp%d: I/O abort ", unit);
    	    goto daterr;

    	case ACPIOCERR:
    	    printf("acp%d: program error ", unit);
    	    goto daterr;

    	case ACPIOCOVR:
    	    printf("acp%d: overrun error ", unit);
    	    goto daterr;

    	case ACPIOCUBE:
    	    printf("acp%d: NXM timeout or UB parity error ", unit);

    	daterr:
    	    printf("chan=%d func=%x\n", chan, hc->hc_func);
    	    if (addr->cmp_flags & FLAGS_DIR)
    		ds->acp_if.if_oerrors++;
    	    else
    		ds->acp_if.if_ierrors++;
    	  }

    	/* was it supervisor or data traffic? */

    	if (chan > ACP_SUPR)
    	    acp_data(ds, hc, cc, cnt);
    	else
    	    acp_supr(ds, hc, cc, cnt, chan);  /* chan = ACP_ALLOC or ACP_SUPR */

    	/*
    	 * Ack the interrupt.  Fix the Mailbox Ready and Done bits:  set
         * DON bits, and clear RDY bits so mailbox may be reused.
    	 */
    	addr->cmp_flags = (addr->cmp_flags & ~FLAGS_RDY) | FLAGS_DON;
	addr->csr |= CSR_INTRA;			/* enable interrupt "a" */

      }
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                      ACPINTB()                              %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */ 
/*  Purpose:                                                       */ 
/*                                                                 */
/*   This is the interrupt handler for system interrupts from the  */
/*   ACP.                                                          */
/*                                                                 */
/*  Call:             acpintb(unit)                                */
/*  Argument:         unit: ACP device unit number                 */
/*  Returns:          nothing                                      */ 
/*  Called by:        network software, address of this routine is */
/*                    defined in af_inet network interface struct  */
/*  Calls to:         printf()                                     */ 
/*                    acpinit()                                    */ 
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

acpintb(unit)
int unit;
  {
    register struct acpregs *addr = (struct acpregs *)acpinfo[unit]->ui_addr;
    register struct acp_softc *ds = &acp_softc[unit];

#ifdef ACPDEBUG
if (acp_debug > 1)
  {
      printf("acp%d: acpintb()\n", unit);
  }
#endif ACPDEBUG

    if (ds->acp_flags & ACPF_OK)
      {
	printf("acp%d: Unexpected System interrupt, status = %d\n",
	    unit, addr->sys_stat);
	addr->csr = 0;
	printf("acp%d: DISABLED!\n", unit);
	ds->acp_if.if_flags &= ~(IFF_RUNNING | IFF_UP);
      }
    else
      {
	if (addr->sys_stat != ACPSTAT_OK)
	  {
	    printf("acp%d: PWRUP Diagnostic failure = %d\n",
		unit, addr->sys_stat);
	    addr->csr = 0;
	  }
	else
	  {
	    ds->acp_flags |= ACPF_OK;
	    addr->csr |= (CSR_IENA | CSR_DMAEN);
	    acpinit(unit);
	  }
      }
  }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                                                             %%*/ 
/*%%                   LOCAL  ROUTINES                           %%*/ 
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                      ACP_ALLOC()                            %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */ 
/*  Purpose:                                                       */ 
/*                                                                 */
/*   This routine allocates control or data paths.  Commands are   */
/*   sent (over DPN 0) from the host to the ALLOC facility on the  */
/*   front end to allocate the paths.  The ALLOC facility returns  */    
/*   a response to the allocation command which indicates success  */    
/*   or failure.  Note that DPN 0 is used only for the ALLOC       */    
/*   commands, and is not a control path as it was been in the     */    
/*   past.  The paths are symbolically defined as ACP_ALLOC,       */    
/*   ACP_SUPR, and ACP_DATA for allocation messages, control       */    
/*   messages, and data respectively.  The CID field indicates     */
/*   the data path number of the allocation command.  The CID      */
/*   is set here so that the response will have the same CID, and  */
/*   will therefore indicate to which path the response            */
/*   corresponds. (The CID is set in the command and must be       */
/*   returned, untouched, in the response.)                        */
/*                                                                 */
/*  Call:          acp_alloc(ds, type)                             */ 
/*  Argument:      ds:  pointer to ACP device control structure    */
/*                 type:  specifies if path is for control or data */
/*  Returns:       nothing                                         */ 
/*  Called by:     acpinit()                                       */
/*  Calls to:          MGET()                                      */ 
/*                     printf()                                    */ 
/*                     mtod()                                      */ 
/*                     bcopy()                                     */ 
/*                     sizeof()                                    */ 
/*                     IF_ENQUEUE()                                */ 
/*                     acp_start()                                 */ 
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void acp_alloc(ds, type)
struct acp_softc *ds;
int type;
  {
    struct mbuf *m;
    register u_char *bp;

#ifdef ACPDEBUG
if (acp_debug > 4)
  {
      printf("acp%d: acp_alloc()\n", ds->acp_if.if_unit);
  }
#endif ACPDEBUG

    MGET(m, M_DONTWAIT, MT_DATA);	/* try to get init buffer */
    if (m == 0)
      {
    	printf("acp%d: couldn't get init buffer\n", ds->acp_if.if_unit);
    	return;
      }

	/* modify the path allocation message to get a control path */
	/* or a data path                                           */

    if (type == TYPE_CNTL)
      {
        alloc_msg[CID_OFFSET] = ACP_SUPR;	/* set CID for response */
        alloc_msg[DPN_OFFSET] = ACP_SUPR;	/* path number          */
        alloc_msg[TYPE_OFFSET] = TYPE_CNTL;	/* path type = control  */
      }
    else
      {
        alloc_msg[CID_OFFSET] = ACP_DATA;	/* set CID for response  */
        alloc_msg[DPN_OFFSET] = ACP_DATA;	/* path number           */
        alloc_msg[TYPE_OFFSET] = TYPE_DATA;	/* path type = data      */
      }

    bp = mtod(m, u_char *);		/* point to data section of mbuf */

    bcopy(alloc_msg, bp, sizeof(alloc_msg));  /* set sys params msg in mbuf */

#ifdef ACPDEBUG
if (acp_debug > 5)
  {
printf("acp_alloc():  ");
prt_bytes(bp, sizeof(alloc_msg));   /* print 12-byte header + data */
printf("\n");
  }
#endif ACPDEBUG


    m->m_len = sizeof(alloc_msg);		/* set msg length */

    IF_ENQUEUE(&(ds->acp_cb[ACP_ALLOC].dc_oq), m);	/* output queue   */

    acp_start(ds, &(ds->acp_cb[ACP_ALLOC]));	/* start ouput of data  */
  }


/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                      ACP_INIT()                             %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */ 
/*  Purpose:                                                       */ 
/*                                                                 */
/*   This routine builds and sends an initialization message to    */
/*   the ACP.  A canned Set System Parameters Message is sent to   */
/*   start HDLC.                                                   */
/*                                                                 */
/*  Call:          acp_init(ds)                                    */ 
/*  Argument:      ds:  pointer to ACP device control structure    */
/*  Returns:                nothing                                */ 
/*  Called by:         acpinit()                                   */
/*  Calls to:          MGET()                                      */ 
/*                     printf()                                    */ 
/*                     mtod()                                      */ 
/*                     bcopy()                                     */ 
/*                     sizeof()                                    */ 
/*                     IF_ENQUEUE()                                */ 
/*                     acp_start()                                 */ 
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void acp_init(ds)
struct acp_softc *ds;
  {
    struct mbuf *m;
    register u_char *bp;

#ifdef ACPDEBUG
if (acp_debug > 5)
  {
      printf("acp%d: acp_init()\n", ds->acp_if.if_unit);
  }
#endif ACPDEBUG

    MGET(m, M_DONTWAIT, MT_DATA);	/* try to get init buffer */
    if (m == 0)
      {
    	printf("acp%d: couldn't get init buffer\n", ds->acp_if.if_unit);
    	return;
      }

    bp = mtod(m, u_char *);		/* point to data section of mbuf */

    bcopy(ssp_msg, bp, sizeof(ssp_msg));  /* put msg into mbuf */

#ifdef ACPDEBUG
if (acp_debug > 4)
  {
printf("acp_init():  ssp msg\n");
prt_bytes(bp, sizeof(ssp_msg));		/* print 12-byte header + data */
printf("\n");
  }
#endif ACPDEBUG

    m->m_len = sizeof(ssp_msg);		/* set msg length */

    IF_ENQUEUE(&(ds->acp_cb[ACP_SUPR].dc_oq), m);	/* output queue   */

    acp_start(ds, &(ds->acp_cb[ACP_SUPR]));	/* start ouput of data  */
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      ACP_START()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine attempts to start output of data queued on a    */
/*    specific LCN.  If the LCN was not already busy and data is   */
/*    available for output, the data is copied into the LCN's I/O  */
/*    buffer and an I/O request queued to the ACP.                 */
/*                                                                 */
/*  Call:              acpstart(ds, dc)                            */
/*  Arguments:         ds:  pointer to device control structure    */
/*                     dc:  pointer to the Logical Channel control */
/*                            block structure                      */
/*  Returns:           nothing                                     */
/*  Called by:         acpoutput()                                 */
/*                     acp_init()                                  */
/*                     acp_data()                                  */
/*                     acp_supr()                                  */
/*  Calls to:          IF_DEQUEUE()                                */
/*                     if_wubaput()                                */
/*                     acp_ioreqs()                                */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void acp_start(ds, dc)
register struct acp_softc *ds;
register struct acp_cb *dc;
  {
    register struct mbuf *m;
    int len;

    /*
     * If output isn't active, attempt to
     * start sending a new packet.
     */

#ifdef ACPDEBUG
if (acp_debug > 7)
  {
      printf("acp: acp_start()\n");
  }
#endif ACPDEBUG

    if ((dc->dc_flags & DC_OBUSY) || (dc->dc_oq.ifq_len == 0))
    	return;

    IF_DEQUEUE(&dc->dc_oq, m);	/* remove data from LCN output queue */

    len = if_wubaput(&dc->dc_ifuba, m);	/* copy data to mapped mem */

    if (len > ds->acp_maxout)
      {

#ifdef ACPDEBUG
if (acp_debug > 7)
  {
    printf("acp: %d byte msg sent.\n", len);
  }
#endif ACPDEBUG

	ds->acp_maxout = len;
      }

    dc->dc_flags |= DC_OBUSY;

    acp_iorq(ds, dc, len, ACPWRT+ACPEOS);	/* build I/O request, enqueue */
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      ACP_IORQ()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine builds ACP I/O requests and queues them for     */
/*    delivery to the ACP. If the ACP I/O request comm regs are    */
/*    not busy, the I/O request is passed to the ACP.              */
/*                                                                 */
/*  Call:            acp_iorq(ds, dc, len, func)                   */
/*  Argument:        ds:   pointer to device control block struct  */
/*                   dc:   pointer to the Logical Channel control  */
/*                                block structure                  */
/*                   len:  byte count                              */
/*                   func: the function:  read or write            */
/*  Returns:         nothing                                       */
/*  Called by:         acpinit()                                   */
/*                     acp_start()                                 */
/*                     acp_data()                                  */
/*                     acp_supr()                                  */
/*  Calls to:          start_chn()                                 */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void acp_iorq(ds, dc, len, func)
struct acp_softc *ds;
struct acp_cb *dc;
int len, func;
  {
    register struct hdx_chan *hc;
    register int info;


#ifdef ACPDEBUG
if (acp_debug > 6)
      printf("acp: acp_iorq()\n");
#endif ACPDEBUG

    /* get appropriate UNIBUS mapping info */

    if ((func & FCN_MASK) == ACPRED)	/* read or write? */
      {
    	hc = &dc->dc_rchan;		/* read */
    	info = dc->dc_ifuba.ifu_r.ifrw_info;
      }
    else
      {
    	hc = &dc->dc_wchan;		/* write */
    	info = dc->dc_ifuba.ifu_w.ifrw_info;
      }

    /* set channel info */

    hc->hc_adx = (u_char)((info & 0x30000) >> 12);	/* address bits 17-16 */
    hc->hc_addr = (unsigned short)(info & 0xffff);	/* address bits 15-00 */
    hc->hc_cnt = len;					/* byte count         */
    hc->hc_func = (u_char)func;				/* I/O function       */

    if (dc->dc_lcn > ACP_SUPR)
        hc->hc_sbfc = SBFCN_DATA;		/* I/O subfunction for data */ 
    else
        hc->hc_sbfc = SBFCN_SUPR;		/* I/O subfunction for cntrl */

    /*
     * If ACP comm regs busy, queue start i/o for later.
     */
    if (ds->acp_sioq.sq_head)
      {
    	(ds->acp_sioq.sq_tail)->hc_next = hc;
    	ds->acp_sioq.sq_tail = hc;
    	hc->hc_next = 0;
    	return;
      }

    /* start i/o on channel now */

    ds->acp_sioq.sq_head = hc;
    ds->acp_sioq.sq_tail = hc;
    hc->hc_next = 0;
    start_chn(ds);
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      START_CHN()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine copies ACP I/O requests into the ACP comm regs  */
/*    and notifies the ACP.                                        */
/*                                                                 */
/*  Call:              start_chn(ds)                               */
/*  Argument:          ds:  pointer to device control block struct */
/*  Returns:           nothing                                     */
/*  Called by:         acpinta()                                   */
/*                     acp_iorq()                                  */
/*  Calls to:          none                                        */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void start_chn(ds)
struct acp_softc *ds;
  {
    register struct hdx_chan *hc = ds->acp_sioq.sq_head;
    register struct acpregs *addr =
    	(struct acpregs *)acpinfo[ds->acp_if.if_unit]->ui_addr;

    /*
     * Set up comm regs.
     */

#ifdef ACPDEBUG
if (acp_debug > 7)
  {
      printf("acp: start_chn()\n");
  }
#endif ACPDEBUG

    addr->req_chan = hc->hc_chan >> 1;
    addr->req_adx = hc->hc_adx;
    addr->req_addr = hc->hc_addr;
    addr->req_cnt = hc->hc_cnt;
    addr->req_fcn = hc->hc_func;
    addr->req_sbf = hc->hc_sbfc;
    if (hc->hc_chan & 1)
	addr->req_flags = FLAGS_RDY | FLAGS_DIR;
    else 
	addr->req_flags = FLAGS_RDY;

    addr->csr |= CSR_INTRA;
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      ACP_DATA()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine is called when a data channel I/O completes.    */
/*    If the completion was for a write, an attempt is made to     */
/*    start output on the next packet waiting for output on that   */
/*    LCN.  If the completion was for a read, the received packet  */
/*    is sent to the IP input queue (if no error) and another read */
/*    is started on the LCN.                                       */
/*                                                                 */
/*  Call:              acp_data(ds, hc, cc, rcnt)                  */
/*  Arguments:         ds:  pointer to device control block struct */
/*                     hc:  pointer to half duplex channel control */
/*                               block                             */
/*                     cc:  Mailbox I/O completion status          */
/*                     rcnt: byte count                            */
/*  Returns:           nothing                                     */
/*  Called by:         acpinta()                                   */
/*  Calls to:          m_freem()                                   */
/*                     acp_start()                                 */
/*                     if_rubaget()                                */
/*                     IF_QFULL()                                  */
/*                     IF_DROP()                                   */
/*                     IF_ENQUEUE()                                */
/*                     schednetisr()                               */
/*                     acp_ioreq()                                 */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void acp_data(ds, hc, cc, rcnt)
register struct acp_softc *ds;
register struct hdx_chan *hc;
int cc, rcnt;
{
    register struct acp_cb *dc = &(ds->acp_cb[hc->hc_chan/2]);
    register struct ifqueue *inq = &ipintrq;
    register struct mbuf *m;

    if (hc->hc_chan & 0x01)		/* was it read or write? */	
      {					/*   write, fire up next output */
    	ds->acp_if.if_opackets++;
	if (dc->dc_ifuba.ifu_xtofree)
	  {
	    m_freem(dc->dc_ifuba.ifu_xtofree);
	    dc->dc_ifuba.ifu_xtofree = 0;
	  }
    	dc->dc_flags &= ~DC_OBUSY;
    	acp_start(ds, dc);
      }
    else				/*   read, process rcvd packet */
      {

#ifdef ACPDEBUG
if (acp_debug > 6)
  {
printf("acp: data read completed, cc = %d, cnt = %d\n", cc, rcnt);
prt_bytes((u_char *)(dc->dc_ifuba.ifu_r.ifrw_addr), (rcnt < 20 ? rcnt:20));
  }
#endif ACPDEBUG

    	if (cc == ACPIOCOK)
    	  {				 /* Queue good packet for input */
    	    ds->acp_if.if_ipackets++;
	    if (rcnt > ds->acp_maxin)
	      {

#ifdef ACPDEBUG
if (acp_debug > 4)
  {
    printf("acp: %d byte msg received.\n", rcnt);
  }
#endif ACPDEBUG

		ds->acp_maxin = rcnt;
	      }
				/* call if_rubaget() to pull read data  */
				/*  off the interface, the args are the */
				/*  ifuba struct, the length of data,   */
				/*  and the 0 indicates that no trailer */
				/*  protocol is used                    */
    	    m = if_rubaget(&(dc->dc_ifuba), rcnt, 0, &(ds->acp_if));
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

    	acp_iorq(ds, dc, ACPMTU, ACPRED+ACPSTR);

      }
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                      ACP_RESPONSE()                         %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */ 
/*  Purpose:                                                       */ 
/*                                                                 */
/*   This routine sends a Control Interface Message (CIM) response */
/*   to the front end to indicate that a CIM command from the      */
/*   front end was successfully received.  Presently there are two */    
/*   types of CIM responses sent to the front end:  frame level    */    
/*   up, and frame level down.  Future applications may send a     */    
/*   CIM response to DCP CIM commands.  The basic philosophy with  */    
/*   CIMs is that there is always a paired command/response which  */    
/*   is exchanged between the host and the front end.              */    
/*   Currently, the front end does not process the responses from  */    
/*   the host, they are merely discarded.  The one thing left to   */    
/*   do in the case that the front end does ever look at these     */    
/*   responses is to use the same CID (Command ID field, bytes 5   */
/*   to 8 of the CIM header) that was present in the command.      */    
/*                                                                 */
/*  Call:          acp_response(ds, response)                      */ 
/*  Argument:      ds:  pointer to ACP device control structure    */
/*                 response:  response for CIM command field       */
/*                   the response value = command value + 1        */
/*  Returns:       nothing                                         */ 
/*  Called by:     supr_msg()                                      */
/*  Calls to:          MGET()                                      */ 
/*                     printf()                                    */ 
/*                     mtod()                                      */ 
/*                     bcopy()                                     */ 
/*                     sizeof()                                    */ 
/*                     IF_ENQUEUE()                                */ 
/*                     acp_start()                                 */ 
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void acp_response(ds, response)
struct acp_softc *ds;
u_char response;
  {
    struct mbuf *m;
    u_char *bp;

    MGET(m, M_DONTWAIT, MT_DATA);	/* try to get init buffer */
    if (m == 0)
      {
    	printf("acp%d: couldn't get init buffer\n", ds->acp_if.if_unit);
    	return;
      }
			/* put response in CIM cmd field */

    response_msg[CMD_OFFSET] = response;	

    bp = mtod(m, u_char *);		/* point to data section of mbuf */

    bcopy(response_msg, bp, sizeof(response_msg));  /* put msg in mbuf */

#ifdef ACPDEBUG
if (acp_debug > 6)
  {
printf("acp_response():  ");
prt_bytes(bp, sizeof(response_msg));   /* print messge  */
printf("\n");
  }
#endif ACPDEBUG

    m->m_len = sizeof(response_msg);		/* set msg length */

    IF_ENQUEUE(&(ds->acp_cb[ACP_SUPR].dc_oq), m);	/* output queue   */

    acp_start(ds, &(ds->acp_cb[ACP_SUPR]));	/* start ouput of data  */
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      ACP_SUPR()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine is called when a supervisor I/O completes.      */
/*    If the completion was for a write, an attempt is made to     */
/*    start output on the next supervisor command waiting for      */
/*    output.  If the completion was for a read, the received      */
/*    supervisor message is processed and another read is started. */
/*                                                                 */
/*  Call:              acp_supr(ds, hc, cc, rcnt, channel)         */
/*  Argument:          ds:   pointer to dev control block struct   */
/*                     hc:   pointer to the Half Duplex cntrl      */
/*                                 block structure                 */
/*                     cc:   Mailbox I/O completion status         */
/*                     rcnt:  byte count, length of data           */
/*                     channel:  indicates ACP_ALLOC or ACP_SUPR   */
/*  Returns:           nothing                                     */
/*  Called by:         acpinta()                                   */
/*  Calls to:          m_freem()                                   */
/*                     acp_start()                                 */
/*                     supr_msg()                                  */
/*                     acp_ioreq()                                 */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void acp_supr(ds, hc, cc, rcnt, chan)
register struct acp_softc *ds;
register struct hdx_chan *hc;
int cc, rcnt, chan;
  {
    register struct acp_cb *dc = &(ds->acp_cb[chan]);
    register u_char *p;

    /* was it read or write? */

    if (hc->hc_chan & 0x01)
      {
	if (dc->dc_ifuba.ifu_xtofree)
	  {
	    m_freem(dc->dc_ifuba.ifu_xtofree);
	    dc->dc_ifuba.ifu_xtofree = 0;
	  }
    	dc->dc_flags &= ~DC_OBUSY;
    	acp_start(ds, dc);
      }
    else
      {

#ifdef ACPDEBUG
if (acp_debug > 3)
  {
printf("acp: acp_supr(), read completed, cc = %d, cnt = %d\n", cc, rcnt);
prt_bytes((u_char *)(dc->dc_ifuba.ifu_r.ifrw_addr), rcnt);
printf("\n");
  }
#endif ACPDEBUG

    	if (cc == ACPIOCOK)
    	  {
    	    p = (u_char *)(dc->dc_ifuba.ifu_r.ifrw_addr);

    	    /* process supervisor message */
    	    supr_msg(ds, p);
    	  }
    	/* hang a new supr read */
    	acp_iorq(ds, dc, ACPMTU, ACPRED+ACPSTR);
      }
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      SUPR_MSG()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*       This routine processes received supervisor messages.      */
/*       Depending on the message type, the appropriate action is  */
/*       taken.  Note that the RSF field is checked for responses. */
/*       The RSF field is 4 bytes long, but ony that least         */
/*       significant byte is used.                                 */
/*                                                                 */
/*  Call:              supr_msg(ds, p)                             */
/*  Arguments:         ds:  pointer to dev control block struct    */
/*                     p:   pointer to a character array           */
/*                              containing the supervisor message  */
/*  Returns:           nothing                                     */
/*  Called by:         acp_supr()                                  */
/*  Calls to:          printf()                                    */
/*                     IF_DEQUEUE()                                */
/*                     m_freem()                                   */
/*                     acp_response()                              */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void supr_msg(ds, p)
struct acp_softc *ds;
u_char p[];
  {
    register struct acp_cb *dc;
    register int lcn;
    register struct mbuf *m;

    switch (p[3])
      {
    case RSP_ALLOC:			/*  alloc response   */
        if (p[RSF_OFFSET])	/* check if RSF is 0 for success */
          {
            printf("acp%d:  attempt to allocate path failed ");
            printf("rsf field = %x\n", p[RSF_OFFSET]);
            return;
          }
        else
          {
            if (p[CID_OFFSET] >=  ACP_SUPR  &&  p[CID_OFFSET] <= ACP_DATA )
                ds->acp_path |= p[CID_OFFSET];
            else
              {
                printf("acp%d:  path allocation ",ds->acp_if.if_unit);
                printf("response contains invalid DPN = %d\n", p[CID_OFFSET]);
              }
          }
        break;

    case RSP_DEALLOC:			/*  dealloc response */
        if (p[RSF_OFFSET])	/* check if RSF is 0 for success */
          {
            printf("acp%d:  attempt to deallocate path failed ");
            printf("rsf field = %x\n", p[RSF_OFFSET]);
            return;
          }
        break;

    case RSP_SSP:			/*  set sys parm rsp */
        if (p[RSF_OFFSET])	/* check if RSF is 0 for success */
          {
            printf("acp%d:  attempt to set HDLC system parameters failed\n");
            return;
          }
        break;

    case CMD_FLUP:			/*   frame level up  */

	/* check that the data path was successfully allocated, we */
	/* know that the control path was successfully allocated   */
	/* otherwise the FLUP command would not have been issued   */

        if ((ds->acp_path & ACP_DATA) == 0)
          { 
            printf("acp%d:  data path was not successfully allocated\n",
			ds->acp_if.if_unit);
          } 
        ds->acp_if.if_flags |= IFF_UP;
        printf("acp%d:  frame level up\n", ds->acp_if.if_unit);
        acp_response(ds, RSP_FLUP);	/* send response to front end */
        break;

    case CMD_FLDWN:			/* frame level down  */
        ds->acp_if.if_flags &= ~IFF_UP;
        dc = ds->acp_cb;
        for(lcn=ACP_ALLOC;lcn<=NACPCH;lcn++)	/* for all LCN's */
          {
            while (dc->dc_oq.ifq_len) /* drop pending data */
              {
 	        IF_DEQUEUE(&dc->dc_oq, m);
    	        m_freem(m);
              }
	    dc++;
    	  }
        printf("acp%d:  frame level down\n", ds->acp_if.if_unit);
        acp_response(ds, RSP_FLDWN);	/* send response to front end */
    	break;

    default:
    	printf("acp%d: supervisor error, code=%x\n",
	   ds->acp_if.if_unit, p[3]);
      }
  }


#ifdef ACPDEBUG

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                            PRT_ADDR()                             %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */ 
/*  Purpose:                                                             */ 
/*                                                                       */
/*	This routine is used to print internet addresses in the		 */
/*	standard A.B.C.D format.  Note that this routine is for          */
/*	debugging purposes (ifdef ACPDEBUG).                             */
/*									 */
/*  Call:  		prt_addr(addr)                                   */ 
/*  Argument:      	addr:  internet address structure                */
/*  Returns:  		nothing                                          */ 
/*  Called by:  	acpoutput()                                      */
/*  Calls to:   	printf()                                         */ 
/*									 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void prt_addr(addr)
struct in_addr addr;
  {
#ifndef FOURTWO
        union {
                struct in_addr ip;
                struct {                /* (assume Class A network number) */
                        u_char s_net;
                        u_char s_host;
                        u_char s_lh;
                        u_char s_impno;
                } imp;
        } imp_addr;
        imp_addr.ip = addr;
    printf("%d.%d.%d.%d", imp_addr.imp.s_net, imp_addr.imp.s_host,
                imp_addr.imp.s_lh, imp_addr.imp.s_impno);
#else
    printf("%d.%d.%d.%d", addr.s_net, addr.s_host, addr.s_lh, addr.s_impno);
#endif
  }

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                            PRT_BYTES()                            %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */ 
/*  Purpose:                                                             */ 
/*                                                                       */
/*	This routine is used to print a string of bytes in hex.		 */
/*	Note that this routine is for debugging purposes (ifdef          */
/*	ACPDEBUG).                                                       */
/*									 */
/*  Call:  		prt_bytes(bp, cnt)                               */ 
/*  Argument:      	bp:  pointer to the string                       */
/*                 	cnt: number of bytes                             */
/*  Returns:  		nothing                                          */ 
/*  Called by:  	acp_data()                                       */
/*              	acp_supr()                                       */
/*              	supr_msg()                                       */
/*  Calls to:   	printf()                                         */ 
/*									 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void prt_bytes(bp, cnt)
u_char *bp;
int cnt;
  {
    while(cnt--)
      {
	printf(" %x", *bp++ & 0xff);
      }
  }

#endif ACPDEBUG

#endif NACP
