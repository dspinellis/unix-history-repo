/*************************************************************************/
/*									 */
/*									 */
/*	 ________________________________________________________	 */
/*	/							 \	 */
/*     |	  AAA	       CCCCCCCCCCCCCC	 CCCCCCCCCCCCCC	  |	 */
/*     |	 AAAAA	      CCCCCCCCCCCCCCCC	CCCCCCCCCCCCCCCC  |	 */
/*     |	AAAAAAA	      CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |	 */
/*     |       AAAA AAAA      CCCC		CCCC		  |	 */
/*     |      AAAA   AAAA     CCCC		CCCC		  |	 */
/*     |     AAAA     AAAA    CCCC		CCCC		  |	 */
/*     |    AAAA       AAAA   CCCC		CCCC		  |	 */
/*     |   AAAA	 AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |	 */
/*     |  AAAA	  AAAAAAAAAAA CCCCCCCCCCCCCCCC	CCCCCCCCCCCCCCCC  |	 */
/*     | AAAA	   AAAAAAAAA   CCCCCCCCCCCCCC	 CCCCCCCCCCCCCC	  |	 */
/*	\________________________________________________________/	 */
/*									 */
/*	Copyright (c) 1986 by Advanced Computer Communications		 */
/*	720 Santa Barbara Street, Santa Barbara, California  93101	 */
/*	(805) 963-9431							 */
/*									 */
/*									 */
/*  File:		if_dda.c					 */
/*									 */
/*  Project:		DDN-X.25 Network Interface Driver for ACP 5250	 */
/*			and ACP 6250					 */
/*									 */
/*  Function:		This is a network interface driver supporting	 */
/*			the ACP5250/6250 under UNIX versions 4.2, 4.3,	 */
/*			4.3-tahoe, Ultrix versions 1.2 and 2.0, and	 */
/*			under VMS,  TWG WIN/VX and TGV Multinet.	 */
/*									 */
/*  Components:		required: if_dda.c, if_ddareg.h, if_ddavar.h,	 */
/*			  and one of: if_dda_uqbus.c if_dda_bibus.c	 */
/*			optional: if_pi.c, if_pivar.h, if_x29.c,	 */
/*				  if_vmsx29.c				 */
/*									 */
/*************************************************************************/


#include "dda.h"

#if NDDA > 0

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								 %%*/
/*%%		       SYSTEM CONFIGURATION			 %%*/
/*%%								 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if !defined(ACC_ULTRIX) && !defined(ACC_BSD) && !defined(ACC_VMS)
	ERROR
		an ACC OS specific option must be defined in your config file
	ERROR
#endif

/*
 *	now define the un-set options to zero
 */
#if !defined(ACC_ULTRIX)
#define	ACC_ULTRIX	00
#endif

#if !defined(ACC_BSD)
#define	ACC_BSD		00
#endif

#if !defined(ACC_VMS)
#define ACC_VMS		00
#endif

/*
 * the define DDA_MSGQ enables the message queue.  this adds 2k to the
 * data size of the driver.  It should only be used during driver development
 */

/*#define DDA_MSGQ		/* uncomment this to enable message queue */

/*
 * The following line disables the use of the histogram facilities.  This
 * value (DDA_HISTOGRAM) is automatically undefined for all 4.2 and ULTRIX
 * 1.2 systems which do not support the histogram facilities.
 */

#define DDA_HISTOGRAM		/* comment this out to disable histogram */

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								 %%*/
/*%%		       INCLUDE FILES				 %%*/
/*%%								 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef SIMULATION		/* real unix system */
#include "../machine/pte.h"	/* page table entries */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/buf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/vmmac.h"
#include "../h/errno.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/ioctl.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#  if ACC_BSD > 42 || ACC_ULTRIX > 12
#    include "../netinet/in_var.h"
#  endif
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"

#include "../vaxif/if_ddareg.h"
#include "../vaxif/if_ddavar.h"

#else	SIMULATION
#include "machine/pte.h"	/* page table entries */

#include "h/param.h"
#include "h/systm.h"
#include "h/mbuf.h"
#include "h/buf.h"
#include "h/protosw.h"
#include "h/socket.h"
#include "h/vmmac.h"
#include "h/errno.h"
#include "h/dir.h"
#include "h/user.h"
#include "h/kernel.h"
#include "h/ioctl.h"

#include "vax/cpu.h"
#include "vax/mtpr.h"

#include "net/if.h"
#include "net/netisr.h"
#include "net/route.h"
#include "netinet/in.h"
#include "netinet/in_systm.h"
#  if ACC_BSD > 42 || ACC_ULTRIX > 12
#    include "netinet/in_var.h"
#  endif
#include "netinet/ip.h"
#include "netinet/ip_var.h"
#include "if_ddareg.h"
#include "if_ddavar.h"

#  ifndef	SIOCACPCONFIG
#    define	SIOCACPCONFIG	_IOWR(i,40,struct ifreq)
#  endif
#  ifndef	INET
#    define	INET	1
#  endif

extern	struct	ifqueue ipintrq;	/* IP input queue */
#endif	SIMULATION

#if ACC_VMS > 00
#  ifdef eunice
#    define WINS
#  else
#    define MULTINET
#  endif
#endif

#if ACC_VMS > 00
#  ifdef WINS
#    include <vms/adpdef.h>	/* Define Adapters */
#    include <vms/dcdef.h>	/* Define AT$_UBA, adapter type */
#  else MULTINET
#    include "../vaxif/if_ddaioctl.h"	/* not in ioctl.h */
#  endif
#endif 

/* disable histogram functions for BSD 4.2 and ULTRIX 1.2 */

#if ACC_BSD == 42 || ACC_ULTRIX == 12
#  undef DDA_HISTOGRAM
#endif

/* Ultrix doesn't have syslog, so use printf instead.  Since the two
 * functions take different arg list formats, embed the open paren in
 * the defined symbol; provide DDAELOG to close the call while keeping
 * parentheses matched.	 The argument to DDALOG is ignored for printf;
 * for log(), debugging messages use LOG_DEBUG, all others use LOG_ERR.
 */
#if (ACC_BSD > 42 || ACC_VMS > 00) && !defined(SIMULATION)
#  include "syslog.h"
#  define DDALOG(s)	log( s,
#else
#  define DDALOG(s)	printf(
#endif
#define DDAELOG		)

#ifndef	DDADEBUG
#define	PRIVATE		static		/* hide our internal functions */
#else
#define	PRIVATE				/* let the world see them */
#endif

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								 %%*/
/*%%		       GLOBAL FUNCTIONS				 %%*/
/*%%								 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

int		ddaprobe();
int		ddaattach();
int		ddareset();
int		ddainit();
int		ddaoutput();
int		ddatimer();
int		ddaioctl();
int		ddainta();	/* service interrupt "a" from front end */
int		ddaintb();	/* service interrupt "b" from front end */


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								 %%*/
/*%%		       LOCAL  FUNCTIONS				 %%*/
/*%%								 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void send_config();
PRIVATE struct dda_cb *locate_x25_lcn();
PRIVATE struct dda_cb *find_free_lcn();
PRIVATE boolean	convert_ip_addr();
PRIVATE u_long	convert_x25_addr();
PRIVATE boolean	make_x25_call();
PRIVATE void	dda_start();
PRIVATE void	dda_rrq();
PRIVATE void	dda_wrq();
PRIVATE int	start_chn();
PRIVATE void	dda_data();
PRIVATE void	dda_supr();
PRIVATE void	supr_msg();
PRIVATE boolean	decode_ring();
PRIVATE void	decode_answer();
PRIVATE void	clear_lcn();
PRIVATE void	send_restart();
PRIVATE void	send_supr();
PRIVATE void	start_supr();
PRIVATE void	abort_io();
PRIVATE void	prt_addr();

#ifdef DDA_PAD_OR_RAW
PRIVATE int	dda_decode_type();
#endif

#ifdef DDA_PADOPT
PRIVATE void	x29_data();
PRIVATE void	x29_supr();
PRIVATE void	x29_init();
#endif DDA_PADOPT

#ifdef DDA_RAWOPT
PRIVATE void	pi_data();
PRIVATE void	pi_supr();
PRIVATE void	pi_init();
PRIVATE int	pi_circuit_to_handle_protocol();
#endif DDA_RAWOPT

#ifdef DDADEBUG
PRIVATE void	prt_bytes();
#endif

PRIVATE char    *fmt_x25();

#ifdef DDA_HISTOGRAM
PRIVATE void	hist_init();	/* histogram functions */
PRIVATE void	hist_lcn_state();
PRIVATE void	hist_all_lcns();
PRIVATE void	hist_link_state();
PRIVATE void	hist_read();
PRIVATE int	hist_copyout();

#else DDA_HISTOGRAM		/* make all histogram functions no-op's */
#define hist_init(a,b)
#define hist_lcn_state(a,b,c)
#define hist_all_lcns(a,b)
#define hist_link_state(a,b,c)
#define hist_read(a)
#define hist_copyout(a,b)
#endif DDA_HISTOGRAM


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								 %%*/
/*%%		       LOCAL  VARIABLES				 %%*/
/*%%								 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE int	tmo_data_idle = TMO_DATA_IDLE;	/* idle circuit timeout for
						 * all boards */

PRIVATE int	nddach[4] = {	/* number of channels currently in use */
			NDDACH_DEFAULT, NDDACH_DEFAULT,
			NDDACH_DEFAULT, NDDACH_DEFAULT };

PRIVATE char   *dda_product;	/* name of product, like "ACP5250" */
PRIVATE int	dda_hasmaint;	/* do we have a maintenance board? */

/* the message bits are used in the DMESG macros defined in if_ddavar.h */
/* word 1 and 2 (msgs 0   -  63)  are reserved for the IP interface	*/
/* word 3	(msgs 64  -  95)  are reserved for the PI interface	*/
/* word 4	(msgs 96  -  127) are reserved for the X.29 interface	*/
/* word 5 and 6 (msgs 128 -  191) are reserved for debugging main module*/
/* word 7	(msgs 192 -  223) are reserved for debugging the PI     */
/* word 8	(msgs 224 -  255) are reserved for debugging X29	*/
/* word 9	(msgs 256 -  287) are reserved for call logging		*/
#define NDMESGWORDS	9
#define MAXDMSGS	(NDMESGWORDS * 32)
PRIVATE long	ddamsgs[NDDA][NDMESGWORDS];
/*					       |  |  |  |   |   |   |   |   |
 * default:	all informational messages on, /--/--/--/   |   |   |   |   |
 *		all debug messages off,	--------------------/---/---/---/   |
 *		log busys, but not calls or I/O aborts ---------------------/
 */

/* Must be as large as the larger of (trtab, ddactl, dnload): */
char	dda_iobuf[sizeof(struct ddactl)];

struct dda_softc dda_softc[NDDA];	/* per device infomation */

/*  header for building command to be sent to the front end in	 */
/*  response to ACPCONFIG user command				 */

PRIVATE u_char acpconfig_msg[] = {
	LINE_CNTL,		/* set command code */
	0x00,			/* not used */
	0x00,			/* not used */
	0x00,			/* extension length (set at runtime) */
	0x00,			/* cmd space */
	0x00,
	0x00,
	0x00,
	0x00
};

PRIVATE u_char bfr_size_msg[] =
{
 SET_BFR_SIZE,			/* set command code */
 0x00,				/* not used */
 0x00,				/* not used */
 0x01,				/* extension length */
 0x00,				/* cmd space */
 0x00,
 0x00,
 0x00,
 0x00
};

PRIVATE u_char	ddacb_cmnd[4] = { CALL, 0, 0, 0 };
PRIVATE u_char	ddacb_called_addr[16] = {0};
PRIVATE u_char	ddacb_calling_addr[16] = {0};
PRIVATE u_char	ddacb_facilities[64] = {0};
PRIVATE u_char	ddacb_protocol[5] = {0};
PRIVATE u_char	ddacb_user_data[64] = {0};

#ifdef DDADEBUG
u_char		dda_silo_counter;
u_char		dda_debug_silo[256];
#endif

/* Table of baud rate values and the associated parameter for the Set	*/
/* System Parameters message, ddainit_msg.  The 'parameter1' is nonzero */
/* for valid baud rate divisors.  These are nominal baud rates.		*/

PRIVATE struct baud {
    char	    b_value;
    u_char	    parameter1; /* first byte of baud rate setting  */
    u_char	    parameter2; /* second byte of baud rate setting */
}		ddabaud_rate[] = {
    { 1, 0x02, 0x00 },		/* 2.00M */
    { 2, 0x03, 0x00 },		/* 1.33M */
    { 3, 0x04, 0x00 },		/* 1.00M */
    { 4, 0x08, 0x00 },		/* 500K	 */
    { 5, 0x10, 0x00 },		/* 250K	 */
    { 6, 0x28, 0x00 },		/* 100K	 */
    { 7, 0x3e, 0x00 },		/* 64K	 */
    { 8, 0x47, 0x00 },		/* 56K	 */
    { 9, 0x85, 0x00 },		/* 30K	 */
    { 10, 0xd0, 0x00 },		/* 19.2K */
    { 11, 0xa1, 0x01 },		/* 9600	 */
    { 12, 0x41, 0x03 },		/* 4800	 */
    { 13, 0x83, 0x06 },		/* 2400	 */
    { 14, 0x05, 0x0d },		/* 1200	 */
    { 0, 0, 0 }
};

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								 %%*/
/*%% Address Translation Table for Internet <-> X.25 addresses	 %%*/
/*%%								 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#define DDANATT 32	/* number of addr translation table entries */
PRIVATE int	dda_num_addr_tr[NDDA] = {0};	/* number of address
						 * translations */

 /* currently stored */
PRIVATE struct dda_addr_tr {	/* X.25 PDN address translation table */
    u_long	    ip_addr;			/* internet address */
    u_char	    x25_addr[MAXADDRLEN];	/* X.25 address */
}		dda_addr_tr[NDDA][DDANATT] = {{ 0L, ""}}; /* null */

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								 %%*/
/*%% Aliasing of IP address for 4.2 ==> 4.3 compatibility	 %%*/
/*%% Note: this union is not required in 4.2, since the s_net	 %%*/
/*%% field and its friends are in an include file.  We use it to %%*/
/*%% minimize the number of #ifdef dependencies in the code.	 %%*/
/*%%								 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifdef s_net			/* 4.2 */
# undef s_net
# undef s_host
# undef s_lh
# undef s_impno
#endif

union imp_addr {
    struct in_addr  ip;
    struct imp {
	u_char		s_net;
	u_char		s_host;
	u_char		s_lh;
	u_char		s_impno;
    }		    imp;
};

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								 %%*/
/*%%		       GLOBAL ROUTINES				 %%*/
/*%%								 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifdef	ACP_BI
#include "if_dda_bibus.c"
#else
#include "if_dda_uqbus.c"
#endif

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%			  DDAIOCTL()				 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*								   */
/*  Purpose:							   */
/*								   */
/*   This routine processes device dependent ioctl's.  Supported   */
/*   ioctls set the host's internet address for this network	   */
/*   interface, or send Set System Parameters Message to the ACP.  */
/*   The logic for setting the interface address must remain	   */
/*   compatible with both ifconfig and acpconfig programs.	   */
/*   If the ioctl comes from the acpconfig program, the front end  */
/*   is not initialized because the user will specify explicitly   */
/*   what parameters are desired.  If the ioctl comes from the	   */
/*   ifconfig program, the fron end is initialized with default	   */
/*   parameters in the ddainit_msg array.			   */
/*								   */
/*  Call:	     ddaioctl(ifp, cmd, data)			   */
/*  Argument:	     ifp:   pointer to the network interface data  */
/*				 structure, ifnet		   */
/*		     cmd:   identifies the type of ioctl	   */
/*		     data:  information for the ioctl		   */
/*  Returns:	     0 for success, or the nonzero error value:	   */
/*				  EINVAL invalid ioctl request	   */
/*  Called by:	      network software, address of this routine is */
/*		      defined in af_inet network interface struct  */
/*  Calls to:	      splimp()					   */
/*		      if_rtinit()				   */
/*		      in_netof()				   */
/*		      in_lnaof()				   */
/*		      ddainit()					   */
/*		      send_config()				   */
/*		      DDALOG()					   */
/*		      splx()					   */
/*								   */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifdef MULTINET
volatile int	StatQuery_Completed;	/* Polled for board stat ioctl */
#endif

ddaioctl(ifp, cmd, data)
register struct ifnet *ifp;
int		cmd;
caddr_t		data;
{
    register struct dda_softc *ds = &dda_softc[ifp->if_unit];
    struct ifreq   *ifr = (struct ifreq *) data;

#if defined(DDA_PADOPT) && defined(WINS)
    int		    prealloc_x29();	/* Preallocate UCBs for X29 */
#endif

#if ACC_BSD == 42 || ACC_ULTRIX == 12
    struct sockaddr_in *sin = (struct sockaddr_in *) & ifr->ifr_addr;
#else
    struct ifaddr  *ifa = ds->dda_if.if_addrlist;
#endif

    int		    s;
    int		    error = 0;
    int		    i;
    register struct dda_addr_tr *atp, *btp;
    struct trtab   *tr;
    struct ddactl  *da;
    char	    arg2[MAXADDRLEN], code;

#ifdef DDADEBUG
    if (DDADBCH(4, ifp->if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: ioctl()\n", ifp->if_unit DDAELOG;
    }
#endif DDADEBUG

    /*
     * This may not be necessary here, but under some flavors of BSDish
     * systems (2.0ULTRIX) this routine is apparently called at splimp(). In
     * the case that we are currently processing ioctls issued from acpconfig
     * in /etc/rc, the board may not have come on line yet - so we need to be
     * able to process the B interrupt while in the delay loop below. 
     */
#ifndef MULTINET
    s = spl0();
#endif

    switch (cmd) {
      case SIOCSIFADDR:
	if (!suser())
	    return EACCES;

#if ACC_BSD == 42 || ACC_ULTRIX == 12
	if (ifp->if_flags & IFF_RUNNING)
	    if_rtinit(ifp, -1);	/* delete previous route */
	ifp->if_addr = *(struct sockaddr *) sin;
	ifp->if_net = in_netof(sin->sin_addr);
	ifp->if_host[0] = in_lnaof(sin->sin_addr);
	if (ifp->if_flags & IFF_RUNNING)
	    if_rtinit(ifp, RTF_UP);
	else
	    ddainit(ifp->if_unit);
	ds->dda_ipaddr.s_addr = ((struct sockaddr_in *) & ifp->if_addr)->sin_addr.s_addr;
#else					 /* 4.3 networking */
	if (ifa->ifa_addr.sa_family != AF_INET)
	    return (EINVAL);
	if ((ifp->if_flags & IFF_RUNNING) == 0)
	    ddainit(ifp->if_unit);
	ds->dda_ipaddr = IA_SIN(ifa)->sin_addr;
#endif					/* 4.3 networking */
	break;

      case SIOCACPCONFIG:
	/* process ioctl from acpconfig program */

	code = *(ifr->ifr_data);

	/*********************************************************
	 *							 *
	 *	Commands n, h, q, and r are non-privileged	 *
	 *							 *
	 *********************************************************/

	if (!suser() && code != 'n' && code != 'h' && code != 'q' && code != 'r')
	    return EACCES;

#if ACC_BSD == 42 || ACC_ULTRIX == 12
	sin = (struct sockaddr_in *) & ds->dda_if.if_addr;
	if (in_netof(sin->sin_addr) == 0)
#else 
	if (ds->dda_if.if_addrlist == 0)
#endif 
	{
	    error = EDESTADDRREQ;	/* error, no internet address */
	    goto exit;
	}
	/* for command to set baud rate, look up the value for the  */
	/* baud rate divisor in the ddabaud_rate table, put value   */
	/* in the Set System Parameters message, ddainit_msg        */

	if (code >= 1 && code <= 14) {
	    register struct baud *p;

	    if (error = diags_completed(ds))
		goto exit;
	    for (p = ddabaud_rate; p->b_value; p++) {
		if ((*(ifr->ifr_data) - p->b_value) == 0)
		    break;
	    }
	    /* if internal clock not set, do so */
	    if ((ds->dda_init & DDA_INTCLOCK) == 0) {
		ds->dda_init |= DDA_INTCLOCK;
		acpconfig_msg[MSG_OFFSET] = CLOCK_CNTL;
		acpconfig_msg[MSG_OFFSET + 1] = INTERNAL_CLOCK;
		acpconfig_msg[MSG_OFFSET + 2] = BAUD_CNTL;
		acpconfig_msg[MSG_OFFSET + 3] = p->parameter1;
		acpconfig_msg[MSG_OFFSET + 4] = p->parameter2;
		acpconfig_msg[MSG_LENGTH] = 5;
	    } else {
		acpconfig_msg[MSG_OFFSET] = BAUD_CNTL;
		acpconfig_msg[MSG_OFFSET + 1] = p->parameter1;
		acpconfig_msg[MSG_OFFSET + 2] = p->parameter2;
		acpconfig_msg[MSG_LENGTH] = 3;
	    }

	    if ((p->b_value == 0) || (p->parameter1 == 0))
		error = EINVAL;	/* baud rate value invalid */
	    else
		send_config(ds, acpconfig_msg);	/* send message to front end */
	    goto exit;
	}
	switch (code) {
	  case 'a':		/* Add address translation table entry */
	    if (error = diags_completed(ds))
		goto exit;
	    if (dda_num_addr_tr[ifp->if_unit] >= DDANATT) {	/* table already full */
		error = ENOMEM;
		goto exit;
	    }

	    /*
	     * Copy in user arguments and point "tr" at them.  Then scan the
	     * translation table and either find location to insert or flag
	     * error 
	     */
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct trtab))) {
		error = EFAULT;
		goto exit;
	    }
	    tr = (struct trtab *) dda_iobuf;
	    for (i = 0, atp = &dda_addr_tr[ifp->if_unit][0];
		 i < dda_num_addr_tr[ifp->if_unit]; i++, atp++) {
		if (atp->ip_addr == tr->ipaddr) {
		    if (bcmp((char *) atp->x25_addr,
			     (char *) tr->x25addr, MAXADDRLEN)) {
			error = EADDRINUSE;
			goto exit;
		    } else /* addresses are the same, just ignore ioctl */
			goto exit;
		}
		if (atp->ip_addr > tr->ipaddr)	/* insert entry here */
		    break;
	    }
	    for (btp = &dda_addr_tr[ifp->if_unit][dda_num_addr_tr[ifp->if_unit]];
		 btp > atp; btp--) {	/* open up space for a new entry */
		btp->ip_addr = (btp - 1)->ip_addr;
		bcopy((btp - 1)->x25_addr, btp->x25_addr, MAXADDRLEN);
	    }
	    atp->ip_addr = tr->ipaddr;
	    bcopy(tr->x25addr, atp->x25_addr, MAXADDRLEN);
	    dda_num_addr_tr[ifp->if_unit]++;	/* one more table entry */
	    goto exit;

	  case 'D':
	    if (error = diags_completed(ds))
		goto exit;
	    /* clear table for use by 'r' flag of acpconfig */
	    for (i = 0, atp = &dda_addr_tr[ifp->if_unit][0];
		 i < dda_num_addr_tr[ifp->if_unit]; i++, atp++) {
		atp->ip_addr = 0L;
		atp->x25_addr[0] = 0;
	    }
	    dda_num_addr_tr[ifp->if_unit] = 0;
	    goto exit;

	  case 'd':		/* Delete address translation table entry */
	    if (error = diags_completed(ds))
		goto exit;
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct trtab))) {
		error = EFAULT;
		goto exit;
	    }
	    tr = (struct trtab *) dda_iobuf;
	    error = EFAULT;	/* in case inet address not in table */
	    for (i = 0, atp = &dda_addr_tr[ifp->if_unit][0];
		 i < dda_num_addr_tr[ifp->if_unit]; i++, atp++) {
		if (atp->ip_addr == tr->ipaddr) {
		    error = 0;	/* found it: cancel error */
		    for (; i < dda_num_addr_tr[ifp->if_unit] - 1; i++, atp++) {
			atp->ip_addr = (atp + 1)->ip_addr;
			bcopy((atp + 1)->x25_addr, atp->x25_addr, MAXADDRLEN);
		    }
		    atp->ip_addr = 0L;	/* clear last vacated entry */
		    atp->x25_addr[0] = 0;
		    dda_num_addr_tr[ifp->if_unit]--;	/* one fewer table
							 * entries */
		    break;
		}
	    }
	    goto exit;


	  case 'f':		/* -f facility status */

	    /*
	     * The first byte of the "msg" selects the flow control parameter
	     * and the "drval" field holds the status (on or off). 
	     */
	    if (error = diags_completed(ds))
		goto exit;
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct ddactl))) {
		error = EFAULT;
		goto exit;
	    }
	    if (ds->dda_firmrev < 0x21) {	/* need 2.0 or above ROMs */
		error = ENOPROTOOPT;
		goto exit;
	    }
	    da = (struct ddactl *) dda_iobuf;
	    switch (da->msg[0]) {
	      case 0:		/* packet */
		if (da->drval)
		    ds->dda_init |= DDA_PKTNEG;
		else
		    ds->dda_init &= ~DDA_PKTNEG;
		break;
	      case 1:		/* window */
		if (da->drval)
		    ds->dda_init |= DDA_WNDNEG;
		else
		    ds->dda_init &= ~DDA_WNDNEG;
		break;
	    }
	    goto exit;

	  case 'o':		/* Set options */
	    if (error = diags_completed(ds))
		goto exit;
	    if (ds->dda_firmrev < 0x21) {	/* need 2.1 or above ROMs */
		error = ENOPROTOOPT;
		goto exit;
	    }
	    if (ds->dda_state != S_DISABLED) {	/* must bring link down */
		error = EINPROGRESS;
		goto exit;
	    }
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct ddactl))) {
		error = EFAULT;
		goto exit;
	    }
	    da = (struct ddactl *) dda_iobuf;
	    acpconfig_msg[MSG_OFFSET] = PKT_OPTIONS;
	    acpconfig_msg[MSG_OFFSET + 1] = da->msg[0];
	    acpconfig_msg[MSG_LENGTH] = 2;
#ifdef DDADEBUG
	    if (DDADBCH(4, ifp->if_unit)) {
		DDALOG(LOG_DEBUG) "dda%d: acpconfig_msg is %x %x %x\n",
		    ifp->if_unit, acpconfig_msg[MSG_LENGTH],
		    acpconfig_msg[MSG_OFFSET], acpconfig_msg[MSG_OFFSET + 1] DDAELOG;
	    }
#endif DDADEBUG

	    send_config(ds, acpconfig_msg);
	    goto exit;

	  case 'N':		/* read network id */
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct ddactl))) {
		error = EFAULT;
		goto exit;
	    }
	    da = (struct ddactl *) dda_iobuf;
	    ds->dda_net_id = da->drval;
	    goto exit;

	  case 'r':		/* Read address translation table entry */

	    /*
	     * The value stored in "ipaddr" is not an address, but an index
	     * of a translation table entry to read out.  The x25_addr field
	     * in the input structure is not used. 
	     */
	    if (error = diags_completed(ds))
		goto exit;
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct trtab))) {
		error = EFAULT;
		goto exit;
	    }
	    tr = (struct trtab *) dda_iobuf;
	    i = tr->ipaddr;
	    if (i >= DDANATT) {	/* scanned the whole table */
		error = EFAULT;
		goto exit;
	    }
	    tr->ipaddr = dda_addr_tr[ifp->if_unit][i].ip_addr;
	    bcopy(dda_addr_tr[ifp->if_unit][i].x25_addr, tr->x25addr, MAXADDRLEN);
	    if (copyout(tr, ifr->ifr_data, sizeof(struct trtab)))
		error = EFAULT;
	    goto exit;

#ifdef DDA_HISTOGRAM
	  case 'h':		/* read histogram */
	    if (error = diags_completed(ds))
		goto exit;
	    hist_read(ifp->if_unit);
	    if (hist_copyout(ifp->if_unit, ifr->ifr_data))
		error = EFAULT;
	    goto exit;

	  case 'H':		/* read and reset histogram */
	    if (error = diags_completed(ds))
		goto exit;
	    hist_read(ifp->if_unit);
	    if (hist_copyout(ifp->if_unit, ifr->ifr_data))
		error = EFAULT;
	    else
		hist_init(ifp->if_unit, 1);
	    goto exit;
#endif DDA_HISTOGRAM

	  case 'v':		/* -v variable value */

	    /*
	     * There are two "variables" in the driver which can be set via
	     * ioctl: packet size, and window size.  The "drval" field holds
	     * the value and the first byte of the "msg" selects the variable.
	     * Note that the selector is another little undocumented piece of
	     * the interface between here and the acpconfig program. It is
	     * coupled to the ordering of a little string table inside that
	     * program; new parameters should be added at the end, not the
	     * middle! 
	     */
	    /* No check to see if powerup diags are completed */
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct ddactl))) {
		error = EFAULT;
		goto exit;
	    }
	    da = (struct ddactl *) dda_iobuf;
	    switch (da->msg[0]) {
	      case 0:		/* set logging (obsolete) */
	      case 1:		/* set debug (obsolete) */
	      case 2:		/* set debug unit (obsolete) */
		error = EINVAL;
		break;

		/*
		 * For both packet and window sizes, we check that the link
		 * is currently down.  The new parameters will be sent to the
		 * FEP when the link is next brought up.  See processing for
		 * -u flag. 
		 */
	      case 3:		/* set packetsize */
		if (error = diags_completed(ds))
		    goto exit;
		if (ds->dda_firmrev < 0x21) {	/* need 2.1 or above ROMs */
		    error = ENOPROTOOPT;
		    goto exit;
		}
		if (ds->dda_state != S_DISABLED) {	/* must bring link down */
		    error = EINPROGRESS;
		    goto exit;
		}

		/*
		 * X.25 (1984) section 7.2.2.1.1 says 12 (4096 byte packets)
		 * BBN report 5760 (September 1984) 14.2.1.2 says 10. We just
		 * check for 12. 
		 */
		if (da->drval < 4 || da->drval > 12)
		    error = EINVAL;
		else {
		    int             packetsize = 1 << da->drval;

		    acpconfig_msg[MSG_LENGTH] = 3;
		    acpconfig_msg[MSG_OFFSET] = MAX_PKT_SZ;	/* Max negotiable */
		    /* pkt size */
		    acpconfig_msg[MSG_OFFSET + 1] = packetsize & 0xFF;
		    acpconfig_msg[MSG_OFFSET + 2] = (packetsize >> 8) & 0xFF;
		    send_config(ds, acpconfig_msg);
		}
		break;

	      case 4:		/* set windowsize */
		if (error = diags_completed(ds))
		    goto exit;
		if (ds->dda_firmrev < 0x21) {	/* need 2.0 or above ROMs */
		    error = ENOPROTOOPT;
		    goto exit;
		}
		if (ds->dda_state != S_DISABLED) {	/* must bring link down */
		    error = EINPROGRESS;
		    goto exit;
		}
		if (da->drval < 1 || da->drval > 127)
		    error = EINVAL;
		else {
		    acpconfig_msg[MSG_LENGTH] = 2;
		    acpconfig_msg[MSG_OFFSET] = MAX_PKT_WN;	/* Max negotiable */
		    /* pkt window */
		    acpconfig_msg[MSG_OFFSET + 1] = da->drval;
		    send_config(ds, acpconfig_msg);
		}
		break;
	    }
	    goto exit;

	  case 'm':		/* -m message */
	    if (error = diags_completed(ds))
		goto exit;
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct ddactl))) {
		error = EFAULT;
		goto exit;
	    }
	    da = (struct ddactl *) dda_iobuf;
	    send_config(ds, da->msg);
	    goto exit;

	  case 'n':		/* -n svc_count */
	    if (error = diags_completed(ds))
		goto exit;
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct ddactl))) {
		error = EFAULT;
		goto exit;
	    }
	    if (ds->dda_firmrev < 0x21) {	/* need 2.1 or above ROMs */
		error = ENOPROTOOPT;
		goto exit;
	    }
	    da = (struct ddactl *) dda_iobuf;
	    i = 0;		/* i holds the return value */
	    if (da->drval == 0)
		i = nddach[ifp->if_unit];
	    else if (ds->dda_state != S_DISABLED) {	/* must bring link down */
		error = EINPROGRESS;
		goto exit;
	    } else {
		if (!suser()) {
		    error = EACCES;
		    goto exit;
		}
		if (da->drval < 1 || da->drval > NDDACH)
		    error = EINVAL;
		else {
		    acpconfig_msg[MSG_LENGTH] = 2;
		    acpconfig_msg[MSG_OFFSET] = SVC_LIMIT;
		    acpconfig_msg[MSG_OFFSET + 1] = da->drval;
		    nddach[ifp->if_unit] = da->drval;
		    send_config(ds, acpconfig_msg);
		}
	    }
	    if (copyout(&i, ifr->ifr_data, sizeof(int)))
		error = EFAULT;
	    goto exit;

	  case 'c':		/* -c msgnum  -- dis/enable driver mesg */
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct ddactl))) {
		error = EFAULT;
		goto exit;
	    }
	    da = (struct ddactl *) dda_iobuf;
	    if (da->drval < 0 || da->drval >= MAXDMSGS)
		error = EINVAL;
	    else {
		u_char          new_val;

		DMESGTOG(ifp->if_unit, da->drval);
		new_val = DMESGVAL(ifp->if_unit, da->drval) ? 1 : 0;
		/* 1 means disabled, 0 means enabled */
		if (copyout(&new_val, ifr->ifr_data, sizeof(u_char)))
		    error = EFAULT;
	    }
	    goto exit;

	  case 't':		/* -t sec  -- set data idle timeout */
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct ddactl))) {
		error = EFAULT;
		goto exit;
	    }
	    da = (struct ddactl *) dda_iobuf;
	    if (da->drval < 1)
		error = EINVAL;
	    else
		tmo_data_idle = da->drval / DDA_TIMEOUT;
	    goto exit;

	  case 'q':		/* driver/FE/shm/silo state queries */
	    if (copyin(ifr->ifr_data, dda_iobuf, sizeof(struct ddactl))) {
		error = EFAULT;
		goto exit;
	    }
	    da = (struct ddactl *) dda_iobuf;
	    switch (da->msg[0]) {
	    case 0:		/* front end state query */
		if ((error = diags_completed(ds)) == 0) {
		    int s2 = splimp();

		    /* need 2.0 or above ROMs */
		    if (ds->dda_firmrev < 0x21) {
			error = ENOPROTOOPT;
			splx(s2);	/* We got it and woke up */
			break;
		    }
#ifdef	MULTINET
		    StatQuery_Completed = 0;
		    send_supr(ds, STATQUERY, 0, 0);
		    splx(s2);	/* drop ioctl so we can be scheduled */
		    while (!StatQuery_Completed);
#else	MULTINET
		    send_supr(ds, STATQUERY, 0, 0);
		    sleep(dda_iobuf, PSLEP); /* Interruptible with ^C */
		    splx(s2);	/* We got it and woke up */
#endif	MULTINET

		    if (copyout(dda_iobuf, ifr->ifr_data,
			     sizeof(struct ddactl)))
			error = EFAULT;
		}
		break;
	    case 1:		/* driver state query */
		da->msg[0] = ds->dda_state;
		da->msg[1] = ds->dda_init;
		da->msg[2] = ds->dda_flags;
		da->msg[3] = ds->dda_firmrev;
		if (copyout(dda_iobuf, ifr->ifr_data,
			     sizeof(struct ddactl)))
		    error = EFAULT;
		break;
#ifdef DDADEBUG
	    case 2:		/* debug query */
		if (copyout(dda_debug_silo, ifr->ifr_data, 256))
		    error = EFAULT;
		break;
#endif
#if defined(DDADEBUG) && defined(ACP_BI)
	    case 3:		/* shm/biic query (temporary) */
		{
		    register struct uba_device *ui = ddainfo[ifp->if_unit];
		    dda_dump_shm((SYSGEN_BLOCK *) ds->dda_mapreg);
		    dda_dump_biic_regs((struct biic_regs *) ui->ui_addr);
                }
		break;
#endif
	    default:
		error = EINVAL;
	    }
	    goto exit;

	  case '0':		/* -u 0 */
	    if (error = diags_completed(ds))
		goto exit;
	    acpconfig_msg[MSG_OFFSET] = LINK_DISABLE;
	    acpconfig_msg[MSG_LENGTH] = 1;
	    hist_link_state(ifp->if_unit, ds->dda_state, S_GOING_DOWN);
	    ds->dda_state = S_GOING_DOWN;
	    break;
	  case '1':		/* -u 1 */
	    if (error = diags_completed(ds))
		goto exit;
	    acpconfig_msg[MSG_OFFSET] = LINK_LOOPBACK;
	    acpconfig_msg[MSG_OFFSET + 1] = LOOP_NONE;
	    acpconfig_msg[MSG_OFFSET + 2] = DTE_DCE_MODE;
	    acpconfig_msg[MSG_OFFSET + 3] = DTE;
	    acpconfig_msg[MSG_OFFSET + 4] = LINK_ENABLE;
	    acpconfig_msg[MSG_LENGTH] = 5;
	    ds->dda_state = S_COMING_UP;
	    hist_init(ifp->if_unit, 0);
	    break;
	  case '2':		/* -u 2 */
	    if (error = diags_completed(ds))
		goto exit;
	    acpconfig_msg[MSG_OFFSET] = LINK_LOOPBACK;
	    acpconfig_msg[MSG_OFFSET + 1] = LOOP_NONE;
	    acpconfig_msg[MSG_OFFSET + 2] = DTE_DCE_MODE;
	    acpconfig_msg[MSG_OFFSET + 3] = DCE;
	    acpconfig_msg[MSG_OFFSET + 4] = LINK_ENABLE;
	    acpconfig_msg[MSG_LENGTH] = 5;
	    ds->dda_state = S_COMING_UP;
	    hist_init(ifp->if_unit, 0);
	    break;
	  case '3':		/* -u 3 */
	    if (error = diags_completed(ds))
		goto exit;
	    acpconfig_msg[MSG_OFFSET] = LINK_LOOPBACK;
	    acpconfig_msg[MSG_OFFSET + 1] = LOOP_EXTERNAL;
	    acpconfig_msg[MSG_OFFSET + 2] = LINK_ENABLE;
	    acpconfig_msg[MSG_LENGTH] = 3;
	    ds->dda_state = S_COMING_UP;
	    hist_init(ifp->if_unit, 0);
	    break;
	  case '4':		/* -u 4 */
	    if (error = diags_completed(ds))
		goto exit;
	    acpconfig_msg[MSG_OFFSET] = LINK_LOOPBACK;
	    acpconfig_msg[MSG_OFFSET + 1] = LOOP_INTERNAL;
	    acpconfig_msg[MSG_OFFSET + 2] = LINK_ENABLE;
	    acpconfig_msg[MSG_LENGTH] = 3;
	    ds->dda_state = S_COMING_UP;
	    hist_init(ifp->if_unit, 0);
	    break;
	  case 'b':		/* -b 0 */
	    if (error = diags_completed(ds))
		goto exit;
	    acpconfig_msg[MSG_OFFSET] = CLOCK_CNTL;
	    acpconfig_msg[MSG_OFFSET + 1] = EXTERNAL_CLOCK;
	    acpconfig_msg[MSG_LENGTH] = 2;
	    ds->dda_init &= ~DDA_INTCLOCK;
	    break;
	  case 'S':		/* select DDN standard X.25 service */
	    /* -s 0 or -s standard */
	    if (error = diags_completed(ds))
		goto exit;
	    if (ds->dda_if.if_flags & IFF_UP && ds->dda_init & DDA_PDN) {
		error = EALREADY;
		goto exit;	/* no PDN->DDN mode change if running */
	    }
	    ds->dda_init &= ~(DDA_BASIC | DDA_PDN);
	    ds->dda_init |= DDA_STANDARD;
	    goto exit;
	  case 'T':		/* select DDN basic X.25 service */
	    /* -s 1 or -s basic */
	    if (error = diags_completed(ds))
		goto exit;
	    if (ds->dda_if.if_flags & IFF_UP && ds->dda_init & DDA_PDN) {
		error = EALREADY;
		goto exit;	/* no PDN->DDN mode change if running */
	    }
	    ds->dda_init &= ~(DDA_PDN | DDA_STANDARD);
	    ds->dda_init |= DDA_BASIC;
	    goto exit;
	  case 'U':		/* select X.25 Public Data Network service */
	    /* -s 2 or -s pdn */
	    if (error = diags_completed(ds))
		goto exit;
	    if (ds->dda_if.if_flags & IFF_UP && (ds->dda_init &
					      (DDA_BASIC | DDA_STANDARD))) {
		error = EALREADY;
		goto exit;	/* no DDN->PDN mode change if running */
	    }
	    ds->dda_init &= ~(DDA_BASIC | DDA_STANDARD);
	    ds->dda_init |= DDA_PDN;
	    goto exit;

	  case 'e':		/* set buffer size */
	    /* -e size size is encoded in second byte */
	    if (error = diags_completed(ds))
		goto exit;

	    /*
	     * check to see if we have newer at least version 2.2 roms. 
	     */
	    if (ds->dda_firmrev < 0x22) {
		error = ENOPROTOOPT;
		goto exit;
	    }
	    if (ds->dda_if.if_flags & IFF_UP) {
		error = EALREADY;
		goto exit;	/* no PDN->DDN mode change if running */
	    }
	    bfr_size_msg[MSG_OFFSET] = ifr->ifr_data[1];
	    send_config(ds, bfr_size_msg);
	    bufreset(ifp->if_unit);
	    goto exit;

#ifdef	ACP_BI
	  case 'L':
	    {	struct dda_dnload dl;
		if (copyin(ifr->ifr_data, &dl, sizeof(struct dda_dnload))) {
		    error = EFAULT;
		    goto exit;
		}
		error = dda_dload(ifp->if_unit, &dl);
		goto exit;
	    }
#endif

#if defined(DDA_PADOPT) && defined(WINS)
	  case 'x':		/* Preallocate UCBs for X29 -- VMS only */
	    printf("Preallocated %d PTYs for X29\n", prealloc_x29());
	    goto exit;
#endif

	  case 'z':		/* reset specified front-end device, -z */
/* second parm is supposed to be uban, but ddareset doesn't care about it */
	    ddareset(ifp->if_unit, 0);
	    goto exit;
	  default:
	    error = EINVAL;
	    goto exit;
	}
	if ((*(ifr->ifr_data) != '0') && (ds->dda_init & DDA_PDN) &&
#if ACC_BSD == 42 || ACC_ULTRIX == 12
	    (convert_ip_addr(sin->sin_addr, (u_char *) arg2, ds) == 0)
#else
	    (convert_ip_addr(ds->dda_ipaddr, (u_char *) arg2, ds) == 0)
#endif
	    ) {
	    error = EADDRNOTAVAIL;
	    goto exit;
	}
	send_config(ds, acpconfig_msg);
	break;

      default:
	error = EINVAL;
    }

exit:
#ifndef	MULTINET
    splx(s);
#endif
    return (error);
}


/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDAOUTPUT()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*   This routine is called by the network software when it has an */
/*   IP datagram to send out this interface.  An attempt is made   */
/*   to find a LCN which has a virtual circuit open to the         */
/*   indicated host.  If an LCN is found the packet is queued for  */
/*   output on that LCN.                                           */
/*                                                                 */
/*  Call:            ddaoutput(ifp, m0, dst)                       */
/*  Arguments:       ifp:  locates the network interface, ifnet    */
/*                   m0:   locates an mbuf buffer                  */
/*                   dst:  is the socket destination address       */
/*  Returns:         0 for success, or one of following nonzero    */
/*                        error indications:                       */
/*                               ENETDOWN                          */
/*                               EAFNOSUPPORT                      */
/*                               ENOBUFS                           */
/*  Called by:     network software, address of this routine is    */
/*                 defined in the dda_if network interface struct  */
/*  Calls to:      DDALOG()                                        */
/*                 m_freem()                                       */
/*                 splimp()                                        */
/*                 locate_x25_lcn()                                */
/*                 IF_QFULL()                                      */
/*                 IF_DROP()                                       */
/*                 splx()                                          */
/*                 IF_ENQUEUE()                                    */
/*                 dda_start()                                     */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

ddaoutput(ifp, m0, dst)
struct ifnet   *ifp;
struct mbuf    *m0;
struct sockaddr_in *dst;
{
    register struct mbuf *m = m0;
    register struct dda_softc *ds = &dda_softc[ifp->if_unit];
    register struct dda_cb *dc;
    register struct ifqueue *oq;
    struct mbuf    *prev;
    int             s;
    union imp_addr  imp_addr;

#ifdef DDADEBUG
    if (DDADBCH(1, ifp->if_unit)) {
	imp_addr.ip = dst->sin_addr;
	DDALOG(LOG_DEBUG) "dda%d: ddaoutput: dst = %d.%d.%d.%d\n",
	    ifp->if_unit, imp_addr.imp.s_net, imp_addr.imp.s_host,
	    imp_addr.imp.s_lh, imp_addr.imp.s_impno DDAELOG;
    }
#endif DDADEBUG

    if ((ds->dda_if.if_flags & IFF_UP) == 0)
	return (ENETDOWN);

    switch (dst->sin_family) {

#ifdef INET
      case AF_INET:
	break;
#endif INET

      default:
	DMESG(ifp->if_unit, 2,
	      (DDALOG(LOG_ERR) "dda%d: can't handle af%d\n", ifp->if_unit,
	       dst->sin_family DDAELOG));
	m_freem(m0);
	return (EAFNOSUPPORT);
    }

#ifdef DDADEBUG
    if (DDADBCH(2, ifp->if_unit)) {
	imp_addr.ip = dst->sin_addr;
	DDALOG(LOG_DEBUG) "dda%d: ddaoutput: dst = %d.%d.%d.%d\n",
	    ifp->if_unit, imp_addr.imp.s_net, imp_addr.imp.s_host,
	    imp_addr.imp.s_lh, imp_addr.imp.s_impno DDAELOG;
    }
#endif DDADEBUG

    /* Mod to V1.5b ==> V1.5b1 */
    /* In 4.3, the IP code may pass mbuf chains with 0-length mbufs */
    /* This causes "transfer count = 0" messages and might even     */
    /* cause actual garbage data transmission if the mbuf is at the */
    /* end of the chain (we don't think it ever will be, but one    */
    /* can't be too sure...so we scan the chain first).		    */
    /* WE DO ASSUME that there is at least one nonempty mbuf!	    */
    /* (ULTRIX: we don't know, but the code is at worst harmless)   */

    while (m0->m_len == 0) {
	m = m0;
	m0 = m0->m_next;
	m->m_next = 0;
	m_freem(m);
    }
    /* Now we know the first mbuf (at m0)  is not zero length	    */
    prev = m0;
    m = m0->m_next;
    while (m) {
	if (m->m_len == 0) {
	    prev->m_next = m->m_next;
	    m->m_next = 0;
	    m_freem(m);
	    m = prev->m_next;
	} else {
	    prev = m;
	    m = m->m_next;
	}
    }
    m = m0;			/* reset m to beginning of modified chain */

    s = splimp();		/* disable interrrupts */

    /* try to find an LCN */

    if (dc = locate_x25_lcn(ds, dst->sin_addr)) {	/* if found */
#ifdef DDADEBUG
	if (DDADBCH(2, ifp->if_unit)) {
	    DDALOG(LOG_DEBUG) "dda%d: ddaoutput: lcn found = %d\n", ifp->if_unit,
		dc->dc_lcn DDAELOG;
	}
#endif DDADEBUG
	oq = &(dc->dc_oq);	/* point to output queue */
	if (IF_QFULL(oq)) {	/* if q full */
	    IF_DROP(oq);	/* drop the data */
	    m_freem(m);
	    ds->dda_if.if_collisions++;	/* for netstat display */
	    splx(s);
	    return (ENOBUFS);
	}
	IF_ENQUEUE(oq, m);	/* otherwise queue it */
	dda_start(ds, dc);	/* and try to output */
	splx(s);
	return (0);
    } else {			/* if no circuit available */
	IF_DROP(&ifp->if_snd);	/* drop the data */
	m_freem(m);
	splx(s);
	return (EHOSTUNREACH);
    }

}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDATIMER()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  This routine is entered to perform timer management. The       */
/*  LCN table is scanned for active timers (nonzero) which are     */
/*  decremented.  If a timer expires (becomes zero), the proper    */
/*  action is taken.                                               */
/*                                                                 */
/*                                                                 */
/*  Call:              ddatimer(unit)                              */
/*  Arguments:         unit:  ACP device unit number               */
/*  Returns:           nothing                                     */
/*  Called by:         ddainit()                                   */
/*  Calls to:          splimp()                                    */
/*                     send_restart()                              */
/*                     clear_lcn()                                 */
/*                     splx()                                      */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

int
ddatimer(unit)
int             unit;
{
    register struct dda_softc *ds = &dda_softc[unit];
    register struct dda_cb *dc;
    register int    s, lcn;

#ifdef DDADEBUG
    if (DDADBCH(3, unit)) {
	DDALOG(LOG_DEBUG) "dda%d: ddatimer()\n", unit DDAELOG;
    }
#endif DDADEBUG

    ds->dda_if.if_timer = DDA_TIMEOUT;	/* restart timer */

    dc = ds->dda_cb;

    s = splimp();

    /* LCNLINK */
    for (lcn = 0; lcn <= nddach[unit]; lcn++) {	/* scan all LCN's */
#ifdef DDADEBUG
	if (dc->dc_out_t && lcn > 0 && (--(dc->dc_out_t) == 0)) {
	    DDALOG(LOG_DEBUG) "dda%d: write completion timeout lcn %d\n",
		unit, lcn DDAELOG;
	}
#endif
	if (dc->dc_timer && (--(dc->dc_timer) == 0)) {	/* if a timer expired */
	    if (dc->dc_state == LC_RESTART) {	/* if a restart was out */
		send_restart(ds);	/* send another one */
		break;
	    } else {		/* otherwise */
#ifdef DDA_PAD_OR_RAW
		/* if it is not an X.29 connection */
		if ((dc->dc_flags & (DC_X29W | DC_X29 | DC_RAW)) == 0)
#endif
		    clear_lcn(ds, dc);	/* clear the LCN */
	    }
	}
	dc++;
    }
    splx(s);
}


/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                     DIAGS_COMPLETED()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*    This routine checks to see that power up diagnostics have completed*/
/*    It waits for a while if necessary.                                 */
/*  Call:          diags_completed(ds)                                   */
/*  Argument:      ds - pointer to softc structure;                      */
/*  Returns:       0 if board is up, EINTR if it never came on line.     */
/*  Called by:     ddaioctl()                                            */
/*  Calls to:                                                            */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

diags_completed(ds)
struct dda_softc *ds;
{
    int             nretries = 10;

    /*
     * It's overkill to check this on EVERY ioctl, because it only matters if
     * we are going to touch the board.  But the driver has had repeated
     * problems with not checking it when it should have - overkill is
     * preferred.  The delays are here rather then in the acpconfig program
     * due to a bug in acpconfig.  They will only be executed during
     * /etc/rc.local when the board has not had a chance to do the "B"
     * interrupt yet.  At that time the machine will be running essentially
     * single thread so it won't really matter where the delays are.  (ie, if
     * we put the delay into acpconfig and kept calling here 10 times, the
     * machine would not do anything else useful in the meantime - might as
     * well loop here). 
     */
    while (((ds->dda_flags & DDAF_OK) == 0) && nretries-- > 0)
	DELAY(3000000);
    if ((ds->dda_flags & DDAF_OK) == 0)	/* never came on line... */
	return (EINTR);
    else
	return (0);
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                             %%*/
/*%%                   LOCAL  FUNCTIONS                          %%*/
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      SEND_CONFIG()                          %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*   Send a Set System Parameters Message to the front end in      */
/*   response to an ACPCONFIG command from the user.               */
/*                                                                 */
/*  Call:          send_config(ds, p)                              */
/*  Argument:      ds:  pointer to ACP device control structure    */
/*		   p: pointer to the message			   */
/*  Returns:           nothing                                     */
/*  Called by:         ddaioctl()                                  */
/*  Calls to:          MGET()                                      */
/*                     DDALOG()                                    */
/*                     mtod()                                      */
/*                     bcopy()                                     */
/*                     sizeof()                                    */
/*                     start_supr()                                 */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
send_config(ds, p)
struct dda_softc *ds;
u_char		 *p;
{
    struct mbuf    *m;
    register u_char *bp;
    int             length;

#ifdef DDADEBUG
    if (DDADBCH(7, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: send_config()\n", ds->dda_if.if_unit DDAELOG;
    }
#endif DDADEBUG

    MGET(m, M_DONTWAIT, MT_DATA);
    if (m == 0) {
	DMESG(ds->dda_if.if_unit, 18,
	      (DDALOG(LOG_ERR) "dda%d: can't get bfr for acpconfig msg\n",
	       ds->dda_if.if_unit DDAELOG));
	return;
    }
    bp = mtod(m, u_char *);	/* point to data section of mbuf */

    length = p[MSG_LENGTH] + MSG_OFFSET;	/* msg length */
    if (length > MLEN - 1) {

	/*
	 * Supervisory messages have to fit in a small mbuf.  The driver
	 * itself is careful never to get in trouble this way, but now that
	 * we have "-m" the user could.  Dropping such a message is not
	 * likely to cause any big problems, and the user can rephrase the
	 * request. 
	 */
	DMESG(ds->dda_if.if_unit, 19,
	      (DDALOG(LOG_ERR) "dda%d: supervisor message too long\n",
	       ds->dda_if.if_unit DDAELOG));
	m->m_next = 0;
	m_freem(m);
	return;
    }
    bcopy(p, bp, length);
    m->m_len = length;		/* set msg length */

#ifdef DDADEBUG
    if (DDADBCH(8, ds->dda_if.if_unit)) {
	prt_bytes(ds->dda_if.if_unit, "send_config", bp, length);
    }
#endif DDADEBUG

    start_supr(ds, m);
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      LOCATE_X25_LCN()                       %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine tries to locate an X25 LCN associated with a    */
/*    remote internet address.  A linear search of the LCN table   */
/*    is made for a matching address.  If the search succeeds, the */
/*    LCN is returned.  If the search fails, the LCN table is      */
/*    searched for an unused table entry.  If an unused table      */
/*    entry is found, an X25 call is generated to the host         */
/*    specified in the destination internet address.  If no LCN is */
/*    available, zero is returned.                                 */
/*                                                                 */
/*  Call:              locate_x25_lcn(ds, ip_addr)                 */
/*  Argument:          ds:   pointer to dev control block struct   */
/*                     ip_addr:  IP address                        */
/*  Returns:           pointer to the dda control block which      */
/*                     contains LCN, else zero for failure         */
/*  Called by:         ddaoutput()                                 */
/*  Calls to:          convert_ip_addr()                           */
/*                     make_x25_call()                             */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE struct dda_cb *
locate_x25_lcn(ds, ip_addr)
struct dda_softc *ds;
struct in_addr  ip_addr;
{
    register int    lcn, maxlcn;
    register struct dda_cb *dc;
    union imp_addr  imp_addr;

#ifdef DDADEBUG
    if (DDADBCH(9, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: locate_x25_lcn()\n",
	    ds->dda_if.if_unit DDAELOG;
    }
#endif DDADEBUG

    imp_addr.ip = ip_addr;	/* DDN X.25 doesn't know net number */

    if (!(ds->dda_init & DDA_PDN)) {
	if ((imp_addr.imp.s_net & 0x80) == 0x00) {	/* class A */
	    imp_addr.imp.s_net = 0;
	    imp_addr.imp.s_lh = 0;
	} else if ((imp_addr.imp.s_net & 0xc0) == 0x80) {	/* class B */
	    imp_addr.imp.s_net = 0;
	    imp_addr.imp.s_host = 0;
	} else {		/* class C, should check for class C */
	    imp_addr.imp.s_net = 0;
	    imp_addr.imp.s_host = 0;
	    imp_addr.imp.s_lh = 0;
	}
    }
    /* LCNLINK */
    maxlcn = nddach[ds->dda_if.if_unit];
    dc = &(ds->dda_cb[1]);	/* scan LCN table for addr match */
    for (lcn = 1; lcn <= maxlcn; lcn++) {
	if ((dc->dc_key.key_addr.s_addr == imp_addr.ip.s_addr)
	&& (dc->dc_state == LC_CALL_PENDING || dc->dc_state == LC_DATA_IDLE))
	    return (dc);	/* return LCN */
	dc++;
    }

    if ((dc = find_free_lcn(ds)) == 0)
	return (0);

    ddacb_user_data[0] = (u_char) 0;		/* we have no user data */

    if (convert_ip_addr(ip_addr, ddacb_called_addr, ds)
	&& make_x25_call(ds, dc, ip_addr, X25_PROTO_IP)) {
						/* addr can be converted */
	dc->dc_inaddr = ip_addr;		/* store dest ip addr */
	dc->dc_key.key_addr.s_addr = imp_addr.ip.s_addr;
						/* store match key */
#ifdef DDADEBUG
	if (DDADBCH(9, ds->dda_if.if_unit)) {
	    imp_addr.ip = ip_addr;
	    DDALOG(LOG_DEBUG)
		"dda%d: locate_x25_lcn: made call to %d.%d.%d.%d\n",
		ds->dda_if.if_unit, imp_addr.imp.s_net, imp_addr.imp.s_host,
		imp_addr.imp.s_lh, imp_addr.imp.s_impno DDAELOG;

	}
#endif DDADEBUG
	return (dc);		/* and return the LCN */
    } else {
	return (0);		/* give up */
    }
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      FIND_FREE_LCN()                        %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine tries to locate a free X25 LCN.                 */
/*    The LCN table is searched for an unused entry.               */
/*    If no LCN is available, zero is returned.                    */
/*                                                                 */
/*  Call:              find_free_lcn(ds)                           */
/*  Argument:          ds:   pointer to dev control block struct   */
/*  Returns:           pointer to the dda control block which      */
/*                     contains LCN, else zero for failure         */
/*  Called by:         locate_x25_lcn()                            */
/*		       supr_msg()				   */
/*		       xxcntl()					   */
/*  Calls to:          DDALOG()                                    */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE struct dda_cb *
find_free_lcn(ds)
struct dda_softc *ds;
{
    struct dda_cb  *dc;
    register int    lcn, maxlcn, dwnflg = 0;

    /* LCNLINK */
    dc = &(ds->dda_cb[1]);	/* scan LCN table for free entry */
    maxlcn = nddach[ds->dda_if.if_unit];
    for (lcn = 1; lcn <= maxlcn; lcn++) {
#ifdef DDA_PAD_OR_RAW
	if (dc->dc_state == LC_IDLE && (dc->dc_flags & (DC_X29W | DC_X29 | DC_RAW)) == 0)
#else
	if (dc->dc_state == LC_IDLE)
#endif DDA_PAD_OR_RAW
	    break;
	else if (dc->dc_state == LC_RESTART || dc->dc_state == LC_DOWN)
	    dwnflg = 1;
	dc++;
    }

    /* LCNLINK */
    if (lcn > maxlcn) {		/* if we didn't find a free entry */
	if (LOG_BUSY) {
	    if (dwnflg)
		DDALOG(LOG_ERR) "dda%d: no circuits available (link not up)\n",
		    ds->dda_if.if_unit DDAELOG;
	    else
		DDALOG(LOG_ERR) "dda%d: all circuits in use\n",
		    ds->dda_if.if_unit DDAELOG;
	}
	return (0);		/* return empty handed */
    }
    return (dc);
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      CONVERT_IP_ADDR()                      %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    Based on the type of X.25 service, this routine performs     */
/*    one of two functions.  For PDN X.25 service, the address     */
/*    translation table is searched for presence of local X.25     */
/*    address (which was entered by the user via acpconfig).       */
/*                                                                 */
/*    For DDN X.25 service, this routine accepts an internet       */
/*    address and attempts to translate to an equivalent X25       */
/*    address.  This follows the guidelines in the DDN X25         */
/*    interface spec.  The resultant X25 address is stored in the  */
/*    X25 called addr buffer.                                      */
/*                                                                 */
/*    NOTE: Although front end was designed to accept ASCII coded  */
/*    digits for the address fields, we only supply the binary     */
/*    values.  The front end only uses low four bits to extract    */
/*    the binary value from the ASCII digits, so this works out.   */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*  Call:              convert_ip_addr(ip_addr, x25addr, ds)       */
/*  Argument:          ip_addr:  IP address                        */
/*                     x25addr:  X.25 address                      */
/*                     ds:       &dda_softc[unit]                  */
/*  Returns:           1 for success                               */
/*  Called by:         locate_x25_lcn()                            */
/*                     make_x25_call()                             */
/*                     ddaioctl()                                  */
/*  Calls to:          bcopy()                                     */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE boolean
convert_ip_addr(ip_addr, x25addr, ds)
struct in_addr  ip_addr;
u_char          x25addr[];
struct dda_softc *ds;

{
    register struct dda_addr_tr *atp, *hip, *lop;
    register int    temp;
    union imp_addr  imp_addr;

/****************************************************************/
/* processing for Public Data Network (PDN) X.25 service        */
/* search address translation table for local address           */
/****************************************************************/

    if (ds->dda_init & DDA_PDN) {
	x25addr[0] = 0;		/* clear result X.25 address length */
	lop = &dda_addr_tr[ds->dda_if.if_unit][0];	/* set up for binary
							 * search */
	hip = &dda_addr_tr[ds->dda_if.if_unit][dda_num_addr_tr[ds->dda_if.if_unit]];
	while (lop < hip - 1) {	/* binary search loop */
	    atp = lop + (hip - lop) / 2;
	    if (atp->ip_addr > ip_addr.s_addr)
		hip = atp;
	    else
		lop = atp;
	}
	if (lop->ip_addr == ip_addr.s_addr)
	    bcopy(lop->x25_addr, x25addr, MAXADDRLEN);
    }
/****************************************************************/
/* processing for DDN Standard or Basic X.25 service            */
/* convert IP address to X.25 address                           */
/****************************************************************/

    else {
	int             imp_no, imp_port;


	imp_addr.ip = ip_addr;
	if ((imp_addr.imp.s_net & 0x80) == 0x00) {	/* class A */
	    imp_no = imp_addr.imp.s_impno;
	    imp_port = imp_addr.imp.s_host;
	} else if ((imp_addr.imp.s_net & 0xc0) == 0x80) {	/* class B */
	    imp_no = imp_addr.imp.s_impno;
	    imp_port = imp_addr.imp.s_lh;
	} else {		/* class C */
	    imp_no = imp_addr.imp.s_impno / 32;
	    imp_port = imp_addr.imp.s_impno % 32;
	}

	x25addr[0] = 12;	/* set addr length */
	x25addr[1] = 0;		/* clear DNIC */
	x25addr[2] = 0;
	x25addr[3] = 0;
	x25addr[4] = 0;

	if (imp_port < 64) {	/* Physical:  0000 0 IIIHH00 [SS] *//* s_impno
				 *  -> III, s_host -> HH */
	    x25addr[5] = 0;	/* set flag bit */
	    x25addr[6] = imp_no / 100;
	    x25addr[7] = (imp_no % 100) / 10;
	    x25addr[8] = imp_no % 10;
	    x25addr[9] = imp_port / 10;
	    x25addr[10] = imp_port % 10;
	} else {		/* Logical:   0000 1 RRRRR00 [SS]	 *//* s
				 * _host * 256 + s_impno -> RRRRR */
	    temp = (imp_port << 8) + imp_no;
	    x25addr[5] = 1;
	    x25addr[6] = temp / 10000;
	    x25addr[7] = (temp % 10000) / 1000;
	    x25addr[8] = (temp % 1000) / 100;
	    x25addr[9] = (temp % 100) / 10;
	    x25addr[10] = temp % 10;
	}

	x25addr[11] = 0;	/* clear rest of addr */
	x25addr[12] = 0;
    }

#ifdef DDADEBUG
    if (DDADBCH(11, ds->dda_if.if_unit)) {
	imp_addr.ip = ip_addr;
	DDALOG(LOG_DEBUG) "dda%d: convert_ip_addr: %d.%d.%d.%d ==> %s\n",
	    ds->dda_if.if_unit, imp_addr.imp.s_net, imp_addr.imp.s_host,
	    imp_addr.imp.s_lh, imp_addr.imp.s_impno,
	    fmt_x25(&x25addr[1], (int) x25addr[0]) DDAELOG;
    }
#endif DDADEBUG

    return (x25addr[0] ? 1 : 0);
}




/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      CONVERT_X25_ADDR()                     %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine accepts an X25 address and attempts to          */
/*    translate to an equivalent internet address.  For DDA this   */
/*    follows the guidelines in the DDA X25 interface spec.  The   */
/*    resultant internet address is returned to the caller.        */
/*                                                                 */
/*  Call:              convert_x25_addr(x25addr, ds)               */
/*  Argument:          x25addr:  X.25 address                      */
/*                     ds:       &dda_softc[unit]                  */
/*                     dc: pointer to allocated dda_cb structure   */
/*  Returns:           IP address for success, else zero for fail  */
/*  Called by:         supr_msg()                                  */
/*  Calls to:          DDALOG()                                    */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE          u_long
convert_x25_addr(x25addr, ds, dc)
u_char          x25addr[];
struct dda_softc *ds;
struct dda_cb  *dc;

{
    register int    cnt, temp;
    union imp_addr  imp_addr;
    register struct dda_addr_tr *atp;

    dc->dc_inaddr.s_addr = imp_addr.ip.s_addr = 0L;
    if (ds->dda_init & DDA_PDN) {
	for (atp = &dda_addr_tr[ds->dda_if.if_unit][0];
	     atp < &dda_addr_tr[ds->dda_if.if_unit][dda_num_addr_tr[ds->dda_if.if_unit]]; atp++) {
	    if (bcmp((char *) atp->x25_addr, (char *) x25addr, x25addr[0] + 1) == 0) {
		/* set key address and print address up */
		dc->dc_inaddr.s_addr = imp_addr.ip.s_addr = atp->ip_addr;
		break;
	    }
	}
    } else {
	int             imp_no, imp_port;
	struct in_addr  my_addr;

	my_addr = ds->dda_ipaddr;
	if (((cnt = x25addr[0]) < MINADDRLEN - 1) || (cnt > MAXADDRLEN - 1)) {
	    DMESG(ds->dda_if.if_unit, 20,
		  (DDALOG(LOG_ERR) "dda%d: illegal X25 address length!\n",
		   ds->dda_if.if_unit DDAELOG));
	    return (0L);
	}
	switch (x25addr[5] & 0x0f) {
	  case 0:		/* Physical:  0000 0 IIIHH00 [SS]	 */
	    imp_no =
		((int) (x25addr[6] & 0x0f) * 100) +
		((int) (x25addr[7] & 0x0f) * 10) +
		((int) (x25addr[8] & 0x0f));


	    imp_port =
		((int) (x25addr[9] & 0x0f) * 10) +
		((int) (x25addr[10] & 0x0f));
	    break;
	  case 1:		/* Logical:   0000 1 RRRRR00 [SS]	 */
	    temp = ((int) (x25addr[6] & 0x0f) * 10000)
		+ ((int) (x25addr[7] & 0x0f) * 1000)
		+ ((int) (x25addr[8] & 0x0f) * 100)
		+ ((int) (x25addr[9] & 0x0f) * 10)
		+ ((int) (x25addr[10] & 0x0f));

	    imp_port = temp >> 8;
	    imp_no = temp & 0xff;
	    break;
	  default:
	    DMESG(ds->dda_if.if_unit, 21,
		  (DDALOG(LOG_ERR) "dda%d: illegal X25 address format!\n",
		   ds->dda_if.if_unit DDAELOG));
	    return (0L);
	}

	dc->dc_inaddr = imp_addr.ip = my_addr;	/* use local net number */
	if ((imp_addr.imp.s_net & 0x80) == 0x00) {	/* class A */
	    imp_addr.imp.s_net = 0;	/* mask net number */
	    imp_addr.imp.s_lh = 0;	/* mask logical host */
	    imp_addr.imp.s_host = imp_port;
	    ((union imp_addr *) & dc->dc_inaddr.s_addr)->imp.s_host = imp_port;
	    imp_addr.imp.s_impno = imp_no;
	    ((union imp_addr *) & dc->dc_inaddr.s_addr)->imp.s_impno = imp_no;
	} else if ((imp_addr.imp.s_net & 0xc0) == 0x80) {	/* class B */
	    imp_addr.imp.s_net = 0;
	    imp_addr.imp.s_lh = imp_port;
	    ((union imp_addr *) & dc->dc_inaddr.s_addr)->imp.s_lh = imp_port;
	    imp_addr.imp.s_host = 0;
	    imp_addr.imp.s_impno = imp_no;
	    ((union imp_addr *) & dc->dc_inaddr.s_addr)->imp.s_impno = imp_no;
	} else {		/* class C */
	    imp_addr.imp.s_impno = (imp_no << 5) + imp_port;
	    ((union imp_addr *) & dc->dc_inaddr.s_addr)->imp.s_impno = imp_addr.imp.s_impno;
	    imp_addr.imp.s_lh = 0;
	    imp_addr.imp.s_host = 0;
	    imp_addr.imp.s_net = 0;
	}
    }

#ifdef DDADEBUG
    if (DDADBCH(12, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: convert_x25_addr: %s ==> %d.%d.%d.%d\n",
	    ds->dda_if.if_unit, fmt_x25(&x25addr[1], (int) x25addr[0]),
	    imp_addr.imp.s_net, imp_addr.imp.s_host, imp_addr.imp.s_lh,
	    imp_addr.imp.s_impno DDAELOG;
    }
#endif DDADEBUG

    return (imp_addr.ip.s_addr);
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      MAKE_X25_CALL()                        %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine places an X25 call using the X25 Call Msg       */
/*    buffer.  The calling LCN is placed in the appropriate state  */
/*    and a timer is started.  Based on dda_init flag, implement   */
/*    DDN standard or basic service.  (If PDN mode is set, then    */
/*    the logic for basic service is followed.)                    */
/*                                                                 */
/*  Call:              make_x25_call(ds, dc, ip_addr, proto	   */
/*				     udlen, ud) 		   */
/*  Arguments:         ds:  pointer to device control structure    */
/*                     dc:  pointer to the Logical Channel control */
/*                            block structure                      */
/*                     ip_addr: callee's ip address                */
/*                     proto: protocol identifier byte             */
/*		       udlen: user data length			   */
/*		       ud:    user data				   */
/*  Returns:           one for success, zero for failure           */
/*  Called by:         locate_x25_lcn()                            */
/*  Calls to:          MGET()                                      */
/*                     mtod()                                      */
/*                     convert_ip_addr()                           */
/*                     bcopy()                                     */
/*                     IF_ENQUEUE()                                */
/*                     start_supr()                                */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE boolean
make_x25_call(ds, dc, ip_addr, proto, udlen, ud)
register struct dda_softc *ds;
register struct dda_cb *dc;
struct in_addr  ip_addr;
u_char          proto;
u_char		udlen;
u_char		*ud;
{
    register struct mbuf *m_callbfr;
    register u_char *cb;
    union imp_addr  imp_addr;

#if ACC_BSD == 42 || ACC_ULTRIX == 12
    struct sockaddr_in *our_addr;
#endif 

#ifdef DDADEBUG
    if (DDADBCH(13, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: make_x25_call: lcn used = %d\n",
	    ds->dda_if.if_unit, dc->dc_lcn DDAELOG;
    }
#endif DDADEBUG

    MGET(m_callbfr, M_DONTWAIT, MT_DATA);	/* try to get call cmnd
						 * buffer */
    if (m_callbfr == 0) {
	DMESG(ds->dda_if.if_unit, 22,
	      (DDALOG(LOG_ERR) "dda%d: couldn't get mbuf for call command\n",
	       ds->dda_if.if_unit DDAELOG));
	return (0);
    }
    cb = mtod(m_callbfr, u_char *);

    if (ds->dda_net_id == TRANSPAC) {
	ddacb_calling_addr[0] = 0;	/* send a 0 length calling address */
    } else {
#if ACC_BSD == 42 || ACC_ULTRIX == 12
	our_addr = (struct sockaddr_in *) & (ds->dda_if.if_addr);
	(void) convert_ip_addr(our_addr->sin_addr, ddacb_calling_addr, ds);
#else
	(void) convert_ip_addr(ds->dda_ipaddr, ddacb_calling_addr, ds);
#endif
    }

    ddacb_protocol[0] = 4;
    ddacb_protocol[1] = proto;	/* protocol type */
    ddacb_protocol[2] = 0;
    ddacb_protocol[3] = 0;
    ddacb_protocol[4] = 0;

    /*
     * CCITT standard facilities must precede DDN specific facilities See BBN
     * report 5476 section 2.1.2.  Firmware preceding rev 0x20 does not
     * support packet size / window negotiation. 
     */
    ddacb_facilities[0] = 0;	/* initialize facilities length */
    if (ds->dda_firmrev >= 0x21) {
	ddacb_facilities[0] = 0;
	if (ds->dda_init & DDA_PKTNEG) {
	    int             n = ddacb_facilities[0];	/* length so far */

	    ddacb_facilities[n + 1] = X25_FACIL_PKTSIZE;
	    ddacb_facilities[n + 2] = PKTSIZE_LARGE;
	    ddacb_facilities[n + 3] = PKTSIZE_LARGE;
	    ddacb_facilities[0] += 3;
	}
	if (ds->dda_init & DDA_WNDNEG) {
	    int             n = ddacb_facilities[0];	/* length so far */

	    ddacb_facilities[n + 1] = X25_FACIL_WINSIZE;
	    ddacb_facilities[n + 2] = WINSIZE_LARGE;
	    ddacb_facilities[n + 3] = WINSIZE_LARGE;
	    ddacb_facilities[0] += 3;
	}
    }
    if ((ds->dda_init & (DDA_BASIC | DDA_PDN)) == 0) {	/* DDN standard mode,
							 * tell callee */
	int             n = ddacb_facilities[0];	/* length so far */

	ddacb_facilities[0] += 4;	/* additional facility bytes */
	ddacb_facilities[n + 1] = DDN_FACIL_MARKER; /* end of CCITT stuff, */
	ddacb_facilities[n + 2] = DDN_FACIL_MARKER; /* and start DDN local */
	ddacb_facilities[n + 3] = X25_FACIL_DDA;    /* DDA standard mode */
	ddacb_facilities[n + 4] = FAC_DDASTD;
    }

    ddacb_cmnd[0] = CALL;	/* set command code */
    ddacb_cmnd[1] = dc->dc_lcn << 1;	/* set channel id */
    ddacb_cmnd[2] = 0;
    ddacb_cmnd[3] = (ddacb_called_addr[0] + 1) +	/* tally cmnd ext len */
	(ddacb_calling_addr[0] + 1) +
	(ddacb_protocol[0] + 1) +
	(ddacb_facilities[0] + 1) +
	(ddacb_user_data[0] + 1);

    if ((unsigned) ddacb_cmnd[3] + 4 > MLEN) {
	DMESG(ds->dda_if.if_unit, 38, (DDALOG(LOG_ERR)
	    "dda%d: make_x25_call message too large for mbuf (%d bytes)\n",
	    ds->dda_if.if_unit, (unsigned) ddacb_cmnd[3] + 4 DDAELOG));
	return 0;	/* failure */
    }

    m_callbfr->m_len = ddacb_cmnd[3] + 4;

    /* copy command header */
    bcopy(ddacb_cmnd, cb, 4);
    cb += 4;

    /* copy called address */
    bcopy(ddacb_called_addr, cb, ddacb_called_addr[0] + 1);
    cb += (ddacb_called_addr[0] + 1);

    /* copy calling address */
    bcopy(ddacb_calling_addr, cb, ddacb_calling_addr[0] + 1);
    cb += (ddacb_calling_addr[0] + 1);

    /* copy protocol */
    bcopy(ddacb_protocol, cb, ddacb_protocol[0] + 1);
    cb += (ddacb_protocol[0] + 1);

    /* copy facilities */
    bcopy(ddacb_facilities, cb, ddacb_facilities[0] + 1);
    cb += (ddacb_facilities[0] + 1);

    /* copy user data */
    bcopy(ddacb_user_data, cb, ddacb_user_data[0] + 1);
    cb += (ddacb_user_data[0] + 1);

    hist_lcn_state(ds->dda_if.if_unit, dc->dc_state, LC_CALL_PENDING);
    dc->dc_state = LC_CALL_PENDING;	/* set state */
    dc->dc_timer = TMO_CALL_PENDING;	/* start call timeout */

#ifdef DDADEBUG
    if (DDADBCH(13, ds->dda_if.if_unit)) {
	prt_bytes(ds->dda_if.if_unit, "make_x25_call: call_bfr",
		  mtod(m_callbfr, u_char *), m_callbfr->m_len);
    }
#endif DDADEBUG
    if (LOG_CALLS) {
	imp_addr.ip = ip_addr;
	DDALOG(LOG_ERR) "dda%d: Calling %d.%d.%d.%d (%s) on lcn %d\n",
	    ds->dda_if.if_unit, imp_addr.imp.s_net, imp_addr.imp.s_host,
	    imp_addr.imp.s_lh, imp_addr.imp.s_impno,
	    fmt_x25(&ddacb_called_addr[1], (int) ddacb_called_addr[0]),
	    dc->dc_lcn
	    DDAELOG;
    }
    start_supr(ds, m_callbfr);
    return (1);
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDA_START()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine attempts to start output of data queued on a    */
/*    specific LCN.  If the LCN was not already busy and data is   */
/*    available for output, the data is copied into the LCN's I/O  */
/*    buffer and a write request queued to the ACP device.  Data   */
/*    is passed in mbuf(s) from IP to ddaoutput(), ddaoutput()     */
/*    queues the data, and the data is dequeued here.              */
/*                                                                 */
/*  Call:              dda_start(ds, dc)                           */
/*  Arguments:         ds:  pointer to device control structure    */
/*                     dc:  pointer to the Logical Channel control */
/*                            block structure                      */
/*  Returns:           nothing                                     */
/*  Called by:         ddaoutput()                                 */
/*                     x25_init()                                  */
/*                     make_x25_call()                             */
/*                     supr_msg()                                  */
/*                     send_supr()                                 */
/*                     dda_data()                                  */
/*                     dda_supr()                                  */
/*  Calls to:          IF_DEQUEUE()                                */
/*                     dda_wrq()                                   */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
dda_start(ds, dc)
register struct dda_softc *ds;
register struct dda_cb *dc;
{
    register struct mbuf *m;
    register struct hdx_chan *hc = &dc->dc_wchan;
    register int    s;

#ifdef DDADEBUG
    if (DDADBCH(14, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: dda_start()\n", ds->dda_if.if_unit DDAELOG;
    }
#endif DDADEBUG

    /*
     * If output isn't active, attempt to start sending a new packet. 
     */

    if ((dc->dc_flags & DC_OBUSY) || (dc->dc_oq.ifq_len == 0) ||
	((dc->dc_lcn != 0) && (dc->dc_state != LC_DATA_IDLE))) {
	return;
    }
    if (dc->dc_lcn != 0)
	dc->dc_timer = tmo_data_idle;
/*
 *  Raise priority whenever touching dc_oq because
 *  the mbufs on this queue may be asynchronously
 *  freed upon receipt of a line status msg, restart,
 *  clear, or reset.
 */
    s = splimp();
    IF_DEQUEUE(&dc->dc_oq, m);
    splx(s);
    if (m == 0) {		/* XXX this is a bug catcher XXX */

	DMESG(ds->dda_if.if_unit, 24,
	 (DDALOG(LOG_ERR) "dda%d: dequeued NULL mbuf in IP output chain!\n",
	  ds->dda_if.if_unit DDAELOG));
	DMESG(ds->dda_if.if_unit, 24,
	      (DDALOG(LOG_ERR) "RESET dda%d MANUALLY: use /etc/acpconfig dda%d -z\n",
	       ds->dda_if.if_unit, ds->dda_if.if_unit DDAELOG));

	ds->dda_if.if_flags &= ~(IFF_RUNNING | IFF_UP);
	hist_link_state(ds->dda_if.if_unit, ds->dda_state, S_DISABLED);
	ds->dda_state = S_DISABLED;
	dda_disable(ds->dda_if.if_unit);
	return;
    }
    s = splimp();
    hc->hc_mbuf = m;
    hc->hc_curr = m;
#ifdef DDA_PAD_OR_RAW		/* crufty kludge to get the Qbit */
    if (dc->dc_flags & (DC_X29 | DC_X29W | DC_RAW)) {	/* raw or x29? */
	if (m->m_len < (MLEN - 1))	/* small mbuf? */
	    hc->hc_sbfc = m->m_dat[MLEN - 1];	/* ok, get the subfunc byte */
	else
	    hc->hc_sbfc = 0;	/* subfunc must be zero for large buffers */
    } else
	hc->hc_sbfc = 0;	/* subfunc must be zero for ip buffers */
#else
    hc->hc_sbfc = 0;
#endif
    splx(s);
    dc->dc_flags |= DC_OBUSY;
    dda_wrq(ds, hc, 0);		/* write request to ACP */
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDA_DATA()                             %%*/
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
/*  Call:              dda_data(ds, hc, cc, cnt)                   */
/*  Argument:          ds:  device control block                   */
/*                     hc:  half duplex channel control block      */
/*                     cc:   Mailbox I/O completion status         */
/*                     cnt:  byte count                            */
/*  Returns:           nothing                                     */
/*  Called by:         ddainta()                                   */
/*  Calls to:          m_freem()                                   */
/*                     dda_start()                                 */
/*                     IF_QFULL()                                  */
/*                     IF_DROP()                                   */
/*                     IF_ENQUEUE()                                */
/*                     schednetisr()                               */
/*                     dda_rrq()                                   */
/*                     dda_wrq()                                   */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
dda_data(ds, hc, cc, cnt)
register struct dda_softc *ds;
register struct hdx_chan *hc;
int             cc, cnt;
{
    register struct dda_cb *dc = &(ds->dda_cb[hc->hc_chan / 2]);
    struct ifqueue *inq = &ipintrq;	/* IP input queue */

/* note that this routine is a weird case in which Ultrix 2.0 behaves like
 * a 4.2 system rather than a 4.3 system.  This is reflected in the structure
 * of conditional compilation segments.
 */
#if ACC_BSD > 42			/* 4.3bsd or newer */
    register struct mbuf *m, *mb;
    struct ifnet   *ifp;
#else					/* 4.2, or all flavors of Ultrix */
    register struct mbuf *m;
#endif

#ifdef DDADEBUG
    if (DDADBCH(18, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: dda_data: chan=%d cc=%x cnt=%x\n",
	    ds->dda_if.if_unit, hc->hc_chan, cc, cnt DDAELOG;
    }
#endif DDADEBUG

#if ACC_BSD > 42
    ifp = &ds->dda_if;
#endif

    if (hc->hc_chan & 0x01) {	/* was it read or write? *//* write, fire up
				 * next output */
#ifdef DDADEBUG
	dc->dc_out_t = TMO_OFF;	/* turn off output completion timer */
#endif
	hc = &dc->dc_wchan;
	if ((hc->hc_func != DDAABT) && (hc->hc_curr = hc->hc_curr->m_next))
	    dda_wrq(ds, hc, 0);
	else {
	    m_freem(hc->hc_mbuf);
	    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	    if (hc->hc_func == DDAABT) {
		hc->hc_func &= ~DDAABT;
		hc->hc_inv &= ~INVALID_MBUF;
	    } else
		ds->dda_if.if_opackets++;
	    dc->dc_flags &= ~DC_OBUSY;
	    dda_start(ds, dc);
	}
    } else {			/* read, process rcvd packet */
	hc = &dc->dc_rchan;

#ifdef DDADEBUG
	if (DDADBCH(19, ds->dda_if.if_unit)) {
	    u_char         *p;

	    p = mtod(hc->hc_curr, u_char *);
	    prt_bytes(ds->dda_if.if_unit, "received data", p, (cnt < 64 ? cnt : 64));
	}
#endif DDADEBUG

	if (cc == DDAIOCOK) {	/* Queue good packet for input */
#ifdef DDADEBUG
	    if (DDADBCH(19, ds->dda_if.if_unit)) {
		DDALOG(LOG_DEBUG) "dda%d: dda_data: chan=%d DDAIOCOK\n",
		    ds->dda_if.if_unit, hc->hc_chan DDAELOG;
	    }
#endif DDADEBUG
	    ds->dda_if.if_ipackets++;
	    hist_lcn_state(ds->dda_if.if_unit, dc->dc_state, LC_DATA_IDLE);
	    if (dc->dc_state == LC_DATA_IDLE)
		dc->dc_timer = tmo_data_idle;
	    hc->hc_curr->m_len += cnt;	/* update byte count */
	    m = hc->hc_mbuf;	/* que mbuf chain */

#if ACC_BSD > 42
	    /* Prepend ifp pointer for 4.3 */
	    MGET(mb, M_DONTWAIT, MT_DATA);
	    if (mb == 0) {
		DMESG(ds->dda_if.if_unit, 26,
		(DDALOG(LOG_ERR) "dda%d: couldn't get mbuf for ifp header\n",
		 ds->dda_if.if_unit DDAELOG));
		m_freem(m);
		return;
	    }
	    *(mtod(mb, struct ifnet **)) = ifp;
	    mb->m_len = sizeof(ifp);
	    mb->m_next = m;

	    if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(mb);
	    } else {
		IF_ENQUEUE(inq, mb);
		schednetisr(NETISR_IP);
	    }
#else
	    if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	    } else {
		IF_ENQUEUE(inq, m);
		schednetisr(NETISR_IP);
	    }
#endif
	    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	} else if (cc == DDAIOCOKP) {	/* good completion, more data pending */
	    hc->hc_curr->m_len += cnt;
	} else {
	    m_freem(hc->hc_mbuf);
	    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	}
	/* hang a new data read */
	dda_rrq(ds, hc);
    }
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DDA_SUPR()                             %%*/
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
/*  Call:              dda_supr(ds, hc, cc)                        */
/*  Argument:          ds:  device control block                   */
/*                     hc:  half duplex channel control block      */
/*                     cc:   Mailbox I/O completion status         */
/*  Returns:           nothing                                     */
/*  Called by:         ddainta()                                   */
/*  Calls to:          dda_start()                                 */
/*                     mtod()                                      */
/*                     supr_msg()                                  */
/*                     m_free()                                    */
/*                     dda_rrq()                                   */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
dda_supr(ds, hc, cc, cnt)
register struct dda_softc *ds;
struct hdx_chan *hc;
int             cc;
int             cnt;
{
    register struct dda_cb *dc = &(ds->dda_cb[hc->hc_chan / 2]);
    u_char         *p;

#ifdef DDADEBUG
    if (DDADBCH(20, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: dda_supr: chan=%d cc=%x\n",
	    ds->dda_if.if_unit, hc->hc_chan, cc DDAELOG;
    }
#endif DDADEBUG

    /* an odd-numbered channel indicates a write */
    /* the supr msg is assumed to be in 1 mbuf   */

    if (hc->hc_chan & 0x01) {
	m_freem(hc->hc_mbuf);
	hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	dc->dc_flags &= ~DC_OBUSY;
	dda_start(ds, dc);
    }
    /* otherwise, process the read */

    else {
	if (cc == DDAIOCOK) {
	    p = mtod(hc->hc_curr, u_char *);	/* point to data in mbuf */
#ifdef DDA_PAD_OR_RAW
	    switch (dda_decode_type(ds, p)) {
#  ifdef DDA_PADOPT
	      case 1:
#    ifdef DDADEBUG
		if (DDADBCH(20, ds->dda_if.if_unit)) {
		    printf("dda%d: dda_supr(): case 1: chan = %x, p = %x\n",
			   ds->dda_if.if_unit, hc->hc_chan, *p);
		}
#    endif DDADEBUG
		x29_supr(ds, p);
		break;
#  endif
#  ifdef DDA_RAWOPT
	      case 2:
#    ifdef DDADEBUG
		if (DDADBCH(20, ds->dda_if.if_unit)) {
		    printf("dda%d: dda_supr(): case 2: chan = %x, p = %x\n",
			   ds->dda_if.if_unit, hc->hc_chan, *p);
		}
#    endif DDADEBUG
		hc->hc_curr->m_len += cnt;
		pi_supr(ds, hc->hc_curr);
		/* don't free mbuf here */
		hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
		dda_rrq(ds, hc);/* hang a new supr read */
		return;
#  endif
	      default:
		supr_msg(ds, p);/* process supervisor message */
		break;
	    }
#else DDA_PAD_OR_RAW
	    supr_msg(ds, p);	/* process supervisor message */
#endif DDA_PAD_OR_RAW
	} else if (cc == DDAIOCOKP) {
	    DMESG(ds->dda_if.if_unit, 28,
		  (DDALOG(LOG_ERR) "dda%d: truncated supervisor message\n",
		   ds->dda_if.if_unit DDAELOG));
	}
	m_freem(hc->hc_mbuf);
	hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	dda_rrq(ds, hc);	/* hang a new supr read */
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
/*       taken.                                                    */
/*                                                                 */
/*  Call:              supr_msg(ds, p)                             */
/*  Arguments:         ds:  pointer to dev control block struct    */
/*                     p:   pointer to a character array           */
/*                              containing the supervisor message  */
/*  Returns:           nothing                                     */
/*  Called by:         dda_supr()                                  */
/*  Calls to:          DDALOG()                                    */
/*                     IF_DEQUEUE()                                */
/*                     m_freem()                                   */
/*                     send_restart()                              */
/*                     send_supr()                                 */
/*                     dda_start()                                 */
/*                     decode_ring()                               */
/*                     decode_answer()                             */
/*                     convert_x25_addr()                          */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
supr_msg(ds, p)
struct dda_softc *ds;
u_char          p[];

{
    register struct dda_cb *dc;
    register int    lcn;
    register int    maxlcn;
    union imp_addr  imp_addr;

#ifdef DDADEBUG
    if (DDADBCH(21, ds->dda_if.if_unit)) {
	prt_bytes(ds->dda_if.if_unit, "supr_msg", p, (int) (4 + p[3]));
    }
#endif DDADEBUG

    maxlcn = nddach[ds->dda_if.if_unit];	/* obtain SVC limit */
    switch (p[0]) {
      case LINE_STATUS:	/* link status msg */
	if (p[2] == LINK_UP) {	/* if link came up */
#ifdef DDADEBUG
	    if (DDADBCH(21, ds->dda_if.if_unit)) {
		DDALOG(LOG_DEBUG) "dda%d: supr_msg: HDLC link up\n",
		    ds->dda_if.if_unit DDAELOG;
	    }
#endif DDADEBUG
	    send_restart(ds);	/* send restart msg */
	    ds->dda_state = S_COMING_UP;
	} else {		/* if link went down */
	    ds->dda_if.if_flags &= ~IFF_UP;	/* ? should call if_down() ? */
	    hist_link_state(ds->dda_if.if_unit, ds->dda_state, S_DISABLED);
	    ds->dda_state = S_DISABLED;
	    dc = ds->dda_cb;
	    /* LCNLINK */
	    for (lcn = 0; lcn <= maxlcn; lcn++) {	/* for all LCN's */
		dc->dc_inaddr.s_addr = 0;	/* forget dest address */
		dc->dc_key.key_addr.s_addr = 0;
		dc->dc_wsizein = dc->dc_wsizeout = 0;
		dc->dc_pktsizein = dc->dc_pktsizeout = 0;
		dc->dc_state = LC_DOWN;	/* set state */
		dc->dc_timer = TMO_OFF;	/* stop timer */
		dc++;
	    }
	    hist_all_lcns(ds->dda_if.if_unit, LC_DOWN);
	    abort_io(ds->dda_if.if_unit, ALL_CHANS);
#ifdef DDA_PADOPT
	    x29_init(ds->dda_if.if_unit, 1);
#endif
	    if (p[2] == LINK_DISABLED)	/* link disabled */
		DMESG(ds->dda_if.if_unit, 29,
		      (DDALOG(LOG_ERR) "dda%d:  link disabled\n",
		       ds->dda_if.if_unit DDAELOG));
	    else
		DMESG(ds->dda_if.if_unit, 30,
		      (DDALOG(LOG_ERR) "dda%d:  link down\n", ds->dda_if.if_unit DDAELOG));
	}
	break;

      case RESTART:		/* restart received */
	if (ds->dda_cb[0].dc_state != LC_RESTART) {	/* if not restarting */

#ifdef DDADEBUG
	    if (DDADBCH(21, ds->dda_if.if_unit)) {
		DDALOG(LOG_DEBUG)
		    "dda%d: supr_msg: RESTART rcvd, no RESTART pending\n",
		    ds->dda_if.if_unit DDAELOG;
	    }
#endif DDADEBUG
	    send_supr(ds, RSTRT_ACK, 0, 0);	/* send restart ack */
	}
	/* fall thru */
      case RSTRT_ACK:		/* restart ack */
	if ((ds->dda_state == S_COMING_UP) || (ds->dda_state == S_LINK_UP)) {
	    if (p[0] == RSTRT_ACK) {
		DMESG(ds->dda_if.if_unit, 31,
		      (DDALOG(LOG_ERR) "dda%d: Restart Ack received\n",
		       ds->dda_if.if_unit DDAELOG));
	    } else {		/* restart. print cause and diagnostic. */
		DMESG(ds->dda_if.if_unit, 31,
		      (DDALOG(LOG_ERR) "dda%d: Restart (%x %x) received\n",
		       ds->dda_if.if_unit, p[2], p[3] ? p[4] : 0 DDAELOG));
	    }

	    ds->dda_if.if_flags |= IFF_UP;
	    hist_link_state(ds->dda_if.if_unit, ds->dda_state, S_LINK_UP);
	    ds->dda_state = S_LINK_UP;
	    dc = ds->dda_cb;
	    /* LCNLINK */
	    for (lcn = 0; lcn <= maxlcn; lcn++) {	/* for all LCN's */
		dc->dc_state = LC_IDLE;	/* set state */
		dc->dc_timer = TMO_OFF;	/* stop timer */
		dc->dc_inaddr.s_addr = 0;	/* forget address */
		dc->dc_key.key_addr.s_addr = 0;
		dc->dc_wsizein = dc->dc_wsizeout = 0;
		dc->dc_pktsizein = dc->dc_pktsizeout = 0;
		dc++;
	    }
	    hist_all_lcns(ds->dda_if.if_unit, LC_IDLE);
	    abort_io(ds->dda_if.if_unit, ALL_CHANS);
	    DMESG(ds->dda_if.if_unit, 32,
		  (DDALOG(LOG_ERR) "dda%d: (%s rev %d.%d) link up\n",
		   ds->dda_if.if_unit, dda_product,
	      (ds->dda_firmrev >> 4) & 0xF, ds->dda_firmrev & 0xF DDAELOG));
#ifdef DDA_PAD_OR_RAW
	    x29_init(ds->dda_if.if_unit, 1);

	    /*
	     * wake up all processes that tried to open a x29 device but
	     * slept because the board was not up 
	     */
	    wakeup(&ds->dda_state);
#endif DDA_PAD_OR_RAW
	} else
#ifdef DDADEBUG
	if (DDADBCH(21, ds->dda_if.if_unit)) {
	    DDALOG(LOG_ERR) "dda%d:  Unexpected RESTART in state %x\n",
		ds->dda_if.if_unit, ds->dda_state DDAELOG;
	}
#endif DDADEBUG
	break;

      case ANSWER:		/* call answered */
	lcn = p[1] / 2;
	dc = &(ds->dda_cb[lcn]);
	if (dc->dc_state == LC_CALL_PENDING) {	/* if a call pending */
	    hist_lcn_state(ds->dda_if.if_unit, dc->dc_state, LC_DATA_IDLE);
	    decode_answer(p, dc);
	    dc->dc_state = LC_DATA_IDLE;	/* set state */
	    dc->dc_timer = tmo_data_idle;	/* start timer */
	    dda_start(ds, dc);	/* try to send data */
	}
	if (LOG_CALLS) {
	    DDALOG(LOG_ERR) "dda%d: lcn %d connected\n",
		ds->dda_if.if_unit, lcn DDAELOG;
	}
	break;

      case RING:		/* incoming call */
	/* if ring looks ok, and we find a free LCN to assign */
	if (decode_ring(p) && (dc = find_free_lcn(ds))) {
	    dc->dc_key.key_addr.s_addr =
		convert_x25_addr(ddacb_calling_addr, ds, dc);
#ifdef DDADEBUG
	    if (DDADBCH(21, ds->dda_if.if_unit)) {
		imp_addr.ip = dc->dc_inaddr;
		DDALOG(LOG_DEBUG)
		    "dda%d: supr_msg: got call from %d.%d.%d.%d\n",
		    ds->dda_if.if_unit, imp_addr.imp.s_net, imp_addr.imp.s_host,
		    imp_addr.imp.s_lh, imp_addr.imp.s_impno DDAELOG;
	    }
#endif DDADEBUG
	    hist_lcn_state(ds->dda_if.if_unit, dc->dc_state, LC_DATA_IDLE);
	    dc->dc_state = LC_DATA_IDLE;	/* set state */
	    dc->dc_timer = tmo_data_idle;	/* start timer */
	    dc->dc_pktsizein = 0;
	    dc->dc_pktsizeout = 0;
	    dc->dc_wsizein = 0;
	    dc->dc_wsizeout = 0;
	    send_supr(ds, ANSWER, (int) dc->dc_lcn * 2,
		      (int) p[2]);		/* send answer */
	    if (LOG_CALLS) {
		imp_addr.ip = dc->dc_inaddr;
		DDALOG(LOG_ERR)
		    "dda%d: Accepting call from %d.%d.%d.%d (%s) on lcn %d\n",
		    ds->dda_if.if_unit, imp_addr.imp.s_net, imp_addr.imp.s_host,
		    imp_addr.imp.s_lh, imp_addr.imp.s_impno,
		    fmt_x25(&ddacb_calling_addr[1],
		    (int) ddacb_calling_addr[0]), dc->dc_lcn DDAELOG;
	    }
	} else {		/* if no free LCN's */
	    send_supr(ds, CLEARVC, p[2], 0);	/* clear call */
	    if (LOG_CALLS) {
		DDALOG(LOG_ERR) "dda%d: Rejecting call from %s on VC 0x%x\n",
		    ds->dda_if.if_unit,
		    fmt_x25(&ddacb_calling_addr[1], ddacb_calling_addr[0]),
		    p[1] DDAELOG;
	    }
	}
	break;

      case CLEARLC:		/* clear by LCN */

	/*
	 * This could mean one of three things: If we have a call request
	 * outstanding, this message means the call has failed. If we have a
	 * clear request outstanding, this message completes the cleanup; the
	 * channel is now available for reuse. If we have a call active, this
	 * message means the other side is closing the circuit. 
	 */
	lcn = p[1] / 2;		/* get LCN */
	dc = &(ds->dda_cb[lcn]);
	if (dc->dc_state != LC_CLR_PENDING) {	/* if no clear pending */
	    send_supr(ds, CLEARLC, p[1], 0);	/* ack the clear */
	}
	if (dc->dc_state == LC_CALL_PENDING	/* if call is cleared */
	    && (LOG_CALLS || DMESGVAL(ds->dda_if.if_unit, 33) == 0)) {
	    imp_addr.ip = dc->dc_inaddr;
	    DDALOG(LOG_ERR)
		"dda%d: Call to %d.%d.%d.%d on lcn %d failed (%x %x)\n",
		ds->dda_if.if_unit, imp_addr.imp.s_net, imp_addr.imp.s_host,
		imp_addr.imp.s_lh, imp_addr.imp.s_impno, dc->dc_lcn, p[2], p[4]
		DDAELOG;

	} else if (LOG_CALLS) {
	    if (dc->dc_state == LC_CLR_PENDING) {	/* did we clear it? *//* y
							 * es, IP address is
							 * already gone.  Say
							 * channel is free.  */
		DDALOG(LOG_ERR) "dda%d: Cleared lcn %d\n",
		    ds->dda_if.if_unit, dc->dc_lcn DDAELOG;
	    } else {		/* cleared by net, print more info */
		imp_addr.ip = dc->dc_inaddr;
		DDALOG(LOG_ERR)
		    "dda%d: Cleared lcn %d to %d.%d.%d.%d (%x %x)\n",
		    ds->dda_if.if_unit, dc->dc_lcn, imp_addr.imp.s_net,
		    imp_addr.imp.s_host, imp_addr.imp.s_lh, imp_addr.imp.s_impno,
		    p[2], p[4] DDAELOG;
	    }
	}
	hist_lcn_state(ds->dda_if.if_unit, dc->dc_state, LC_IDLE);
	/* LCNLINK delete */
	dc->dc_state = LC_IDLE;	/* set state */
	dc->dc_timer = TMO_OFF;	/* stop timer */
	dc->dc_inaddr.s_addr = 0;	/* forget address */
	dc->dc_key.key_addr.s_addr = 0;
	dc->dc_wsizein = dc->dc_wsizeout = 0;
	dc->dc_pktsizein = dc->dc_pktsizeout = 0;
	abort_io(ds->dda_if.if_unit, lcn);
	if (LOG_CALLS) {
	    printf("dda%d: Cleared LCN %d cause code %x diag code %x\n",
		   ds->dda_if.if_unit, dc->dc_lcn, p[2], p[4]);
	}
	break;

      case CLEARVC:		/* clear by VCN */

	send_supr(ds, CLEARVC, p[1], 0);	/* send clear ack */
	if (LOG_CALLS) {
	    DDALOG(LOG_ERR) "dda%d: Network cleared VC %x (%x %x)\n",
		ds->dda_if.if_unit, p[1], p[2], p[4] DDAELOG;
	}
#ifdef DDADEBUG
	else if (DDADBCH(21, ds->dda_if.if_unit)) {
	    DDALOG(LOG_DEBUG) "dda%d: supr_msg: CLEARVC VCN=%x\n",
		ds->dda_if.if_unit, p[1] DDAELOG;
	}
#endif DDADEBUG
	break;

      case RESET:		/* X25 reset */
	lcn = p[1] / 2;		/* get LCN */
	dc = &(ds->dda_cb[lcn]);
	send_supr(ds, RESET_ACK, p[1], 0);	/* send reset ack */
	abort_io(ds->dda_if.if_unit, lcn);
	imp_addr.ip = dc->dc_inaddr;
	DMESG(ds->dda_if.if_unit, 34,
	      (DDALOG(LOG_ERR)
	       "dda%d: X25 RESET (%x %x) on lcn %d: %d.%d.%d.%d\n",
	       ds->dda_if.if_unit, p[2], p[4], lcn, imp_addr.imp.s_net,
	       imp_addr.imp.s_host, imp_addr.imp.s_lh, imp_addr.imp.s_impno
	       DDAELOG));
	break;

      case INTERRUPT:		/* X25 interrupt */
	lcn = p[1] / 2;		/* get LCN */
	dc = &(ds->dda_cb[lcn]);
	imp_addr.ip = dc->dc_inaddr;
	DMESG(ds->dda_if.if_unit, 35,
	      (DDALOG(LOG_ERR)
	       "dda%d: X25 INTERRUPT (%x) on lcn %d: %d.%d.%d.%d\n",
	       ds->dda_if.if_unit, p[2], lcn, imp_addr.imp.s_net,
	       imp_addr.imp.s_host, imp_addr.imp.s_lh, imp_addr.imp.s_impno
	       DDAELOG));
	break;

      case STATRESP:		/* Statistics Response from FEP */

	/*
	 * Copy the whole message into a static buffer, dda_iobuf. The buffer
	 * is viewed as a (struct ddactl).  Wake up the ioctl thread which
	 * will copy the message out for acpconfig. 
	 */
	{
	    struct ddactl  *da = (struct ddactl *) dda_iobuf;

	    bcopy(p, da->msg, max(4 + p[3], sizeof(da->msg)));
#ifdef MULTINET
	    StatQuery_Completed = 1;
#else
	    wakeup(dda_iobuf);
#endif
	    break;
	}

      default:
	DMESG(ds->dda_if.if_unit, 36,
	      (DDALOG(LOG_ERR) "dda%d: supervisor error (%x %x %x %x)\n",
	       ds->dda_if.if_unit, p[0], p[1], p[2], p[3] DDAELOG));
    }
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                   DECODE_ANSWER()                           %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*        This routine looks at the answer message from the FE     */
/*  and decodes it to find the negtiated packet and window sizes   */
/*  if they are present.                                           */
/*                                                                 */
/*  Call:              decode_answer(p, dc)	                   */
/*  Argument:          p: pointer to mbuf data for ANSWER message  */
/*                     dc: pointer to relavant lcn structure       */
/*  Returns:           nothing                                     */
/*  Called by:         supr_msg()                                  */
/*  Calls to:                                                      */
/*                     DDALOG()                                    */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
decode_answer(p, dc)
u_char         *p;
struct dda_cb  *dc;
{
    register u_char *cp;
    int             i, faclen;

    dc->dc_pktsizein = 0;
    dc->dc_pktsizeout = 0;
    dc->dc_wsizein = 0;
    dc->dc_wsizeout = 0;
    cp = p + 4;			/* skip over code, lcn, vcn and count in
				 * answer message */
    /* cp now points to length of called address */
    cp += *cp + 1;		/* skip over called address and length byte */
    /* cp now points to length of calling address */
    cp += *cp + 1;		/* skip over calling address and length byte */
    /* cp now points to length of protocol */
    cp += *cp + 1;		/* skip over protocol and protocol length
				 * byte */
    /* cp now points to the facilities length */

    faclen = *cp++;
    /* cp now points to start of facilities */
    for (i = 0; i < faclen;) {
	switch (*cp & 0xc0) {
	  case 0x00:		/* single octet parameter field */
	    i += 2;
	    cp += 2;
	    break;
	  case 0x40:		/* double octet parameter field */
	    switch (*cp) {
	      case X25_FACIL_PKTSIZE:	/* 0x42, packet size */
		dc->dc_pktsizein = *(cp + 1);
		dc->dc_pktsizeout = *(cp + 2);
		break;
	      case X25_FACIL_WINSIZE:	/* 0x43, window size */
		dc->dc_wsizein = *(cp + 1);
		dc->dc_wsizeout = *(cp + 2);
		break;
	    }
	    i += 3;
	    cp += 3;
	    break;
	  case 0x80:		/* triple octet parameter field */
	    i += 4;
	    cp += 4;
	    break;
	  case 0xc0:		/* variable-length parameter field */
	    cp++;
	    i += 2 + *cp;
	    cp += 1 + *cp;
	    break;
	    /* Note: No other cases (i.e., default) possible */
	}
    }
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      DECODE_RING()                          %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine parses and validates the incoming call message. */
/*                                                                 */
/*  Call:              decode_ring(p)                              */
/*  Argument:          p:   pointer to the message                 */
/*  Returns:           1 for success, else 0 for failure           */
/*  Called by:         supr_msg()                                  */
/*  Calls to:          bcopy()                                     */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE boolean
decode_ring(p)
register u_char *p;
{
    register int    cnt;

#ifdef DDADEBUG
    if (DDADBCH(22, 0)) {	/* no easy access to unit, assume unit 0 */
	DDALOG(LOG_DEBUG) "dda: decode_ring()\n" DDAELOG;
    }
#endif DDADEBUG

    p += 3;			/* skip to cmnd ext length */
    if (*p++ < 5)		/* is count appropriate */
	return (0);		/* return false if not */

    /* called address */
    if ((cnt = *p + 1) > 16)	/* is called addr len legal? */
	return (0);		/* return false if not */
    bcopy(p, ddacb_called_addr, cnt);	/* copy field */
    p += cnt;

    /* calling address */
    if ((cnt = *p + 1) > 16)	/* is calling addr len legal? */
	return (0);		/* return false if not */
    bcopy(p, ddacb_calling_addr, cnt);	/* copy field */
    p += cnt;

    /* protocol part of user data */
    if ((cnt = *p + 1) > 5)	/* is protocol len legal? */
	return (0);		/* return false if not */
    bcopy(p, ddacb_protocol, cnt);	/* copy field */
    p += cnt;

    /* facilities */
    if ((cnt = *p + 1) > 64)	/* is facilities len legal? */
	return (0);		/* return false if not */
    bcopy(p, ddacb_facilities, cnt);	/* copy field */
    p += cnt;

    /* ignore rest of user data for now */

#ifdef	DDA_PAD_OR_RAW
    if (ddacb_protocol[0] == 0)
	return (0);
#else DDA_PAD_OR_RAW
    if ((ddacb_protocol[0] == 0) || (ddacb_protocol[1] != X25_PROTO_IP))
	return (0);		/* bad if not IP */
#endif DDA_PAD_OR_RAW

#ifndef DDA_PAD_OR_RAW
    return (1);			/* looks ok */
#else
#  ifdef DDA_RAWOPT
    return (1);			/* anything is ok if we're PI interface */
#  else
    if (ddacb_protocol[1] == X25_PROTO_IP || ddacb_protocol[1] == X25_PROTO_X29)
	return (1);		/* looks ok */
    else
	return (0);		/* bad if not IP or X29 */
#  endif
#endif DDA_PAD_OR_RAW
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      CLEAR_LCN()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine clears an X25 circuit and releases any buffers  */
/*    queued for transmission.                                     */
/*                                                                 */
/*  Call:              clear_lcn(ds, dc)                           */
/*  Argument:          ds:   pointer to dev control block struct   */
/*                     dc:  pointer to the Logical Channel control */
/*                            block structure                      */
/*  Returns:           nothing                                     */
/*  Called by:         ddatimer()                                  */
/*  Calls to:          IF_DEQUEUE()                                */
/*                     m_freem()                                   */
/*                     send_supr()                                 */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
clear_lcn(ds, dc)
struct dda_softc *ds;
struct dda_cb  *dc;
{
    register struct mbuf *m;
    register int    s;

#ifdef DDADEBUG
    if (DDADBCH(23, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: clear_lcn(%d)\n", ds->dda_if.if_unit,
	    dc->dc_lcn DDAELOG;
    }
#endif DDADEBUG

    if (dc->dc_state == LC_CLR_PENDING) {	/* Unfortunately, we can't
						 * display the destination's
						 * IP address, as we cleared
						 * it when we entered
						 * clear-pending state (to
						 * prevent new data from
						 * being queued to this
						 * channel). */
	DMESG(ds->dda_if.if_unit, 37,
	      (DDALOG(LOG_ERR) "dda%d: Clear request lost -- lcn %d\n",
	       ds->dda_if.if_unit, dc->dc_lcn DDAELOG));
	return;
    }
    hist_lcn_state(ds->dda_if.if_unit, dc->dc_state, LC_CLR_PENDING);
    dc->dc_state = LC_CLR_PENDING;	/* set state */
    dc->dc_timer = TMO_CLR_PENDING;	/* start clear timer */
    dc->dc_inaddr.s_addr = 0;	/* clear associated address */
    dc->dc_key.key_addr.s_addr = 0;
    dc->dc_wsizein = dc->dc_wsizeout = 0;
    dc->dc_pktsizein = dc->dc_pktsizeout = 0;
/*
 *  Raise priority whenever dc_oq is touched.
 */
    s = splimp();
    while (dc->dc_oq.ifq_len) {	/* drop any pending data */
	IF_DEQUEUE(&dc->dc_oq, m);
	m_freem(m);
    }
    splx(s);
    send_supr(ds, CLEARLC, (int) dc->dc_lcn * 2, 0);	/* send clear msg */
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      SEND_RESTART()                         %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine marks all LCNs as being in a restarting state   */
/*    and sends a restart command to X25.                          */
/*                                                                 */
/*  Call:              send_restart(ds)                            */
/*  Argument:          ds:   pointer to dev control block struct   */
/*  Returns:           nothing                                     */
/*  Called by:         ddatimer()                                  */
/*                     supr_msg()                                  */
/*  Calls to:          IF_DEQUEUE()                                */
/*                     m_freem()                                   */
/*                     send_supr()                                 */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
send_restart(ds)
struct dda_softc *ds;
{
    register struct dda_cb *dc;
    register int    lcn;
    register int    maxlcn;

#ifdef DDADEBUG
    if (DDADBCH(24, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d: send_restart()\n", ds->dda_if.if_unit DDAELOG;
    }
#endif DDADEBUG

    dc = ds->dda_cb;
    /* LCNLINK */
    maxlcn = nddach[ds->dda_if.if_unit];
    for (lcn = 0; lcn <= maxlcn; lcn++) {	/* for all LCN's */
	dc->dc_state = LC_RESTART;	/* set state */
	dc->dc_timer = TMO_RESTART;	/* start restart timeout */
	dc->dc_inaddr.s_addr = 0;	/* forget address */
	dc->dc_key.key_addr.s_addr = 0;
	dc->dc_wsizein = dc->dc_wsizeout = 0;
	dc->dc_pktsizein = dc->dc_pktsizeout = 0;
	dc++;
    }
    hist_all_lcns(ds->dda_if.if_unit, LC_RESTART);
    abort_io(ds->dda_if.if_unit, ALL_CHANS);
    send_supr(ds, RESTART, 0, 0);	/* send restart msg */
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      SEND_SUPR()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine is used to send short (4 bytes only) supervisor */
/*    commands, except that longer ANSWER messages may be sent.    */
/*                                                                 */
/*  Call:              send_supr(ds, cmd, p1, p2)                  */
/*  Argument:          ds:   pointer to dev control block struct   */
/*                     cmd:  type of command                       */
/*                     p1:   2nd byte of supervisor message        */
/*                     p2:   3rd byte of supervisor message        */
/*  Returns:           nothing                                     */
/*  Called by:         supr_msg()                                  */
/*                     clear_lcn()                                 */
/*                     send_restart()                              */
/*  Calls to:          MGET()                                      */
/*                     DDALOG()                                    */
/*                     mtod()                                      */
/*                     IF_ENQUEUE()                                */
/*                     dda_start()                                 */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
send_supr(ds, cmd, p1, p2)
struct dda_softc *ds;
int             cmd, p1, p2;
{
    struct mbuf    *m;
    register u_char *cp;
    u_char         *savcp, *fp, *svcp;
    int             i, faclen;

    MGET(m, M_DONTWAIT, MT_DATA);

    if (m == 0) {
	DMESG(ds->dda_if.if_unit, 23,
	      (DDALOG(LOG_ERR) "dda%d: failed to get supr msg bfr!\n",
	       ds->dda_if.if_unit DDAELOG));
	return;
    }
    cp = savcp = mtod(m, u_char *);

    /* build supervisor message */

    *cp++ = (u_char) cmd;
    *cp++ = (u_char) p1;
    *cp++ = (u_char) p2;
    *cp++ = 0;

    m->m_len = 4;

    if (cmd == ANSWER) {
	register struct dda_cb *dc;

	/* for answer messages p1 is (lcn * 2) */
	dc = &(ds->dda_cb[p1 / 2]);
	*cp++ = 0;		/* zero length called address */
	*cp++ = 0;		/* zero length calling address */
	*cp++ = 0;		/* zero length protocol */

	/* check and copy facilities */
	faclen = 0;
	svcp = cp++;
	for (i = 0, fp = &ddacb_facilities[1]; i < ddacb_facilities[0];) {
	    switch (*fp & 0xc0) {
	      case 0x00:	/* single octet parameter field */
		i += 2;
		fp += 2;
		break;
	      case 0x40:	/* double octet parameter field */

		/*
		 * Note that this code can in some cases attempt to negotiate
		 * the packet size or window away from the default, which
		 * appears to violate the X.25 spec. In fact, the FEP
		 * examines these values and bounds them between the
		 * requested value and the default value thus satisfying X.25 
		 */
		switch (*fp) {
		  case X25_FACIL_PKTSIZE:	/* 0x42, packet size */
		    *cp++ = X25_FACIL_PKTSIZE;
		    if (ds->dda_firmrev < 0x21) {
			*cp++ = PKTSIZE_DEF;	/* Set incoming and outgoing */
			*cp++ = PKTSIZE_DEF;	/* packet size to default */
			dc->dc_pktsizein = dc->dc_pktsizeout = PKTSIZE_DEF;
		    } else {
			*cp++ = *(fp + 1);	/* Answer with requested */
			*cp++ = *(fp + 2);	/* facilities */
			dc->dc_pktsizeout = *(fp + 1);
			dc->dc_pktsizein = *(fp + 2);
		    }
		    faclen += 3;
		    break;
		  case X25_FACIL_WINSIZE:	/* 0x43, window size */
		    *cp++ = X25_FACIL_WINSIZE;
		    if (ds->dda_firmrev < 0x21) {
			*cp++ = WINSIZE_DEF;	/* Set incoming and outgoing */
			*cp++ = WINSIZE_DEF;	/* window size to default */
			dc->dc_wsizein = dc->dc_wsizeout = WINSIZE_DEF;
		    } else {
			*cp++ = *(fp + 1);	/* Answer with requested */
			*cp++ = *(fp + 2);	/* facilities */
			dc->dc_wsizeout = *(fp + 1);
			dc->dc_wsizein = *(fp + 2);
		    }
		    faclen += 3;
		    break;
		}
		i += 3;
		fp += 3;
		break;
	      case 0x80:	/* triple octet parameter field */
		i += 4;
		fp += 4;
		break;
	      case 0xc0:	/* variable-length parameter field */
		fp++;
		i += 2 + *fp;
		fp += 1 + *fp;
		break;
		/* Note: No other cases (i.e., default) possible */
	    }
	}

	if (faclen) {		/* Found facilities to negotiate! */
	    *svcp = faclen;	/* facility length <- faclen */
	    *cp++ = 0;		/* user data length <- 0 */
	    *(savcp + 3) = cp - savcp - 4;	/* set supv message length */
	    m->m_len = cp - savcp;	/* set mbuf message length */
	}
    }				/* (end of answer message case) */
# ifdef DDADEBUG
    if (DDADBCH(25, ds->dda_if.if_unit)) {
	prt_bytes(ds->dda_if.if_unit, "send_supr", savcp, m->m_len);
    }
#endif DDADEBUG
    start_supr(ds, m);
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%				START_SUPR()			       %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*	Start i/o on the supervisor channel, checking for queue full.	 */
/*	Added to revision 2.0 so that "queue full" checking would be	 */
/*	applied uniformly to all supervisory channel output.		 */
/*                                                                       */
/*  Call:          start_supr(ds, m)                                     */
/*  Argument:      ds:  softc structure for board			 */
/*		   m:	mbuf holding message				 */
/*  Returns:       nothing                                               */
/*  Called by:     send_supr(), send_config(), make_x25_call()		 */
/*  Calls to:      DDALOG(), dda_start(), IF_ENQUEUE()			 */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
start_supr(ds, m)
struct dda_softc *ds;
struct mbuf    *m;
{
    register int    s;


#ifdef DDADEBUG
    if (DDADBCH(27, ds->dda_if.if_unit))
	DDALOG(LOG_DEBUG) "dda%d: start_supr\n", ds->dda_if.if_unit DDAELOG;
#endif DDADEBUG

    if (IF_QFULL(&(ds->dda_cb[0].dc_oq))) {
	DMESG(ds->dda_if.if_unit, 27,
	(DDALOG(LOG_ERR) "dda%d: supervisory channel overflow (maxlen=%d)\n",
	 ds->dda_if.if_unit, ds->dda_cb[0].dc_oq.ifq_maxlen DDAELOG));
	ds->dda_cb[0].dc_oq.ifq_maxlen += ds->dda_cb[0].dc_oq.ifq_maxlen;
    }
/*
 *  Raise priority whenever you touch dc_oq.  
 *  We do not want to be interrupted in the middle of adding
 *  an mbuf to the output queue because the interrupt may indicate
 *  a condition that will cause the mbuf to be freed.
 *  (The mbufs are freed on receipt of a line status msg, restart,
 *  clear, or reset.)
 */
    s = splimp();
#ifdef DDA_PAD_OR_RAW
    m->m_dat[MLEN - 1] = 0;
#endif
    IF_ENQUEUE(&(ds->dda_cb[0].dc_oq), m);
    splx(s);
    dda_start(ds, &(ds->dda_cb[0]));
}


/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%				ABORT_IO()   		               %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*	Abort outstanding I/O upon receipt of a line status message, 	 */
/*	restart, clear, or reset.                                        */
/*	The contents of the output queue (dc_oq) is cleared for each     */
/*	lcn;  all I/O queued on either the read or write queue           */
/*	(dc_rchan and dc_wchan) is marked invalid; all I/O queued on     */
/*	the sioq is marked invalid;                                      */
/*                                                                       */
/*  Call:          abort_io()        			                 */
/*  Argument:      none						         */
/*  Returns:       nothing                                               */
/*  Called by:                                                           */
/*  Calls to:      IF_DEQUEUE()                                          */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
PRIVATE void
abort_io(unit, lcn)
int             unit, lcn;
{
    register struct dda_cb *dc;
    register struct dda_softc *ds = &dda_softc[unit];
    register struct hdx_chan *hc;
    register struct mbuf *m;
    register int    lchan;
    register int    s;
    register struct hdx_chan *ptr;
    int             start, end;

    /* set up range of lcns affected */
    if (lcn == ALL_CHANS) {
	start = 1;
	end = nddach[unit];
    } else
	start = end = lcn;
#ifdef DDADEBUG
    if (DDADBCH(28, unit))
	DDALOG(LOG_DEBUG) "dda%d: abort_io on lcn's %d - %d\n",
	    unit, start, end DDAELOG;
#endif DDADEBUG
    s = splimp();
/*
 * Invalidate writes on the sioq for specified channel(s)
 */
    if (ptr = ds->dda_sioq.sq_head)
	for (; ptr; ptr = ptr->hc_next)	/* scan sioq */
	    if ((ptr->hc_chan & 0x01) &&
		((lcn == ALL_CHANS) || (lcn == ptr->hc_chan >> 1))
		&& (ptr->hc_chan != 1)) {
#ifdef DDADEBUG
		if (DDADBCH(28, unit))
		    DDALOG(LOG_DEBUG)
			"dda%d: abort_io--invalidating sioq lcn %d\n",
			unit, ptr->hc_chan >> 1 DDAELOG;
#endif DDADEBUG
		ptr->hc_inv |= INVALID_MBUF;
	    }
/*
 * For each selected lcn, clear the output queue and
 * add an hdx struct to the sioq that will generate an
 * abort.
 */
    for (lchan = start; lchan <= end; lchan++) {	/* for selected LCNs */
	dc = &dda_softc[unit].dda_cb[lchan];
	hc = &dc->dc_wchan;
	while (dc->dc_oq.ifq_len) {
	    IF_DEQUEUE(&dc->dc_oq, m);
	    m_freem(m);
	}

	if (hc->hc_mbuf && !(hc->hc_inv & INVALID_MBUF)) {
	    if (dc->dc_flags & DC_OBUSY) {	/* output pending */
#ifdef DDADEBUG
		if (DDADBCH(28, unit))
		    DDALOG(LOG_DEBUG)
			"dda%d: abort_io--queueing abort: lcn %d\n",
			unit, lchan DDAELOG;
#endif DDADEBUG

		hc->hc_inv |= INVALID_MBUF;
		hc->hc_func = DDAABT;
/*
 * Add to the sioq
 */
		dda_wrq(ds, hc, DDAABT);
	    }
	}
    }
    splx(s);
}

#ifdef DDADEBUG


/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                            PRT_BYTES()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*    This routine is used to print a label, followed by the contents of */
/*	a buffer in hex, 16 bytes per line.  Each line is preceded by	 */
/*	the device name and unit number.				 */
/*                                                                       */
/*  Call:          prt_bytes(unit, label, bp, cnt)			 */
/*  Argument:      unit: dda unit number to be displayed		 */
/*		   label: pointer to string to be displayed		 */
/*		   bp:  pointer to the buffer to be dumped		 */
/*                 cnt: number of bytes in buffer			 */
/*  Returns:       nothing                                               */
/*  Called by:     dda_data()                                            */
/*                 dda_supr()                                            */
/*                 supr_msg()                                            */
/*  Calls to:      DDALOG()                                              */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
PRIVATE void
prt_bytes(unit, label, bp, cnt)
int             unit;
char           *label;
u_char         *bp;
int             cnt;
{
    char            hexbuf[50];	/* (worst case: 3 * 16 + 1 = 49 bytes) */
    char           *p;
    int             i;
    static char     hex[] = "0123456789abcdef";

    DDALOG(LOG_DEBUG) "dda%d: %s\n", unit, label DDAELOG;
    while (cnt > 0) {
	i = (cnt > 16) ? 16 : cnt;
	cnt -= i;
	p = hexbuf;
	while (--i >= 0) {
	    *p++ = ' ';
	    *p++ = hex[*bp >> 4];
	    *p++ = hex[*bp++ & 0x0f];
	}
	*p++ = '\0';
	DDALOG(LOG_DEBUG) "dda%d: %s\n", unit, hexbuf DDAELOG;
    }
}

#endif



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                            FMT_X25()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*    This routine is used to format an X.25 address for inclusion in	 */
/*    an error message.  The previous return value is invalidated each	 */
/*    time the function is called, as it is stored in a static buffer	 */
/*  Note:          The X.25 address is apparently sometimes stored in    */
/*                 BCD, and other times (PDN mode) in ASCII.  So we mask */
/*                 off the high order bits to make ourselves immune.	 */
/*  Call:          fmt_x25(bp, cnt)                                      */
/*  Argument:      bp:  pointer to the string                            */
/*                 cnt: number of bytes (usually from address[0])        */
/*  Returns:       pointer to an internal buffer containing the string;	 */
/*		   string is 1 to 15 digits, null-terminated.		 */
/*  Called by:     make_x25_call()                                       */
/*                 supr_msg()                                            */
/*                 convert_x25_addr()					 */
/*  Calls to:      none							 */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
PRIVATE char *
fmt_x25(bp, cnt)
register u_char *bp;
register int    cnt;
{
    char           *p;
    static char     x25buf[20];	/* worst case is 15 digits plus trailing null */

    /* (Don't put this on the stack!) */
    p = x25buf;
    if (cnt >= sizeof(x25buf))
	cnt = sizeof(x25buf) - 1;	/* (oops!) */
    while (cnt--)
	*p++ = (*bp++ & 0x0f) + '0';
    *p++ = '\0';
    return (x25buf);
}

#ifdef DDA_HISTOGRAM
/*----------------------- HISTOGRAM SUPPORT ---------------------------------*/


/* the histogram array */
struct timeval  histogram[NDDA][HISTSIZE];

/* these two structures save the time of the last change in the state of the
 * lcn table or the board status.
 */

struct timeval  last_lcn_time[NDDA] = {0L, 0L};
struct timeval  last_brd_time[NDDA] = {0L, 0L};

/* h_lcn_level: the current number of active lcns */
int             h_lcn_level[NDDA] = {0};

/*#define DDA_HIST_DEBUG 1     /* set this to debug history features */


/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                           HIST_INIT()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*    This routine initializes the histogram facility when coming up or  */
/*    after a reset.                                                     */
/*  Call:          hist_init(unit,reset)                                 */
/*  Argument:      unit - board number to initialize.                    */
/*                 reset - set to 1 to force an init.                    */
/*  Returns:       nothing.                                              */
/*  Called by:     ddaioctl()                                            */
/*  Calls to:      microtime()                                           */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
hist_init(unit, reset)
int             unit;
int             reset;
{
    int             s;
    register int    i;
    struct dda_cb  *dc;

    if (last_lcn_time[unit].tv_sec != 0L && !reset)
	return;			/* histogram for this unit already enabled */
    bzero(histogram[unit], sizeof(struct timeval) * HISTSIZE);
    h_lcn_level[unit] = 0;
    dc = dda_softc[unit].dda_cb;
    s = splimp();
    for (i = 0; i < NDDACH + 1; i++) {
	if (dc++->dc_state == LC_DATA_IDLE)
	    h_lcn_level[unit]++;
    }
    splx(s);
    microtime(&histogram[unit][H_START]);
#ifdef DDA_HIST_DEBUG
    DDALOG(LOG_DEBUG) "hist_init: starting at level %d\n",
	h_lcn_level[unit] DDAELOG;
#endif
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                     HIST_LCN_STATE()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*    This routine changes the histogram depending on how the state of   */
/*    a channel has changed.                                             */
/*  Call:          hist_lcn_state(unit, old_state, new_state)            */
/*  Argument:      old_state: the old state of the lcn.                  */
/*                 new_state: the state the lcn is changing to.          */
/*                 unit: unit this applies to                            */
/*  Returns:       nothing.                                              */
/*  Called by:                                                           */
/*  Calls to:      timevalsub(), timevaladd(), microtime()               */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
hist_lcn_state(unit, old_state, new_state)
int             unit;
u_char          old_state;
u_char          new_state;
{
    struct timeval  tv, tmpv;

    /*
     * this structure for determining state transitions is much more general
     * than is necessary right now.  However it allows easy changes to the
     * state transition table for the histogram so I will leave it in until
     * it settles down 
     */
    switch (old_state) {
      case LC_DATA_IDLE:
	switch (new_state) {
	  case LC_DATA_IDLE:
	    break;
	  default:		/* all other states */
	    microtime(&tv);
	    tmpv = tv;
	    timevalsub(&tv, &last_lcn_time[unit]);
#ifdef DDA_HIST_DEBUG
	    DDALOG(LOG_DEBUG) "hist_lcn_state: adding %ld.%ld to level %d--\n",
		tv.tv_sec, tv.tv_usec, h_lcn_level[unit] DDAELOG;
#endif
	    timevaladd(&histogram[unit][h_lcn_level[unit]], &tv);
	    last_lcn_time[unit] = tmpv;
	    if (--h_lcn_level[unit] < 0)	/* safety net for driver
						 * errors */
		h_lcn_level[unit] = 0;
	    break;
	}
	break;
      default:
	switch (new_state) {
	  case LC_DATA_IDLE:
	    microtime(&tv);
	    tmpv = tv;
	    timevalsub(&tv, &last_lcn_time[unit]);
#ifdef DDA_HIST_DEBUG
	    DDALOG(LOG_DEBUG) "hist_lcn_state: adding %ld.%ld to level %d++\n",
		tv.tv_sec, tv.tv_usec, h_lcn_level[unit] DDAELOG;
#endif
	    timevaladd(&histogram[unit][h_lcn_level[unit]], &tv);
	    last_lcn_time[unit] = tmpv;
	    if (++h_lcn_level[unit] > NDDACH)	/* safety net for driver
						 * errors */
		h_lcn_level[unit] = NDDACH;
	    break;
	  default:		/* all other states */
	    break;
	}
	break;
    }
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                     HIST_ALL_LCNS()                               %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*    This routine changes the histogram when the state of all the lcns  */
/*    are changed as a group.                                            */
/*  Call:          hist_lcn_state(unit, state)                           */
/*  Argument:      state: state that all lcn are going to.  Currently not*/
/*                        used.                                          */
/*                 unit: unit this applies to                            */
/*  Returns:       nothing.                                              */
/*  Called by:                                                           */
/*  Calls to:      timevalsub(), timevaladd(), microtime()               */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
hist_all_lcns(unit, state)
int             unit, state;
{
    struct timeval  tmpv, tv;

#ifdef lint
    state = state;
#endif
    if (last_brd_time[unit].tv_sec == 0L
	|| last_lcn_time[unit].tv_sec == 0L)
	return;			/* see if we have initialized yet */
    microtime(&tv);
    tmpv = tv;
    timevalsub(&tv, &last_lcn_time[unit]);
#ifdef DDA_HIST_DEBUG
    DDALOG(LOG_DEBUG) "hist_all_lcns: adding %ld.%ld to level %d\n",
	tv.tv_sec, tv.tv_usec, h_lcn_level[unit] DDAELOG;
#endif
    timevaladd(&histogram[unit][h_lcn_level[unit]], &tv);
    last_lcn_time[unit] = tmpv;
    h_lcn_level[unit] = 0;
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                     HIST_LINK_STATE()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*    This routine changes the histogram depending on how the state of   */
/*    the link has changed.                                              */
/*  Call:          hist_link_state(old_state, new_state)                 */
/*  Argument:      old_state: the old state of the link.                 */
/*                 new_state: the state the link is changing to.         */
/*                 unit: unit this applies to                            */
/*  Returns:       nothing.                                              */
/*  Called by:                                                           */
/*  Calls to:      timevalsub(), timevaladd(), microtime()               */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
hist_link_state(unit, old_state, new_state)
int             unit;
u_char          old_state;
u_char          new_state;
{
    struct timeval  tv, tmpv;

    /*
     * this structure for determining state transitions is much more general
     * than is necessary right now.  However it allows easy changes to the
     * state transition table for the histogram so I will leave it in until
     * it settles down 
     */
    switch (old_state) {
      case S_LINK_UP:
	switch (new_state) {
	  case S_LINK_UP:
	    break;
	  default:		/* all other states */
#ifdef DDA_HIST_DEBUG
	    DDALOG(LOG_DEBUG) "hist_link_state: link down\n" DDAELOG;
#endif
	    microtime(&tv);
	    tmpv = tv;
	    timevalsub(&tv, &last_lcn_time[unit]);
	    timevaladd(&histogram[unit][h_lcn_level[unit]], &tv);
	    tv = tmpv;
	    timevalsub(&tv, &last_brd_time[unit]);
	    timevaladd(&histogram[unit][H_LINK_UP], &tv);
	    last_brd_time[unit].tv_sec = 0L;
	    break;
	}
	break;
      default:			/* all other states */
	switch (new_state) {
	  case S_LINK_UP:
#ifdef DDA_HIST_DEBUG
	    DDALOG(LOG_DEBUG) "hist_link_state: link up\n" DDAELOG;
#endif
	    microtime(&last_brd_time[unit]);

	    /*
	     * reset last_lcn_time so 0 entry will not accumulate the time
	     * that we were down 
	     */
	    last_lcn_time[unit] = last_brd_time[unit];
	    break;
	  default:
	    break;
	}
	break;
    }
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                         HIST_READ()                               %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*    This routine prepares the histogram table for reading by making    */
/*    all entries current.                                               */
/*  Call:          hist_read(unit)                                       */
/*  Argument:      unit : board to use.                                  */
/*  Returns:       nothing                                               */
/*  Called by:     ddaioctl()                                            */
/*  Calls to:      timevalsub(), timevaladd(), microtime()               */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
hist_read(unit)
int             unit;
{
    struct timeval  tmpv, tv;

    microtime(&tv);
    tmpv = tv;
    histogram[unit][H_END] = tmpv;
    histogram[unit][H_TMO].tv_sec = tmo_data_idle * DDA_TIMEOUT;
    histogram[unit][H_TMO].tv_usec = 0L;
    if (last_brd_time[unit].tv_sec) {
	timevalsub(&tv, &last_lcn_time[unit]);
#ifdef DDA_HIST_DEBUG
	DDALOG(LOG_DEBUG) "hist_read: adding %ld.%ld to level %d\n",
	    tv.tv_sec, tv.tv_usec, h_lcn_level[unit] DDAELOG;
#endif
	timevaladd(&histogram[unit][h_lcn_level[unit]], &tv);
	last_lcn_time[unit] = tmpv;
	tv = tmpv;
	timevalsub(&tv, &last_brd_time[unit]);
	timevaladd(&histogram[unit][H_LINK_UP], &tv);
	last_brd_time[unit] = tmpv;
    }
}



/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                         HIST_COPYOUT()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*    This routine prepares the histogram table for reading by making    */
/*    all entries current.                                               */
/*  Call:          hist_copyout(unit, to)                                */
/*  Argument:      unit : board to use.                                  */
/*                 to   : address in user space to copy to.              */
/*  Returns:       return value from copyout                             */
/*  Called by:     ddaioctl()                                            */
/*  Calls to:      copyout()                                             */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE int
hist_copyout(unit, to)
int             unit;
caddr_t         to;
{
    return ((copyout(histogram[unit], to, sizeof(struct timeval) * HISTSIZE)));
}

#endif DDA_HISTOGRAM

#ifdef DDA_PAD_OR_RAW

#if ACC_BSD > 42
#  include "uba.h"
#  include "bk.h"
#  include "conf.h"
#  include "proc.h"
#  include "tty.h"
#  include "map.h"
#  include "vm.h"
#  include "bkmac.h"
#  include "clist.h"
#  include "file.h"
#  include "uio.h"
#endif

#if ACC_BSD == 42 || ACC_ULTRIX > 00
#  include "bk.h"
#  include "../h/conf.h"
#  include "../h/proc.h"
#  include "../h/tty.h"
#  include "../h/map.h"
#  include "../h/vm.h"
#  if ACC_ULTRIX > 12
#    include "uba.h"
#  endif
#  include "../h/bk.h"
#  ifdef SIMULATION
#    include "Clist.h"
#  else
#    include "../h/clist.h"
#  endif
#  include "../h/file.h"
#  include "../h/uio.h"
#endif

PRIVATE int
dda_decode_type(ds, p)
struct dda_softc *ds;
u_char         *p;
{
    register u_char *cp;
    int             i, usrlen;

#ifdef DDADEBUG
    if (DDADBCH(20, ds->dda_if.if_unit)) {
	printf(" dda_decode_type():  p[0]= %x ", *p);
    }
#endif DDADEBUG

    switch (p[0]) {
      case LINE_STATUS:	/* link status msg */
      case RESTART:		/* restart received */
      case RSTRT_ACK:		/* restart ack */
      case STATRESP:		/* Statistics Response from FEP */
      case CLEARVC:		/* clear by VCN */
	return (0);
      case RESET:		/* X25 reset */
	return (1);
      case ANSWER:
      case CLEARLC:
      case INTERRUPT:
      case INTR_ACK:
	i = p[1] / 2;		/* get lcn */
	if (ds->dda_cb[i].dc_flags & (DC_X29 | DC_X29W))
	    return (1);
	else if (ds->dda_cb[i].dc_flags & (DC_RAW))
	    return (2);
	else
	    return (0);
    }
    if (p[0] != RING) {		/* let standard dda handle it */
	return (0);
    }
    cp = p + 4;			/* skip over code, lcn, vcn and count in
				 * (ring?) answer message */
    /* cp now points to length of called address */
    cp += *cp + 1;		/* skip over called address and length byte */
    /* cp now points to length of calling address */
    cp += *cp + 1;		/* skip over calling address and length byte */
    /* cp now points to length of protocol */
    if (*cp == 0)
	return (0);

    usrlen = *cp++;
    if (usrlen) {
#ifdef DDA_RAWOPT
	if (pi_circuit_to_handle_protocol(*cp))
	    return (2);
#endif
#ifdef DDADEBUG
	if (DDADBCH(20, ds->dda_if.if_unit)) {
	    printf(" dda_decode_type():  return value = %x ", *cp);
	}
#endif DDADEBUG
	switch (*cp) {
	  case X25_PROTO_IP:
	    return (0);
	  case X25_PROTO_X29:
	    return (1);
	  default:
	    return (2);
	}
    } else
	return (0);
}
#endif DDA_PAD_OR_RAW

#ifdef SIMULATION
#  ifdef DDA_PADOPT
#    include "if_x29.c"
#  endif
#  ifdef DDA_RAWOPT
#    include "if_pi.c"
#  endif
#else
#  ifdef DDA_PADOPT
#    if ACC_VMS > 00
#      include "../vaxif/if_vmsx29.c"
#    else
#      include "../vaxif/if_x29.c"
#    endif
#  endif
#  ifdef DDA_RAWOPT
#    include "../vaxif/if_pi.c"
#  endif
#endif

#ifdef DDA_MSGQ
u_char          ddamsgq[MSGQSIZE];
PRIVATE u_char  *mqptr = 0;

#define MSGQEND	(ddamsgq+MSGQSIZE)

dda_mqstr(s)
char           *s;
{
    if (mqptr == 0)
	mqptr = ddamsgq;
    while (*s) {
	*mqptr++ = *s++;
	if (mqptr >= MSGQEND)
	    mqptr = ddamsgq;
    }
    *mqptr = '\0';
}

dda_mqnum(num, type)
int             num, type;
{
    if (mqptr == 0)
	mqptr = ddamsgq;
    if ((mqptr + sizeof(int) + 2) >= MSGQEND)
	mqptr = ddamsgq;
    *mqptr++ = type;
    *((int *) mqptr) = num;
    mqptr += sizeof(int);
    *mqptr = '\0';
}

#endif DDA_MSGQ

/* link in support for steve's test-jig */
#ifdef	SIMULATION
#include "if_dda_sim.c"
#endif

/*
		Revision History:

18-Dec-87: V3.0 - Brad Engstrom
	Added the -t flag to acpconfig and the 't' case in ddaioctl to allow
	setting of the idle circuit timeout.
	The constant TMO_DATA_IDLE was changed to a variable called
	tmo_data_idle.
11-Mar-88: V3.0 - Brad Engstrom
	Modified the history routine to return the current value of the
	timeout. Also fixed bug so that level 0 records amount of time 0
	circuits were in use only when link is up.
11-Mar-88: V3.0 - Brad Engstrom
	Changed handling of supervisor channel overflows to double the max q
	length each time it overflows.  This Will prevent a flood of console
	messages while still notifying the user that there has been an
	overflow.
21-Mar-88: V3.0 - Brad Engstrom
	Fixed bug in writing the facilities field for packet and window size
	negotiation.  This was in the routine make X.25 call.  Previously
	constants were used to index into the facilities buffer now offsets
	from the current facilities length are used.
12-Apr-88: V3.0 - Brad Engstrom
	Added ability to handle class b and class c addressing.  The changes
	affect locate_x25_lcn, convert_x25_addr, and convert_ip_addr.  The
	modifications came from fixes sent to Wollongong by Lars Poulson.
12-Apr-88: V3.0 - Brad Engstrom
	Made modifications so the driver will work under Ultrix or BSD. In
	cases where there are differences between 4.3 and 4.2 bsd (shown by
	#ifdef BSD4_3) Ultrix 1.2 is exactly like a 4.2 system. Ultrix 2.0 is
	like 4.3 in most cases. New macros were added to distinquish between
	systems.  These are BSD4_2 and BSD43_OR_ULTRIX20.
13-Apr-88: V3.0 - Brad Engstrom
	ddareset() was called from ddaintb without arguments.  This could
	cause ddareset to return without doing anything. Proper arguments were
	inserted.  In ddaioctl the priority level s may be used without being
	set.  This was fixed.
18-Apr-88: V3.0 - Brad Engstrom
	Added the use of a key field in the dda_cb structure.  Previously the
	dc_inaddr field was used both for printing the ip address (-l command)
	and for searching for circuits that were open to a destination.  Using
	this for a cicuit matching address means that the network and local
	host fields needed to be masked off, thus making this field less
	usefull for printing.  Now two fields are used dc_inaddr is used for
	printing.  dc_key is used for circuit matching.  In PDN mode the
	full ip address is used as the key.  In DDN mode just the imp number
	and host(port) number are used.
18-Apr-88: V3.0 - Brad Engstrom
	Made histogram facilities a compile time option.  The histogram is
	enabled if DDA_HISTOGRAM is defined.  The facilities are always
	disabled when using 4.2 or ULTRIX 1.2 as the kernel does not have the
	proper support routines available.
22-Apr-88: V3.0 - Brad Engstrom
	Added new option to -v command to set the dda_db_unit variable.
22-Apr-88: V3.0 - Brad Engstrom
	Added the DMESG macro and the msgbits array to allow selective
	disabling of driver error messages.  To enable or disable an error
	message the -c command of acpconfig is used. The msgbits array holds
	info about whether each message is enabled or disabled.  Setting a bit
	to 1 disables a message.  Clearing a bit to 0 enables a message.
	All messages start as enabled.
22-Apr-88: V3.0 - Brad Engstrom
	Added check for DDAMAINT_BRD in probe routine.  If DDAMAINT_BRD is
	defined then assume we are using a maintenence board so don't try to
	find the firmware id because it won't be there. Fake info that was
	supposed to be contained in the firmware id.
25-Apr-88: V3.0 - Brad Engstrom
	Added check in locate_x25_lcn to see if state of lc is LC_CALL_PENDING
	or LC_DATA_IDLE in the loop that looks for an already open lc.  This
	will prevent an address of 0.0.0.0 from matching a circuit that is not
	in use.  If the address is invalid then the imp will kick it out.
26-Apr-88: V3.0 - Brad Engstrom
	Changed the -n command case so that a command of the form "-n 0" will
	return the number of channels currently available.  This will be used
	by the -l command and possible by the -h command to determine the
	number of available circuits.
10-May-88: V3.0 - Brad Engstrom
	Made all occurences of the length of and X.25 address refer to the
	constants MAXADDRLEN and MINADDRLEN defined in if_ddavar.h.  These
	constants include the 1 byte  for encoding the length.
02-Jun-88: V3.0 - Brad Engstrom
	Change the check for the firmware revision level to 2.2 for the -e
	command.  This command will crash [56]250s that don't have at least
	v2.2 firmware.
12-Jul-88: V3.0 - Brad Engstrom
	Deleted case for class_b_c addressing.
20-Jul-88: V3.0 - Brad Engstrom
	Fixed bug in parsing facilities that would cause the kernel to hang.
	The bug was not incrmenting pointers when an urecognized 2 octet
	facility was seen.  Fixes were applied to send_supr() and
	decode_answer()
30-Aug-88: V4.0 - Brad Engstrom
	Modified driver to support X.29 and a programmers interface.  Includes
	files if_x29.c, if_pi.c, and if_pivar.h
30-Aug-88: V4.0 - Brad Engstrom
	Added support for debug logging under the control of the DDA_MSGQ
	define.   Information is extracted using the new -p command of
	acpconfig.
30-Aug-88: V4.0 - Brad Engstrom
	Modified start_chan to check the ready bit before touching the
	comregs.  Also modified dda_rrq and dda_wrq to raise ipl before
	touching the sioq.  These changes fixed a bug where the FE was losing
	I/O requests.
20-Oct-88: V4.0 - Steve Johnson
	Added SIMULATION #ifdef for simulation support
08-Jan-89: V4.1 - Steve Johnson
	MERGE 4.0 and 3.1
10-Oct-88: V3.1 - Charles Carvalho
	Replace prt_x25 with fmt_x25, which returns a pointer to a formatted
	message instead of printing its data; this allows error messages to be
	output with a single call to DDALOG (or syslog).  Move prt_addr
	inline, for same reason.  Add IP address to some error messages;
	trim excess text from some error messages.  Allocate channels
	for incoming calls from lowest channel up; we do linear searches of
	the lcn table, so it's to our advantage to use the lowest numbers for
	all active circuits. (The lcn is not related to the virtual circuit
	number, so there is no need to allocate incoming channels from the
	top down.)  Modify prt_bytes to take unit number and descriptive
	string to be printed along with the buffer and byte count; it now
	formats up to 16 bytes at a time and prints a full line with each call
	to DDALOG rather than calling DDALOG for each byte.
17-Oct-88: V3.1 - Charles Carvalho
	Add definitions for DDALOG and DDAELOG, which translate into a call to
	DDALOG() or log().
26-Oct-88: V3.1 - Charles Carvalho
	Change index for 'v' ioctl to preserve compatibility with previous 
	versions.  Restrict maximum window size to 127, not 128.
7-Nov-88: V3.2 - Charles Carvalho
	Fix check for no free circuits when processing RING
17-Feb-89: V4.3.0 - Paul Traina
	Added TGV changes for Multinet.
8-Mar-89: V4.3.1 - Steve Johnson
	Installed 'Q' ioctl to support obtaining an internal trace log used
	for debugging only -- not documented for general user.  acpconfig
	dda0 -q 2 dumps 256 bytes from the dda_debug_silo[] array
13-Mar-89: V4.3.2 - Paul Traina
	Updated Multinet support.
17-Apr-89: V4.3.3 - Steve Johnson
	Split bus and simulation related code out to included files for first
	shot at 7000 and tahoe design.  Don't reset timeout counter in
	dda_data() unless link really is in idle state.
28-Apr-89: V4.3.4 - Paul Traina
	Modified changes of 17-Apr-89, added minimal tahoe support until
	driver modified to use 4.3uba transfers.
	Fixed timeout fix of 17-Apr-89 to do what was intended.
	Fixed code dealing with maintenance board, reformatted with indent
	to repair readablility.
09-May-89: V4.3.5 - Paul Traina
	Minimal tahoe support completed,  based on BSD4_3TAHOE define which
	must be uncommented manually.  Finalizing for ECO.
24-May-89: V4.3.6 - Paul Traina
	Ultrix 3.0 support added.  Revised 4.3 tahoe support for automatic
	invocation.
	*** NOTE: one of the three OS defines (ACC_BSD, ACC_ULTRIX, ACC_VMS)
	    in if_dda.c must be set to a non-zero value for the driver to
	    compile.
	Attempting multiple-os support based upon weird variables from include
	files is not acceptable with the latest proliferation of OS versions.
20-Jun-89: V4.3.7 - Paul Traina
	Removed crufty old debug stuff and integrated it with the log-message
	code.  Now X29 and PI modules can be debuged properly (no #if 0's!).
22-Jun-89:	  - Paul Traina
	Diddled ring-decode logic to check for proper ring packet decoding
	before attempting to find a free lcn.  This will make it easier to deal
	with the race condition with find_free_lcn().
	Modified ACC os specific equates to be set as options in the config
	file.  This way, most users won't ever edit if_dda.c.
18-Jul-89:	  - Paul Traina
	Driver will no longer return errors if duplicate address-translation
	entries are made.  Errors will only happen if a redefiniton is
	attempted.
	Moved dc_key.ttyline out of union, creating dc_line.
26-Jul-89:	  - Paul Traina f/Brad Engstrom
	Added support for called user-data field (two new params to
	make_x25_call) to support extended pad mode in the X.29 module.
01-Aug-89:	  - Paul Traina
	Made ddamsgs uninitialized -- it gets inited in ddaattach now.
03-Aug-89:	  - Paul Traina
	Changed hist_copyout definition to PRIVATE.
15-Aug-89:	  - Paul Traina
	Made dda_softc and dda_iobuf non-private.
18-Aug-89:	  - Paul Traina
	Somehow, ddareset was removed from the 'z' ioctl.
28-Aug-89:	  - Paul Traina
	Changed make_x25_call so that it checks length of data to be stuffed
	into the mbuf before actually copying data in.  Removed udlen and
	ud parameters to the routine, as the public areas will be plugged
	with data before being called.  (May need to splimp()).
22-Sep-89:	  - Paul Traina
	The order of the 'v' ioctl parameters was screwed up.  This caused
	window and packet size setting to fail.
23-Oct-89:	  - Paul Traina
	Added further support for Steve's yetchy simulation.  Updated main
	module to work with BI version of dda board.
29-Oct-89:	  - Paul Traina
	Acpconfig inconsistancy (again): removed the 'p', and 'Q' ioctls.
	Since all of these are queries, I placed them under the 'q' ioctl
	with a new switch.  Some day we should just scrap the whole mess
	and design a proper ioctl interface.
11-Nov-89:	  - Paul Traina
	Moved rrq/wrq routines into bus modules because we can do several
	queue reads and writes when working with the BI.
*/
#endif NDDA > 0
