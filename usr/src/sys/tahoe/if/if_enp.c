/*	if_enp.c	1.1	86/07/20	*/

#include "enp.h"
#define ENPBPTE 128
#if NENP > 0

/*
 * Modified 3 Com Ethernet Controller interface
 * enp modifications added S. F. Holmgren
 */

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
#include "uio.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"
#include "../netinet/in.h"
#include "../h/ioctl.h"

#include "../netinet/in_systm.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"
#include "../netinet/if_ether.h"

#include "../tahoevba/vbavar.h"
#include "../tahoeif/if_enp.h"
#include "../machine/mtpr.h"
#include "../tahoeif/if_debug.h"

#define ENP0_PHYSADDR	0xf40000	/* board # 0 physical base addr */
#define ENP1_PHYSADDR	0xf60000	/* board # 1 physical base addr */
#define ENPSTART	0xf02000	/* standard enp start addr 	*/

int	enpprobe(), enpattach(), enpintr();
extern	nulldev();
caddr_t	vtoph();
struct  mbuf *m_tofree();
struct  vba_device *enpinfo[ NENP ];

/*	Maximun 2 controllers per system supported			*/

long  enpstd[] = { ENP0_PHYSADDR+0x1000,ENP1_PHYSADDR+0x1000, 0 };
extern	char	enp0utl[], enp1utl[];	/* enp accessible ram map	*/
char	*enpmap[]= { enp0utl, enp1utl };
extern	long	ENP0map[], ENP1map[];
long	*ENPmap[] = {ENP0map, ENP1map};
long	ENPmapa[] = {0xfff41000, 0xfff61000};
long	enpismapped[NENP];

unsigned short intvec[4] = 
	{ 0xc1, 0xc2, 0xc3, 0xc4 };	/* intrvec of upto 4 enps	*/

struct  vba_driver enpdriver = 
{
/* use of prom based version 
	enpprobe, 0, enpattach, 0, 0,	enpintr,
*/
	enpprobe, 0, nulldev, 0, 
	enpstd,   "enp", enpinfo, "ENP 20", 0
};

int     enpinit(),
	enpioctl(),
	enpoutput(),
	enpreset(),
	enpbroadcast(),
	enptimeout();

int	enpcopy();

struct  mbuf *enpget();

extern  struct ifnet loif;

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * es_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */

struct 	enp_softc	enp_softc[NENP];
long	stat_addr[NENP];	/* enp statistic addr (for nstat use) */
long	ring_addr[NENP];	/* enp dev ring addresses (for nstat use) */
int 	numenp = NENP;
int	enp_intr = 0, 		/* no. of enp_to_host interrupts */
	host_intr = 0;		/* no. of host_to_enp interrupts */
short	enpram[NENP];		/* open/close flags for enp devices */
/*	Debugging tools, used to trace input packets */
extern 	int	printerror;	/* error print flag, from if_ace.c */
int	save_enp_inpkt = 0;
#define	ENPTRACE(X)	if (save_enp_inpkt) X;

struct 	inp_err 	enperr[NENP];

/*
 * Probe for device.
 */

enpprobe(reg)
caddr_t reg;
{
	static	int 	unit=0;
	register ENPDEVICE	*addr = (ENPDEVICE *)reg;

	if( (badaddr( addr, 2 ) ) || (badaddr( &addr->enp_ram[0], 2 ) ) )
		return( 0 );
	addr->enp_state = S_ENPRESET; /* controller is reset by vbus reset */
	/* save address of statistic area for nstat uses	*/

	stat_addr[unit] = (long) &(addr->enp_stat); 
	ring_addr[unit++] = (long) &(addr->enp_toenp); 

	return( ENPSIZE );
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets. 
 */

enpattach( md )
register struct vba_device *md;
{
	struct enp_softc 	*es = &enp_softc[md->ui_unit];
	register struct ifnet 	*ifp = &es->es_if;
	register ENPDEVICE 	*addr = (ENPDEVICE *)md->ui_addr;
	struct sockaddr_in 	*sin;

	enpgetaddr( md->ui_unit );

	ifp->if_unit = md->ui_unit;
	ifp->if_name = "enp";
	ifp->if_mtu = ETHERMTU;

/*	bcopy(&es->es_boardaddr, es->es_enaddr, sizeof(es->es_enaddr)); */

	sin = (struct sockaddr_in *)&es->es_if.if_addr;
	sin->sin_family = AF_INET;

	ifp->if_init = enpinit;
	ifp->if_ioctl = enpioctl;
	ifp->if_output = enpoutput;
	ifp->if_reset = enpreset;
	if_attach(ifp);
}


/*
 * Reset of interface after UNIBUS reset.
 */
enpreset(unit)
int unit;
{
	register struct vba_device *md;

	if (unit >= NENP || (md = enpinfo[unit]) == 0 || md->ui_alive == 0)
		return(ENODEV);

	enpinit(unit);
}

/*
 * Initialization of interface; clear recorded pending
 * operations.
 */

enpinit( unit )
int unit;
{
	struct enp_softc 	*es = &enp_softc[unit];
	ENPDEVICE 		*addr;
	int i, s;
	u_char *cp, *ap;
	register struct ifnet 	*ifp = &es->es_if;
	register struct sockaddr_in *sin, *sinb;

	sin = (struct sockaddr_in *)&ifp->if_addr;

	if ( !enpismapped[unit] ) {
		ioaccess(ENPmap[unit],ENPmapa[unit],ENPBPTE);
		++enpismapped[unit];
	}
	if ((addr = (ENPDEVICE *)enpinfo[unit]->ui_addr) == (ENPDEVICE *)0)
		return(ENODEV);
	s = splimp();
	RESET_ENP( addr );
	DELAY( 200000 );

#ifdef notdef
/* only needed if not downloading ( ie, ROM-resident ENP code) */
	addr->enp_intrvec = intvec[unit];
	ENP_GO( addr,ENPSTART );
	DELAY( 200000 );
/* end of ROM-resident */
#endif notdef

	es->es_if.if_flags |= IFF_UP|IFF_RUNNING; /* open for business*/
	splx(s);

	if_rtinit( &es->es_if,RTF_UP );
	arpwhohas(&es->es_ac, &sin->sin_addr);
}


/*
 * Ethernet interface interrupt.
 */

enpintr( unit )
{
	register ENPDEVICE 		*addr;
	register BCB			*bcbp;
	register struct vba_device	 *md;

	enp_intr++;

	if (unit >= NENP || (md = enpinfo[unit]) == 0)
		return;

	addr = (ENPDEVICE *)md->ui_addr;

	if( IS_ENP_INTR(addr) == 0 )
		return;

	ACK_ENP_INTR( addr );

	while( (bcbp = (BCB *)ringget( &addr->enp_tohost )) != 0 )
	{
		enpread( &enp_softc[ unit ],bcbp, unit );
		ringput( &addr->enp_enpfree,bcbp ); 
	}
	return(0);
}

#define	MAXBLEN	1500
char	errpkt[MAXBLEN];
int	bufptr = 0;
int	maxl_tosave = 200;		/* save only the first 200 bytes */

saverrpkt(errbuf, errtype, len)
register u_char *errbuf;
int errtype, len;
{
	int remain, newptr;

	remain = MAXBLEN - bufptr;
	if (remain < 50)		/* if too small			*/
		return;			/* no space avail		*/
	len = (len > maxl_tosave || len <= 0) ? maxl_tosave : len;
	len = len > remain ? (remain - 2*sizeof(len)): len;
	newptr = bufptr + len + 2*sizeof(len);
	if (newptr <= MAXBLEN) {
		enpcopy((char *)&len, &errpkt[bufptr], sizeof(len));
		enpcopy((char *)&errtype, &errpkt[bufptr+sizeof(len)],
			sizeof(errtype));
		enpcopy(errbuf, &errpkt[bufptr+(2*sizeof(len))], len);
	}
	bufptr = newptr;
}

/*
 * Read input packet, examine its packet type, and enqueue it.
 */

enpread( es, bcbp, unit )
struct	enp_softc *es;
register BCB *bcbp;
int	unit;
{
	register struct ether_header *enp;
	struct mbuf *m;
	long int  s, v;
	register short *vp = (short *)&v,
			*sp;
	int len, off, resid, enptype;
	register struct ifqueue *inq;

	es->es_if.if_ipackets++; 

	/*
	 * Get input data length.
	 * Get pointer to ethernet header (in input buffer).
	 * Deal with trailer protocol: if type is PUP trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */

	len = bcbp->b_msglen - SIZEOF_ETHEADER;
#ifdef TAHOE
	sp = (short *)&bcbp->b_addr;
	*vp = *sp; vp[1] = sp[1];
	enp = (struct ether_header *) v;
#else
	enp = (struct ether_header *)bcbp->b_addr;
#endif TAHOE

#define enpdataaddr(enp, off, type) ((type)(((caddr_t)(((char *)enp)+SIZEOF_ETHEADER)+(off))))

	enptype = enp->ether_type;
	if (enptype >= ETHERPUP_TRAIL && enptype < ETHERPUP_TRAIL+ETHERPUP_NTRAILER) 
	{
		off = (enptype - ETHERPUP_TRAIL) * 512;
		if (off >= ETHERMTU) {
			enperr[unit].bad_offset++;
			ENPTRACE(saverrpkt((char *)enp, B_OFFSET, bcbp->b_msglen)); 

			goto badinput;
		}
		enptype = *enpdataaddr(enp, off, u_short *);
		resid = *(enpdataaddr(enp, off+2, u_short *));

		if (off + resid > len) {
			enperr[unit].bad_length++;
			ENPTRACE(saverrpkt((char *)enp, B_LENGTH, bcbp->b_msglen)); 
			goto badinput;
		}
		len = off + resid;
	} 
	else
		off = 0;

	if( len == 0 ) {
		enperr[unit].bad_length++;
		ENPTRACE(saverrpkt((char *)enp, B_LENGTH, bcbp->b_msglen)); 
		goto badinput;
	}
	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; enpget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */

	m = enpget(bcbp, len, off);
	if( m == 0 )  {
		enperr[unit].h_nobuffer++; /* host runs out of buf */
		goto badinput;
	}
	if( off )
	{
		m->m_off += 2 * sizeof (u_short);
		m->m_len -= 2 * sizeof (u_short);
	}

	switch (enptype) 
	{
#ifdef INET
	case ETHERPUP_IPTYPE:
#ifdef notdef
		arpipin(enp, m);
#endif notdef
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;

	case ETHERPUP_ARPTYPE:
		arpinput(&es->es_ac, m);
		return(0);
#endif
	default:	/* unrecognized ethernet header */
		enperr[unit].bad_packetype++;
		if (printerror) { 
			printf("\nenp%d: Undefined packet type 0x%x ", unit,
				enp->ether_type);
			printf("from host: %x.%x.%x.%x.%x.%x\n", 
				enp->ether_shost[0], enp->ether_shost[1], 
				enp->ether_shost[2], enp->ether_shost[3],
				enp->ether_shost[4], enp->ether_shost[5]);
		}	/* end debugging aid	*/
		ENPTRACE(saverrpkt((char *)enp, B_PACKETYPE, bcbp->b_msglen)); 
		m_freem(m);
		goto badinput;
	}

	if (IF_QFULL(inq)) 
	{
		enperr[unit].inq_full++;
		IF_DROP(inq);
		m_freem(m);
		return(0);
	}
	s = splimp();
	IF_ENQUEUE(inq, m);
	splx(s);
badinput:
	return(0);         /* sanity */
}

/*
 * Ethernet output routine. (called by user)
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 * If destination is this address or broadcast, send packet to
 * loop device to kludge around the fact that 3com interfaces can't
 * talk to themselves.
 */

enpoutput(ifp, m0, dst)
struct ifnet *ifp;
struct mbuf *m0;
struct sockaddr *dst;
{
	int type, s, error;
	struct ether_addr edst;
	struct in_addr idst;

	register struct enp_softc *es = &enp_softc[ifp->if_unit];
	register struct mbuf *m = m0;
	register struct ether_header *enp;
	register int off, i;

	struct mbuf *mcopy = (struct mbuf *) 0;         /* Null */
	int unit = ifp->if_unit;

	switch( dst->sa_family )
	{
#ifdef INET
	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;

		/* translate internet to ethernet address */

		switch(arpresolve(&es->es_ac, m, &idst, &edst)) {

		   	case ARPRESOLVE_WILLSEND:
				return (0);	/* if not yet resolved */
		   	case ARPRESOLVE_BROADCAST:
				mcopy = m_copy(m, 0, (int)M_COPYALL);
				if (mcopy)
					looutput(&loif, mcopy, dst);

				/* falls through ... */
		   	case ARPRESOLVE_OK:
				break;
		}
		off = ((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;
		if ((ifp->if_flags & IFF_NOTRAILERS) == 0)
			if (off > 0 && (off & 0x1ff) == 0 &&
			    m->m_off >= MMINOFF + 2 * sizeof (u_short)) 
			{
				type = ETHERPUP_TRAIL + (off>>9);
				m->m_off -= 2 * sizeof (u_short);
				m->m_len += 2 * sizeof (u_short);
				*mtod(m, u_short *) = ETHERPUP_IPTYPE;
				*(mtod(m, u_short *) + 1) = m->m_len;
				goto gottrailertype;
			}

		type = ETHERPUP_IPTYPE;
		off = 0;
		goto gottype;
#endif

#ifdef notdef
	case AF_RAW:
		enp = mtod(m, struct ether_header *);
		if (m->m_len < sizeof *enp) 
		{
			error = EMSGSIZE;
			goto bad;
		}
		goto gotheader;
#endif

	case AF_UNSPEC:
		enp = (struct ether_header *)dst->sa_data;
		bcopy( enp->ether_dhost, &edst, sizeof(edst));
		type = enp->ether_type;
		goto gottype;

	default:
		if (printerror)
		    printf("enp%d: can't handle af%d\n", unit,dst->sa_family);
		error = EAFNOSUPPORT;
		goto bad;
	}

gottrailertype:
	/*
	 * Packet to be sent as trailer: move first packet
	 * (control information) to end of chain.
	 */
	while (m->m_next)
		m = m->m_next;
	m->m_next = m0;
	m = m0->m_next;
	m0->m_next = 0;
	m0 = m;

gottype:
	/*
         * Add local net header.  If no space in first mbuf,
         * allocate another.
         */
	if (m->m_off > MMAXOFF ||
	    MMINOFF + SIZEOF_ETHEADER > m->m_off) 
	{
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) 
		{
			enperr[unit].h_nobuffer++; /* host runs out of buf */
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = SIZEOF_ETHEADER;
	} 
	else
	{
		m->m_off -= SIZEOF_ETHEADER;
		m->m_len += SIZEOF_ETHEADER;
	}
	enp = mtod(m, struct ether_header *);
	bcopy( &edst, enp->ether_dhost, sizeof(enp->ether_dhost) );
	enp->ether_type = type;
gotheader:
	bcopy( es->es_enaddr, enp->ether_shost, sizeof(enp->ether_shost));

	/*
	 * Queue message on interface if possible 
	 */

	s = splimp();	
	if( enpput( unit,m ) )
	{
		error = ENOBUFS;
		enperr[unit].c_nobuffer++; /* controller runs out of buf */
		goto qfull;
	}
	splx( s );	
	es->es_if.if_opackets++; 
	return(0);
qfull:
	splx( s );	
	m0 = m;
bad:
	m_freem(m0);
	return(error);
}

/*
 * Routine to copy from mbuf chain to transmitter
 * buffer in Multibus memory.
 */

enpput( unit,m )
int unit;
struct mbuf *m;
{
	register BCB *bcbp;
	register ENPDEVICE *addr;
	register struct mbuf *mp;
	register u_char *bp;
	int	 ctr = 0;
	long int	v;
	register short *vp = (short *)&v,
			*sp;

	addr = (ENPDEVICE *)enpinfo[ unit ]->ui_addr;

	if ( ringempty( &addr->enp_hostfree ) ) 
			return( 1 );	

	bcbp = (BCB *)ringget( &addr->enp_hostfree );
	bcbp->b_len = 0;
#ifdef TAHOE
	sp = (short *)&bcbp->b_addr;
	*vp = *sp; vp[1] = sp[1];
	bp = (u_char *)v;
#else
	bp = (u_char *)bcbp->b_addr;
#endif TAHOE
	for (mp = m; mp; mp = mp->m_next) 
	{
		register unsigned len;
		u_char *mcp;

		len = mp->m_len;
		if( len == 0 )
			continue;
		mcp = mtod( mp,u_char * );
		enpcopy( mcp,bp,len );
		bp += len;
		bcbp->b_len += len;
	}
	bcbp->b_len = max( MINPKTSIZE,bcbp->b_len );
	bcbp->b_reserved = 0;
	if ( ringput( &addr->enp_toenp,bcbp ) == 1 ) {
		host_intr++;
		INTR_ENP( addr );
	}
	m_freem(m);
	return( 0 );
}

/*
 * Routine to copy from Multibus memory into mbufs.
 *
 * Warning: This makes the fairly safe assumption that
 * mbufs have even lengths.
 */
struct mbuf *
enpget( bcbp, totlen, off0 )
register BCB *bcbp;
int totlen, off0;
{
	register struct mbuf *m;
	register int off = off0;
	register unsigned char *cp;
	long int	v;
	register short *vp = (short *)&v,
			*sp;

	int len;
	struct mbuf *top = 0;
	struct mbuf **mp = &top;

#ifdef TAHOE
	sp = (short *)&bcbp->b_addr;
	*vp = *sp; vp[1] = sp[1];
	cp = (unsigned char *)v + SIZEOF_ETHEADER;
#else
	cp = (unsigned char *)bcbp->b_addr + SIZEOF_ETHEADER;
#endif TAHOE

	while( totlen > 0 )
	{
		u_char *mcp;

		MGET(m, M_DONTWAIT, MT_DATA);
		if (m == 0) 
			goto bad;
		if( off )
		{
			len = totlen - off;
#ifdef TAHOE
			sp = (short *)&bcbp->b_addr;
			*vp = *sp; vp[1] = sp[1];
			cp = (unsigned char *)v + SIZEOF_ETHEADER
				+ off;
#else
			cp = (unsigned char *)bcbp->b_addr + 
				SIZEOF_ETHEADER + off;
#endif TAHOE
		} 
		else
			len = totlen;


		if (len >= CLBYTES) {
			struct mbuf *p;

			MCLGET(p, 1);
			if (p != 0) {
				m->m_len = len = CLBYTES;
				m->m_off = (int)p - (int)m;
			} else  {
				m->m_len = len = MIN(MLEN, len);
				m->m_off = MMINOFF;
				}
		} else  { 
			m->m_len = len = MIN(MLEN, len);
			m->m_off = MMINOFF;
		}

		mcp = mtod(m, u_char *);
		enpcopy(cp, mcp, len);
		cp += len;
		*mp = m;
		mp = &m->m_next;
		if (off == 0) 
		{
			totlen -= len;
			continue;
		}
		off += len;
		if (off == totlen) 
		{
#ifdef TAHOE
			sp = (short *)&bcbp->b_addr;
			*vp = *sp; vp[1] = sp[1];
			cp = (unsigned char *)v + SIZEOF_ETHEADER;
#else
			cp = (unsigned char *)bcbp->b_addr + SIZEOF_ETHEADER;
#endif TAHOE
			off = 0;
			totlen = off0;
		}
	}
	return (top);
bad:
	m_freem(top);
	return (0);
}

/*
 * Process an ioctl request.
 *    this can be called via the "socket" route for SIOCSIFADDR or
 *	by the cdev/inode route for SIOCSIFCCFWR/RD
 *
 */

enpioctl(ifp, cmd, data)
register struct ifnet *ifp;
int cmd;
caddr_t data;
{
	register int	unit = ifp->if_unit;
	register struct vba_device *md;
	int s, error = 0;
	struct sockaddr_in	*sin;
	struct sockaddr		*sa;
	struct enp_softc	*es = &enp_softc[ifp->if_unit];
	ENPDEVICE		*addr;
	struct config_entry	*cf;
	struct ifreq *ifr	= (struct ifreq *)data;
	struct sockaddr_in	*et_addr;
	int code, i;


	if (unit >= NENP || (md = enpinfo[unit]) == 0 || md->ui_alive == 0)
		return(ENODEV);

	switch (cmd) {

	case SIOCSIFADDR:
		s = splimp();
		sa = (struct sockaddr *)&ifr->ifr_addr;
		if (sa->sa_family == AF_UNSPEC ) {
			if (sa->sa_data[0] & 1){ /*broad or multi-cast*/
				splx( s );
				return( EINVAL );
			}
			bcopy(sa->sa_data,es->es_enaddr,sizeof(es->es_enaddr));
			enpinit( ifp->if_unit);
			break;
		}
		sin = (struct sockaddr_in *)&ifr->ifr_addr;
		if (sin->sin_family != AF_INET){
			splx( s );
			return( EINVAL );
		}
		if (ifp->if_flags & IFF_RUNNING)
			if_rtinit(ifp, -1);     /* delete previous route */
		enpsetaddr(ifp, sin);
		enpinit(ifp->if_unit);
		enpgetaddr( ifp->if_unit );
		splx(s);
		break;


	case SIOCSETETADDR:	/* Set Ethernet station address */
		s = splimp();
		ifp->if_flags &= (~IFF_RUNNING | IFF_UP);
		et_addr = (struct sockaddr_in *)&ifr->ifr_addr;
		addr = (ENPDEVICE *)enpinfo[ifp->if_unit]->ui_addr;

		/* Set station address and reset controller board */
		{
		u_char	*to = &addr->enp_addr.e_baseaddr.ea_addr[0];
		char	*from = &et_addr->sin_zero[2];
		int	i;

		for (i = 0 ; i < ETHADDR_SIZE; i++) 
			*to++ = (u_char) (~(*from++ & 0xff));
		}
		enpcopy(&addr->enp_addr.e_listsize, &code, sizeof(code)); 
		code |= E_ADDR_SUPP;
		enpcopy(&code, &addr->enp_addr.e_listsize, sizeof(code)); 
		enpreset(ifp->if_unit);		/* Re-initialize */
		enpgetaddr(ifp->if_unit);
		splx(s);
		break;

	case SIOCGETETADDR:	/* Get Foreign Hosts' Ethernet addresses */
		arpwhohas(&es->es_ac, (struct in_addr *)ifr->ifr_data);
		break;

	default:
		error = EINVAL;
	}
	return(error);
}

enpsetaddr(ifp, sin)
register struct ifnet *ifp;
register struct sockaddr_in *sin;
{

	ifp->if_addr = *(struct sockaddr *)sin;
	ifp->if_net = in_netof(sin->sin_addr);
	ifp->if_host[0] = in_lnaof(sin->sin_addr);
	sin = (struct sockaddr_in *)&ifp->if_broadaddr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(ifp->if_net, INADDR_ANY);
	ifp->if_flags |= IFF_BROADCAST;
}


/*
 * Get the ethernet addr, store it and print it
 * Read the ethernet address off the board, one byte at a time.
 *	put it in enp_softc
 */


enpgetaddr( unit )
int unit;
{
	register struct enp_softc	*es = &enp_softc[unit];
	register ENPDEVICE *addr =(ENPDEVICE *)enpinfo[unit]->ui_addr;
	int i;
	
#ifdef TAHOE
	enpcopy(&addr->enp_addr.e_baseaddr, &es->es_boardaddr, sizeof(es->es_boardaddr));
#else
	es->es_boardaddr = addr->enp_addr.e_baseaddr;
#endif TAHOE
	bcopy(&es->es_boardaddr, es->es_enaddr, ETHADDR_SIZE);
	return( 1 );
}

/*
 * enpram device
 *
 */

enpr_open( dev )
{
	register int	unit = minor(dev);
	register struct vba_device *md;
	register ENPDEVICE	*addr;

	if (unit >= NENP || (md = enpinfo[unit]) == 0 || md->ui_alive == 0 ||
	    (addr = (ENPDEVICE *)md->ui_addr) == (ENPDEVICE *)0)
		return(ENODEV);
	if (addr->enp_state != S_ENPRESET)
		return(EACCES);  /* enp is not in reset state, don't open  */
	if ( !enpismapped[unit] ) {
		ioaccess(ENPmap[unit],ENPmapa[unit],ENPBPTE);
		++enpismapped[unit];
	}
	enpram[unit] = ENP_OPEN;
	return( 0 );
}

enpr_close(dev)
{
	enpram[minor(dev)] = ENP_CLOSE;
	return( 0 );
}

enpr_read( dev,uio )
int dev;
register struct uio *uio;
{
	register ENPDEVICE *addr;
	register struct iovec *iov;
	register r=0;

	if (enpram[minor(dev)] != ENP_OPEN)
		return(EACCES);
	if ( uio->uio_offset > RAM_SIZE )
		return( ENODEV );
	if ( uio->uio_offset + iov->iov_len > RAM_SIZE )
		iov->iov_len = RAM_SIZE - uio->uio_offset;
	addr = (ENPDEVICE *)enpinfo[ minor( dev ) ]->ui_addr;
	iov  = uio->uio_iov;

	if( r = enpcopyout( &addr->enp_ram[ uio->uio_offset ], iov->iov_base,
			 iov->iov_len ) )
		 return( r );

	uio->uio_resid -= iov->iov_len;
	iov->iov_len = 0;

	return( 0 );
}

enpr_write( dev,uio )
int dev;
register struct uio *uio;
{
	register ENPDEVICE *addr;
	register struct iovec *iov;
	register r=0;

	if (enpram[minor(dev)] != ENP_OPEN)
		return(EACCES);
	addr = (ENPDEVICE *)enpinfo[ minor( dev ) ]->ui_addr;
	iov  = uio->uio_iov;

	if ( uio->uio_offset > RAM_SIZE )
		return( ENODEV );
	if ( uio->uio_offset + iov->iov_len > RAM_SIZE )
		iov->iov_len = RAM_SIZE - uio->uio_offset;
	if( r = enpcopyin( iov->iov_base, &addr->enp_ram[ uio->uio_offset ],
			iov->iov_len ) )
		return( r );

	uio->uio_resid -= iov->iov_len;
	iov->iov_len = 0;

	return( 0 );
}

enpr_ioctl( dev,cmd,arg,fflag )
dev_t dev;
caddr_t *arg;
{
	register ENPDEVICE *addr;
	long int	v;
	register short	*vp = (short *)&v, *sp;
	register unit = minor(dev);
	register struct vba_device *md;

	if (unit >= NENP || (md = enpinfo[unit]) == 0 || md->ui_alive == 0 ||
	    (addr = (ENPDEVICE *)md->ui_addr) == (ENPDEVICE *)0)
		return(ENODEV);
	switch( cmd )
	{
		case ENPIOGO:
/* not needed if prom based version */
#ifdef TAHOE
			sp = (short *)&addr->enp_base;
			v = (int)addr;
			*sp = *vp; sp[1] = vp[1];
#else
			addr->enp_base = (int)addr;
#endif TAHOE
			addr->enp_intrvec = intvec[ unit ];
			ENP_GO( addr, ENPSTART );
			DELAY( 200000 );
			enpattach( enpinfo[ unit ] );
			enpinit( unit );
			addr->enp_state = S_ENPRUN;  /* it is running now */
/* end of not needed */

			break;

		case ENPIORESET:
			RESET_ENP( addr );
			addr->enp_state = S_ENPRESET;  /* it is reset now */
			DELAY( 100000 );
			break;
	}
	return( 0 );
}

/* 
 * routines to synchronize enp and host 
 */

static
ringinit( rp,size )
register RING *rp;
{
	register int	i;
	register short *sp; 

	rp->r_rdidx = rp->r_wrtidx = 0;
	rp->r_size = size;
}

static
ringempty( rp )
register RING *rp;
{
	return( rp->r_rdidx == rp->r_wrtidx );
}

static
ringfull( rp )
register RING *rp;
{
	register short idx;

	idx = (rp->r_wrtidx + 1) & (rp->r_size-1);
	return( idx == rp->r_rdidx );
}

static
ringput( rp,v )
register RING *rp;
{
	register int idx;
	register short *vp = (short *)&v,
		       *sp;

	idx = (rp->r_wrtidx + 1) & (rp->r_size-1);
	if( idx != rp->r_rdidx )
	{
#ifdef TAHOE
		sp = (short *)&rp->r_slot[ rp->r_wrtidx ];
		*sp = *vp; sp[1] = vp[1];
#else
		rp->r_slot[ rp->r_wrtidx ] = v;
#endif TAHOE
		rp->r_wrtidx = idx;
		if( (idx -= rp->r_rdidx) < 0 )
			idx += rp->r_size;
		return( idx );			/* num ring entries */
	}
	return( 0 );
}

static
ringget( rp )
register RING *rp;
{
	register int i = 0;
	long int v;
	register short *vp = (short *)&v,
		       *sp;

	if( rp->r_rdidx != rp->r_wrtidx )
	{
#ifdef TAHOE
		sp = (short *)&rp->r_slot[ rp->r_rdidx ];
		*vp = *sp; vp[1] = sp[1];
		i = v;
#else
		i = rp->r_slot[ rp->r_rdidx ];
#endif TAHOE
		rp->r_rdidx = (++rp->r_rdidx) & (rp->r_size-1);
	}
	return( i );
}

#ifdef notdef
struct mbuf *
m_tofree( rp )
register RING *rp;
{
	long int v = 0;
	register short *vp = (short *)&v,
		       *sp;

	if( rp->r_rdidx != rp->r_wrtidx )
	{
#ifdef TAHOE
		sp = (short *)&rp->r_slot[ rp->r_rdidx ];
		*vp = *sp; vp[1] = sp[1];
		/* *sp = 0xffff; sp[1] = 0xffff; */
#else
		v = rp->r_slot[ rp->r_rdidx ];
#endif TAHOE
	  	rp->r_rdidx = (++rp->r_rdidx) & (rp->r_size-1);
	}
	return( (struct mbuf *)v );
}
#endif
static
fir( rp )
register RING *rp;
{
	long int v;
	register short *vp = (short *)&v,
		       *sp;

	if( rp->r_rdidx != rp->r_wrtidx )
#ifdef TAHOE
	{
		sp = (short *)&rp->r_slot[ rp->r_rdidx ];
		*vp = *sp; vp[1] = sp[1];
		return( v );
	}
#else
		return( rp->r_slot[ rp->r_rdidx ] );
#endif TAHOE
	else   
		return( 0 );
}


static
prtbytes( addr )
register char *addr;
{
	register int i;

	for( i = 0; i < 12; i++ )
	{
		printf("%X ",*addr&0377);
		addr++;
	}
	printf("\n");
}

static
enpcopy(from, to, cnt)
register char *from, *to;
register cnt;
{
	register c;
	register short *f, *t;

	if (((int)from & 01) && ((int)to & 01)) {
					/* source & dest at odd addresses */
		*to++ = *from++;
		--cnt;
	}
	if (cnt > 1 && (((int)to & 01)==0) && (((int)from & 01)==0)) {
		t = (short *) to;
		f = (short *) from;
		for( c = cnt>>1; c; --c)	/* even address copy */
			*t++ = *f++;
		cnt &= 1;
		if ( cnt ) {			/* odd len */
			from = (char *) f;
			to   = (char *) t;
			*to = *from;
		}
	}
	while (cnt-- > 0)	/* one of the address(es) must be odd */
		*to++ = *from++;

}

static
enpcopyin(userv, kernv, cnt)
{

	if (useracc(userv, cnt, 1)) {
		enpcopy( userv, kernv, cnt );
		return( 0 );
	}
	else	return( EFAULT );
}


static
enpcopyout(kernv, userv, cnt)
{

	if (useracc(userv, cnt, 0)) {
		enpcopy( kernv, userv, cnt );
		return( 0 );
	}
	else	return( EFAULT );
}
#endif
