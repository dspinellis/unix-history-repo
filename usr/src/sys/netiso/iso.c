/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/*
 * $Header: iso.c,v 4.11 88/09/19 14:58:35 root Exp $ 
 * $Source: /usr/argo/sys/netiso/RCS/iso.c,v $ 
 *
 * iso.c: miscellaneous routines to support the iso address family
 */

#ifndef lint
static char *rcsid = "$Header: iso.c,v 4.11 88/09/19 14:58:35 root Exp $";
#endif


#include "../h/types.h"
#include "../h/param.h"
#include "../h/ioctl.h"
#include "../h/mbuf.h"
#include "../h/domain.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/uio.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/errno.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../net/af.h"

#include "../netiso/iso.h"
#include "../netiso/iso_var.h"
#include "../netiso/iso_snpac.h"
#include "../netiso/iso_pcb.h"
#include "../netiso/clnp.h"
#include "../netiso/argo_debug.h"

#ifdef ISO

int	iso_interfaces = 0;		/* number of external interfaces */
extern	struct ifnet loif;	/* loopback interface */


/*
 * FUNCTION:		iso_init
 *
 * PURPOSE:			initialize the iso address family
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	1) zeros the maptab table.
 *
 * NOTES:			
 */
iso_init()
{
	extern struct maptab	iso_snpac[];
	extern int 				iso_snpac_size;

 	bzero((caddr_t)iso_snpac, iso_snpac_size * sizeof(struct snpa_cache));
}

/*
 * FUNCTION:		iso_addrmatch1
 *
 * PURPOSE:			decide if the two iso_addrs passed are equal
 *
 * RETURNS:			true if the addrs match, false if they do not
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
iso_addrmatch1(isoaa, isoab)
struct iso_addr	*isoaa, *isoab;		/* addresses to check */
{
	int	compare_len;

	IFDEBUG(D_ROUTE)
		printf("iso_addrmatch1: comparing lengths: %d to %d\n", isoaa->isoa_len,
			isoab->isoa_len);
		printf("a:\n");
		dump_buf((caddr_t)isoaa, isoaa->isoa_len);
		printf("b:\n");
		dump_buf((caddr_t)isoab, isoab->isoa_len);
	ENDDEBUG

	if ((compare_len = isoaa->isoa_len) != isoab->isoa_len) {
		IFDEBUG(D_ROUTE)
			printf("iso_addrmatch1: returning false because of lengths\n");
		ENDDEBUG
		return 0;
	}
	
	/* TODO : generalize this to all afis with masks */
	if(	isoaa->isoa_afi == AFI_37 ) {
		/* must not compare 2 least significant digits, or for
		 * that matter, the DSP
		 */
		compare_len = ADDR37_IDI_LEN - 1; 
	}

	IFDEBUG(D_ROUTE)
		int i;
		char *a, *b;

		a = (char *) isoaa;
		b = (char *) isoab;

		for (i=0; i<compare_len; i++) {
			printf("<%x=%x>", a[i]&0xff, b[i]&0xff);
			if (a[i] != b[i]) {
				printf("\naddrs are not equal at byte %d\n", i);
				return(0);
			}
		}
		printf("\n");
		printf("addrs are equal\n");
		return (1);
	ENDDEBUG
	return (!bcmp((caddr_t)isoaa, (caddr_t)isoab, compare_len));
}

/*
 * FUNCTION:		iso_addrmatch
 *
 * PURPOSE:			decide if the two sockadrr_isos passed are equal
 *
 * RETURNS:			true if the addrs match, false if they do not
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
iso_addrmatch(sisoa, sisob)
struct sockaddr_iso	*sisoa, *sisob;		/* addresses to check */
{
	return(iso_addrmatch1(&sisoa->siso_addr, &sisob->siso_addr));
}

/*
 * FUNCTION:		iso_netmatch
 *
 * PURPOSE:			similar to iso_addrmatch but takes sockaddr_iso
 *					as argument.
 *
 * RETURNS:			true if same net, false if not
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
iso_netmatch(sisoa, sisob)
struct sockaddr_iso *sisoa, *sisob;
{
	u_char			bufa[sizeof(struct sockaddr_iso)];
	u_char			bufb[sizeof(struct sockaddr_iso)];
	register int	lena, lenb;

	lena = iso_netof(&sisoa->siso_addr, bufa);
	lenb = iso_netof(&sisob->siso_addr, bufb);

	IFDEBUG(D_ROUTE)
		printf("iso_netmatch: comparing lengths: %d to %d\n", lena, lenb);
		printf("a:\n");
		dump_buf(bufa, lena);
		printf("b:\n");
		dump_buf(bufb, lenb);
	ENDDEBUG

	return ((lena == lenb) && (!bcmp(bufa, bufb, lena)));
}

/*
 * FUNCTION:		iso_hashchar
 *
 * PURPOSE:			Hash all character in the buffer specified into
 *					a long. Return the long.
 *
 * RETURNS:			The hash value.
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			The hash is achieved by exclusive ORing 4 byte
 *					quantities. 
 */
u_long
iso_hashchar(buf, len)
register caddr_t	buf;		/* buffer to pack from */
register int		len;		/* length of buffer */
{
	register u_long	h = 0;
	register int	i;

	for (i=0; i<len; i+=4) {
		register u_long	l = 0;

		if ((len - i) < 4) {
			/* buffer not multiple of 4 */
			switch (len - i) {
				case 3:
					l |= buf[i+2] << 8;
				case 2:
					l |= buf[i+1] << 16;
				case 1:
					l |= buf[i] << 24;
					break;
				default:
					printf("iso_hashchar: unexpected value x%x\n", len - i);
					break;
			}
		} else {
			l |= buf[i] << 24;
			l |= buf[i+1] << 16;
			l |= buf[i+2] << 8;
			l |= buf[i+3];
		}

		h ^= l;
	}
	
	h ^= (u_long) (len % 4);

	return(h);
}

/*
 * FUNCTION:		iso_hash
 *
 * PURPOSE:			Fill in fields of afhash structure based upon addr passed.
 *
 * RETURNS:			none
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
iso_hash(siso, hp)
struct sockaddr_iso	*siso;		/* address to perform hash on */
struct afhash		*hp;		/* RETURN: hash info here */
{
	u_long			buf[sizeof(struct sockaddr_iso)+1/4];
	register int	bufsize;


	bzero(buf, sizeof(buf));

	bufsize = iso_netof(&siso->siso_addr, buf);
	hp->afh_nethash = iso_hashchar((caddr_t)buf, bufsize);

	IFDEBUG(D_ROUTE)
		printf("iso_hash: iso_netof: bufsize = %d\n", bufsize);
	ENDDEBUG

	hp->afh_hosthash = iso_hashchar((caddr_t)&siso->siso_addr, 
		siso->siso_addr.isoa_len);

	IFDEBUG(D_ROUTE)
		printf("iso_hash: %s: nethash = x%x, hosthash = x%x\n",
			clnp_iso_addrp(&siso->siso_addr), hp->afh_nethash, 
			hp->afh_hosthash);
	ENDDEBUG
}

/*
 * FUNCTION:		iso_netof
 *
 * PURPOSE:			Extract the network portion of the iso address.
 *					The network portion of the iso address varies depending
 *					on the type of address. The network portion of the
 *					address will include the IDP. The network portion is:
 *			
 *						TYPE			DESC
 *					t37					The AFI and x.121 (IDI)
 *					osinet				The AFI, orgid, snetid
 *					rfc986				The AFI, vers and network part of
 *										internet address.
 *
 * RETURNS:			number of bytes placed into buf.
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Buf is assumed to be big enough
 */
iso_netof(isoa, buf)
struct iso_addr	*isoa;		/* address */
caddr_t			buf;		/* RESULT: network portion of address here */
{
	u_int		len = 1;	/* length of afi */

	switch (isoa->isoa_afi) {
		case AFI_37:
			/*
			 * Due to classic x.25 tunnel vision, there is no
			 * net portion of an x.121 address.  For our purposes
			 * the AFI will do, so that all x.25 -type addresses
			 * map to the single x.25 SNPA. (Cannot have more than
			 * one, obviously).
			 */

			break;

/* 		case AFI_OSINET:*/
		case AFI_RFC986: {
			u_short	idi;	/* value of idi */

			/* osinet and rfc986 have idi in the same place */
			CTOH(isoa->rfc986_idi[0], isoa->rfc986_idi[1], idi);

			if (idi == IDI_OSINET)
/*
 *	Network portion of OSINET address can only be the IDI. Clearly,
 *	with one x25 interface, one could get to several orgids, and
 *	several snetids.
				len += (ADDROSINET_IDI_LEN + OVLOSINET_ORGID_LEN + 
						OVLOSINET_SNETID_LEN);
 */
				len += ADDROSINET_IDI_LEN;
			else if (idi == IDI_RFC986) {
				u_long				inetaddr;
				struct ovl_rfc986	*o986 = (struct ovl_rfc986 *)isoa;

				/* bump len to include idi and version (1 byte) */
				len += ADDRRFC986_IDI_LEN + 1;

				/* get inet addr long aligned */
				bcopy(o986->o986_inetaddr, &inetaddr, sizeof(inetaddr));
				inetaddr = ntohl(inetaddr);	/* convert to host byte order */

				IFDEBUG(D_ROUTE)
					printf("iso_netof: isoa ");
					dump_buf(isoa, sizeof(*isoa));
					printf("iso_netof: inetaddr 0x%x ", inetaddr);
				ENDDEBUG

				/* bump len by size of network portion of inet address */
				if (IN_CLASSA(inetaddr)) {
					len += 4-IN_CLASSA_NSHIFT/8;
					IFDEBUG(D_ROUTE)
						printf("iso_netof: class A net len is now %d\n", len);
					ENDDEBUG
				} else if (IN_CLASSB(inetaddr)) {
					len += 4-IN_CLASSB_NSHIFT/8;
					IFDEBUG(D_ROUTE)
						printf("iso_netof: class B net len is now %d\n", len);
					ENDDEBUG
				} else {
					len += 4-IN_CLASSC_NSHIFT/8;
					IFDEBUG(D_ROUTE)
						printf("iso_netof: class C net len is now %d\n", len);
					ENDDEBUG
				}
			} else
				len = 0;
		} break;

		default:
			len = 0;
	}

	bcopy((caddr_t)isoa, buf, len);
	IFDEBUG(D_ROUTE)
		printf("in_netof: isoa ");
		dump_buf(isoa, len);
		printf("in_netof: net ");
		dump_buf(buf, len);
	ENDDEBUG
	return len;
}

/*
 *	The following is a kludge until I figure something out. Since AFISO
 *	allows >1 addr/ifp, SIOCGIFADDR can possibly return more than one
 *	address. Rather than changing the ifreq structure, I have set up
 *	the ioctl so it will return the address in sequential calls. When
 *	all addresses have been read, EADDRNOTAVAIL will be returned.
 *
 *	A call to delete or set an addr will cause a subsequent get to
 *	retrieve the first addr, even if the first had already been read.
 *
 *	The static pointer iso_ia keeps track of which addrs have been read.
 */
static struct iso_ifaddr *iso_iap = NULL;

/*
 * FUNCTION:		iso_control
 *
 * PURPOSE:			Generic iso control operations (ioctl's).
 *					Ifp is 0 if this is not an interface-specific ioctl.
 *
 * RETURNS:			unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			The iso address family will allow more than one
 *					iso address per interface as long as they are different
 *					iso address types. The three types currently supported 
 *					are rfc986, t37, and osinet.
 */
iso_control(so, cmd, data, ifp)
struct socket	*so;		/* socket ioctl arrived on */
int				cmd;		/* ioctl to perform */
caddr_t			data;		/* data for the ioctl */
struct ifnet	*ifp;		/* optional interface ptr */
{
	register struct ifreq		*ifr = (struct ifreq *)data;
	register struct iso_ifaddr	*ia = 0;
	struct ifaddr				*ifa;
	struct mbuf					*m;


	switch (cmd) {
	case SIOCSIFADDR:
		if (!suser())
			return (u.u_error);

		if (ifp == 0)
			panic("iso_control: SIOCSIFADDR");

		/* 
		 *	Check if a iso address of same type exists for ifp 
		 *	If it does, then return an error.
		 */
		for (ia = iso_ifaddr; ia; ia = ia->ia_next) {
			if (ia->ia_ifp == ifp) {
				struct sockaddr_iso *siso; 

				siso = (struct sockaddr_iso *)&ifr->ifr_addr;
				if (iso_eqtype(&IA_SIS(ia)->siso_addr, &siso->siso_addr))
					return(EADDRNOTAVAIL);
			}
		}

		/*
		 *	Go ahead and create new ifaddr
		 *
		 *	Link addr into list of iso addresses
		 */
		m = m_getclr(M_WAIT, MT_IFADDR);
		if (m == (struct mbuf *)NULL)
			return (ENOBUFS);
		if (ia = iso_ifaddr) {
			for ( ; ia->ia_next; ia = ia->ia_next)
				;
			ia->ia_next = mtod(m, struct iso_ifaddr *);
		} else
			iso_ifaddr = mtod(m, struct iso_ifaddr *);
		iso_iap = iso_ifaddr;
		
		/*
		 *	Link addr into list on interface
		 */
		ia = mtod(m, struct iso_ifaddr *);
		if (ifa = ifp->if_addrlist) {
			for ( ; ifa->ifa_next; ifa = ifa->ifa_next)
				;
			ifa->ifa_next = (struct ifaddr *) ia;
		} else
			ifp->if_addrlist = (struct ifaddr *) ia;

		ia->ia_ifp = ifp;
		IA_SIS(ia)->siso_family = AF_ISO;
		if (ifp != &loif)
			iso_interfaces++;
		return (iso_ifinit(ifp, ia, &ifr->ifr_addr));

	case SIOCGIFADDR:
		for (ia = iso_iap; ia; ia = ia->ia_next) {
			if (ia->ia_ifp == ifp) {
				ifr->ifr_addr = ia->ia_addr;
				iso_iap = ia->ia_next;
				return (0);
			}
		}
		iso_iap = iso_ifaddr;
		return(EADDRNOTAVAIL);

	case SIOCDIFADDR: /* TODO: what about this ioctl on other families */
		if (!suser())
			return (u.u_error);
		iso_iap = iso_ifaddr;

		if (ifp == 0)
			panic("iso_control: SIOCDIFADDR");

		return (iso_ifdelete(ifp, &ifr->ifr_addr));

	default:
		if (ifp == 0 || ifp->if_ioctl == 0)
			return (EOPNOTSUPP);
		return ((*ifp->if_ioctl)(ifp, cmd, data));
	}
}

struct ifaddr *
iso_ifwithidi(addr)
	register struct sockaddr *addr;
{
	register struct ifnet *ifp;
	register struct ifaddr *ifa;
	register u_int af = addr->sa_family;

	if (af != AF_ISO)
		return (0);
	IFDEBUG(D_ROUTE)
		printf(">>> iso_ifwithidi addr\n");
		dump_isoaddr( (struct sockaddr_iso *)(addr));
		printf("\n");
	ENDDEBUG
	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		IFDEBUG(D_ROUTE)
			printf("iso_ifwithidi ifnet %s\n", ifp->if_name);
		ENDDEBUG
		for (ifa = ifp->if_addrlist; ifa; ifa = ifa->ifa_next) {
			IFDEBUG(D_ROUTE)
				printf("iso_ifwithidi address ");
				dump_isoaddr( (struct sockaddr_iso *)(&ifa->ifa_addr));
			ENDDEBUG
			if (ifa->ifa_addr.sa_family != addr->sa_family)
				continue;

#define	IFA_SIS(ifa)\
	((struct sockaddr_iso *)&((ifa)->ifa_addr))

			IFDEBUG(D_ROUTE)
				printf(" af same, args to iso_eqtype:\n");
				printf("0x%x ", IFA_SIS(ifa)->siso_addr);
				printf(" 0x%x\n",
				&(((struct sockaddr_iso *)addr)->siso_addr));
			ENDDEBUG

			if (iso_eqtype(&(IFA_SIS(ifa)->siso_addr), 
				&(((struct sockaddr_iso *)addr)->siso_addr))) {
				IFDEBUG(D_ROUTE)
					printf("ifa_ifwithidi: ifa found\n");
				ENDDEBUG
				return (ifa);
			}
			IFDEBUG(D_ROUTE)
				printf(" iso_eqtype failed\n");
			ENDDEBUG
		}
	}
	return ((struct ifaddr *)0);
}

/*
 * FUNCTION:		iso_ifinit
 *
 * PURPOSE:			Initialize an interface's iso address and
 *					routing table entry.
 *
 * RETURNS:			unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
iso_ifinit(ifp, ia, siso)
register struct ifnet		*ifp;	/* interface to initialize */
register struct iso_ifaddr	*ia;	/* addr on ifnet list */
struct sockaddr_iso			*siso;	/* address to use */
{
	struct sockaddr oldaddr;
	struct sockaddr_iso netaddr;
	int s = splimp(), error;

	/*
	 *	Make sure the address make sense
	 */
	if (!iso_ck_addr(&siso->siso_addr))
		return(EINVAL);
	IFDEBUG(D_ROUTE)
		int i;
		char *ptr;

		ptr = (char *) siso;
		printf("The size of sockaddr_iso is %d\n", 
			sizeof(struct sockaddr_iso));
		for(i=0; i< sizeof(struct sockaddr_iso); i++) {
			printf("sockaddr[%d] = 0x%x\n", i, *ptr++ & 0xff);
		}
	ENDDEBUG

	oldaddr = ia->ia_addr;
	ia->ia_addr = *(struct sockaddr *)siso;
	bzero((caddr_t)&netaddr, sizeof (netaddr));
	netaddr.siso_family = AF_ISO;

	/*
	 * Give the interface a chance to initialize
	 */
	if (ifp->if_ioctl && (error = (*ifp->if_ioctl)(ifp, SIOCSIFADDR, ia))) {
		splx(s);
		ia->ia_addr = oldaddr;
		return (error);
	}
	splx(s);

	/*
	 * Add route for the network.
	 */
	if (ifp->if_flags & IFF_LOOPBACK)
		rtinit(&ia->ia_addr, &ia->ia_addr, (int)SIOCADDRT, RTF_HOST|RTF_UP);
	else {
		int len;

		len = iso_netof(&siso->siso_addr, (caddr_t)&netaddr.siso_addr);
		netaddr.siso_addr.isoa_len = len;
		rtinit((struct sockaddr *)&netaddr, &ia->ia_addr,
		    (int)SIOCADDRT, RTF_UP);
	}
	ia->ia_flags |= IFA_ROUTE;
	return (0);
}


/*
 * FUNCTION:		iso_ifdelete
 *
 * PURPOSE:			Delete an iso address from an interface.
 *
 * RETURNS:			0 or unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
iso_ifdelete(ifp, siso)
struct ifnet		*ifp;	/* interface to delete addr from */
struct sockaddr_iso	*siso;	/* address to delete */
{
	struct iso_addr 			*isoa = &siso->siso_addr;
	register struct iso_ifaddr	*ia, *prev = 0;
	register struct ifaddr		*ifa;
	int 						s;
	struct sockaddr_iso 		netaddr;

	/* 
	 *	Find the iso address of same type as specified and delete it.
	 */
	for (ia = iso_ifaddr; ia; prev = ia, ia = ia->ia_next) {
		if (ia->ia_ifp == ifp) {
			if (iso_eqtype(&IA_SIS(ia)->siso_addr, isoa)) {
				/*
				 * Delete any previous route for the address.
				 */
				IFDEBUG(D_IOCTL)
					printf("iso_ifdelete: delete %s\n", clnp_iso_addrp(isoa))
				ENDDEBUG
				s = splimp();
				bzero((caddr_t)&netaddr, sizeof (netaddr));
				netaddr.siso_family = AF_ISO;
				if (ia->ia_flags & IFA_ROUTE) {
					if (ifp->if_flags & IFF_LOOPBACK)
						rtinit(&ia->ia_addr, &ia->ia_addr, (int)SIOCDELRT, 
							RTF_HOST);
					else {
						(void) iso_netof(&siso->siso_addr, 
							(caddr_t)&netaddr.siso_addr);
						rtinit((struct sockaddr *)&netaddr, &ia->ia_addr, 
							(int)SIOCDELRT, 0);
					}
					ia->ia_flags &= ~IFA_ROUTE;
				}
				splx(s);

				/*
				 *	Remove from list of iso addresses
				 */
				if (prev == 0)
					iso_ifaddr = ia->ia_next;
				else
					prev->ia_next = ia->ia_next;
				if (ifp != &loif)
					iso_interfaces--;
				
				/*
				 *	Remove from list of addrs on ifnet structure
				 */
				if ((ifa = ifp->if_addrlist) == (struct ifaddr *)ia)
					ifp->if_addrlist = ia->ia_ifa.ifa_next;
				else {
					for (; ifa; ifa = ifa->ifa_next) {
						if (ifa->ifa_next == (struct ifaddr *)ia) {
							ifa->ifa_next = ia->ia_ifa.ifa_next;
							break;
						}
					}
				}

				/*
				 *	Free the iso_ifaddr mbuf
				 */
				m_free(dtom(ia));
				return (0);
			}
		}
	}
	return(EADDRNOTAVAIL);
}

/*
 * FUNCTION:		iso_ck_addr
 *
 * PURPOSE:			return true if the iso_addr passed is 
 *					within the legal size limit for an iso address.
 *
 * RETURNS:			true or false
 *
 * SIDE EFFECTS:	
 *
 */
iso_ck_addr(isoa)
struct iso_addr	*isoa;	/* address to check */
{
	return (isoa->isoa_len <= 20);

}


/*
 * FUNCTION:		iso_eqtype
 *
 * PURPOSE:			Determine if two iso addresses are of the same type.
 *  This is flaky.  Really we should consider all type 47 addrs to be the
 *  same - but there do exist different structures for 47 addrs.
 *  Gosip adds a 3rd.
 *
 * RETURNS:			true if the addresses are the same type
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			By type, I mean rfc986, t37, or osinet
 *
 *					This will first compare afis. If they match, then
 *					if the addr is not t37, the idis must be compared.
 */
iso_eqtype(isoaa, isoab)
struct iso_addr	*isoaa;		/* first addr to check */
struct iso_addr	*isoab;		/* other addr to check */
{
	if (isoaa->isoa_afi == isoab->isoa_afi) {
		if (isoaa->isoa_afi == AFI_37)
			return(1);
		else 
			return (!bcmp(&isoaa->isoa_u, &isoab->isoa_u, 2));
	}
	return(0);
}

/*
 * FUNCTION:		iso_iaonnetof()
 *
 * PURPOSE:			Find and interface addresss based on the network
 *					portion of an address.
 *
 * RETURNS:			ptr to an interface address 
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
struct iso_ifaddr *
iso_iaonnetof(siso)
	struct sockaddr_iso *siso;
{
	register struct iso_ifaddr *ia;

	for (ia = iso_ifaddr; ia; ia = ia->ia_next)
		if (iso_netmatch( (struct sockaddr_iso *)(&ia->ia_ifa.ifa_addr), siso) )
			return (ia);
	return ((struct iso_ifaddr *)0);
}

#ifdef	NARGOXTWENTYFIVE > 0
#include "cons.h"
#endif	NARGOXTWENTYFIVE > 0
/*
 * FUNCTION:		iso_nlctloutput
 *
 * PURPOSE:			Set options at the network level
 *
 * RETURNS:			E*
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			This could embody some of the functions of
 *					rclnp_ctloutput and cons_ctloutput.
 */
iso_nlctloutput(cmd, optname, pcb, m)
int			cmd;		/* command:set or get */
int			optname;	/* option of interest */
caddr_t		pcb;		/* nl pcb */
struct mbuf	*m;			/* data for set, buffer for get */
{
	struct isopcb	*isop = (struct isopcb *)pcb;
	int				error = 0;	/* return value */
	caddr_t			data;		/* data for option */
	int				data_len;	/* data's length */

	IFDEBUG(D_ISO)
		printf("iso_nlctloutput: cmd %x, opt %x, pcb %x, m %x\n",
			cmd, optname, pcb, m);
	ENDDEBUG

	if ((cmd != PRCO_GETOPT) && (cmd != PRCO_SETOPT))
		return(EOPNOTSUPP);

	data = mtod(m, caddr_t);
	data_len = (m)->m_len;

	IFDEBUG(D_ISO)
		printf("iso_nlctloutput: data is:\n");
		dump_buf(data, data_len);
	ENDDEBUG

	switch (optname) {

#ifdef	NARGOXTWENTYFIVE > 0
		case CONSOPT_X25CRUD:
			if (cmd == PRCO_GETOPT) {
				error = EOPNOTSUPP;
				break;
			}

			if (data_len > MAXX25CRUDLEN) {
				error = EINVAL;
				break;
			}

			IFDEBUG(D_ISO)
				printf("iso_nlctloutput: setting x25 crud\n");
			ENDDEBUG

			bcopy(data, (caddr_t)&isop->isop_x25crud, data_len);
			isop->isop_x25crud_len = data_len;
			break;
#endif	NARGOXTWENTYFIVE > 0

		default:
			error = EOPNOTSUPP;
	}

	return error;
}

/*
 * FUNCTION:		iso_routeifp
 *
 * PURPOSE:			Route on a sockaddr and return ifp
 *
 * RETURNS:			ifp of outgoing interface, or null
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
struct ifnet *
iso_routeifp(dst)
struct sockaddr	*dst;		/* destination to route to */
{
	struct route	ro;
	struct ifnet	*ifp = NULL;

	ro.ro_dst = *dst;
	ro.ro_rt = NULL;

	IFDEBUG(D_ROUTE)
		printf("iso_routeifp: dst:");
		dump_isoaddr(dst);
	ENDDEBUG

	rtalloc(&ro);

	if (ro.ro_rt) {
		ifp = ro.ro_rt->rt_ifp;
		RTFREE(ro.ro_rt);
	}

	IFDEBUG(D_ROUTE)
		printf("iso_routeifp: ifp x%x", ifp);
		if (ifp)
			printf(" (%s%d)\n", ifp->if_name, ifp->if_unit);
		else
			printf("\n");
	ENDDEBUG

	return(ifp);
}
#endif ISO

#ifdef ARGO_DEBUG

/*
 * FUNCTION:		dump_isoaddr
 *
 * PURPOSE:			debugging
 *
 * RETURNS:			nada 
 *
 */
dump_isoaddr(s)
	struct sockaddr_iso *s;
{
	caddr_t c = (caddr_t)&(s->siso_addr.isoa_u);
	register int i;

	if( s->siso_family == AF_ISO) {
		printf("len %d family: 0x%x  suffix 0x%x, afi 0x%x,",
			(int)s->siso_addr.isoa_len,
			(int)s->siso_family, 
			s->siso_tsuffix, (int)s->siso_addr.isoa_afi);
		if( s->siso_addr.isoa_len > sizeof(struct sockaddr) )
			printf("ERROR-ADDR TOO BIG %d\n", s->siso_addr.isoa_len);
		else  {
			if (s->siso_addr.isoa_len != 0) {
			/* -2 because we already skipped the afi */
					for (i=0; i<s->siso_addr.isoa_len-2; i++)
						printf("0x%x.", *(c+i)&0xff);
					printf("0x%x\n", *(c+i)&0xff);
			}
		}
	} else if( s->siso_family == AF_INET) {
		/* hack */
		struct sockaddr_in *sin = (struct sockaddr_in *)s;

		printf("%d.%d.%d.%d: %d", 
			(sin->sin_addr.s_addr>>24)&0xff,
			(sin->sin_addr.s_addr>>16)&0xff,
			(sin->sin_addr.s_addr>>8)&0xff,
			(sin->sin_addr.s_addr)&0xff,
			sin->sin_port);
	}
}

#endif ARGO_DEBUG
