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
/* $Header: iso.h,v 4.9 88/09/11 18:06:38 hagens Exp $ */
/* $Source: /usr/argo/sys/netiso/RCS/iso.h,v $ */

#ifndef __ISO__
#define __ISO__

/*
 *	Return true if this is a multicast address
 *	This assumes that the bit transmission is lsb first. This 
 *	assumption is valid for 802.3 but not 802.5. There is a
 *	kludge to get around this for 802.5 -- see if_lan.c
 *	where subnetwork header is setup.
 */
#define	IS_MULTICAST(snpa)\
	((snpa)[0] & 0x01)
	
/*
 * Protocols
 */
#define	ISOPROTO_TP0	25		/* connection oriented transport protocol */
#define	ISOPROTO_TP1	26		/* not implemented */
#define	ISOPROTO_TP2	27		/* not implemented */
#define	ISOPROTO_TP3	28		/* not implemented */
#define	ISOPROTO_TP4	29		/* connection oriented transport protocol */
#define	ISOPROTO_TP		ISOPROTO_TP4	 /* tp-4 with negotiation */
#define	ISOPROTO_CLTP	30		/* connectionless transport (not yet impl.) */
#define	ISOPROTO_CLNP	31		/* connectionless internetworking protocol */
#define	ISOPROTO_X25	32		/* cons */
#define	ISOPROTO_INACT_NL	33	/* inactive network layer! */
#define	ISOPROTO_ESIS	34		/* ES-IS protocol */

#define	ISOPROTO_RAW	255		/* raw clnp */
#define	ISOPROTO_MAX	256

#define	ISO_PORT_RESERVED		1024
#define	ISO_PORT_USERRESERVED	5000
/*
 * Port/socket numbers: standard network functions
 * NOT PRESENTLY USED
 */
#define	ISO_PORT_MAINT		501
#define	ISO_PORT_ECHO		507
#define	ISO_PORT_DISCARD	509
#define	ISO_PORT_SYSTAT		511
#define	ISO_PORT_NETSTAT	515
/*
 * Port/socket numbers: non-standard application functions
 */
#define ISO_PORT_LOGIN		513
/*
 * Port/socket numbers: public use
 */
#define ISO_PORT_PUBLIC		1024		/* high bit set --> public */

/*
 *	Network layer protocol identifiers
 */
#define ISO8473_CLNP	0x81
#define	ISO9542_ESIS	0x82
#define ISO9542X25_ESIS	0x8a

#ifndef IN_CLASSA_NET
#include "../netinet/in.h"
#endif IN_CLASSA_NET

/*
 *	Type 37 Address
 *
 *	This address is named for the value of its AFI (37). This format
 *	supports an X.121 address. A type 37 address has the following format:
 *
 *  <----- idp -------> <- dsp ->
 *  <- afi -> <- idi -> <- dsp ->
 *  | "37"   | 7 bytes | 9 bytes |
 *
 *	The idi contains 14 bcd digits of X.121 address.
 *	The use of the dsp part is unknown.
 *
 *	The afi is considered the "network" portion of the address.
 *  This means that you can't have multihoming in the x.25 environment.
 *  Makes loopback a bear.
 */
#ifdef BIGSOCKADDRS
#define	ADDR37_IDI_LEN		7			/* 14 bcd digits == 7 octets */
#define	ADDR37_DSP_LEN		9
#else
#define	ADDR37_IDI_LEN		7			/* 14 bcd digits == 7 octets */
#define	ADDR37_DSP_LEN		3			/* this is a lie to fit in sockaddr */
#endif BIGSOCKADDRS
struct addr_37 {
	u_char a37_idi[ADDR37_IDI_LEN];		/* initial domain identifier */
	u_char a37_dsp[ADDR37_DSP_LEN];		/* domain specific part */
};

struct ovl_37 {	/* overlay for type 37 address */
	u_char	o37_afi;					/* afi */
	u_char	o37_x121[ADDR37_IDI_LEN];	/* X.121 address */
	u_char	o37_dsp[ADDR37_DSP_LEN];	/* unknown use at this time */
};

/*
 *	OSINET address
 *
 *	This style address is used by the OSINET group
 *	An OSINET address has the following (variable-length) format
 *
 *  <----- idp ------->  <---------------- dsp --------------------------->
 *  <- afi -> <- idi ->  <---------------- dsp --------------------------->
 *  | "47"   | "0004"   |             11 bytes                            |
 *  |  afi(1)| osinetid | orgid(2) | subnet id(2) | (4-8) | nsap sel(1) |
 *
 *	the afi, orgid, and subnet id are considered the "network" portion of
 *	the address.
 */
#ifdef BIGSOCKADDRS
#define ADDROSINET_IDI_LEN		2
#define ADDROSINET_DSP_LEN		11
#else
#define ADDROSINET_IDI_LEN		2
#define ADDROSINET_DSP_LEN		8		/* this is a lie to fit in sockaddr */
#endif BIGSOCKADDRS
struct addr_osinet {
	u_char aosi_idi[ADDROSINET_IDI_LEN];		/* initial domain identifier */
	u_char aosi_dsp[ADDROSINET_DSP_LEN];		/* domain specific part */
};

#define	OVLOSINET_ID_LEN		2
#ifdef BIGSOCKADDRS
#define	OVLOSINET_ORGID_LEN		2
#define	OVLOSINET_SNETID_LEN	2
#define	OVLOSINET_SNPA_LEN		8
#define	OVLOSINET_NSAP_LEN		1
#else
#define	OVLOSINET_ORGID_LEN		2
#define	OVLOSINET_SNETID_LEN	2
#define	OVLOSINET_SNPA_LEN		5		/* this is a lie to fit in sockaddr */
#define	OVLOSINET_NSAP_LEN		1
#endif BIGSOCKADDRS
struct ovl_osinet { /* overlay for osinet address */
	u_char	oosi_afi;							/* afi */
	u_char	oosi_id[OVLOSINET_ID_LEN];			/* osinet id */
	u_char	oosi_orgid[OVLOSINET_ORGID_LEN];	/* orgid */
	u_char	oosi_snetid[OVLOSINET_SNETID_LEN];	/* subnet id */
	u_char	oosi_snpa[OVLOSINET_SNPA_LEN];		/* snpa */
	u_char	oosi_nsap[OVLOSINET_NSAP_LEN];		/* nsap sel */
};

/*
 *	RFC 986 address
 *
 *	This style address is used when DOD internet addresses are used
 *	The format of rfc986 addresses is:
 *
 *  <----- idp -------> <---------------- dsp -------------------->
 *  <- afi -> <- idi -> <---------------- dsp -------------------->
 *  | "47"   | "0006"  |             6 bytes                       |
 *  |  afi(1)| idi(2)  | version (1) | inet addr (4) | proto id(1) |
 *
 *	the afi, idi, and network portion of the inet address are considered 
 *	the "network" portion of the address.
 */
#define ADDRRFC986_IDI_LEN		2
#define ADDRRFC986_DSP_LEN		6
struct addr_rfc986 {
	u_char	a986_idi[ADDRRFC986_IDI_LEN];		/* initial domain identifier */
	u_char	a986_dsp[ADDRRFC986_DSP_LEN];		/* domain specific part */
};

#define	OVLRFC986_INET_LEN		4
struct ovl_rfc986 {
	u_char	o986_afi;							/* afi */
	u_char	o986_idi[ADDRRFC986_IDI_LEN];		/* idi */
	u_char	o986_vers;							/* version */
	u_char	o986_inetaddr[OVLRFC986_INET_LEN];	/* internet address */
	u_char	o986_upid;							/* upper protocol id */
};
#define RFC986V1				0x01			/* version of 986 addr */

#define	AFI_37		0x37	/* bcd of "37" */
#define AFI_OSINET	0x47	/* bcd of "47" */
#define AFI_RFC986	0x47	/* bcd of "47" */
#define	AFI_SNA		0x00	/* SubNetwork Address; invalid really...
								- used by ES-IS */

/* the idi for type 37 addresses is very different than the others */
#define	IDI_OSINET	0x0004	/* bcd of "0004" */	
#define	IDI_RFC986	0x0006	/* bcd of "0006" */

/*
 *	This address type is used to store a subnetwork address in a 
 *	sockaddr_iso. The isoa_len field should contain the length of the
 *	subnetwork address plus the length of the afi (ie +1 ).
 *
 *	This address format is used only by the ES-IS protocol
 */
#define	ADDRSNA_IDI_LEN		7
struct addr_sn {
	char	sna_addr[ADDRSNA_IDI_LEN];		/* subnetwork address */
};

/* 
 *	Type 47 is the biggest address: 11 bytes. The length of iso_addr
 *	is 13 bytes.
 */
struct iso_addr {
	u_char	isoa_afi;						/* authority and format id */
	union {
		struct addr_37		addr_37;		/* type 37 */
		struct addr_osinet	addr_osinet;	/* type osinet */
		struct addr_rfc986	addr_rfc986;	/* type rfc986 */
		struct addr_sn		addr_sn;		/* subnetwork address */
	} 		isoa_u;
	u_char	isoa_len;						/* length (in bytes) */
};

#define t37_idi		isoa_u.addr_37.a37_idi
#define	t37_dsp		isoa_u.addr_37.a37_dsp
#define osinet_idi	isoa_u.addr_osinet.aosi_idi
#define	osinet_dsp	isoa_u.addr_osinet.aosi_dsp
#define	rfc986_idi	isoa_u.addr_rfc986.a986_idi
#define	rfc986_dsp	isoa_u.addr_rfc986.a986_dsp
#define sna_idi		isoa_u.addr_sn.sna_addr

/*
 *	An iso_addr is 18 bytes, a sockaddr_iso is therefore 20 bytes.
 *	the struct sockaddr data field has been changed to 22 bytes.
 *
 * severly changed osinet and t37 addresses from argo code, we don't want
 * sockaddrs to grow bigger than the original 16 bytes so we changed the 
 * t37 and osinet addresses so that they were only 10 bytes long
 */
struct sockaddr_iso {
	u_short 			siso_family;		/* family */
	u_short 			siso_tsuffix;		/* transport suffix */
	struct 	iso_addr	siso_addr;			/* network address */
};

#define NSAPTYPE_UNKNOWN	-1
#define NSAPTYPE_INET 		0
#define NSAPTYPE_X121BCD	1
#define NSAPTYPE_X121BIN	2
#define NSAPTYPE_DCCBCD		3
#define NSAPTYPE_DCCBIN		4
#define NSAPTYPE_OSINET 	5
#define NSAPTYPE_RFC986 	6

#ifdef KERNEL

extern int iso_netmatch();
extern int iso_hash(); 
extern int iso_addrmatch();
extern struct iso_ifaddr *iso_iaonnetof();
extern	struct domain isodomain;
extern	struct protosw isosw[];

#else
/* user utilities definitions from the iso library */

char *iso_ntoa();
struct hostent *iso_gethostbyname(), *iso_gethostbyaddr();

#endif KERNEL

#endif __ISO__
