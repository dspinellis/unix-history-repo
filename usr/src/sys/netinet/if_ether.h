/*	if_ether.h	4.2	82/12/16	*/

/*
 * Structure of a 10Mb/s Ethernet header.
 */
struct	ether_header {
	u_char	ether_dhost[6];
	u_char	ether_shost[6];
	u_short	ether_type;
};

#define	ETHERPUP_PUPTYPE	4		/* PUP protocol */
#define	ETHERPUP_IPTYPE	8		/* IP protocol */

/*
 * The ETHERPUP_NTRAILER packet types starting at ETHERPUP_TRAIL have
 * (type-ETHERPUP_TRAIL)*512 bytes of data followed
 * by a PUP type (as given above) and then the (variable-length) header.
 */
#define	ETHERPUP_TRAIL	16		/* Trailer PUP */
#define	ETHERPUP_NTRAILER	16

#define	ETHERMTU	1500
#define	ETHERMIN	(60-14)
