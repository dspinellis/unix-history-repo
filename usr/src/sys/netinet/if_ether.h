/*	if_ether.h	4.1	82/12/16	*/

/*
 * Structure of a 10Mb/s Ethernet header.
 */
struct	eth_header {
	u_char	eth_dhost[6];
	u_char	eth_shost[6];
	u_short	eth_type;
};

#define	ETHPUP_PUPTYPE	4		/* PUP protocol */
#define	ETHPUP_IPTYPE	8		/* IP protocol */

/*
 * The ETHPUP_NTRAILER packet types starting at ETHPUP_TRAIL have
 * (type-ETHPUP_TRAIL)*512 bytes of data followed
 * by a PUP type (as given above) and then the (variable-length) header.
 */
#define	ETHPUP_TRAIL	16		/* Trailer PUP */
#define	ETHPUP_NTRAILER	16
