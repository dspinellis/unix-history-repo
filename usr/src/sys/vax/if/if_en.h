/*	if_en.h	4.1	81/11/30	*/

/*
 * Structure of a Ethernet header.
 */
struct	en_header {
	u_char	en_shost;
	u_char	en_dhost;
	u_short	en_type;
};

#define	ENPUP_IPTYPE	0x0800		/* IP protocol */
/*
 * The ENPUP_NTRAILER packet types starting at ENPUP_TRAIL have
 * (type-ENPUP_TRAIL)*512 bytes of data followed
 * by a PUP type (as given above).
 */
#define	ENPUP_TRAIL	0x1000		/* Trailer PUP */
#define	ENPUP_NTRAILER	16
