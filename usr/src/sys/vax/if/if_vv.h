/*	if_vv.h	4.1	82/06/04	*/
/*
 * Local network header for V2LNI Ring
 * This is arbitrated by "V2LNI-PEOPLE@MIT-MC"
 * (aka Joel N. Chiappa)
 */

struct vv_header {
	 /* the first two fields are required by the hardware */
	u_char	vh_dhost;	/* destination address */
	u_char	vh_shost;	/* source address */
	/* the next three fields are the local network header */
	u_char	vh_version;	/* header version */
	u_char	vh_type;	/* packet type => protocol number */
	short	vh_info;	/* protocol-specific information */
};

#define	RING_VERSION	1	/* current version of v2lni header */

/*
 * Packet types (protocol numbers) in v2lni header
 */
#define	RING_IP		1
#define	RING_IPTrailer	2
#define	RING_WHOAMI	0xa5	/* insure some bit transitions */

#define	VV_BROADCAST	0	/* hardware-defined broadcast address */

/*
 * Proteon V2LNI Hardware definitions
 * register bit definitions - new style
 */
#define	VV_ENB	01		/* Enable Operation */
#define	VV_DEN	02		/* Enable DMA */
#define	VV_HEN	04		/* Host Relay Enable (Rcv) */
#define	VV_CPB	04		/* Clear Packet Buffer (Xmit) */
#define	VV_STE	010		/* Self Test Enable (Rcv) */
#define	VV_UT1	010		/* Unused (Xmit) */
#define	VV_LPB	020		/* Modem Disable (Rcv) */
#define	VV_INR	020		/* Initialize Ring (Xmit) */
#define	VV_RST	040		/* Reset */
#define	VV_IEN	0100		/* Interrupt Enable */
#define	VV_RDY	0200		/* Done */
#define	VV_DPR	0400		/* Data Present (Rcv) */
#define	VV_RFS	0400		/* Refused (Xmit) */
#define	VV_NXM	01000		/* Non Existent Memory */
#define	VV_OVR	02000		/* Overrun */
#define	VV_ODB	04000		/* Odd Byte (Achtung, mein Fuehrer) (Rcv) */
#define	VV_UT2	04000		/* Unused (Xmit) */
#define	VV_LDE	010000		/* Link Data Error (Rcv) */
#define	VV_OPT	010000		/* Output Timeout (Xmit) */
#define	VV_NOK	020000		/* Ring Not OK */
#define	VV_BDF	040000		/* Bad Format in Operation */
#define	VV_NIR	0100000		/* Not in Ring */

#define	VVXERR	(VV_NXM|VV_OVR|VV_OPT|VV_BDF)	/* Xmit errs */
#define	VVRERR	(VV_NXM|VV_OVR|VV_ODB|VV_BDF)	/* Rcv errs */
#define	VVFE	(VV_NXM|VV_OVR)			/* Fatal errors */

#define VV_IBITS \
"\10\20NIR\17BDF\16NOK\15LDE\14ODB\13OVR\12NXM\11DPR\10RDY\7IEN\6RST\5LPB\4STE\3HEN\2DEN\1ENB"

#define VV_OBITS \
"\10\20NIR\17BDF\16NOK\15OPT\13OVR\12NXM\11RFS\10RDY\7IEN\6RST\5INR\3HEN\2DEN\1ENB"

/* device registers */
struct vvreg {
	short	vvicsr;		/* input csr */
	u_short	vviwc;		/* input word count */
	u_short	vviba;		/* input addr lo */
	u_short	vviea;		/* input addr hi */
	short	vvocsr;		/* output csr */
	u_short	vvowc;		/* output word count */
	u_short	vvoba;		/* output addr lo */
	u_short	vvoea;		/* output addr hi */
};

#define	VVRETRY	7
