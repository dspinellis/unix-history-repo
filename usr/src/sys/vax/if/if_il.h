/*	if_il.h	4.2	82/06/23	*/

/*
 * Structure of an Ethernet header -- transmit format
 */
struct	il_xheader {
	u_char	ilx_dhost[6];		/* Destination Host */
	u_short	ilx_type;		/* Type of packet */
};

/*
 * Structure of an Ethernet header -- receive format
 */
struct	il_rheader {
	u_char	ilr_status;		/* Frame Status */
	u_char	ilr_fill1;
	u_short	ilr_length;		/* Frame Length */
	u_char	ilr_dhost[6];		/* Destination Host */
	u_char	ilr_shost[6];		/* Source Host */
	u_short	ilr_type;		/* Type of packet */
};

#define	ILPUP_PUPTYPE	0x0400		/* PUP protocol */
#define	ILPUP_IPTYPE	0x0800		/* IP protocol */

/*
 * The ILPUP_NTRAILER packet types starting at ILPUP_TRAIL have
 * (type-ILPUP_TRAIL)*512 bytes of data followed
 * by a PUP type (as given above) and then the (variable-length) header.
 */
#define	ILPUP_TRAIL	0x1000		/* Trailer PUP */
#define	ILPUP_NTRAILER	16

/*
 * Structure of statistics record
 */
struct	il_stats {
	u_short	ils_fill1;
	u_short	ils_length;		/* Length (should be 62) */
	u_char	ils_addr[6];		/* Ethernet Address */
	u_short	ils_frames;		/* Number of Frames Received */
	u_short	ils_rfifo;		/* Number of Frames in Receive FIFO */
	u_short	ils_xmit;		/* Number of Frames Transmitted */
	u_short	ils_xcollis;		/* Number of Excess Collisions */
	u_short	ils_frag;		/* Number of Fragments Received */
	u_short	ils_lost;		/* Number of Times Frames Lost */
	u_short	ils_multi;		/* Number of Multicasts Accepted */
	u_short	ils_rmulti;		/* Number of Multicasts Rejected */
	u_short	ils_crc;		/* Number of CRC Errors */
	u_short	ils_align;		/* Number of Alignment Errors */
	u_short	ils_collis;		/* Number of Collisions */
	u_short	ils_owcollis;		/* Number of Out-of-window Collisions */
	u_short	ils_fill2[8];
	char	ils_module[8];		/* Module ID */
	char	ils_firmware[8];	/* Firmware ID */
};

/*
 * Structure of Collision Delay Time Record
 */
struct	il_collis {
	u_short	ilc_fill1;
	u_short	ilc_length;		/* Length (should be 0-32) */
	u_short	ilc_delay[16];		/* Delay Times */
};
