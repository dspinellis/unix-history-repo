/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_il.h	6.4 (Berkeley) %G%
 */

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
