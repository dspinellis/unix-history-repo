/*
 * Copyright (c) 1988, 1993 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *      @(#)if_llc.h	7.3 (Berkeley) %G%
 */

/*
 * IEEE 802.2 Link Level Control headers, for use in conjunction with
 * 802.{3,4,5} media access control methods.
 *
 * Headers here do not use bit fields due to shortcommings in many
 * compilers.
 */

struct llc {
	u_char	llc_dsap;
	u_char	llc_ssap;
	union {
	    struct {
		u_char control;
		u_char format_id;
		u_char class;
		u_char window_x2;
	    } type_u;
	    struct {
		u_char num_snd_x2;
		u_char num_rcv_x2;
	    } type_i;
	    struct {
		u_char control;
		u_char num_rcv_x2;
	    } type_s;
	    struct {
	        u_char control;
		struct frmrinfo {
			u_char rej_pdu_0;
			u_char rej_pdu_1;
			u_char frmr_control;
			u_char frmr_control_ext;
			u_char frmr_cause;
		} frmrinfo;
	    } type_frmr;
	    struct {
		u_char control;
		u_char org_code[3];
		u_short ether_type;
	    } type_snap;
	    struct {
		u_char control;
		u_char control_ext;
	    } type_raw;
	} llc_un;
};
#define llc_control            llc_un.type_u.control
#define	llc_control_ext        llc_un.type_raw.control_ext
#define llc_fid                llc_un.type_u.format_id
#define llc_class              llc_un.type_u.class
#define llc_window             llc_un.type_u.window_x2
#define llc_frmrinfo           llc_un.type_frmr.frmrinfo
#define llc_frmr_pdu0          llc_un.type_frmr.frmrinfo.rej_pdu0
#define llc_frmr_pdu1          llc_un.type_frmr.frmrinfo.rej_pdu1
#define llc_frmr_control       llc_un.type_frmr.frmrinfo.frmr_control
#define llc_frmr_control_ext   llc_un.type_frmr.frmrinfo.frmr_control_ext
#define llc_frmr_cause         llc_un.type_frmr.frmrinfo.frmr_control_ext

/*
 * Don't use sizeof(struct llc_un) for LLC header sizes
 */
#define LLC_ISFRAMELEN 4
#define LLC_UFRAMELEN  3
#define LLC_FRMRLEN    7

/*
 * Unnumbered LLC format commands
 */
#define LLC_UI		0x3
#define LLC_UI_P	0x13
#define LLC_DISC	0x43
#define	LLC_DISC_P	0x53
#define LLC_UA		0x63
#define LLC_UA_P	0x73
#define LLC_TEST	0xe3
#define LLC_TEST_P	0xf3
#define LLC_FRMR	0x87
#define	LLC_FRMR_P	0x97
#define LLC_DM		0x0f
#define	LLC_DM_P	0x1f
#define LLC_XID		0xaf
#define LLC_XID_P	0xbf
#define LLC_SABME	0x6f
#define LLC_SABME_P	0x7f

/*
 * Supervisory LLC commands
 */
#define	LLC_RR		0x01
#define	LLC_RNR		0x05
#define	LLC_REJ		0x09

/*
 * Info format - dummy only
 */
#define	LLC_INFO	0x00

/*
 * ISO PDTR 10178 contains among others
 */
#define LLC_X25_LSAP	0x7e
#define LLC_SNAP_LSAP	0xaa
#define LLC_ISO_LSAP	0xfe






