/* spkt.h - include file for session providers (SS-PROVIDER) */

/* 
 * $Header: /f/osi/h/RCS/spkt.h,v 7.2 91/02/22 09:25:07 mrose Interim $
 *
 *
 * $Log:	spkt.h,v $
 * Revision 7.2  91/02/22  09:25:07  mrose
 * Interim 6.8
 * 
 * Revision 7.1  89/11/27  10:30:35  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:56:00  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef	_SSAP_
#include "ssap.h"		/* definitions for SS-USERs */
#endif

#include "tsap.h"		/* definitions for TS-USERs */


/*  */

#define	ssapPsig(sb, sd) \
{ \
    if ((sb = findsblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "invalid session descriptor"); \
    } \
    if (!(sb -> sb_flags & SB_CONN)) {\
	(void) sigiomask (smask); \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "session descriptor not connected"); \
    } \
    if (sb -> sb_flags & SB_FINN) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "session descriptor finishing"); \
    } \
    if (sb -> sb_flags & SB_RELEASE) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "release in progress"); \
    } \
    if (sb -> sb_flags & SB_MAP) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "majorsync in progress"); \
    } \
    if (sb -> sb_flags & SB_RS) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "resync in progress"); \
    } \
    if (sb -> sb_flags & SB_RA) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "awaiting your resync response"); \
    } \
    if (sb -> sb_flags & SB_AI) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "activity interrupt/discard in progress"); \
    } \
    if (sb -> sb_flags & SB_AIA) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "awaiting your activity interrupt/discard response"); \
    } \
    if (sb -> sb_flags & (SB_ED | SB_EDACK | SB_ERACK)) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "exception in progress"); \
    } \
}

#define	ssapXsig(sb, sd) \
{ \
    if ((sb = findsblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "invalid session descriptor"); \
    } \
    if (!(sb -> sb_flags & SB_CONN)) {\
	(void) sigiomask (smask); \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "session descriptor not connected"); \
    } \
    if (sb -> sb_flags & SB_FINN) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "session descriptor finishing"); \
    } \
    if (sb -> sb_flags & SB_RELEASE) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "release in progress"); \
    } \
    if (sb -> sb_flags & SB_RS) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "resync in progress"); \
    } \
    if (sb -> sb_flags & SB_RA) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "awaiting your resync response"); \
    } \
    if (sb -> sb_flags & SB_AI) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "activity interrupt/discard in progress"); \
    } \
    if (sb -> sb_flags & SB_AIA) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "awaiting your activity interrupt/discard response"); \
    } \
    if (sb -> sb_flags & SB_ED) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "exception in progress"); \
    } \
}

#define	ssapRsig(sb, sd) \
{ \
    if ((sb = findsblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "invalid session descriptor"); \
    } \
    if (!(sb -> sb_flags & SB_CONN)) {\
	(void) sigiomask (smask); \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "session descriptor not connected"); \
    } \
    if (sb -> sb_flags & SB_FINN) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "session descriptor finishing"); \
    } \
    if (sb -> sb_flags & SB_RELEASE) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "release in progress"); \
    } \
    if (sb -> sb_flags & SB_RS) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "resync in progress"); \
    } \
    if (sb -> sb_flags & SB_AI) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "activity interrupt/discard in progress"); \
    } \
    if (sb -> sb_flags & SB_AIA) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "awaiting your activity interrupt/discard response"); \
    } \
    if (sb -> sb_flags & SB_AE) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "activity end in progress"); \
    } \
    if (sb -> sb_flags & SB_ED) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "exception in progress"); \
    } \
}

#define	ssapAsig(sb, sd) \
{ \
    if ((sb = findsblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "invalid session descriptor"); \
    } \
    if (!(sb -> sb_flags & SB_CONN)) {\
	(void) sigiomask (smask); \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "session descriptor not connected"); \
    } \
    if (sb -> sb_flags & SB_FINN) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "session descriptor finishing"); \
    } \
    if (sb -> sb_flags & SB_RELEASE) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "release in progress"); \
    } \
    if (sb -> sb_flags & SB_MAA) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "awaiting your majorsync response"); \
    } \
    if (sb -> sb_flags & SB_RS) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "resync in progress"); \
    } \
    if (sb -> sb_flags & SB_AI) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "activity interrupt/discard in response"); \
    } \
    if (sb -> sb_flags & (SB_ED | SB_EDACK | SB_ERACK)) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "exception in progress"); \
    } \
}

#define	ssapFsig(sb, sd) \
{ \
    if ((sb = findsblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "invalid session descriptor"); \
    } \
    if (!(sb -> sb_flags & SB_CONN)) {\
	(void) sigiomask (smask); \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "session descriptor not connected"); \
    } \
    if (!(sb -> sb_flags & SB_FINN)) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "session descriptor not finishing"); \
    } \
    if (sb -> sb_flags & SB_RELEASE) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "release in progress"); \
    } \
    if (sb -> sb_flags & SB_MAA) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "awaiting your majorsync response"); \
    } \
    if (sb -> sb_flags & SB_RS) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "resync in progress"); \
    } \
    if (sb -> sb_flags & SB_RA) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "awaiting your resync response"); \
    } \
    if (sb -> sb_flags & SB_AI) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "activity interrupt/discard in progress"); \
    } \
    if (sb -> sb_flags & SB_AIA) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "awaiting your activity interrupt/discard response"); \
    } \
    if (sb -> sb_flags & (SB_ED | SB_EDACK | SB_ERACK)) { \
	(void) sigiomask (smask); \
	return ssaplose (si, SC_OPERATION, NULLCP, \
			    "exception in progress"); \
    } \
}

#define	missingP(p) \
{ \
    if (p == NULL) \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "mandatory parameter \"%s\" missing", "p"); \
}

#define	refmuchP(sr) \
{ \
    if ((sr) -> sr_ulen > SREF_USER_SIZE \
	    || (sr) -> sr_clen > SREF_COMM_SIZE \
	    || (sr) -> sr_alen > SREF_ADDT_SIZE \
	    || (sr) -> sr_vlen > SREF_USER_SIZE) \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
		"bad format for reference"); \
}

#define	idmuchP(sd) \
{ \
    if ((sd) -> sd_len > SID_DATA_SIZE) \
	return ssaplose (si, SC_PARAMETER, NULLCP, \
		"bad format for activity ID"); \
}

#define	toomuchP(sb,b,n,m,p) \
{ \
    if (b == NULL) \
	n = 0; \
    else \
	if (n > (sb -> sb_version < SB_VRSN2 ? m : ENCLOSE_MAX)) { \
	    (void) sigiomask (smask); \
	    return ssaplose (si, SC_PARAMETER, NULLCP, \
			    "too much %s user data, %d octets", p, n); \
	} \
}


#define	NULLTX	((struct TSAPdata *) 0)
#define	NULLSD	((struct SSAPactid *) 0)
#define	NULLSR	((struct SSAPref *) 0)


#ifndef	lint
#ifndef	__STDC__
#define	copySSAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d/* */_cc = min (i, sizeof d -> d/* */_data)) > 0) \
	bcopy (base, d -> d/* */_data, d -> d/* */_cc); \
}

#define	copySPKTdata(s,d) \
{ \
    d -> d/* */_data = s -> s_udata, d -> d/* */_cc = s -> s_ulen; \
    s -> s_udata = NULL; \
}
#else
#define	copySSAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d##_cc = min (i, sizeof d -> d##_data)) > 0) \
	bcopy (base, d -> d##_data, d -> d##_cc); \
}

#define	copySPKTdata(s, d) \
{ \
    d -> d##_data = s -> s_udata, d -> d##_cc = s -> s_ulen; \
    s -> s_udata = NULL; \
}
#endif
#else
#define	copySSAPdata(base,len,d)	bcopy (base, (char *) d, len)

#define	copySPKTdata(s,d)	bcopy (s -> s_udata, (char *) d, s -> s_ulen)
#endif


#define	AB_TIM		30	/* drain for 30 seconds on ABORTs */
#define	RF_TIM		30	/* drain for 30 seconds on REFUSEs */


#define	SC_REFUSE	(SC_BASE << 1)	/* larger than any sc_reason */


int	spktlose (), ssaplose ();

/*  */

struct ssapblk {
    struct ssapblk *sb_forw;	/* doubly-linked list */
    struct ssapblk *sb_back;	/*   .. */

    int     sb_fd;		/* transport descriptor */

    int	    sb_version;		/* version number of protocol */
#define	SB_VRSN1	0	/*   1 */
#define	SB_VRSN2	1	/*   2 */
#define	SB_ALLVRSNS	((1 << SB_VRSN1) | (1 << SB_VRSN2))
    int	    sb_vrsnmask;	/* for initiating SPM... */

    long    sb_flags;		/* our state */
#define	SB_NULL		0x000000
#define	SB_CONN		0x000001/* connected */
#define	SB_FINN		0x000002/* other side wants to finish */
#define	SB_INIT		0x000004/* this side initiated the session */
#define	SB_ASYN		0x000008/* asynchronous */
#define	SB_EXPD		0x000010/* expedited service available on transport */
#define	SB_CD		0x000020/* CD request in progress */
#define	SB_CDA		0x000040/* awaiting CD response from user */
#define	SB_GTC		0x000080/* GTC request in progress */
#define	SB_MAP		0x000100/* MAP request in progress */
#define	SB_MAA		0x000200/* awaiting MAP response from user */
#define	SB_AE		0x000400/* MAP is really AE */
#define	SB_RS		0x000800/* RS request in progress */
#define	SB_RA		0x001000/* awaiting RS response from user */
#define	SB_AI		0x002000/* AI request in progress */
#define	SB_AIA		0x004000/* awaiting AI response from user */
#define	SB_ED		0x008000/* user exception in progress */
#define	SB_EDACK	0x010000/* awaiting user exception to be cleared */
#define	SB_ERACK	0x020000/* awaiting provider exception to be cleared */
#define	SB_Vact		0x040000/* activity in progress */
#define	SB_Vsc		0x080000/* okay to reply to minorsync */
#define	SB_Vnextact	0x100000/* activity MAP sent/received */
#define	SB_RELEASE	0x200000/* release in progress */

    struct ssapkt *sb_retry;	/* initial/final spkt */

    long    sb_V_A;		/* lowest unconfirmed ssn */
    long    sb_V_M;		/* next ssn */
    long    sb_V_R;		/* lowest ssn for resynchronization */
    int	    sb_rs;		/* resynchronization type
				     (an SYNC_xxx code, plus...) */
#define	SYNC_INTR	3	/* Activity Interrupt */
#define	SYNC_DISC	4	/* Activity Discard */
    long    sb_rsn;		/* resync serial number */
    u_char  sb_rsettings;	/* proposed token settings */

    int	    sb_pr;		/* SPDU to prepare for (an SPDU code) */
    struct ssapkt *sb_xspdu;	/* saved expedited SPDU
				   (really should be a linked list!) */

    struct ssapkt *sb_spdu;	/* for concatenated SPDUs */

    struct qbuf sb_qbuf;	/* for segmented (T)SSDUs */
    int	    sb_len;		/*   .. */
    int	    sb_code;		/*   .. */

    u_char  sb_options;		/* connect options */
    u_char  sb_settings;	/* tokens settings on connect */

    u_short sb_tsdu_us;		/* our max TSDU size */
    u_short sb_tsdu_them;	/* their max TSDU size */
#define	BAD_TSDU_SIZE(s)	((s) ? (s) < DT_MINSIZE : 0)
#define	GET_TSDU_SIZE(s)	((s) < DT_MINSIZE ? 0 : (s))

    u_char  sb_owned;		/* tokens we own */
    u_short sb_requirements;	/* functional units selected */

    struct SSAPaddr sb_initiating;	/* initiator */
    struct SSAPaddr sb_responding;	/* responder */

    int	    sb_maxtime;		/* for SPM response during S-CONNECT */

    IFP	    sb_DataIndication;		/* INDICATION handlers */
    IFP	    sb_TokenIndication;		/*   .. */
    IFP	    sb_SyncIndication;		/*   .. */
    IFP	    sb_ActivityIndication;	/*   .. */
    IFP	    sb_ReportIndication;	/*   .. */
    IFP	    sb_ReleaseIndication;	/*   .. */
    IFP	    sb_AbortIndication;		/*   .. */
};
#define	NULLBP		((struct ssapblk *) 0)

int	freesblk ();
struct ssapblk *newsblk (), *findsblk ();


int	ts2sslose ();

int	spkt2sd ();
struct ssapkt *sb2spkt ();

/*    SPKT datastructure */

struct ssapkt {
    int	    s_errno;

    int	    s_mask;

    u_char    s_code;
#define	SPDU_CN		0x0d	/* CONNECT */
#define	SPDU_AC		0x0e	/* ACCEPT */
#define	SPDU_RF		0x0c	/* REFUSE */
#define	SPDU_FN		0x09	/* FINISH */
#define	SPDU_DN		0x0a	/* DISCONNECT */
#define	SPDU_NF		0x08	/* NOT FINISHED */
#define	SPDU_AB		0x19	/* ABORT */
#define	SPDU_AA		0x1a	/* ABORT ACCEPT */
#define	SPDU_DT		SPDU_GT	/* DATA TRANSFER */
#define	SPDU_EX		0x05	/* EXPEDITED */
#define	SPDU_TD		0x21	/* TYPED DATA */
#define	SPDU_CD		0x3d	/* CAPABILITY DATA */
#define	SPDU_CDA	0x3e	/* CAPABILITY DATA ACK */
#define	SPDU_GT		0x01	/* GIVE TOKENS */
#define	SPDU_PT		0x02	/* PLEASE TOKENS */
#define	SPDU_GTC	0x15	/* GIVE TOKENS CONFIRM */
#define	SPDU_GTA	0x16	/* GIVE TOKENS ACK */
#define	SPDU_MIP	0x31	/* MINOR SYNCHRONIZATION POINT */
#define	SPDU_MIA	0x32	/* MINOR SYNC ACK */
#define	SPDU_MAP	0x29	/* MAJOR SYNCHRONIZATION POINT */
#define	SPDU_MAA	0x2a	/* MAJOR SYNC ACK */
#define	SPDU_RS		0x35	/* RESYNCHRONIZE */
#define	SPDU_RA		0x22	/* RESYNCHRONIZE ACK */
#define	SPDU_PR		0x07	/* PREPARE */
#define	SPDU_ER		0x00	/* EXCEPTION REPORT */
#define	SPDU_ED		0x30	/* EXCEPTION DATA */
#define	SPDU_AS		0x2d	/* ACTIVITY START */
#define	SPDU_AR		0x1d	/* ACTIVITY RESUME */
#define	SPDU_AI		SPDU_AB	/* ACTIVITY INTERRUPT */
#define	SPDU_AIA	SPDU_AA	/* ACTIVITY INTERRUPT ACK */
#define	SPDU_AD		0x39	/* ACTIVITY DISCARD */
#define	SPDU_ADA	0x3a	/* ACTIVITY DISCARD ACK */
#define	SPDU_AE		SPDU_MAP/* ACTIVITY END */
#define	SPDU_AEA	SPDU_MAA/* ACTIVITY END ACK */

    u_long    s_li;
#define	SPDU_MAXLEN	65535	/* segment if SSDU larger */

/* A nice magic number:
	for the GT SPDU, 2 octets
	for the DT SPDU, 2 octets + 3 octets for the enclosure option

    2 + 2 + 3 = 7
 */
#define	SSDU_MAGIC	7

    union {
	struct {		/* CONNECT/ACCEPT SPDU */
#define	SMASK_CN_REF	0x0001
	    struct SSAPref un_cn_reference;

	    struct {
#define	SMASK_CN_OPT	0x0002
		u_char	un_cn_options;
#define	CR_OPT_NULL	0x00
#define	CR_OPT_EXTD	0x01	/* will receive extended concatenated SPDUs,
				   this implementation DOESN'T; segmenting is
				   enough... */
#define	CR_OPT_MASK	CR_OPT_EXTD

#define	SMASK_CN_TSDU	0x0004
		u_short	un_cn_tsdu_init;
		u_short	un_cn_tsdu_resp;

#define	SMASK_CN_VRSN	0x0008
		u_char  un_cn_version;

#define	SMASK_CN_ISN	0x0010
#define	SIZE_CN_ISN	6
		u_long	un_cn_isn;

#define	SMASK_CN_SET	0x0020
		u_char  un_settings;
	    }	un_cn_item;

#define	SMASK_AC_TOKEN	0x0040
	    u_char	un_ac_token;/* ACCEPT SPDU only */

#define	SMASK_CN_REQ	0x0080
	    u_short	un_cn_requirements;

#define	SMASK_CN_CALLING 0x0100
	    char	un_cn_calling[SSSIZE];
	    int		un_cn_callinglen;

#define	SMASK_CN_CALLED	0x0200
	    char	un_cn_called[SSSIZE];
	    int		un_cn_calledlen;
	}	un_cn;
#define	CN_SIZE		512
#define	CONNECT_MAX	10240	/* someday support CDO/OA SPDUs and Data
				   Overflow PI... */
#define CN_BASE_SIZE	56
#define	AC_SIZE		512
#define AC_BASE_SIZE	62

	struct {		/* REFUSE SPDU */
#define	SMASK_RF_REF	0x0001
	    struct SSAPref un_rf_reference;

#define	SMASK_RF_DISC	0x0002
	    u_char	un_rf_disconnect;
#define	RF_DISC_RELEASE	0x01	/* release transport connection */
#define	RF_DISC_MASK	RF_DISC_RELEASE

#define	SMASK_RF_REQ	0x0004
	    u_short	un_rf_requirements;

#define	SMASK_RF_VRSN	0x0008
	    u_char	un_rf_version;

	    char       *un_rf_rdata;
	    int		un_rf_rlen;
	}	un_rf;
#define	RF_SIZE		513
#define RF_BASE_SIZE	13
	
	struct {		/* FINISH SPDU */
#define	SMASK_FN_DISC	0x0001
	    u_char	un_fn_disconnect;
#define	FN_DISC_RELEASE	0x01	/* release transport connection */
#define	FN_DISC_MASK	FN_DISC_RELEASE
	}	un_fn;
#define	FN_SIZE		512
#define FN_BASE_SIZE	6
	    
				/* DISCONNECT SPDU */
#define	DN_SIZE		512
#define DN_BASE_SIZE	3

				/* NOT FINISHED SPDU */
#define	NF_SIZE		512
#define NF_BASE_SIZE	3

	struct {		/* ABORT SPDU */
#define	SMASK_AB_DISC	0x0001
	    u_char	un_ab_disconnect;
#define	AB_DISC_RELEASE	0x01	/* release transport connection */
#define	AB_DISC_USER	0x02	/* user abort */
#define	AB_DISC_PROTO	0x04	/* protocol error */
#define	AB_DISC_UNKNOWN	0x08	/* no reason */
#define	AB_DISC_MASK	(AB_DISC_RELEASE | AB_DISC_USER | AB_DISC_PROTO \
			    | AB_DISC_UNKNOWN)

#define	SMASK_AB_REFL	0x0002
#define	AB_REFL_SIZE	9
	    u_char	un_ab_reflect[AB_REFL_SIZE];
	}	un_ab;
#define	AB_SIZE		9
#define AB_BASE_SIZE	17
#define	SMASK_SPDU_AB	0x0004
				/* to distinguish between AB and AI SPDUs */

	
				/* ABORT ACCEPT SPDU */
#define	AA_SIZE		0
#define AA_BASE_SIZE	0
#define	SMASK_SPDU_AA	0x0001	/* to distinguish between AA and AIA SPDUs */


				/* DATA TRANSFER SPDU */
#define	DT_SIZE		65535
#define	DT_MINSIZE	64	/* don't segment if MSS < this */
#define DT_BASE_SIZE	3
	
				/* EXPEDITED DATA SPDU */
#define	EX_SIZE		SX_EXSIZE
#define EX_BASE_SIZE	0

				/* TYPED DATA SPDU */
#define	TD_SIGHS	65535	/* should be TD_SIZE, but <tsap.h>
				   got there first */
#define	TD_MINSIZE	64	/* don't segment if MSS < this */
#define TD_BASE_SIZE	3

				/* CAPABILITY DATA SPDU */
#define	CD_SIZE		SX_CDSIZE
#define CD_BASE_SIZE	3

				/* CAPABILITY DATA ACK SPDU */
#define	CDA_SIZE	SX_CDASIZE
#define CDA_BASE_SIZE	3

	struct {		/* GIVE TOKENS SPDU */
#define	SMASK_GT_TOKEN	0x0001
	    u_char	un_gt_token;
	}	un_gt;
#define	GT_SIZE		0
#define GT_BASE_SIZE	3
#define	SMASK_SPDU_GT	0x0002	/* to distinguish between DT and GT SPDUs */
	
	struct {		/* PLEASE TOKENS SPDU */
#define	SMASK_PT_TOKEN	0x0001
	    u_char	un_pt_token;
	}	un_pt;
#define	PT_SIZE		512
#define PT_BASE_SIZE	6

				/* GIVE TOKENS CONFIRM SPDU */
#define	GTC_SIZE	0
#define GTC_BASE_SIZE	0

				/* GIVE TOKENS ACK SPDU */
#define	GTA_SIZE	0
#define GTA_BASE_SIZE	0

	struct {		/* MINOR SYNC POINT SPDU */
#define	SMASK_MIP_SYNC	0x0001
	    u_char	un_mip_sync;
#define	MIP_SYNC_NOEXPL	0x01	/* NO EXPLICIT ACK REQUIRED */
#define	MIP_SYNC_MASK	MIP_SYNC_NOEXPL

#define	SMASK_MIP_SERIAL 0x0002
	    u_long	un_mip_serial;
	}	un_mip;
#define	MIP_SIZE	512
#define MIP_BASE_SIZE	14

	struct {		/* MINOR SYNC ACK SPDU */
#define	SMASK_MIA_SERIAL 0x0001
	    u_long	un_mia_serial;
	}	un_mia;
#define	MIA_SIZE	512
#define MIA_BASE_SIZE	11

	struct {		/* MAJOR SYNC POINT SPDU */
#define	SMASK_MAP_SYNC	0x0001
	    u_char	un_map_sync;
#define	MAP_SYNC_NOEND	0x01	/* ACTIVITY NOT ENDED (i.e., MAP not AE) */
#define	MAP_SYNC_MASK	MAP_SYNC_NOEND

#define	SMASK_MAP_SERIAL 0x0002
	    u_long	un_map_serial;
	}	un_map;
#define	MAP_SIZE	512
#define MAP_BASE_SIZE	14

	struct {		/* MAJOR SYNC ACK SPDU */
#define	SMASK_MAA_SERIAL 0x0001
	    u_long	un_maa_serial;
	}	un_maa;
#define	MAA_SIZE	512
#define MAA_BASE_SIZE	11

	struct {		/* RESYNCHRONIZE SPDU */
#define	SMASK_RS_SET	0x0001
	    u_char	un_rs_settings;

#define	SMASK_RS_TYPE	0x0002
	    u_char	un_rs_type;
#define	SYNC_OK(r)	(((unsigned) (r)) <= SYNC_SET)

#define	SMASK_RS_SSN	0x0004
	    u_long	un_rs_serial;
    }	    un_rs;
#define	RS_SIZE		512
#define RS_BASE_SIZE	17

	struct {		/* RESYNCHRONIZE ACK SPDU */
#define	SMASK_RA_SET	0x0001
	    u_char	un_ra_settings;

#define	SMASK_RA_SSN	0x0002
	    u_long	un_ra_serial;
	}    un_ra;
#define	RA_SIZE		512
#define RA_BASE_SIZE	14

	struct {		/* PREPARE SPDU */
#define	SMASK_PR_TYPE	0x0001
	    u_char	    un_pr_type;
#define	PR_MAA		1	/* expect SPDU_MAA */
#define	PR_RS		2	/* expect SPDU_RS */
#define	PR_RA		3	/* expect SPDU_RA */
#define	PR_AB		4	/* expect SPDU_AB */
#define	PR_MAX		PR_AB
	}    un_pr;
#define	PR_SIZE		0
#define PR_BASE_SIZE	3

				/* EXCEPTION REPORT SPDU */
#define	ER_SIZE		0
#define ER_BASE_SIZE	0

	struct {		/* EXCEPTION DATA SPDU */
#define	SMASK_ED_REASON	0x0001
	    u_char	un_ed_reason;
#define	SP_OK(r)	(((r) < SP_PROCEDURAL \
				&& (r) != SP_RSVD1 \
				&& (r) != SP_RSVD2) \
			    || (r) == SP_DEMAND)
	}    un_ed;
#define	ED_SIZE		512
#define ED_BASE_SIZE	6

	struct {		/* ACTIVITY START SPDU */
#define	SMASK_AS_ID	0x0001
	    struct SSAPactid un_as_id;
	}    un_as;	
#define	AS_SIZE		512
#define AS_BASE_SIZE	11

	struct {		/* ACTIVITY RESUME SPDU */
	    struct {
#define	SMASK_AR_REF	0x0001
		struct SSAPref un_ar_reference;

#define	SMASK_AR_OID	0x0002
		struct SSAPactid un_ar_oid;

#define	SMASK_AR_SSN	0x0004
		u_long	un_ar_serial;
	    }	un_ar_link;

#define	SMASK_AR_ID	0x0008
	    struct SSAPactid un_ar_id;
	}    un_ar;
#define	AR_SIZE		512
#define AR_BASE_SIZE	29

	struct {		/* ACTIVITY INTERRUPT (ABORT) SPDU */
#define	SMASK_AI_REASON	0x0001	/* don't collide with SMASK_AB_DISC */
	    u_char	un_ai_reason;
	}    un_ai;
#define	AI_SIZE		0
#define AI_BASE_SIZE	3

				/* ACTIVITY INTERRUPT (ABORT) ACK SPDU */
#define	AIA_SIZE	0
#define AIA_BASE_SIZE	0


	struct {		/* ACTIVITY DISCARD SPDU */
#define	SMASK_AD_REASON	0x0001
	    u_char	un_ad_reason;
	}    un_ad;
#define	AD_SIZE		0
#define AD_BASE_SIZE	3

				/* ACTIVITY DISCARD ACK SPDU */
#define	ADA_SIZE	0
#define ADA_BASE_SIZE	0

				/* ACTIVITY END (MAJOR SYNC) SPDU */
#define	AE_SIZE		512
#define AE_BASE_SIZE	8

				/* ACTIVITY END (MAJOR SYNC) ACK SPDU */
#define	AEA_SIZE	MAA_SIZE
#define AEA_BASE_SIZE	MAA_BASE_SIZE
    }	s_un;
#define	s_cn_reference	s_un.un_cn.un_cn_reference
#define	s_options	s_un.un_cn.un_cn_item.un_cn_options
#define s_tsdu_init	s_un.un_cn.un_cn_item.un_cn_tsdu_init
#define s_tsdu_resp	s_un.un_cn.un_cn_item.un_cn_tsdu_resp
#define s_cn_version	s_un.un_cn.un_cn_item.un_cn_version
#define s_isn		s_un.un_cn.un_cn_item.un_cn_isn
#define s_settings	s_un.un_cn.un_cn_item.un_settings
#define	s_ac_token	s_un.un_cn.un_ac_token
#define	s_cn_require	s_un.un_cn.un_cn_requirements
#define	s_calling	s_un.un_cn.un_cn_calling
#define	s_callinglen	s_un.un_cn.un_cn_callinglen
#define	s_called	s_un.un_cn.un_cn_called
#define	s_calledlen	s_un.un_cn.un_cn_calledlen

#define	s_rf_reference	s_un.un_rf.un_rf_reference
#define	s_rf_disconnect	s_un.un_rf.un_rf_disconnect
#define	s_rf_require	s_un.un_rf.un_rf_requirements
#define	s_rf_version	s_un.un_rf.un_rf_version
#define	s_rdata		s_un.un_rf.un_rf_rdata
#define	s_rlen		s_un.un_rf.un_rf_rlen

#define	s_fn_disconnect	s_un.un_fn.un_fn_disconnect

#define	s_ab_disconnect	s_un.un_ab.un_ab_disconnect
#define	s_reflect	s_un.un_ab.un_ab_reflect

#define	s_gt_token	s_un.un_gt.un_gt_token

#define	s_pt_token	s_un.un_pt.un_pt_token

#define	s_mip_sync	s_un.un_mip.un_mip_sync
#define	s_mip_serial	s_un.un_mip.un_mip_serial

#define	s_mia_serial	s_un.un_mia.un_mia_serial

#define	s_map_sync	s_un.un_map.un_map_sync
#define	s_map_serial	s_un.un_map.un_map_serial

#define	s_maa_serial	s_un.un_maa.un_maa_serial

#define	s_rs_settings	s_un.un_rs.un_rs_settings
#define	s_rs_type	s_un.un_rs.un_rs_type
#define	s_rs_serial	s_un.un_rs.un_rs_serial

#define	s_ra_settings	s_un.un_ra.un_ra_settings
#define	s_ra_serial	s_un.un_ra.un_ra_serial

#define	s_pr_type	s_un.un_pr.un_pr_type

#define	s_ed_reason	s_un.un_ed.un_ed_reason

#define	s_as_id		s_un.un_as.un_as_id

#define	s_ar_reference	s_un.un_ar.un_ar_link.un_ar_reference
#define	s_ar_oid	s_un.un_ar.un_ar_link.un_ar_oid
#define	s_ar_serial	s_un.un_ar.un_ar_link.un_ar_serial
#define	s_ar_id		s_un.un_ar.un_ar_id

#define	s_ai_reason	s_un.un_ai.un_ai_reason

#define	s_ad_reason	s_un.un_ad.un_ad_reason


#define	SMASK_ENCLOSE	0x2000
    u_char    s_enclose;
#define	ENCL_BEGIN	0x01	/* beginning of SSDU */
#define	ENCL_END	0x02	/* end of SSDU */
#define	ENCL_MASK	(ENCL_BEGIN | ENCL_END)
#define	ENCLOSE_MAX	65400	/* maximum size of enclosure per segment
				   less slop; slop varies based on SPDU, but
				   we'll always assume the worst case */
#define	SEGMENT_MAX	65528	/* for things other than S-DATA and
				   S-TYPED-DATA under version 2 we allow only
				   ONE enclosure */

#define	SMASK_UDATA_PGI	0x4000
    char   *s_udata;		/* user data PGI */
    int	    s_ulen;		/*   .. */

#define	SMASK_SPDU_EXPD	0x8000	/* SPDU arrived on the expedited connection */

    struct qbuf s_qbuf;		/* user info */
    int	    s_qlen;		/*   .. */
};
#define	NULLSPKT	((struct ssapkt *) 0)


int	freespkt ();
struct ssapkt *newspkt ();

void	text2spkt (), spkt2text ();

int	spkt2tsdu ();
struct ssapkt *tsdu2spkt ();

char   *spkt2str ();
struct ssapkt *str2spkt ();
