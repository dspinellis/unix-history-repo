/* ssaprovider.c - implement the session protocol */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssaprovider.c,v 7.5 91/02/22 09:46:03 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssaprovider.c,v 7.5 91/02/22 09:46:03 mrose Interim $
 *
 *
 * $Log:	ssaprovider.c,v $
 * Revision 7.5  91/02/22  09:46:03  mrose
 * Interim 6.8
 * 
 * Revision 7.4  91/01/10  04:11:29  mrose
 * foo
 * 
 * Revision 7.3  90/11/21  11:31:47  mrose
 * sun
 * 
 * Revision 7.2  90/08/08  14:14:02  mrose
 * update
 * 
 * Revision 7.1  89/11/30  23:51:19  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  22:25:45  mrose
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


/* LINTLIBRARY */

#include <stdio.h>
#include <signal.h>
#include "spkt.h"
#include "tailor.h"

/*    DATA */

static int  once_only = 0;
static struct ssapblk ssapque;
static struct ssapblk *SHead = &ssapque;


int	TDATAser (), TDISCser ();


/*    S-DATA.REQUEST */

int	SDataRequest (sd, data, cc, si)
int	sd;
char   *data;
int	cc;
struct SSAPindication *si;
{
    return SSendRequest (sd, data, cc, 1, 1, si);
}


int	SSendRequest (sd, data, cc, begin, end, si)
int	sd;
char   *data;
int	cc,
	begin,
	end;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    struct udvec uvs[2];
    register struct udvec *uv = uvs;
    register struct ssapblk *sb;

    missingP (data);
    if (cc <= 0)
	return ssaplose (si, SC_PARAMETER, NULLCP,
		    "illegal value for SSDU length (%d)", cc);
    missingP (si);

    smask = sigioblock ();

    ssapPsig (sb, sd);

    uv -> uv_base = data, uv -> uv_len = cc, uv++;
    uv -> uv_base = NULL;

    result = SDataRequestAux (sb, SPDU_DT, uvs, begin, end, si);

    (void) sigiomask (smask);

    return result;
}

/*    S-WRITE.REQUEST (pseudo; write user data vectors) */

int	SWriteRequest (sd, typed, uv, si)
int	sd;
int	typed;
struct udvec *uv;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    missingP (uv);
    missingP (si);

    smask = sigioblock ();

    ssapPsig (sb, sd);

    result = SDataRequestAux (sb, typed ? SPDU_TD : SPDU_DT, uv, 1, 1, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

#define	NSPUV	12	/* really should be MSG_MAXIOVLEN - 4 */


int	SDataRequestAux (sb, code, uv, begin, end, si)
register struct ssapblk *sb;
int	code;
register struct udvec *uv;
int	begin,
	end;
struct SSAPindication *si;
{
    int     cc,   	    
            j,
	    len,
    	    n,
            result;
    register char *bp,
    		  *ep;
    register struct ssapkt *s;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;
    struct udvec vvs[NSPUV];
    register struct udvec  *vv,
    			   *wv;
    struct udvec *xv;

    switch (code) {
	case SPDU_DT: 
	    if ((sb -> sb_requirements & SR_DAT_EXISTS)
		    && !(sb -> sb_owned & ST_DAT_TOKEN))
		return ssaplose (si, SC_OPERATION, NULLCP,
			"data token not owned by you");
	    break;

	case SPDU_TD: 
	    if (!(sb -> sb_requirements & SR_TYPEDATA))
		return ssaplose (si, SC_OPERATION, NULLCP,
			"typed data service unavailable");
	    break;
    }

    n = 0;
    for (vv = uv; vv -> uv_base; vv++)
	n += vv -> uv_len;
    if (n == 0)
	return ssaplose (si, SC_PARAMETER, NULLCP, "zero-length SSDU");
    
    ep = (bp = uv -> uv_base) + (cc = uv -> uv_len);
    while (uv -> uv_base) {
	len = sb -> sb_tsdu_us ? min (n, sb -> sb_tsdu_us - SSDU_MAGIC) : n;
	vv = vvs;
	vvs[0].uv_base = vvs[1].uv_base = NULL;
	vvs[1].uv_inline = 0;

	if (code == SPDU_DT) {
	    if ((s = newspkt (SPDU_GT)) == NULL)
		return ssaplose (si, SC_CONGEST, NULLCP, "out of memory");
	    s -> s_mask |= SMASK_SPDU_GT;

	    if (spkt2tsdu (s, &vv -> uv_base, &vv -> uv_len) == NOTOK) {
		(void) ssaplose (si, s -> s_errno, NULLCP, NULLCP);
		goto out1;
	    }
	    freespkt (s);
	    s = NULL;
	    vv++;
	}

	xv = vv++;

	wv = vvs + NSPUV - 1;
	for (; len > 0 && vv < wv; len -= j) {
	    j = min (cc, len);
	    vv -> uv_base = bp, vv -> uv_len = j, vv -> uv_inline = 1, vv++;
	    bp += j, cc -= j, n -= j;

	    if (bp >= ep) {
		if ((bp = (++uv) -> uv_base) == NULL)
		    break;
		ep = bp + (cc = uv -> uv_len);
	    }
	}
	if (!sb -> sb_tsdu_us && uv -> uv_base) {
	    (void) ssaplose (si, SC_PARAMETER, NULLCP,
			     "too many vector entries in SDU");
	    goto out2;
	}
	vv -> uv_base = NULL;

	vv = xv;
	if ((s = newspkt (code)) == NULL) {
	    (void) ssaplose (si, SC_CONGEST, NULLCP, "out of memory");
	    goto out2;
	}
	if (sb -> sb_tsdu_us) {
	    s -> s_mask |= SMASK_ENCLOSE;
	    if (begin) {
		s -> s_enclose |= ENCL_BEGIN;
		begin = 0;
	    }	    
	    if (end && uv -> uv_base == NULL)
		s -> s_enclose |= ENCL_END;
	}
	if (spkt2tsdu (s, &vv -> uv_base, &vv -> uv_len) == NOTOK) {
	    (void) ssaplose (si, s -> s_errno, NULLCP, NULLCP);
	    goto out3;
	}
	freespkt (s);
	s = NULL;

	if ((result = TWriteRequest (sb -> sb_fd, vvs, td)) == NOTOK)
	    (void) ts2sslose (si, "TWriteRequest", td);

	free (vvs[0].uv_base);
	if (code == SPDU_DT)
	    free (vvs[1].uv_base);

	if (result == NOTOK)
	    return NOTOK;
    }
    return OK;

out3: ;
    if (vvs[1].uv_base && !vvs[1].uv_inline)
	free (vvs[1].uv_base);
out2: ;
    if (vvs[0].uv_base)
	free (vvs[0].uv_base);
out1: ;
    freespkt (s);

    return NOTOK;
}

/*    S-READ.REQUEST (pseudo; synchronous read) */

int	SReadRequest (sd, sx, secs, si)
int	sd;
struct SSAPdata *sx;
int	secs;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    missingP (sx);
    missingP (si);

    smask = sigioblock ();

    if ((sb = findsblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return ssaplose (si, SC_PARAMETER, NULLCP, "invalid session descriptor");
    }
    if (!(sb -> sb_flags & SB_CONN)) {
	(void) sigiomask (smask); 
	return ssaplose (si, SC_PARAMETER, NULLCP, 
			    "session descriptor not connected"); 
    } 
    if (sb -> sb_flags & SB_FINN) { 
	(void) sigiomask (smask); 
	return ssaplose (si, SC_OPERATION, NULLCP, 
			    "session descriptor finishing"); 
    } 

    result = SReadRequestAux (sb, sx, secs, si, 0, NULLTX);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  SReadRequestAux (sb, sx, secs, si, async, tx)
register struct ssapblk *sb;
register struct SSAPdata *sx;
int	secs;
struct SSAPindication *si;
int	async;
struct TSAPdata *tx;
{
    int     eot;
    char    tokens;
    register struct ssapkt *s;

    bzero ((char *) sx, sizeof *sx);
    sx -> sx_qbuf.qb_forw = sx -> sx_qbuf.qb_back = &sx -> sx_qbuf;
    bzero ((char *) si, sizeof *si);

    for (; s = sb2spkt (sb, si, secs, tx); tx = NULLTX) {
	if (!(s -> s_mask & SMASK_SPDU_EXPD))
	    switch (sb -> sb_pr) {
		case SPDU_PR:
		    break;

		case SPDU_MAA:
		    if (s -> s_code == SPDU_MAA)
			sb -> sb_pr = SPDU_PR;
		    break;

		case SPDU_RS:
		    switch (s -> s_code) {
			case SPDU_AB:
#ifdef	notdef
			case SPDU_AI:	/* aka SPDU_AB */
#endif
			    if (s -> s_mask & SMASK_SPDU_AB)
				break;	/* else fall */
			case SPDU_AD:
			case SPDU_RS:
			    sb -> sb_pr = SPDU_PR;
			    break;

			default:
			    goto drop_it;
		    }
		    break;

		case SPDU_RA:
		    switch (s -> s_code) {
			case SPDU_AB:
			    break;

			case SPDU_AA:
#ifdef	notdef
			case SPDU_AIA:	/* aka SPDU_AA */
#endif
			    if (s -> s_mask & SMASK_SPDU_AA)
				break;	/* else fall */
			case SPDU_ADA:
			case SPDU_RA:
			    sb -> sb_pr = SPDU_PR;
			    break;

			default:
drop_it: ;
			    SLOG (ssap_log, LLOG_EXCEPTIONS, NULLCP,
				  ("discarding 0x%x SPDU", s -> s_code));
			    freespkt (s);
			    goto spin;
		    }
		    break;

		case SPDU_AB:
		    if (s -> s_code != SPDU_AB)
			goto drop_it;
		    sb -> sb_pr = SPDU_PR;
		    break;

		default:
		    break;
	    }

	if (sb -> sb_flags & (SB_RS | SB_AI))
	    switch (s -> s_code) {
		case SPDU_PR:
		    switch (s -> s_pr_type) {
			case PR_RS:
			case PR_RA:
			    break;
			default:
			    goto drop_it;
		    }
		    break;

		case SPDU_RS:
		    if (SDoCollideAux (sb -> sb_flags & SB_INIT ? 1 : 0,
				sb -> sb_rs, sb -> sb_rsn,
				(int) s -> s_rs_type, (long) s -> s_rs_serial)
			    != NOTOK)
			goto drop_it;
		    break;

		case SPDU_RA:
		    break;
		  
		case SPDU_AD:
		    if (SDoCollideAux (sb -> sb_flags & SB_INIT ? 1 : 0,
				sb -> sb_rs, sb -> sb_rsn, SYNC_DISC, 0L)
			    != NOTOK)
			goto drop_it;
		    break;

		case SPDU_AB:
#ifdef	notdef
		case SPDU_AI:	/* aka SPDU_AB */
#endif
		    if (s -> s_mask & SMASK_SPDU_AB)
			break;
		    if (SDoCollideAux (sb -> sb_flags & SB_INIT ? 1 : 0,
				sb -> sb_rs, sb -> sb_rsn, SYNC_INTR, 0L)
			    != NOTOK)
			goto drop_it;
		    break;
	    }
	
	if (sb -> sb_flags & (SB_ED | SB_ERACK))
	    switch (s -> s_code) {
		case SPDU_AB:
		    break;

		case SPDU_MAP:
		case SPDU_MIP:
		    if (sb -> sb_flags & SB_ED)
			break;
		    goto drop_it;

		case SPDU_PR:
		    if (s -> s_pr_type == PR_RS)
			break;
		    goto drop_it;

		case SPDU_GT:
		    if ((s -> s_mask & SMASK_SPDU_GT)
			    && (s -> s_mask & SMASK_GT_TOKEN)
			    && (s -> s_gt_token & ST_DAT_TOKEN))
			break;	/* else fall */

		default:
		    goto drop_it;
	    }

	if (sb -> sb_len > 0)
	    switch (s -> s_code) {
		case SPDU_PT:
		case SPDU_EX:
		    break;

		case SPDU_PR:
		    if (s -> s_pr_type != PR_RS)
			break;
		case SPDU_RS:
		case SPDU_ER:
		case SPDU_ED:
		case SPDU_AD:
#ifdef	notdef
		case SPDU_AI:	/* aka SPDU_AB */
#endif
		case SPDU_AB:
		    SLOG (ssap_log, LLOG_EXCEPTIONS, NULLCP,
			  ("flush partially assembled (T))SSDU"));
		    QBFREE (&sb -> sb_qbuf);
		    sb -> sb_len = 0;
		    break;

		case SPDU_GT:
		    if (s -> s_mask & SMASK_SPDU_GT)
			break;	/* else SPDU_DT */
		default:
		    if (sb -> sb_code == s -> s_code)
			break;
		    (void) spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
			    "session protocol mangled: expecting 0x%x, got 0x%x during segmentation",
			    sb -> sb_code, s -> s_code);
		    goto out;
	    }

/* allows AB SPDUs to have 512, not 9, octets (which is fine by me) */
	if (s -> s_ulen > CN_SIZE && sb -> sb_version < SB_VRSN2) {
	    (void) spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
			     "too much user data (%d) in SPDU 0x%x",
			     s -> s_ulen, s -> s_code);
	    goto out;
	}

	if ((s -> s_mask & SMASK_ENCLOSE)
	        && (s -> s_code != SPDU_DT || (s -> s_mask & SMASK_SPDU_GT))
	        && s -> s_code != SPDU_TD) {
	    if (sb -> sb_version < SB_VRSN2) {
		(void) spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
				 "unexpected segmentation for SPDU 0x%x",
				 s -> s_code);
		goto out;
	    }

/* XXX: in practice, I don't think this is unreasonable.  It is
	however not too restrictive */

	    if (s -> s_enclose != ENCL_MASK) {
		(void) spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
				 "non-trivial segmentation (0x%x) for SPDU 0x%x",
				 s -> s_enclose, s -> s_code);
		goto out;
	    }
	}

	switch (s -> s_code) {
	    case SPDU_PT: 
		if (sb -> sb_flags & SB_GTC) {
		    freespkt (s);
		    goto spin;
		}
		tokens = 0;
		if (s -> s_mask & SMASK_PT_TOKEN) {
#define	dotoken(requires,shift,bit,type) \
{ \
		    if ((sb -> sb_requirements & requires) \
			    && (s -> s_pt_token & bit)) \
			tokens |= bit; \
}
			dotokens ();
#undef	dotoken
		}
		si -> si_type = SI_TOKEN;
		{
		    register struct SSAPtoken *st = &si -> si_token;

		    st -> st_type = ST_PLEASE;
		    st -> st_tokens = tokens;
		    st -> st_owned = sb -> sb_owned;
		    copySPKTdata (s, st);
		}
		freespkt (s);
		return DONE;

	    case SPDU_GT: 
		if (s -> s_mask & SMASK_SPDU_GT) {
		    if (sb -> sb_flags & SB_GTC) {
			freespkt (s);
			goto spin;
		    }
		    tokens = 0;
		    if (s -> s_mask & SMASK_GT_TOKEN) {
#define	dotoken(requires,shift,bit,type) \
{ \
			if ((sb -> sb_requirements & requires) \
			    && (s -> s_gt_token & bit)) \
				sb -> sb_owned |= bit, tokens |= bit; \
}
			dotokens ();
#undef	dotoken
		    }
		    freespkt (s);
		    if (tokens & ST_DAT_TOKEN)
			sb -> sb_flags &= ~(SB_ED | SB_ERACK);
		    si -> si_type = SI_TOKEN;
		    {
			register struct SSAPtoken  *st = &si -> si_token;

			st -> st_type = ST_GIVE;
			st -> st_tokens = tokens;
			st -> st_owned = sb -> sb_owned;
		    }
		    return DONE;
		}		/* else fall for case SPDU_DT: */
#ifdef	notdef
	    case SPDU_DT:
#endif
	    case SPDU_TD:
		sb -> sb_code = s -> s_code;
		if (sb -> sb_tsdu_them) {
		    if (!(s -> s_mask & SMASK_ENCLOSE)) {
			(void) spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
				"no segmentation information");
			break;
		    }
		    if ((s -> s_enclose & ENCL_BEGIN)
			    ? sb -> sb_len > 0 : sb -> sb_len == 0) {
			(void) spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
				"segmentation mismatch");
			break;
		    }
		    eot = s -> s_enclose & ENCL_END;
		}
		else
		    eot = 1;
		if (s -> s_qbuf.qb_forw != &s -> s_qbuf) {
		    sb -> sb_qbuf.qb_back -> qb_forw = s -> s_qbuf.qb_forw;
		    s -> s_qbuf.qb_forw -> qb_back = sb -> sb_qbuf.qb_back;
		    s -> s_qbuf.qb_back -> qb_forw = &sb -> sb_qbuf;
		    sb -> sb_qbuf.qb_back = s -> s_qbuf.qb_back;
		    sb -> sb_len += s -> s_qlen;
		    s -> s_qbuf.qb_forw =
			    s -> s_qbuf.qb_back = &s -> s_qbuf;
		    s -> s_qlen = 0;
		}
		if (!eot && (s -> s_code == SPDU_DT) && sb -> sb_spdu) {
		    freespkt (sb -> sb_spdu);
		    sb -> sb_spdu = NULL;
		}
		freespkt (s);
		if (!eot)
		    goto spin;
		sx -> sx_type = sb -> sb_code == SPDU_DT ? SX_NORMAL
				    : SX_TYPED;
		if (sb -> sb_qbuf.qb_forw != &sb -> sb_qbuf) {
		    sx -> sx_qbuf = sb -> sb_qbuf;/* struct copy */
		    sx -> sx_qbuf.qb_forw -> qb_back =
			    sx -> sx_qbuf.qb_back -> qb_forw = &sx -> sx_qbuf;
		    sx -> sx_cc = sb -> sb_len;
		    sb -> sb_qbuf.qb_forw =
			    sb -> sb_qbuf.qb_back = &sb -> sb_qbuf;
		    sb -> sb_len = 0;
		}
		return OK;

	    case SPDU_EX: 
		if (sb -> sb_pr != SPDU_PR) {
		    SLOG (ssap_log, LLOG_EXCEPTIONS, NULLCP,
			  ("buffering XSDU during preparation"));
		    if (sb -> sb_xspdu) {
			(void) spktlose (sb -> sb_fd, si, SC_CONGEST, NULLCP,
				"unable to buffer second XSDU");
			break;
		    }
		    sb -> sb_xspdu = s;
		    goto spin;
		}
		sx -> sx_type = SX_EXPEDITED;
		if (s -> s_qbuf.qb_forw != &s -> s_qbuf) {
		    sx -> sx_qbuf = s -> s_qbuf;/* struct copy */
		    sx -> sx_qbuf.qb_forw -> qb_back =
			    sx -> sx_qbuf.qb_back -> qb_forw = &sx -> sx_qbuf;
		    sx -> sx_cc = s -> s_qlen;
		    s -> s_qbuf.qb_forw =
			    s -> s_qbuf.qb_back = &s -> s_qbuf;
		    s -> s_qlen = 0;
		}
		freespkt (s);
		return OK;

	    case SPDU_CD:
	    case SPDU_CDA:
		if (s -> s_code == SPDU_CD) {
		    sb -> sb_flags |= SB_CDA;
		    sx -> sx_type = SX_CAPDIND;
		}
		else {
		    sb -> sb_flags &= ~SB_CD;
		    sx -> sx_type = SX_CAPDCNF;
		}
		if (s -> s_udata) {
		    register struct qbuf *qb;

		    qb = (struct qbuf *)
			    malloc (sizeof *qb + (unsigned) s -> s_ulen);
		    if (qb == NULL) {
			(void) spktlose (sb -> sb_fd, si, SC_CONGEST, NULLCP,
				    "out of memory");
			break;
		    }
		    bcopy (s -> s_udata, qb -> qb_data = qb -> qb_base,
			    qb -> qb_len = s -> s_ulen);
		    insque (qb, &sx -> sx_qbuf);
		    sx -> sx_cc = s -> s_ulen;
		}
		freespkt (s);
		return OK;

	    case SPDU_GTC:
		if (sb -> sb_flags & SB_Vact) {
		    freespkt (s);
		    goto spin;
		}
#define	dotoken(requires,shift,bit,type) \
{ \
		if (sb -> sb_requirements & requires) \
		    sb -> sb_owned |= bit; \
}
		dotokens ();
#undef	dotoken
		freespkt (s);
		if ((s = newspkt (SPDU_GTA)) == NULL) {
		    (void) spktlose (sb -> sb_fd, si, SC_CONGEST, NULLCP,
			    "out of memory");
		    break;
		}
		if (spkt2sd (s, sb -> sb_fd, 0, si) == NOTOK)
		    break;
		freespkt (s);
		si -> si_type = SI_TOKEN;
		{
		    register struct SSAPtoken  *st = &si -> si_token;

		    st -> st_type = ST_CONTROL;
		    st -> st_tokens = st -> st_owned = sb -> sb_owned;
		}
		return DONE;

	    case SPDU_GTA:
		if (!(sb -> sb_flags & SB_GTC)) {
		    freespkt (s);
		    goto spin;
		}
		sb -> sb_flags &= ~SB_GTC;
spin: ;
		if (!async || sb -> sb_spdu)
		    continue;
		si -> si_type = SI_DATA;
		{
		    register struct SSAPdata *sk = &si -> si_data;

		    bzero ((char *) sk, sizeof *sk);
		    sk -> sx_qbuf.qb_forw = sk -> sx_qbuf.qb_back =
			&sk -> sx_qbuf;
		}
		return DONE;

	    case SPDU_MAP:
#ifdef	notdef
	    case SPDU_AE:	/* aka SPDU_MAP */
#endif
		if (sb -> sb_V_M != s -> s_map_serial) {
		    freespkt (s);
		    goto spin;
		}
		if (!(s -> s_mask & SMASK_MAP_SYNC)
			|| !(s -> s_map_sync & MAP_SYNC_NOEND))
		    goto spdu_ae;
		if (!(sb -> sb_flags & SB_Vsc))
		    sb -> sb_V_A = sb -> sb_V_M;
		sb -> sb_V_M++;
		if (sb -> sb_flags & (SB_ED | SB_ERACK)) {
		    freespkt (s);
		    goto spin;
		}
		if (sb -> sb_requirements & SR_ACTIVITY)
		    sb -> sb_flags |= SB_Vnextact;
		sb -> sb_flags |= SB_MAA;
		si -> si_type = SI_SYNC;
		{
		    register struct SSAPsync *sn = &si -> si_sync;

		    sn -> sn_type = SN_MAJORIND;
		    sn -> sn_ssn = s -> s_map_serial;
		    copySPKTdata (s, sn);
		}
		freespkt (s);
		return DONE;

	    case SPDU_MAA:
#ifdef	notdef
	    case SPDU_AEA:	/* aka SPDU_MAA */
#endif
		if (sb -> sb_V_M != s -> s_maa_serial + 1) {
		    freespkt (s);
		    goto spin;
		}
		sb -> sb_V_A = sb -> sb_V_R = sb -> sb_V_M;
		if (sb -> sb_requirements & SR_ACTIVITY)
		    if (sb -> sb_flags & SB_Vnextact)
			sb -> sb_flags |= SB_Vact;
		    else
		        sb -> sb_flags &= ~SB_Vact;
		sb -> sb_flags &= ~SB_MAP;
		if (sb -> sb_flags & SB_AE) {
		    sb -> sb_flags &= ~SB_AE;
		    si -> si_type = SI_ACTIVITY;
		    {
			register struct SSAPactivity *sv = &si -> si_activity;

			sv -> sv_type = SV_ENDCNF;
			sv -> sv_ssn = s -> s_maa_serial;
			copySPKTdata (s, sv);
		    }
		}
		else {
		    si -> si_type = SI_SYNC;
		    {
			register struct SSAPsync *sn = &si -> si_sync;

			sn -> sn_type = SN_MAJORCNF;
			sn -> sn_ssn = s -> s_maa_serial;
			copySPKTdata (s, sn);
		    }
		}
		freespkt (s);
		return DONE;

	    case SPDU_MIP:
		if (!(sb -> sb_flags & SB_Vsc)) {
		    sb -> sb_V_A = sb -> sb_V_M;
		    sb -> sb_flags |= SB_Vsc;
		}
		sb -> sb_V_M++;
		if (sb -> sb_flags & (SB_ED | SB_ERACK)) {
		    freespkt (s);
		    goto spin;
		}
		si -> si_type = SI_SYNC;
		{
		    register struct SSAPsync *sn = &si -> si_sync;

		    sn -> sn_type = SN_MINORIND;
		    sn -> sn_options = (s -> s_mask & SMASK_MIP_SYNC)
		    	    && (s -> s_mip_sync & MIP_SYNC_NOEXPL)
			    ? SYNC_NOCONFIRM : SYNC_CONFIRM;
		    sn -> sn_ssn = s -> s_mip_serial;
		    copySPKTdata (s, sn);
		}
		freespkt (s);
	        return DONE;

	    case SPDU_MIA:
		if ((sb -> sb_flags & SB_Vsc)
			|| sb -> sb_V_A > s -> s_mia_serial
			|| s -> s_mia_serial >= sb -> sb_V_M) {
		    freespkt (s);
		    goto spin;
		}
		sb -> sb_V_A = s -> s_mia_serial;
		si -> si_type = SI_SYNC;
		{
		    register struct SSAPsync *sn = &si -> si_sync;

		    sn -> sn_type = SN_MINORCNF;
		    sn -> sn_ssn = s -> s_mia_serial;
		    copySPKTdata (s, sn);
		}
		freespkt (s);
	        return DONE;

	    case SPDU_RS:
		if (s -> s_rs_type == SYNC_RESTART
			&& sb -> sb_V_R > s -> s_rs_serial) {
		    freespkt (s);
		    goto spin;
		}
		sb -> sb_flags &= ~SB_RS, sb -> sb_flags |= SB_RA;
		sb -> sb_rs = s -> s_rs_type;
		sb -> sb_rsn = s -> s_rs_serial;
		if (s -> s_mask & SMASK_RS_SET)
		    sb -> sb_rsettings = s -> s_rs_settings;
		else {
		    sb -> sb_rsettings = 0;
#define	dotoken(requires,shift,bit,type) \
{ \
		    if (sb -> sb_requirements & requires) \
			if ((sb -> sb_owned & bit) \
				&& (sb -> sb_flags & SB_INIT)) \
			    sb -> sb_rsettings = ST_INIT_VALUE << shift; \
			else \
			    sb -> sb_rsettings = ST_RESP_VALUE << shift; \
}
		    dotokens ();
#undef	dotoken
		}
		si -> si_type = SI_SYNC;
		{
		    register struct SSAPsync *sn = &si -> si_sync;

		    sn -> sn_type = SN_RESETIND;
		    sn -> sn_options = sb -> sb_rs;
		    sn -> sn_ssn = sb -> sb_rsn;
		    sn -> sn_settings = sb -> sb_rsettings;
		    copySPKTdata (s, sn);
		}
		freespkt (s);
		return DONE;

	    case SPDU_RA:
		sb -> sb_flags &= ~SB_RS;
		sb -> sb_V_A = sb -> sb_V_M = s -> s_ra_serial;
		if (sb -> sb_rs != SYNC_RESTART)
		    sb -> sb_V_R = 0;
		if (s -> s_mask & SMASK_RA_SET)
		    sb -> sb_rsettings = s -> s_ra_settings;
#define	dotoken(requires,shift,bit,type) \
{ \
		if (sb -> sb_requirements & requires) \
		    switch (sb -> sb_rsettings & (ST_MASK << shift)) { \
			dotoken1 (requires,shift,bit,type); \
 \
			dotoken2 (requires,shift,bit,type); \
		    } \
}
#define	dotoken1(requires,shift,bit,type) \
			case ST_CALL_VALUE << shift: \
			    switch (s -> s_ra_settings & (ST_MASK << shift)) { \
				case ST_INIT_VALUE: \
				    if (sb -> sb_flags & SB_INIT) \
					sb -> sb_owned |= bit; \
				    else \
					sb -> sb_owned &= ~bit; \
				    break; \
 \
				case ST_RESP_VALUE: \
				    if (!(sb -> sb_flags & SB_INIT)) \
					sb -> sb_owned |= bit; \
				    else \
					sb -> sb_owned &= ~bit; \
				    break; \
			    } \
			    break;
#define	dotoken2(requires,shift,bit,type) \
			case ST_INIT_VALUE << shift: \
			    if (sb -> sb_flags & SB_INIT) \
				sb -> sb_owned |= bit; \
			    else \
				sb -> sb_owned &= ~bit; \
			    break; \
 \
			case ST_RESP_VALUE << shift: \
			    if (!(sb -> sb_flags & SB_INIT)) \
				sb -> sb_owned |= bit; \
			    else \
				sb -> sb_owned &= ~bit; \
			    break;
		dotokens ();
#undef	dotoken
#undef	dotoken1
#undef	dotoken2
		si -> si_type = SI_SYNC;
		{
		    register struct SSAPsync *sn = &si -> si_sync;

		    sn -> sn_type = SN_RESETCNF;
		    sn -> sn_ssn = sb -> sb_V_M;
		    sn -> sn_settings = sb -> sb_rsettings;
		    copySPKTdata (s, sn);
		}
		freespkt (s);
		return DONE;

	    case SPDU_PR: 
		switch (s -> s_pr_type) {
		    case PR_MAA: 
			sb -> sb_pr = SPDU_MAA;
			break;
		    case PR_RS: 
			sb -> sb_flags &= ~(SB_ED | SB_ERACK);
			sb -> sb_pr = SPDU_RS;
			break;
		    case PR_RA: 
			sb -> sb_pr = SPDU_RA;
			break;
		    case PR_AB: 
			sb -> sb_pr = SPDU_AB;
			break;
		}
		freespkt (s);
		goto spin;

	    case SPDU_ER:	/* this implementation never generates these */
		sb -> sb_flags |= SB_ERACK;
		si -> si_type = SI_REPORT;
		{
		    register struct SSAPreport *sp = &si -> si_report;

		    sp -> sp_peer = 0;
		    sp -> sp_reason = SP_PROTOCOL;
		}
		freespkt (s);
		return DONE;

	    case SPDU_ED:
		if (sb -> sb_owned & ST_DAT_TOKEN)
		    sb -> sb_flags |= SB_EDACK;
		si -> si_type = SI_REPORT;
		{
		    register struct SSAPreport *sp = &si -> si_report;

		    sp -> sp_peer = 1;
		    sp -> sp_reason = s -> s_ed_reason;
		    copySPKTdata (s, sp);
		}
		freespkt (s);
		return DONE;

	    case SPDU_AS:
		if (sb -> sb_flags & SB_Vact) {
		    freespkt (s);
		    goto spin;
		}
		sb -> sb_V_A = sb -> sb_V_M = sb -> sb_V_R = 1;
		sb -> sb_flags |= SB_Vact;
		si -> si_type = SI_ACTIVITY;
		{
		    register struct SSAPactivity *sv = &si -> si_activity;

		    sv -> sv_type = SV_START;
		    sv -> sv_id = s -> s_as_id;	/* struct copy */
		    copySPKTdata (s, sv);
		}
		freespkt (s);
		return DONE;

	    case SPDU_AR:
		if (sb -> sb_flags & SB_Vact) {
		    freespkt (s);
		    goto spin;
		}
		sb -> sb_V_A = sb -> sb_V_M = s -> s_ar_serial + 1;
		sb -> sb_V_R = 1;
		sb -> sb_flags |= SB_Vact;
		si -> si_type = SI_ACTIVITY;
		{
		    register struct SSAPactivity *sv = &si -> si_activity;

		    sv -> sv_type = SV_RESUME;
		    sv -> sv_id = s -> s_ar_id;	/* struct copy */
		    sv -> sv_oid = s -> s_ar_oid;	/* struct copy */
		    if (s -> s_mask & SMASK_AR_REF)	/* struct copy */
			sv -> sv_connect = s -> s_ar_reference;
		    sv -> sv_ssn = s -> s_ar_serial;
		    copySPKTdata (s, sv);
		}
		freespkt (s);
		return DONE;

	    case SPDU_AD:
spdu_ai: ;
		if (!(sb -> sb_flags & SB_Vact)) {
		    freespkt (s);
		    goto spin;
		}
		sb -> sb_flags &= ~(SB_RS | SB_RA), sb -> sb_flags |= SB_AIA;
		sb -> sb_rs = SYNC_INTR;
		si -> si_type = SI_ACTIVITY;
		{
		    register struct SSAPactivity *sv = &si -> si_activity;

		    sv -> sv_type = s -> s_code == SPDU_AI ? SV_INTRIND
			    : SV_DISCIND;
		    sv -> sv_reason = s -> s_ai_reason;
		}
		freespkt (s);
		return DONE;

	    case SPDU_AA:
#ifdef	notdef
	    case SPDU_AIA:	/* aka SPDU_AA */
#endif
		if (s -> s_mask & SMASK_SPDU_AA) {
		    freespkt (s);
		    goto spin;
		}		/* else fall */
	    case SPDU_ADA:
		if (!(sb -> sb_flags & SB_Vact)) {
		    freespkt (s);
		    goto spin;
		}
		sb -> sb_flags &= ~(SB_AI | SB_Vact);
#define	dotoken(requires,shift,bit,type) \
{ \
		if (sb -> sb_requirements & requires) \
		    sb -> sb_owned |= bit; \
}
		dotokens ();
#undef	dotoken
		si -> si_type = SI_ACTIVITY;
		{
		    register struct SSAPactivity *sv = &si -> si_activity;

		    sv -> sv_type = s -> s_code == SPDU_AIA ? SV_INTRCNF
					: SV_DISCCNF;
		}
		freespkt (s);
		return DONE;		

spdu_ae: ;
		if (!(sb -> sb_flags & SB_Vsc))
		    sb -> sb_V_A = sb -> sb_V_M;
		sb -> sb_V_M++;
		sb -> sb_flags &= ~SB_Vnextact;
		sb -> sb_flags |= SB_MAA | SB_AE;
		si -> si_type = SI_ACTIVITY;
		{
		    register struct SSAPactivity *sv = &si -> si_activity;

		    sv -> sv_type = SV_ENDIND;
		    sv -> sv_ssn = s -> s_map_serial;
		    copySPKTdata (s, sv);
		}
		freespkt (s);
		return DONE;

	    case SPDU_FN: 
		sb -> sb_flags |= SB_FINN;
		si -> si_type = SI_FINISH;
		{
		    register struct SSAPfinish *sf = &si -> si_finish;

		    copySPKTdata (s, sf);
		}
		freespkt (s);
		return DONE;

	    case SPDU_AB: 
		if (!(s -> s_mask & SMASK_SPDU_AB))
		    goto spdu_ai;
		sb -> sb_flags &= ~(SB_ED | SB_EDACK | SB_ERACK);
		si -> si_type = SI_ABORT;
		{
		    register struct SSAPabort  *sa = &si -> si_abort;

		    if (!(sa -> sa_peer = (s -> s_ab_disconnect & AB_DISC_USER)
				? 1 : 0))
			sa -> sa_reason = SC_ABORT;
		    sa -> sa_info = s -> s_udata, sa -> sa_cc = s -> s_ulen;
		    sa -> sa_realinfo = s -> s_udata, s -> s_udata = NULL;
		}
#ifdef	notdef		/* only if transport connection is to be re-used */
		freespkt (s);
		if (s = newspkt (SPDU_AA)) {
		    s -> s_mask |= SMASK_SPDU_AA;
		    (void) spkt2sd (s, sb -> sb_fd, sb -> sb_flags & SB_EXPD
				? 1 : 0, (struct SSAPindication *) 0);
		}
#endif
		break;

	    default: 
		(void) spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
			"session protocol mangled: not expecting 0x%x",
			s -> s_code);
		break;
	}
	break;
    }

    if (si -> si_abort.sa_reason == SC_TIMER)
	return NOTOK;

out: ;
    freespkt (s);
    freesblk (sb);

    return NOTOK;
}

/*  */

/* a decision tree (ugh!) */

int	SDoCollideAux (init, localop, localssn, remoteop, remotessn)
int     init,
	localop,
        remoteop;
long    localssn,
        remotessn;
{
    SLOG (ssap_log, LLOG_EXCEPTIONS, NULLCP,
	  ("collide: local<%d,%ld,%s> remote<%d,%ld,%s>",
	   localop, localssn, init ? "initiator" : "responder",
	   remoteop, remotessn, init ? "responder" : "initiator"));

    if (localop == SYNC_DISC)
	return OK;

    if (remoteop == SYNC_DISC)
	return NOTOK;

    if (localop == SYNC_INTR)
	return OK;

    if (remoteop == SYNC_DISC)
	return NOTOK;

    if (localop == SYNC_ABANDON) {
	if (remoteop != SYNC_ABANDON)
	    return OK;

	return (init ? OK : NOTOK);
    }
    else
	if (remoteop == SYNC_ABANDON)
	    return NOTOK;

    if (localop == SYNC_SET) {
	if (remoteop != SYNC_SET)
	    return OK;

	return (init ? OK : NOTOK);
    }
    else
	if (remoteop == SYNC_SET)
	    return NOTOK;

    if (localssn == remotessn)
	return (init ? OK : NOTOK);

    return (localssn < remotessn ? OK : NOTOK);
}

/*    define vectors for INDICATION events */

int	SSetIndications (sd, data, tokens, sync, activity, report, finish,
		abort, si)
int	sd;
IFP	data,	
	tokens,
	sync,
	activity,
	report,
	finish,
	abort;
struct SSAPindication *si;
{
    SBV     smask;
    register struct ssapblk *sb;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    if (data || tokens || sync || activity || report || finish || abort) {
	missingP (data);
	missingP (tokens);
	missingP (sync);
	missingP (activity);
	missingP (report);
	missingP (finish);
	missingP (abort);
    }
    _iosignals_set = 1;

    smask = sigioblock ();

    ssapPsig (sb, sd);

    if (sb -> sb_DataIndication = data)
	sb -> sb_flags |= SB_ASYN;
    else
	sb -> sb_flags &= ~SB_ASYN;
    sb -> sb_TokenIndication = tokens;
    sb -> sb_SyncIndication = sync;
    sb -> sb_ActivityIndication = activity;
    sb -> sb_ReportIndication = report;
    sb -> sb_ReleaseIndication = finish;
    sb -> sb_AbortIndication = abort;

    if (TSetIndications (sb -> sb_fd, TDATAser, TDISCser, td) == NOTOK) {
	sb -> sb_flags &= ~SB_ASYN;
	if (td -> td_reason == DR_WAITING)
	    return ssaplose (si, SC_WAITING, NULLCP, NULLCP);
	else
	    return ts2sslose (si, "TSetIndications", td);
    }

    (void) sigiomask (smask);

    return OK;
}

/*    TSAP interface */

int	spkt2sd (s, sd, expedited, si)
register struct ssapkt *s;
int     sd,
        expedited;
register struct SSAPindication *si;
{
    int     i,
            len,
            result;
    char   *base,
           *dp;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    if (expedited)
	s -> s_mask |= SMASK_SPDU_EXPD;
    if (spkt2tsdu (s, &base, &len) == NOTOK) {
	(void) ssaplose (si, s -> s_errno, NULLCP, NULLCP);
	return NOTOK;
    }
    if (s -> s_code == SPDU_EX) {/* only SX_EXSIZE octets, so no big deal... */
	if (s -> s_udata) {
	    if ((dp = realloc (base, (unsigned) (i = len + s -> s_ulen)))
		    == NULL) {
		free (base);
		(void) ssaplose (si, SC_CONGEST, NULLCP, NULLCP);
		return NOTOK;
	    }
	    bcopy (s -> s_udata, (base = dp) + len, s -> s_ulen);
	    len = i;
	}
    }

    if (len > TX_SIZE)
	expedited = 0;
    if ((result = expedited ? TExpdRequest (sd, base, len, td)
		: TDataRequest (sd, base, len, td)) == NOTOK)
	(void) ts2sslose (si, expedited ? "TExpdRequest" : "TDataRequest", td);

    if (base)
	free (base);

    return result;
}

/*  */

struct ssapkt   *sb2spkt (sb, si, secs, ty)
register struct ssapblk *sb;
register struct SSAPindication *si;
int     secs;
register struct TSAPdata   *ty;
{
    int     cc;
    register struct ssapkt   *s,
			     *p;
    struct TSAPdata txs;
    register struct TSAPdata   *tx = &txs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    if (sb -> sb_pr == SPDU_PR && sb -> sb_xspdu) {
	SLOG (ssap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("returning XSDU buffered during preparation"));
	s = sb -> sb_xspdu;
	sb -> sb_xspdu = NULL;

	return s;
    }

    if (sb -> sb_spdu) {	/* get previous category 0 SPDU */
	SLOG (ssap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("returning category 0 SPDU previously buffered"));
	s = sb -> sb_spdu;
	sb -> sb_spdu = NULL;

	return s;
    }

    if (ty) {
	*tx = *ty;		/* struct copy */
	tx -> tx_qbuf.qb_forw -> qb_back =
		tx -> tx_qbuf.qb_back -> qb_forw = &tx -> tx_qbuf;
	bzero ((char *) ty, sizeof *ty);
	ty -> tx_qbuf.qb_forw = ty -> tx_qbuf.qb_back = &ty -> tx_qbuf;
    }
    else
	if (TReadRequest (sb -> sb_fd, tx, secs, td) == NOTOK) {
	    if (td -> td_reason != DR_TIMER)
		(void) ts2sslose (si, "TReadRequest", td);
	    else
		(void) ssaplose (si, SC_TIMER, NULLCP, NULLCP);

	    return NULL;
	}

    DLOG (ssap_log, LLOG_DEBUG, ("read TSDU, size %d", tx -> tx_cc));

    if ((s = tsdu2spkt (&tx -> tx_qbuf, tx -> tx_cc, (cc = 1, &cc))) == NULL
	    || s -> s_errno != SC_ACCEPT) {
	(void) ssaplose (si, s ? s -> s_errno : SC_CONGEST, NULLCP, NULLCP);
bad1: ;
	freespkt (s);
	TXFREE (tx);
	return NULL;
    }

    if (tx -> tx_expedited)
	s -> s_mask |= SMASK_SPDU_EXPD;
    tx -> tx_cc -= cc;

    switch (s -> s_code) {
	case SPDU_GT: 		/* category 0 SPDUs */
	case SPDU_PT: 
	    if (tx -> tx_cc <= 0)
		goto simple;
	    break;

	case SPDU_EX: 		/* category 1 SPDUs with user data */
	case SPDU_TD: 
	    if (tx -> tx_qbuf.qb_forw != &tx -> tx_qbuf) {
		s -> s_qbuf = tx -> tx_qbuf;/* struct copy */
		s -> s_qbuf.qb_forw -> qb_back =
			s -> s_qbuf.qb_back -> qb_forw = &s -> s_qbuf;
		s -> s_qlen = tx -> tx_cc;
	    }
	    return s;

	case SPDU_CN: 		/* category 1 SPDUs */
	case SPDU_AC: 
	case SPDU_RF: 
	case SPDU_FN: 
	case SPDU_DN: 
	case SPDU_NF: 
	case SPDU_AB: 
	case SPDU_AA: 
	case SPDU_GTC: 
	case SPDU_GTA: 
	case SPDU_PR: 
	    if (tx -> tx_cc <= 0) {
	simple: ;
		TXFREE (tx);
		return s;
	    }
	    (void) ssaplose (si, SC_PROTOCOL, NULLCP,
		    "session protocol mangled: not expecting user information after 0x%x (%d bytes)",
		    s -> s_code, tx -> tx_cc);
	    goto bad1;

	default:
	    (void) ssaplose (si, SC_PROTOCOL, NULLCP,
		    "session protocol mangled: not expecting 0x%x",
		    s -> s_code);
	    goto bad1;
    }

    sb -> sb_spdu = p = s;		/* save category 0 SPDU */

    if ((s = tsdu2spkt (&tx -> tx_qbuf, tx -> tx_cc, (cc = 0, &cc))) == NULL
	    || s -> s_errno != SC_ACCEPT) {
	(void) ssaplose (si, s ? s -> s_errno : SC_CONGEST, NULLCP, NULLCP);
bad2: 	;
	freespkt (s);
	freespkt (p);
	sb -> sb_spdu = NULL;
	TXFREE (tx);
	return NULL;
    }

    if (tx -> tx_expedited)
	s -> s_mask |= SMASK_SPDU_EXPD;
    tx -> tx_cc -= cc;

    switch ((p -> s_code) << 8 | s -> s_code) {
	case (SPDU_GT << 8) | SPDU_DT:	/* category 2 SPDUs with user data */
	    if (tx -> tx_qbuf.qb_forw != &tx -> tx_qbuf) {
		s -> s_qbuf = tx -> tx_qbuf;/* struct copy */
		s -> s_qbuf.qb_forw -> qb_back =
			s -> s_qbuf.qb_back -> qb_forw = &s -> s_qbuf;
		s -> s_qlen = tx -> tx_cc;
	    }
	    break;

	case (SPDU_GT << 8) | SPDU_MIP: /* category 2 SPDUs */
	case (SPDU_PT << 8) | SPDU_MIA: 
	case (SPDU_GT << 8) | SPDU_MAP: 
	case (SPDU_PT << 8) | SPDU_MAA: 
	case (SPDU_GT << 8) | SPDU_RS: 
	case (SPDU_PT << 8) | SPDU_RA: 
	case (SPDU_GT << 8) | SPDU_AS: 
	case (SPDU_GT << 8) | SPDU_AR: 
	case (SPDU_GT << 8) | SPDU_AD: 
	case (SPDU_PT << 8) | SPDU_ADA: 
	case (SPDU_GT << 8) | SPDU_AI:
	case (SPDU_PT << 8) | SPDU_AIA: 
#ifdef	notdef
	case (SPDU_GT << 8) | SPDU_AE: 	/* aka SPDU_MAP */
	case (SPDU_PT << 8) | SPDU_AEA: /* aka SPDU_MAA */
#endif
	case (SPDU_GT << 8) | SPDU_CD: 
	case (SPDU_PT << 8) | SPDU_CDA: 
	case (SPDU_PT << 8) | SPDU_ER: 
	case (SPDU_PT << 8) | SPDU_ED: 
	    if (tx -> tx_cc <= 0) {
		TXFREE (tx);
		break;
	    }
	    (void) ssaplose (si, SC_PROTOCOL, NULLCP,
		    "session protocol mangled: not expecting user information after 0x%x (%d bytes)",
		    s -> s_code, tx -> tx_cc);
		goto bad2;

	default: 
	    (void) ssaplose (si, SC_PROTOCOL, NULLCP,
		    "session protocol mangled: not expecting 0x%x to be concatenated after 0x%x",
		    s -> s_code, p -> s_code);
	    goto bad2;
    }

    switch (s -> s_code) {
	default:
	    if (p -> s_code == SPDU_GT) {
		if ((p -> s_mask & SMASK_GT_TOKEN) && p -> s_gt_token)
		    break;
	    }
	    else {
		if (((p -> s_mask & SMASK_PT_TOKEN) && p -> s_pt_token)
			|| p -> s_ulen)
		    break;
	    }			/* fall... */

	case SPDU_RS: 
	case SPDU_AD: 
	case SPDU_AI: 
	case SPDU_CD: 
	    freespkt (p);
	    sb -> sb_spdu = NULL;
	    break;
    }

    return s;
}

/*  */

static int  TDATAser (sd, tx)
int     sd;
register struct TSAPdata   *tx;
{
    IFP	    abort;
    register struct ssapblk *sb;
    struct SSAPdata sxs;
    register struct SSAPdata   *sx = &sxs;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

    if ((sb = findsblk (sd)) == NULL)
	return;

    abort = sb -> sb_AbortIndication;

    for (;; tx = NULLTX) {
	switch (SReadRequestAux (sb, sx, OK, si, 1, tx)) {
	    case NOTOK: 
		(*abort) (sd, sa);
		return;

	    case OK: 
		(*sb -> sb_DataIndication) (sd, sx);
		break;

	    case DONE: 
		switch (si -> si_type) {
		    case SI_TOKEN: 
			(*sb -> sb_TokenIndication) (sd, &si -> si_token);
			break;

		    case SI_SYNC: 
			(*sb -> sb_SyncIndication) (sd, &si -> si_sync);
			break;

		    case SI_ACTIVITY: 
			(*sb -> sb_ActivityIndication) (sd, &si -> si_activity);
			break;

		    case SI_REPORT: 
			(*sb -> sb_ReportIndication) (sd, &si -> si_report);
			break;

		    case SI_FINISH: 
			(*sb -> sb_ReleaseIndication) (sd, &si -> si_finish);
			break;

		    case SI_DATA: /* partially assembled (T)SSDU */
			break;
		}
		break;
	}

	if (sb -> sb_spdu == NULL)
	    break;
    }
}

/*  */

static int TDISCser (sd, td)
int	sd;
register struct TSAPdisconnect *td;
{
    IFP	    abort;
    register struct ssapblk *sb;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;

    if ((sb = findsblk (sd)) == NULL)
	return;

    (void) ts2sslose (si, NULLCP, td);

    abort = sb -> sb_AbortIndication;

    sb -> sb_fd = NOTOK;
    (void) freesblk (sb);

    (*abort) (sd, &si -> si_abort);
}

/*  */

int	ts2sslose (si, event, td)
register struct SSAPindication *si;
char   *event;
register struct TSAPdisconnect *td;
{
    int     reason;
    char   *cp,
            buffer[BUFSIZ];

    if (event)
	SLOG (ssap_log, LLOG_EXCEPTIONS, NULLCP,
	      (td -> td_cc > 0 ? "%s: %s [%*.*s]": "%s: %s", event,
	       TErrString (td -> td_reason), td -> td_cc, td -> td_cc,
	       td -> td_data));

    cp = "";
    switch (td -> td_reason) {
	case DR_REMOTE: 
	case DR_CONGEST: 
	    reason = SC_CONGEST;
	    break;

	case DR_SESSION: 
	case DR_ADDRESS: 
	    reason = SC_ADDRESS;
	    break;

	case DR_REFUSED:
	    reason = SC_REFUSED;
	    break;

	default: 
	    (void) sprintf (cp = buffer, " (%s at transport)",
		    TErrString (td -> td_reason));
	case DR_NETWORK:
	    reason = SC_TRANSPORT;
	    break;
    }

    if (td -> td_cc > 0)
	return ssaplose (si, reason, NULLCP, "%*.*s%s",
		td -> td_cc, td -> td_cc, td -> td_data, cp);
    else
	return ssaplose (si, reason, NULLCP, "%s", *cp ? cp + 1 : cp);
}

/*    INTERNAL */

struct ssapblk  *newsblk () {
    register struct ssapblk *sb;

    sb = (struct ssapblk   *) calloc (1, sizeof *sb);
    if (sb == NULL)
	return NULL;

    sb -> sb_fd = NOTOK;
    sb -> sb_qbuf.qb_forw = sb -> sb_qbuf.qb_back = &sb -> sb_qbuf;
    sb -> sb_pr = SPDU_PR;

    if (once_only == 0) {
	SHead -> sb_forw = SHead -> sb_back = SHead;
	once_only++;
    }

    insque (sb, SHead -> sb_back);

    return sb;
}


int	freesblk (sb)
register struct ssapblk *sb;
{
    if (sb == NULL)
	return;

    if (sb -> sb_fd != NOTOK) {
	struct TSAPdata		txs;
	struct TSAPdisconnect   tds;

	if (sb -> sb_flags & SB_FINN)
	    /* Wait for a TDiscInd for ses_dn_timer seconds */
	    if (ses_dn_timer >= 0)
		while (TReadRequest (sb -> sb_fd, &txs, ses_dn_timer,
				     &tds) != NOTOK) {
		    TXFREE (&txs);
		}

	(void) TDiscRequest (sb -> sb_fd, NULLCP, 0, &tds);
    }

    if (sb -> sb_retry) {
	sb -> sb_retry -> s_mask &= ~SMASK_UDATA_PGI;
	sb -> sb_retry -> s_udata = NULL, sb -> sb_retry -> s_ulen = 0;
	freespkt (sb -> sb_retry);
    }

    if (sb -> sb_xspdu)
	freespkt (sb -> sb_xspdu);
    if (sb -> sb_spdu)
	freespkt (sb -> sb_spdu);

    QBFREE (&sb -> sb_qbuf);

    remque (sb);

    free ((char *) sb);
}

/*  */

struct ssapblk   *findsblk (sd)
register int sd;
{
    register struct ssapblk *sb;

    if (once_only == 0)
	return NULL;

    for (sb = SHead -> sb_forw; sb != SHead; sb = sb -> sb_forw)
	if (sb -> sb_fd == sd)
	    return sb;

    return NULL;
}
