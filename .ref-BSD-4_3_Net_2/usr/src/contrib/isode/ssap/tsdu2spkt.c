/* tsdu2spkt.c - read/write a SPDU to a TSDU */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/tsdu2spkt.c,v 7.1 91/02/22 09:46:14 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/tsdu2spkt.c,v 7.1 91/02/22 09:46:14 mrose Interim $
 *
 *
 * $Log:	tsdu2spkt.c,v $
 * Revision 7.1  91/02/22  09:46:14  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:25:55  mrose
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
#include "spkt.h"
#include "tailor.h"

/*  */

struct	local_buf {
	char *top;				/* Top of buffer */
	char *ptr;				/* Pointer to working buffer */
	int pgi;				/* Offset of last PGI li */
	int left;				/* Number of bytes left */
	int li;					/* Running spdu length */
	int allocli;				/* Allocated li */
	int len;				/* Current buffer size */
};

/*  */

#define PMASK_NODATA		0x000000
#define	PMASK_CN_ID		0x000001	/*   1: Connection ID */
#define	PMASK_CN_ITEMS		0x000002	/*   5: Connect/Accept Item */
#define	PMASK_SYNC		0x000004	/*  15: Sync Type Item */
#define	PMASK_TOKEN		0x000008	/*  16: Token Item */
#define	PMASK_TDISC		0x000010	/*  17: Transport Disc */
#define	PMASK_USER_REQ		0x000020	/*  20: Session User Req */
#define	PMASK_VERSION		0x000040	/*  22: Version Number */
#define	PMASK_PREPARE		0x000080	/*  24: Prepare type */
#define	PMASK_ENCLOSE		0x000100	/*  25: Enclosure Item */
#define PMASK_TOKEN_SET		0x000200	/*  26: Token Setting Item */
#define	PMASK_RESYNC		0x000400	/*  27: Resync type */
#define	PMASK_LINK		0x000800	/*  33: Linking information */
#define	PMASK_ACT_ID		0x001000	/*  41: Activity ID */
#define	PMASK_SERIAL		0x002000 	/*  42: Serial Number */
#define	PMASK_MIA_DATA		0x004000	/*  46: MIA User Data */
#define	PMASK_REFLECT		0x008000	/*  49: Reflect parameter */
#define	PMASK_REASON		0x010000	/*  50: Refusal Reason */
#define	PMASK_SSAP_CALLING	0x020000	/*  51: Calling SSAP ID */
#define	PMASK_SSAP_CALLED	0x040000	/*  52: Called SSAP ID */
#define	PMASK_UDATA		0x080000	/* 193: User data */
#define	PMASK_XDATA		0x100000	/* 194: Extended user data */

#define PMASK_VARLEN		0x800000	/* PI is Variable Len */
#define PMASK_NOTSUPPORTED	-1		/* Type not supported */


static int si_table[] = {
	PMASK_REFLECT,				/* 0x00: SPDU_ER */
	PMASK_ENCLOSE				/* 0x01: SPDU_GT & SPDU_DT */
	    | PMASK_TOKEN,
	PMASK_TOKEN				/* 0x02: SPDU_PT */
	    | PMASK_ENCLOSE
	    | PMASK_UDATA,
	PMASK_NOTSUPPORTED,			/* 0x03 */
	PMASK_NOTSUPPORTED,			/* 0x04 */
	PMASK_NODATA,				/* 0x05: SPDU_EX */
	PMASK_NOTSUPPORTED,			/* 0x06 */
	PMASK_PREPARE,				/* 0x07: SPDU_PR */
	PMASK_ENCLOSE				/* 0x08: SPDU_NF */
	    | PMASK_UDATA,
	PMASK_TDISC				/* 0x09: SPDU_FN */
	    | PMASK_ENCLOSE
	    | PMASK_UDATA,
	PMASK_ENCLOSE				/* 0x0a: SPDU_DN */
	    | PMASK_UDATA,
	PMASK_NOTSUPPORTED,			/* 0x0b */
	PMASK_CN_ID				/* 0x0c: SPDU_RF */
	    | PMASK_TDISC	
	    | PMASK_USER_REQ
	    | PMASK_VERSION
	    | PMASK_ENCLOSE
	    | PMASK_REASON,
	PMASK_CN_ID				/* 0x0d: SPDU_CN */
	    | PMASK_CN_ITEMS	
	    | PMASK_USER_REQ
	    | PMASK_VERSION
	    | PMASK_SSAP_CALLING
	    | PMASK_SSAP_CALLED
	    | PMASK_UDATA
	    | PMASK_XDATA,
	PMASK_CN_ID				/* 0x0e: SPDU_AC */
	    | PMASK_CN_ITEMS	
	    | PMASK_USER_REQ
	    | PMASK_VERSION
	    | PMASK_SSAP_CALLING
	    | PMASK_TOKEN
	    | PMASK_ENCLOSE
	    | PMASK_SSAP_CALLED
	    | PMASK_UDATA,
	PMASK_NOTSUPPORTED,			/* 0x0f */
	PMASK_NOTSUPPORTED,			/* 0x10 */
	PMASK_NOTSUPPORTED,			/* 0x11 */
	PMASK_NOTSUPPORTED,			/* 0x12 */
	PMASK_NOTSUPPORTED,			/* 0x13 */
	PMASK_NOTSUPPORTED,			/* 0x14 */
	PMASK_NODATA,				/* 0x15: SPDU_GTC */
	PMASK_NODATA,				/* 0x16: SPDU_GTA */
	PMASK_NOTSUPPORTED,			/* 0x17 */
	PMASK_NOTSUPPORTED,			/* 0x18 */
	PMASK_TDISC				/* 0x19: SPDU_AB & SPDU_AI */
	    | PMASK_REFLECT	
	    | PMASK_REASON
	    | PMASK_ENCLOSE
	    | PMASK_UDATA,
	PMASK_NODATA,				/* 0x1a: SPDU_AA & SPDU_AIA */
	PMASK_NOTSUPPORTED,			/* 0x1b */
	PMASK_NOTSUPPORTED,			/* 0x1c */
	PMASK_LINK				/* 0x1d: SPDU_AR */
	    | PMASK_ACT_ID
	    | PMASK_ENCLOSE
	    | PMASK_SERIAL
	    | PMASK_UDATA,			
	PMASK_NOTSUPPORTED,			/* 0x1e */
	PMASK_NOTSUPPORTED,			/* 0x1f */
	PMASK_NOTSUPPORTED,			/* 0x20 */
	PMASK_ENCLOSE,				/* 0x21: SPDU_TD */
	PMASK_TOKEN_SET				/* 0x22: SPDU_RA */
	    | PMASK_ENCLOSE
	    | PMASK_SERIAL	
	    | PMASK_UDATA,
	PMASK_NOTSUPPORTED,			/* 0x23 */
	PMASK_NOTSUPPORTED,			/* 0x24 */
	PMASK_NOTSUPPORTED,			/* 0x25 */
	PMASK_NOTSUPPORTED,			/* 0x26 */
	PMASK_NOTSUPPORTED,			/* 0x27 */
	PMASK_NOTSUPPORTED,			/* 0x28 */
	PMASK_SYNC				/* 0x29: SPDU_MAP & SPDU_AE */
	    | PMASK_ENCLOSE
	    | PMASK_SERIAL	
	    | PMASK_UDATA,
	PMASK_ENCLOSE				/* 0x2a: SPDU_MAA & SPDU_AEA */
	    | PMASK_SERIAL
	    | PMASK_UDATA,
	PMASK_NOTSUPPORTED,			/* 0x2b */
	PMASK_NOTSUPPORTED,			/* 0x2c */
	PMASK_ENCLOSE				/* 0x2d: SPDU_AS */
	    | PMASK_ACT_ID
	    | PMASK_UDATA,
	PMASK_NOTSUPPORTED,			/* 0x2e */
	PMASK_NOTSUPPORTED,			/* 0x2f */
	PMASK_ENCLOSE				/* 0x30: SPDU_ED */
	    | PMASK_REASON				
	    | PMASK_UDATA,
	PMASK_SYNC				/* 0x31: SPDU_MIP */
	    | PMASK_ENCLOSE	
	    | PMASK_SERIAL	
	    | PMASK_UDATA,			
	PMASK_ENCLOSE				/* 0x32: SPDU_MIA */
	    | PMASK_SERIAL				
	    | PMASK_MIA_DATA,
	PMASK_NOTSUPPORTED,			/* 0x33 */
	PMASK_NOTSUPPORTED,			/* 0x34 */
	PMASK_TOKEN_SET				/* 0x35: SPDU_RS */
	    | PMASK_ENCLOSE	
	    | PMASK_RESYNC	
	    | PMASK_SERIAL
	    | PMASK_UDATA,
	PMASK_NOTSUPPORTED,			/* 0x36 */
	PMASK_NOTSUPPORTED,			/* 0x37 */
	PMASK_NOTSUPPORTED,			/* 0x38 */
	PMASK_REASON,				/* 0x39: SPDU_AD */
	PMASK_NODATA,				/* 0x3a: SPDU_ADA */
	PMASK_NOTSUPPORTED,			/* 0x3b */
	PMASK_NOTSUPPORTED,			/* 0x3c */
	PMASK_ENCLOSE				/* 0x3d: SPDU_CD */
	    | PMASK_UDATA,
	PMASK_ENCLOSE				/* 0x3e: SPDU_CDA */
	    | PMASK_UDATA
};
#define	SI_TABLE_LEN		((sizeof si_table) / (sizeof si_table[0]))

/*  */

#define	PGI_CN_ID		1
#define		PI_CALLED_SS	9
#define		PI_CALLING_SS	10
#define		PI_COMMON_REF	11
#define		PI_ADD_INFO	12
#define	PGI_CN_ITEMS		5
#define		PI_PROTOCOL_OPT	19
#define		PI_TSDU_MAXSIZ	21
#define		PI_VERSION	22
#define		PI_ISN		23
#define		PI_TOKEN_SET	26
#define		PI_ISN2		55
#define	PI_SYNC			15
#define	PI_TOKEN		16
#define	PI_TDISC		17
#define	PI_USER_REQ		20
#define	PI_PREPARE		24
#define	PI_ENCLOSE		25
#define	PI_RESYNC		27
#define	PGI_AR_LINK		33
#define		PI_AR_CALLED	9
#define		PI_AR_CALLING	10
#define		PI_AR_COMMON	11
#define		PI_AR_ADDT	12
#define		PI_AR_OLD	41
#define		PI_AR_SERIAL	42
#define	PI_ACT_ID		41
#define	PI_SERIAL		42
#define	PI_MIA_DATA		46
#define	PI_REFLECT		49
#define	PI_REASON		50
#define	PI_SSAP_CALLING		51
#define	PI_SSAP_CALLED		52
#define	PI_UDATA		193
#define	PI_XDATA		194


static int pi_table[] = {
	0,					/* 0x00 */
	PMASK_VARLEN | PMASK_CN_ID,		/* 0x01: Connection ID */
	0, 0, 0,				/* 0x02-04 */
	PMASK_VARLEN | PMASK_CN_ITEMS,		/* 0x05: Connect/Accept Item */
	0, 0, 0,				/* 0x06-08 */
	PMASK_VARLEN | PMASK_CN_ID | PMASK_LINK,/* 0x09: Called Session SS */
	PMASK_VARLEN | PMASK_CN_ID | PMASK_LINK,/* 0x0a: Calling Session SS */
	PMASK_VARLEN | PMASK_CN_ID | PMASK_LINK,/* 0x0b: Common Reference */
	PMASK_VARLEN | PMASK_CN_ID | PMASK_LINK,/* 0x0c: Additional Info */
	0, 0,					/* 0x0d-0e */
	PMASK_SYNC,				/* 0x0f: Sync Type Item */
	PMASK_TOKEN,				/* 0x10: Token Item */
	PMASK_TDISC,				/* 0x11: Transport Disc */
	0,					/* 0x12 */
	PMASK_CN_ITEMS,				/* 0x13: Protocol Option */
	PMASK_USER_REQ,				/* 0x14: Session User Req */
	PMASK_VARLEN | PMASK_CN_ITEMS,		/* 0x15: TSDU Max Size */
	PMASK_VERSION,				/* 0x16: Version Number */
	PMASK_VARLEN | PMASK_CN_ITEMS,		/* 0x17: Initial Serial Num */
	PMASK_PREPARE,				/* 0x18: Prepare Type */
	PMASK_ENCLOSE,				/* 0x19: Enclosure Item */
	PMASK_CN_ITEMS | PMASK_TOKEN_SET,	/* 0x1a: Token setting item */
	PMASK_RESYNC,				/* 0x1b: Resync type */
	0, 0, 0, 0, 0,				/* 0x1c-20 */
	PMASK_VARLEN | PMASK_LINK,		/* 0x21: Activity Link */
	0, 0, 0, 0, 0, 0, 0,			/* 0x22-28 */
	PMASK_VARLEN | PMASK_ACT_ID,		/* 0x29: Activity ID */
	PMASK_VARLEN | PMASK_SERIAL,		/* 0x2a: Serial Number */
	0, 0, 0,				/* 0x2b-2d */
	PMASK_VARLEN | PMASK_MIA_DATA,		/* 0x2e: MIA User Data */
	0, 0,					/* 0x2f-30 */
	PMASK_VARLEN | PMASK_REFLECT,		/* 0x31: Reflect parameter */
	PMASK_VARLEN | PMASK_REASON,		/* 0x32: Refusal Reason */
	PMASK_VARLEN | PMASK_SSAP_CALLING,	/* 0x33: Calling SSAP ID */
	PMASK_VARLEN | PMASK_SSAP_CALLED,	/* 0x34: Called SSAP ID */
	0, 0,					/* 0x35-36 */
	PMASK_VARLEN | PMASK_CN_ITEMS		/* 0x17: 2nd initial s/n */
};
#define	PI_TABLE_LEN		((sizeof pi_table) / (sizeof pi_table[0]))

/*  */

static int pi_length[PI_TABLE_LEN] = {
	0,					/* 0x00 */
	SREF_USER_SIZE				/* 0x01: Connection ID */
	    + SREF_COMM_SIZE
	    + SREF_ADDT_SIZE
	    + 6,
	0, 0, 0,				/* 0x02-04 */
	1 + 4 + 1 + 6 + 1 + 10,			/* 0x05: Connect/Accept Item */
	0, 0, 0,				/* 0x06-08 */
	SREF_USER_SIZE,				/* 0x09: Called Session SS */
	SREF_USER_SIZE,				/* 0x0a: Calling Session SS */
	SREF_COMM_SIZE,				/* 0x0b: Common Reference */
	SREF_ADDT_SIZE,				/* 0x0c: Additional Info */
	0, 0,					/* 0x0d-0e */
	1,					/* 0x0f: Sync Type Item */
	1,					/* 0x10: Token Item */
	1,					/* 0x11: Transport Disc */
	0,					/* 0x12 */
	1,					/* 0x13: Protocol Option */
	2,					/* 0x14: Session User Req */
	4,					/* 0x15: TSDU Max Size */
	1,					/* 0x16: Version Number */
	SIZE_CN_ISN,				/* 0x17: Initial Serial Num */
	1,					/* 0x18: Prepare Type */
	1,					/* 0x19: Enclosure Item */
	1,					/* 0x1a: Token setting item */
	1,					/* 0x1b: Resync type */
	0, 0, 0, 0, 0,				/* 0x1c-20 */
	2 * SREF_USER_SIZE			/* 0x21: Activity Link */
	    + SREF_COMM_SIZE
	    + SREF_ADDT_SIZE
	    + SID_DATA_SIZE
	    + SIZE_CN_ISN
	    + 6 * 2,
	0, 0, 0, 0, 0, 0, 0,			/* 0x22-28 */
	SID_DATA_SIZE,				/* 0x29: Activity ID */
	SIZE_CN_ISN,				/* 0x2a: Serial Number */
	0, 0, 0,				/* 0x2b-2d */
	SEGMENT_MAX /* MIA_SIZE */,		/* 0x2e: MIA User Data */
	0, 0,					/* 0x2f-30 */
	SEGMENT_MAX,				/* 0x31: Reflect parameter */
	RF_SIZE,				/* 0x32: Refusal Reason */
	SSSIZE,					/* 0x33: Calling SSAP ID */
	SSSIZE,					/* 0x34: Called SSAP ID */
	0, 0,					/* 0x35-36 */
	SIZE_CN_ISN,				/* 0x37: 2nd initial s/n */
};

/*  */

#define	If_Set(flag)	if (s -> s_mask & (flag))
#define If_Reset(flag)	if (!(s -> s_mask & (flag)))
#define Set(flag) 	s -> s_mask |= (flag)
#define Reset(flag) 	s -> s_mask &= ~(flag)

#define Put_Item(code,value) \
	put2spdu((code), pi_length[(code)], (value), &c)

#define Put_Ref(r,code) \
{ \
    start_pgi (PGI_CN_ID, &c); \
    if (r.sr_ulen) \
	put2spdu (code, (int) r.sr_ulen, r.sr_udata, &c); \
    if (r.sr_clen) \
	put2spdu (PI_COMMON_REF, (int) r.sr_clen, r.sr_cdata, &c); \
    if (r.sr_alen) \
	put2spdu (PI_ADD_INFO, (int) r.sr_alen, r.sr_adata, &c); \
    end_pgi (&c); \
}

#define	Put_SSN(code,ssn) \
{ \
    if ((ssn) > SERIAL_MAX + 1) { \
	if (c.len) \
	    free (c.top); \
	s -> s_errno = SC_PROTOCOL; \
	return NOTOK; \
    } \
    (void) sprintf (isn, "%lu", (ssn)); \
    put2spdu ((code), strlen (isn), isn, &c); \
}

/* this used to check

	if (s -> s_ulen > (csize)) {
	    if (c.len)
		free (c.top);
	    s -> s_errno = SC_PROTOCOL;
	    return NOTOK;
	}

   but with version 2 session, there's really no point... */

#define Put_UData(csize) \
{ \
    if (s -> s_udata) { \
	put2spdu (PI_UDATA, s -> s_ulen, s -> s_udata, &c); \
    } \
}

#define Put_XData(csize) \
{ \
    if (s -> s_udata) { \
	put2spdu (s -> s_ulen > csize ? PI_XDATA : PI_UDATA, s -> s_ulen, \
		  s -> s_udata, &c); \
    } \
}

#define Put_MData(csize) \
{ \
    if (s -> s_udata) { \
	put2spdu (PI_MIA_DATA, s -> s_ulen, s -> s_udata, &c); \
    } \
}

/*  */

static start_spdu (s, c, basesize)
struct	ssapkt *s;
struct	local_buf *c;
int basesize;
{
    if (s -> s_udata)
	switch (s -> s_code) {
	    case SPDU_DT: 	/* caller responsible for this... */
	    case SPDU_EX: 
	    case SPDU_TD: 
		break;

	    default: 
		if (s -> s_ulen)
		    basesize += s -> s_ulen + (s -> s_ulen > 254 ? 4 : 2);
		break;
	}

    switch (s -> s_code) {
	case SPDU_CN: 
	case SPDU_AC: 
	    If_Set (SMASK_CN_REF) {
		basesize += 2;
		if (s -> s_cn_reference.sr_ulen)
		    basesize += 2 + s -> s_cn_reference.sr_ulen;
		if (s -> s_cn_reference.sr_clen)
		    basesize += 2 + s -> s_cn_reference.sr_clen;
		if (s -> s_cn_reference.sr_alen)
		    basesize += 2 + s -> s_cn_reference.sr_alen;
	    }
	    If_Set (SMASK_CN_CALLING)
		basesize += s -> s_callinglen + 2;
	    If_Set (SMASK_CN_CALLED)
		basesize += s -> s_calledlen + 2;
	    break;

	case SPDU_RF: 
	    If_Set (SMASK_RF_REF) {
		basesize += 2;
		if (s -> s_rf_reference.sr_ulen)
		    basesize += 2 + s -> s_rf_reference.sr_ulen;
		if (s -> s_rf_reference.sr_clen)
		    basesize += 2 + s -> s_rf_reference.sr_clen;
		if (s -> s_rf_reference.sr_alen)
		    basesize += 2 + s -> s_rf_reference.sr_alen;
	    }
	    break;

	case SPDU_AR: 
	    basesize += 2;
	    If_Set (SMASK_AR_REF) {
		if (s -> s_ar_reference.sr_ulen)
		    basesize += 2 + s -> s_ar_reference.sr_ulen;
		if (s -> s_ar_reference.sr_clen)
		    basesize += 2 + s -> s_ar_reference.sr_clen;
		if (s -> s_ar_reference.sr_alen)
		    basesize += 2 + s -> s_ar_reference.sr_alen;
		if (s -> s_ar_reference.sr_vlen)
		    basesize += 2 + s -> s_ar_reference.sr_vlen;
	    }
	    break;
    }

    if (basesize < 254)
	basesize = 254;
    c -> li = c -> pgi = 0;
    c -> len = basesize + ((basesize > 254) ? 4 : 2);
    if ((c -> top = malloc ((unsigned) c -> len)) == NULL) {
	c -> len = 0;
	s -> s_errno = SC_CONGEST;
    }
    else
	s -> s_errno = SC_ACCEPT;
    if ((c -> allocli = c -> left = basesize) > 254)
	c -> ptr = c -> top + 4;
    else
	c -> ptr = c -> top + 2;
}

/*  */

static int end_spdu (code, c)
unsigned char code;
struct	local_buf *c;
{
    if (c -> len) {
	if (c -> allocli > 254) {
	    if (c -> li < 255) {
		bcopy ((c -> top + 2), c -> top, (c -> len - c -> left));
		*(c -> top + 1) = c -> li;
		c -> len = c -> li + 2;
	    }
	    else {
		*(c -> top + 1) = 255;
		*(c -> top + 2) = (c -> li >> 8) & 0xff;
		*(c -> top + 3) = c -> li & 0xff;
		c -> len = c -> li + 4;
	    }
	}
	else {
	    *(c -> top + 1) = c -> li;
	    c -> len = c -> ptr - c -> top;
	}
	*c -> top = code;
	return OK;
    }

    return NOTOK;
}

/*  */

static start_pgi (code, c)
unsigned char code;
struct	local_buf *c;
{
    put2spdu ((int) code, 0, NULLCP, c);
    if (c -> len)
	c -> pgi = (c -> ptr - c -> top - 1);
}


static end_pgi (c)
struct	local_buf *c;
{
    if (c -> len)
	*(c -> top + c -> pgi) = (c -> len - c -> left) - (c -> pgi + 1);
}

/*  */

static	put2spdu (code, li, value, c)
int code;
int li;
char *value;
struct	local_buf *c;
{
    int     cl = li;
    char   *p1,
           *p2;

    if (c -> len) {
	cl += (li < 255) ? 2 : 4;
	if (c -> left >= cl)
	    c -> left -= cl;
	else {
/* XXX:	this clause of Dwight's is all WRONG, WRONG, WRONG.  I think we
	should make start_spdu() smarter, if necessary and change this to

	c -> len = 0;
	return;
*/
	char   *cp;

	    if (c -> allocli < 255)
		cl += 2;
	    cp = realloc (c -> top, (unsigned) (c -> len += cl));
	    if (cp == NULL) {
		c -> len = 0;
		return;
	    }
	    c -> ptr = (c -> top = cp) + (c -> len - c -> left);
	    if (c -> allocli < 255) {
		c -> allocli += cl;
		cl = c -> len - c -> left + 2;
		for (p1 = c -> ptr, p2 = p1 + 2; cl; cl--)
		    *p2-- = *p1--;
		c -> pgi += 2;
		c -> left -= 2;
	    }
	}
	*c -> ptr++ = code & 0xff;
	if (li < 255) {
	    *c -> ptr++ = li;
	    c -> li += 2 + li;
	}
	else {
	    *c -> ptr++ = 255;
	    *c -> ptr++ = (li >> 8) & 0xff;
	    *c -> ptr++ = li & 0xff;
	    c -> li += 4 + li;
	}

	bcopy (value, c -> ptr, li);
	c -> ptr += li;
    }
}

/*  */

int	spkt2tsdu (s, base, len)
register struct ssapkt *s;
char  **base;
int    *len;
{
    struct local_buf    c;
    char    isn[SIZE_CN_ISN + 1];

    c.len = 0;
    switch (s -> s_code) {
	case SPDU_CN: 
	    start_spdu (s, &c, CN_BASE_SIZE);
	    If_Set (SMASK_CN_REF)
		Put_Ref (s -> s_cn_reference, PI_CALLING_SS);
	    If_Set (SMASK_CN_OPT | SMASK_CN_TSDU | SMASK_CN_VRSN | SMASK_CN_ISN
		    | SMASK_CN_SET) {
		start_pgi (PGI_CN_ITEMS, &c);
		If_Set (SMASK_CN_OPT)
		    Put_Item (PI_PROTOCOL_OPT, (char *) &s -> s_options);
		If_Set (SMASK_CN_TSDU) {
		    u_long tsdu_maxsize = (s -> s_tsdu_init & 0xffff) << 16
			| s -> s_tsdu_resp & 0xffff;
		    tsdu_maxsize = htonl (tsdu_maxsize);
		    Put_Item (PI_TSDU_MAXSIZ, (char *) &tsdu_maxsize);
		}
		If_Set (SMASK_CN_VRSN)
		    Put_Item (PI_VERSION, (char *) &s -> s_cn_version);
		If_Set (SMASK_CN_ISN)
		    Put_SSN (PI_ISN, s -> s_isn);
		If_Set (SMASK_CN_SET)
		    Put_Item (PI_TOKEN_SET, (char *) &s -> s_settings);
		end_pgi (&c);
	    }
    	    If_Set (SMASK_CN_REQ) {
		u_short requirements = htons (s -> s_cn_require);
		Put_Item (PI_USER_REQ, (char *) &requirements);
	    }
	    If_Set (SMASK_CN_CALLING)
		put2spdu (PI_SSAP_CALLING, s -> s_callinglen, 
			    s -> s_calling, &c);
	    If_Set (SMASK_CN_CALLED)
		put2spdu (PI_SSAP_CALLED, s -> s_calledlen, 
			    s -> s_called, &c);
	    Put_XData (CN_SIZE);
	    break;

	case SPDU_AC: 
	    start_spdu (s, &c, AC_BASE_SIZE);
	    If_Set (SMASK_CN_REF)
		Put_Ref (s -> s_cn_reference, PI_CALLED_SS);
	    If_Set (SMASK_CN_OPT | SMASK_CN_TSDU | SMASK_CN_VRSN | SMASK_CN_ISN
		    | SMASK_CN_SET) {
		start_pgi (PGI_CN_ITEMS, &c);
		If_Set (SMASK_CN_OPT)
		    Put_Item (PI_PROTOCOL_OPT, (char *) &s -> s_options);
		If_Set (SMASK_CN_TSDU) {
		    u_long tsdu_maxsize = (s -> s_tsdu_init & 0xffff) << 16
			| s -> s_tsdu_resp & 0xffff;
		    tsdu_maxsize = htonl (tsdu_maxsize);
		    Put_Item (PI_TSDU_MAXSIZ, (char *) &tsdu_maxsize);
		}
		If_Set (SMASK_CN_VRSN)
		    Put_Item (PI_VERSION, (char *) &s -> s_cn_version);
		If_Set (SMASK_CN_ISN)
		    Put_SSN (PI_ISN, s -> s_isn);
		If_Set (SMASK_CN_SET)
		    Put_Item (PI_TOKEN_SET, (char *) &s -> s_settings);
		end_pgi (&c);
	    }
	    If_Set (SMASK_AC_TOKEN)
		Put_Item (PI_TOKEN, (char *) &s -> s_ac_token);
	    If_Set (SMASK_CN_REQ) {
		u_short requirements = htons (s -> s_cn_require);
		Put_Item (PI_USER_REQ, (char *) &requirements);
	    }
	    If_Set (SMASK_CN_CALLING)
		put2spdu (PI_SSAP_CALLING, s -> s_callinglen,
			    s -> s_calling, &c);
	    If_Set (SMASK_CN_CALLED)
		put2spdu (PI_SSAP_CALLED, s -> s_calledlen, 
			    s -> s_called, &c);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (AC_SIZE);
	    break;

	case SPDU_RF: 
	    start_spdu (s, &c, RF_BASE_SIZE);
	    If_Set (SMASK_RF_REF)
		Put_Ref (s -> s_rf_reference, PI_CALLED_SS);
	    If_Set (SMASK_RF_DISC)
		Put_Item (PI_TDISC, (char *) &s -> s_rf_disconnect);
	    If_Set (SMASK_RF_REQ) {
		u_short requirements = htons (s -> s_rf_require);
		Put_Item (PI_USER_REQ, (char *) &requirements);
	    }
	    If_Set (SMASK_RF_VRSN)
		Put_Item (PI_VERSION, (char *) &s -> s_rf_version);
	    if (s -> s_rlen > RF_SIZE) {
		if (c.len)
		    free (c.top);
		s -> s_errno = SC_PROTOCOL;
		return NOTOK;
	    }
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    if (s -> s_rlen > 0)
		put2spdu (PI_REASON, s -> s_rlen, s -> s_rdata, &c);
	    break;

	case SPDU_FN: 
	    start_spdu (s, &c, FN_BASE_SIZE);
	    If_Set (SMASK_FN_DISC)
		Put_Item (PI_TDISC, (char *) &s -> s_fn_disconnect);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (FN_SIZE);
	    break;

	case SPDU_DN: 
	    start_spdu (s, &c, DN_BASE_SIZE);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (DN_SIZE);
	    break;

	case SPDU_NF: 
	    start_spdu (s, &c, NF_BASE_SIZE);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (NF_SIZE);
	    break;

	case SPDU_AB: 
#ifdef	notdef
	case SPDU_AI:		/* aka SPDU_AB */
#endif
	    If_Set (SMASK_SPDU_AB) {
		start_spdu (s, &c, AB_BASE_SIZE);
		If_Reset (SMASK_AB_DISC) {
		    s -> s_errno = SC_PROTOCOL;
		    break;
		}
		Put_Item (PI_TDISC, (char *) &s -> s_ab_disconnect);
		If_Set (SMASK_AB_REFL)
		    put2spdu (PI_REFLECT, AB_REFL_SIZE,
				(char *) s -> s_reflect, &c);
		If_Set (SMASK_ENCLOSE)
		    Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
		Put_UData (AB_SIZE);
		break;
	    }
	    start_spdu (s, &c, AI_BASE_SIZE);
	    If_Set (SMASK_AI_REASON)
		put2spdu (PI_REASON, 1, (char *) &s -> s_ai_reason, &c);
	    break;

	case SPDU_AA: 
#ifdef	notdef
	case SPDU_AIA:		/* aka SPDU_AA */
#endif
	    If_Set (SMASK_SPDU_AA) {
		start_spdu (s, &c, AA_BASE_SIZE);
		break;
	    }
	    start_spdu (s, &c, AIA_BASE_SIZE);
	    break;

	case SPDU_GT:
#ifdef	notdef
	case SPDU_DT:		/* aka SPDU_GT */
#endif
	    If_Set (SMASK_SPDU_GT) {
		start_spdu (s, &c, GT_BASE_SIZE);
		If_Set (SMASK_GT_TOKEN)
		    Put_Item (PI_TOKEN, (char *) &s -> s_gt_token);
		break;
	    }			/* else fall */
	    start_spdu (s, &c, DT_BASE_SIZE);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
/* NB: caller responsible for mapping s -> s_udata to user info */
	    break;

	case SPDU_TD: 
	    start_spdu (s, &c, TD_BASE_SIZE);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
/* NB: caller responsible for mapping s -> s_udata to user info */
	    break;

	case SPDU_EX: 
	    start_spdu (s, &c, EX_BASE_SIZE);
/* NB: caller responsible for mapping s -> s_udata to user info */
	    break;

	case SPDU_CD: 
	    start_spdu (s, &c, CD_BASE_SIZE);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (CD_SIZE);
	    break;

	case SPDU_CDA: 
	    start_spdu (s, &c, CDA_BASE_SIZE);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (CDA_SIZE);
	    break;

	case SPDU_PT: 
	    start_spdu (s, &c, PT_BASE_SIZE);
	    If_Set (SMASK_PT_TOKEN)
		Put_Item (PI_TOKEN, (char *) &s -> s_pt_token);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (PT_SIZE);
	    break;

	case SPDU_GTC: 
	    start_spdu (s, &c, GTC_BASE_SIZE);
	    break;

	case SPDU_GTA: 
	    start_spdu (s, &c, GTA_BASE_SIZE);
	    break;

	case SPDU_MIP: 
	    start_spdu (s, &c, MIP_BASE_SIZE);
	    If_Set (SMASK_MIP_SYNC)
		Put_Item (PI_SYNC, (char *) &s -> s_mip_sync);
	    If_Reset (SMASK_MIP_SERIAL) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    Put_SSN (PI_SERIAL, s -> s_mip_serial);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (MIP_SIZE);
	    break;

	case SPDU_MAP: 
#ifdef	notdef
	case SPDU_AE:		/* aka SPDU_MAP */
#endif
	    If_Set (SMASK_MAP_SYNC) {
		start_spdu (s, &c, MAP_BASE_SIZE);
		Put_Item (PI_SYNC, (char *) &s -> s_map_sync);
	    }
	    else
		start_spdu (s, &c, AE_BASE_SIZE);
	    If_Reset (SMASK_MAP_SERIAL) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    Put_SSN (PI_SERIAL, s -> s_map_serial);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (MAP_SIZE);
	    break;

	case SPDU_MIA: 
	    start_spdu (s, &c, MIA_BASE_SIZE);
	    If_Reset (SMASK_MIA_SERIAL) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    Put_SSN (PI_SERIAL, s -> s_mia_serial);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_MData (MIA_SIZE);
	    break;

	case SPDU_MAA: 
#ifdef	notdef
	case SPDU_AEA:		/* aka SPDU_MAA */
#endif
	    start_spdu (s, &c, MAA_BASE_SIZE);
	    If_Reset (SMASK_MAA_SERIAL) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    Put_SSN (PI_SERIAL, s -> s_maa_serial);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (MAA_SIZE);
	    break;

	case SPDU_RS:
	    start_spdu (s, &c, RS_BASE_SIZE);
	    If_Set (SMASK_RS_SET)
		Put_Item (PI_TOKEN_SET, (char *) &s -> s_rs_settings);
	    If_Reset (SMASK_RS_TYPE) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    Put_Item (PI_RESYNC, (char *) &s -> s_rs_type);
	    If_Reset (SMASK_RS_SSN) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    Put_SSN (PI_SERIAL, s -> s_rs_serial);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (RS_SIZE);
	    break;

	case SPDU_RA:
	    start_spdu (s, &c, RA_BASE_SIZE);
	    If_Set (SMASK_RA_SET)
		Put_Item (PI_TOKEN_SET, (char *) &s -> s_ra_settings);
	    If_Reset (SMASK_RA_SSN) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    Put_SSN (PI_SERIAL, s -> s_ra_serial);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (RA_SIZE);
	    break;

	case SPDU_PR: 
	    start_spdu (s, &c, PR_BASE_SIZE);
	    If_Reset (SMASK_PR_TYPE) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    Put_Item (PI_PREPARE, (char *) &s -> s_pr_type);
	    break;

	case SPDU_ER:		/* we don't do these! */
	    s -> s_errno = SC_PROTOCOL;
	    break;

	case SPDU_ED: 
	    start_spdu (s, &c, ED_BASE_SIZE);
	    If_Reset (SMASK_ED_REASON) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    put2spdu (PI_REASON, 1, (char *) &s -> s_ed_reason, &c);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (ED_SIZE);
	    break;

	case SPDU_AS: 
	    start_spdu (s, &c, AS_BASE_SIZE);
	    If_Reset (SMASK_AS_ID) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    put2spdu (PI_ACT_ID, (int) s -> s_as_id.sd_len,
		s -> s_as_id.sd_data, &c);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (AS_SIZE);
	    break;

	case SPDU_AR:
	    start_spdu (s, &c, AR_BASE_SIZE);
	    start_pgi (PGI_AR_LINK, &c);
	    If_Set (SMASK_AR_REF) {
		if (s -> s_ar_reference.sr_called_len)
		    put2spdu (PI_AR_CALLED,
			    (int) s -> s_ar_reference.sr_called_len,
			    s -> s_ar_reference.sr_called, &c);
		if (s -> s_ar_reference.sr_calling_len)
		    put2spdu (PI_AR_CALLING,
			    (int) s -> s_ar_reference.sr_calling_len,
			    s -> s_ar_reference.sr_calling, &c);
		if (s -> s_ar_reference.sr_clen)
		    put2spdu (PI_AR_COMMON, (int) s -> s_ar_reference.sr_clen,
			    s -> s_ar_reference.sr_cdata, &c);
		if (s -> s_ar_reference.sr_alen)
		    put2spdu (PI_AR_ADDT, (int) s -> s_ar_reference.sr_alen,
			    s -> s_ar_reference.sr_adata, &c);
	    }
	    If_Reset (SMASK_AR_OID) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    put2spdu (PI_ACT_ID, (int) s -> s_ar_oid.sd_len,
		    s -> s_ar_oid.sd_data, &c);
	    If_Reset (SMASK_AR_SSN) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    Put_SSN (PI_SERIAL, s -> s_ar_serial);
	    end_pgi (&c);	    
	    If_Reset (SMASK_AR_ID) {
		s -> s_errno = SC_PROTOCOL;
		break;
	    }
	    put2spdu (PI_ACT_ID, (int) s -> s_ar_id.sd_len,
		s -> s_ar_id.sd_data, &c);
	    If_Set (SMASK_ENCLOSE)
		Put_Item (PI_ENCLOSE, (char *) &s -> s_enclose);
	    Put_UData (AR_SIZE);
	    break;

	case SPDU_AD: 
	    start_spdu (s, &c, AD_BASE_SIZE);
	    If_Set (SMASK_AD_REASON) 
		put2spdu (PI_REASON, 1, (char *) &s -> s_ad_reason, &c);
	    break;

	case SPDU_ADA: 
	    start_spdu (s, &c, ADA_BASE_SIZE);
	    break;

	default: 
	    s -> s_errno = SC_PROTOCOL;
	    break;
    }

    if ((end_spdu (s -> s_code, &c) == NOTOK) || (s -> s_errno != SC_ACCEPT)) {
	if (s -> s_errno == SC_ACCEPT)
	    s -> s_errno = SC_CONGEST;
	if (c.len) {
	    free (c.top);
	    c.len = 0;
	}
	*base = NULL;
	*len = 0;
    }
    else {
	*base = c.top;
	*len = c.len;
	s -> s_li = c.li;
    }

#ifdef	DEBUG
    if (ssap_log -> ll_events & LLOG_PDUS)
	spkt2text (ssap_log, s, 0);
#endif

    return c.len ? OK : NOTOK;
}

/*  */

static u_long str2ssn (s, n)
register char  *s;
register int	n;
{
    register u_long u;

    for (u = 0L; n > 0; n--)
	u = u * 10 + *s++ - '0';

    return u;
}

/*  */

/* this is used to pull PCI, not user data... */

#define	advance(n) \
    if (base >= xbase) { \
	if ((base = pullqb (qb, (n))) == NULL) { \
	    s -> s_errno = SC_PROTOCOL; \
	    break; \
	} \
	xbase = base + (n); \
	nread += (n); \
    } \
    else

static char *pullqb (qb, n)
struct qbuf *qb;
int	n;
{
    register int    i;
    int	    once;
    register char  *cp;
    register struct qbuf *qp;
    static char *buffer = NULL;

    if (n > SEGMENT_MAX)
	return NULLCP;

    for (once = 1, cp = buffer, qp = NULL; n > 0; once = 0, cp += i, n -= i) {
	if (qp == NULL && (qp = qb -> qb_forw) == qb)
	    return NULLCP;

	i = min (qp -> qb_len, n);
	if (once && i == n) {	/* special case */
	    cp = qp -> qb_data;
	    qp -> qb_data += i, qp -> qb_len -= i;
	    return cp;
	}

	if (buffer == NULL) {
	    if ((buffer = malloc ((unsigned) SEGMENT_MAX)) == NULL)
		return NULLCP;
	    cp = buffer;
	}
	bcopy (qp -> qb_data, cp, i);

	qp -> qb_data += i, qp -> qb_len -= i;
	if (qp -> qb_len <= 0) {
	    remque (qp);

	    free ((char *) qp);
	    qp = NULL;
	}
    }

    return buffer;
}

/*  */

struct ssapkt *tsdu2spkt (qb, len, cc)
struct qbuf *qb;
int	len,
       *cc;
{
    register int    li;
    int     cat0,
	    nread,
	    pktlen,
            pgilen,
	    pmask,
	    xlen;
    register char *base;
    char   *xbase;
    unsigned char   code,
                    si;
    register struct ssapkt  *s;

    if (cc) {
	cat0 = *cc;
	*cc = 0;
    }
    else
	cat0 = 1;
    if ((base = pullqb (qb, nread = 2)) == NULL
	    || (s = newspkt ((int) (si = *base++))) == NULL)
	return NULLSPKT;

    if (*((u_char *) base) == 255) {
	if ((base = pullqb (qb, 2)) == NULL) {
	    s -> s_errno = SC_PROTOCOL;
	    return s;
	}
	nread += 2;
	s -> s_li = 
		(*((u_char *) base) << 8) + *((u_char *) (base + 1));
    }
    else
	s -> s_li = *((u_char *) base);
    pgilen = pktlen = s -> s_li;

    if (cat0)
	switch (si) {
	    case SPDU_GT:
		Set (SMASK_SPDU_GT);
		break;

	    case SPDU_AB:
		Set (SMASK_SPDU_AB);
		break;

	    case SPDU_AA:
		Set (SMASK_SPDU_AA);
		break;

	    default:
		break;
	}

    if ((si >= SI_TABLE_LEN)
	    || ((pmask = si_table[si]) == PMASK_NOTSUPPORTED)) {
	s -> s_errno = SC_PROTOCOL;
	return s;
    }
    if (len < pktlen + nread) {
	s -> s_errno = SC_PROTOCOL;
	return s;
    }

    s -> s_errno = SC_ACCEPT;
    xbase = base;
    while (pktlen && (s -> s_errno == SC_ACCEPT)) {
	advance (2);
	code = *base++;
	if (*((u_char *) base) == 255) {
	    base++;
	    advance (2);
	    li = (*((u_char *) base) << 8) + *((u_char *) (base + 1));
	    xlen = 2;
	}
	else {
	    li = *((u_char *) base);
	    xlen = 1;
	}
	base += xlen;
	if (xlen > 1)
	    xlen += 2;
	else
	    xlen++;
	switch (code) {
	    case PI_UDATA:
	        if (!(pmask & PMASK_UDATA))
		    s -> s_errno = SC_PROTOCOL;
		break;

	    case PI_XDATA:
	        if (!(pmask & PMASK_XDATA))
		    s -> s_errno = SC_PROTOCOL;
		break;

	    default:
		if (code >= PI_TABLE_LEN || !(pmask & pi_table[code]))
		    s -> s_errno = SC_PROTOCOL;
		break;
	}
	if (s -> s_errno != SC_ACCEPT)
	    break;
	if (!pgilen)
	    pgilen = pktlen;
	pktlen -= (xlen + li);
	if (li > (pgilen -= xlen)) {
	    s -> s_errno = SC_PROTOCOL;
	    break;
	}
	pgilen -= li;
	if (li)
	    advance (li);
	if (code < PI_TABLE_LEN) {
	    if (li) {
		if (pi_table[code] & PMASK_VARLEN) {
		    if (li > pi_length[code]) {
			s -> s_errno = SC_PROTOCOL;
			break;
		    }
		}
		else
		    if (li != pi_length[code]) {
			s -> s_errno = SC_PROTOCOL;
			break;
		    }
	    }
	}

	switch (code) {
	    case PGI_AR_LINK:
		Set (SMASK_AR_OID);/* HACK! */
		goto do_pgi;

	    case PGI_CN_ID: 
		Set (SMASK_CN_REF);/* fall */
	    case PGI_CN_ITEMS: 
do_pgi: ;
		pktlen += li;
		pgilen = li;
		li = 0;
		break;

	    case PI_CALLED_SS: 
	    case PI_CALLING_SS: 
#ifdef	notdef
	    case PI_AR_CALLED:
	    case PI_AR_CALLING:
#endif
		switch (si) {
		    case SPDU_CN: 
		    case SPDU_AC: 
			s -> s_cn_reference.sr_ulen = li;
			bcopy (base, s -> s_cn_reference.sr_udata, li);
			Set (SMASK_CN_REF);
			break;
		    case SPDU_RF: 
			s -> s_rf_reference.sr_ulen = li;
			bcopy (base, s -> s_rf_reference.sr_udata, li);
			Set (SMASK_RF_REF);
			break;
		    case SPDU_AR:
			switch (code) {
			    case PI_AR_CALLED:
				s -> s_ar_reference.sr_called_len = li;
				bcopy (base, s -> s_ar_reference.sr_called,
					li);
				Set (SMASK_AR_REF);
				break;				
			    case PI_AR_CALLING:
				s -> s_ar_reference.sr_calling_len = li;
				bcopy (base, s -> s_ar_reference.sr_calling,
					li);
				Set (SMASK_AR_REF);
				break;
			    default:
				s -> s_errno = SC_PROTOCOL;
				break;
			}
		        break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		base += li;
		break;

	    case PI_COMMON_REF: 
#ifdef	notdef
	    case PI_AR_COMMON:
#endif
		switch (si) {
		    case SPDU_CN: 
		    case SPDU_AC: 
			s -> s_cn_reference.sr_clen = li;
			bcopy (base, s -> s_cn_reference.sr_cdata, li);
			Set (SMASK_CN_REF);
			break;
		    case SPDU_RF: 
			s -> s_rf_reference.sr_clen = li;
			bcopy (base, s -> s_rf_reference.sr_cdata, li);
			Set (SMASK_RF_REF);
			break;
		    case SPDU_AR:
			s -> s_ar_reference.sr_clen = li;
			bcopy (base, s -> s_ar_reference.sr_cdata, li);
			Set (SMASK_AR_REF);
			break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		base += li;
		break;

	    case PI_ADD_INFO: 
#ifdef	notdef
	    case PI_AR_ADDT:
#endif
		switch (si) {
		    case SPDU_CN: 
		    case SPDU_AC: 
			s -> s_cn_reference.sr_alen = li;
			bcopy (base, s -> s_cn_reference.sr_adata, li);
			Set (SMASK_CN_REF);
			break;
		    case SPDU_RF: 
			s -> s_rf_reference.sr_alen = li;
			bcopy (base, s -> s_rf_reference.sr_adata, li);
			Set (SMASK_RF_REF);
			break;
		    case SPDU_AR:
			s -> s_ar_reference.sr_alen = li;
			bcopy (base, s -> s_ar_reference.sr_adata, li);
			Set (SMASK_AR_REF);
			break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		base += li;
		break;

	    case PI_PROTOCOL_OPT: 
		if ((s -> s_options = *base++) & ~CR_OPT_MASK)
		    s -> s_errno = SC_PROTOCOL;
		else
		    Set (SMASK_CN_OPT);
		break;

	    case PI_TSDU_MAXSIZ: 
		{
		    u_long tsdu_maxsize;
		    bcopy (base, (char *) &tsdu_maxsize,
			    pi_length[PI_TSDU_MAXSIZ]);
		    tsdu_maxsize = ntohl (tsdu_maxsize);
		    s -> s_tsdu_init = (tsdu_maxsize >> 16) & 0xffff;
		    s -> s_tsdu_resp = tsdu_maxsize & 0xffff;
		    Set (SMASK_CN_TSDU);
		}
		base += pi_length[PI_TSDU_MAXSIZ];
		break;

	    case PI_VERSION: 
		switch (si) {
		    case SPDU_CN: 
		    case SPDU_AC: 
			s -> s_cn_version = *base++;
			Set (SMASK_CN_VRSN);
			break;
		    case SPDU_RF: 
			s -> s_rf_version = *base++;
			Set (SMASK_RF_VRSN);
			break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		break;

	    case PI_SYNC: 
		switch (si) {
		    case SPDU_MIP: 
			if ((s -> s_mip_sync = *base++) & ~MIP_SYNC_MASK)
			    s -> s_errno = SC_PROTOCOL;
			else
			    Set (SMASK_MIP_SYNC);
			break;
		    case SPDU_MAP: 
			if ((s -> s_map_sync = *base++) & ~MAP_SYNC_MASK)
			    s -> s_errno = SC_PROTOCOL;
			else
			    Set (SMASK_MAP_SYNC);
			break;
		}
		break;

	    case PI_TOKEN_SET: 
		switch (si) {
		    case SPDU_CN: 
		    case SPDU_AC: 
			s -> s_settings = *base++;
			Set (SMASK_CN_SET);
			break;
		    case SPDU_RS:
			s -> s_rs_settings = *base++;
			Set (SMASK_RS_SET);
			break;
		    case SPDU_RA:
			s -> s_ra_settings = *base++;
			Set (SMASK_RA_SET);
			break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		break;

	    case PI_TOKEN: 
		switch (si) {
		    case SPDU_AC: 
			s -> s_ac_token = *base++;
			Set (SMASK_AC_TOKEN);
			break;
		    case SPDU_GT: 
			If_Reset (SMASK_SPDU_GT) {
			    s -> s_errno = SC_PROTOCOL;
			    break;
			}
			s -> s_gt_token = *base++;
			Set (SMASK_GT_TOKEN);
			break;
		    case SPDU_PT: 
			s -> s_pt_token = *base++;
			Set (SMASK_PT_TOKEN);
			break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		break;

	    case PI_TDISC: 
		switch (si) {
		    case SPDU_RF: 
			if ((s -> s_rf_disconnect = *base++) & ~RF_DISC_MASK)
			    s -> s_errno = SC_PROTOCOL;
			else
			    Set (SMASK_RF_DISC);
			break;
		    case SPDU_FN: 
			if ((s -> s_fn_disconnect = *base++) & ~FN_DISC_MASK)
			    s -> s_errno = SC_PROTOCOL;
			else
			    Set (SMASK_FN_DISC);
			break;
		    case SPDU_AB: 
			If_Reset (SMASK_SPDU_AB) {
			    s -> s_errno = SC_PROTOCOL;
			    break;
			}
			if ((s -> s_ab_disconnect = *base++) & ~AB_DISC_MASK)
			    s -> s_errno = SC_PROTOCOL;
			else
			    Set (SMASK_AB_DISC);
			break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		break;

	    case PI_USER_REQ: 
		{
		    u_short requirements;
		    bcopy (base, (char *) &requirements, 2);
		    requirements = ntohs (requirements);
		    if (si != SPDU_RF) {
			s -> s_cn_require = requirements;
			Set (SMASK_CN_REQ);
		    }
		    else {
			s -> s_rf_require = requirements;
			Set (SMASK_RF_REQ);
		    }
		}
		base += 2;
		break;

	    case PI_ISN: 
		switch (si) {
		    case SPDU_CN: 
		    case SPDU_AC: 
			s -> s_isn = str2ssn (base, li);
			Set (SMASK_CN_ISN);
			break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
	    case PI_ISN2:	/* not supported yet */
		base += li;
		break;

	    case PI_ENCLOSE:
		if ((si == SPDU_DT && (s -> s_mask & SMASK_SPDU_GT))
		        || (si == SPDU_AI && !(s -> s_mask & SMASK_SPDU_AB))) {
		    s -> s_errno = SC_PROTOCOL;
		    break;
		}
		if ((s -> s_enclose = *base++) & ~ENCL_MASK)
		    s -> s_errno = SC_PROTOCOL;
		else
		    Set (SMASK_ENCLOSE);
		break;

	    case PI_RESYNC:
		s -> s_rs_type = *base++;
		if (SYNC_OK (s -> s_rs_type))
		    Set (SMASK_RS_TYPE);
		else
		    s -> s_errno = SC_PROTOCOL;
		break;

	    case PI_ACT_ID:
		switch (si) {
		    case SPDU_AS:
			s -> s_as_id.sd_len = li;
			bcopy (base, s -> s_as_id.sd_data, li);
			Set (SMASK_AS_ID);
			break;

		    case SPDU_AR:
			if ((s -> s_mask & SMASK_AR_OID)
				&& s -> s_ar_oid.sd_len == 0) {
			    s -> s_ar_oid.sd_len = li;
			    bcopy (base, s -> s_ar_oid.sd_data, li);
			}
			else {
			    s -> s_ar_id.sd_len = li;
			    bcopy (base, s -> s_ar_id.sd_data, li);
			    Set (SMASK_AR_ID);
			}
			break;

		    default:
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		base += li;
		break;

	    case PI_SERIAL: 
		switch (si) {
		    case SPDU_MIP: 
			s -> s_mip_serial = str2ssn (base, li);
			Set (SMASK_MIP_SERIAL);
			break;
		    case SPDU_MAP: 
			s -> s_map_serial = str2ssn (base, li);
			Set (SMASK_MAP_SERIAL);
			break;
		    case SPDU_MIA: 
			s -> s_mia_serial = str2ssn (base, li);
			Set (SMASK_MIA_SERIAL);
			break;
		    case SPDU_MAA: 
			s -> s_maa_serial = str2ssn (base, li);
			Set (SMASK_MAA_SERIAL);
			break;
		    case SPDU_RS: 
			s -> s_rs_serial = str2ssn (base, li);
			Set (SMASK_RS_SSN);
			break;
		    case SPDU_RA: 
			s -> s_ra_serial = str2ssn (base, li);
			Set (SMASK_RA_SSN);
			break;
		    case SPDU_AR: 
			s -> s_ar_serial = str2ssn (base, li);
			Set (SMASK_AR_SSN);
			break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		base += li;
		break;

	    case PI_MIA_DATA: 
	    case PI_UDATA: 
	    case PI_XDATA: 
		Set (SMASK_UDATA_PGI);
		if (!li)
		    break;
		if (si == SPDU_AB && !(s -> s_mask & SMASK_SPDU_AB)) {
		    s -> s_errno = SC_PROTOCOL;
		    break;
		}
		else
		    if (li > (code != PI_XDATA ? SEGMENT_MAX : CONNECT_MAX)) {
			s -> s_errno = SC_PROTOCOL;
			break;
		    }
		s -> s_udata = malloc ((unsigned) (s -> s_ulen = li));
		if (s -> s_udata == NULL) {
		    s -> s_errno = SC_CONGEST;
		    break;
		}
		bcopy (base, s -> s_udata, li);
		base += li;
		break;

	    case PI_REASON: 
		switch (si) {
		    case SPDU_RF: 
			s -> s_rdata = malloc ((unsigned) (s -> s_rlen = li));
			if (s -> s_rdata == NULL) {
			    s -> s_errno = SC_CONGEST;
			    break;
			}
			bcopy (base, s -> s_rdata, li);
			base += li;
			break;
		    case SPDU_ED: 
			s -> s_ed_reason = *base++;
			if (li == 1 && SP_OK (s -> s_ed_reason))
			    Set (SMASK_ED_REASON);
			else
			    s -> s_errno = SC_PROTOCOL;
			break;
		    case SPDU_AI: 
			If_Set (SMASK_SPDU_AB) {
			    s -> s_errno = SC_PROTOCOL;
			    break;
			}
			s -> s_ai_reason = *base++;
			if (li == 1 && SP_OK (s -> s_ai_reason))
			    Set (SMASK_AI_REASON);
			else
			    s -> s_errno = SC_PROTOCOL;
			break;
		    case SPDU_AD: 
			s -> s_ad_reason = *base++;
			if (li == 1 && SP_OK (s -> s_ad_reason))
			    Set (SMASK_AD_REASON);
			else
			    s -> s_errno = SC_PROTOCOL;
			break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		break;

	    case PI_REFLECT: 
		switch (si) {
		    case SPDU_AB: 
			If_Reset (SMASK_SPDU_AB) {
			    s -> s_errno = SC_PROTOCOL;
			    break;
			}
			if (li > AB_REFL_SIZE) {
			    s -> s_errno = SC_PROTOCOL;
			    break;
			}
			bcopy (base, (char *) s -> s_reflect, li);
			Set (SMASK_AB_REFL);
			break;
		    case SPDU_ER: 
			s -> s_udata = malloc ((unsigned) (s -> s_ulen = li));
			if (s -> s_udata == NULL) {
			    s -> s_errno = SC_CONGEST;
			    break;
			}
			bcopy (base, s -> s_udata, li);
			break;
		    default: 
			s -> s_errno = SC_PROTOCOL;
			break;
		}
		base += li;
		break;

	    case PI_SSAP_CALLING: 
		bcopy (base, s -> s_calling, s -> s_callinglen = li);
		Set (SMASK_CN_CALLING);
		base += li;
		break;

	    case PI_SSAP_CALLED: 
		bcopy (base, s -> s_called, s -> s_calledlen = li);
		Set (SMASK_CN_CALLED);
		base += li;
		break;

	    case PI_PREPARE: 
		if ((s -> s_pr_type = *base++) > PR_MAX)
		    s -> s_errno = SC_PROTOCOL;
		else
		    Set (SMASK_PR_TYPE);
		break;

	    default: 
		s -> s_errno = SC_PROTOCOL;
		break;
	}
    }
/* NB: caller responsible for mapping user info to s -> s_qbuf */

    if (cc)
	*cc = nread;
    {				/* "dangling" qbuf */
	register struct qbuf *qp;

	if ((qp = qb -> qb_forw) != qb && qp -> qb_len <= 0) {
	    remque (qp);

	    free ((char *) qp);
	}
    }

    switch (s -> s_code) {
	case SPDU_AB: 
	    If_Set (SMASK_SPDU_AB) {
		If_Reset (SMASK_AB_DISC)
		    s -> s_errno = SC_PROTOCOL;
	    }
	    break;

	case SPDU_MIP: 
	    If_Reset (SMASK_MIP_SERIAL)
		s -> s_errno = SC_PROTOCOL;
	    break;

	case SPDU_MAP: 
	    If_Reset (SMASK_MAP_SERIAL)
		s -> s_errno = SC_PROTOCOL;
	    break;

	case SPDU_MIA: 
	    If_Reset (SMASK_MIA_SERIAL)
		s -> s_errno = SC_PROTOCOL;
	    break;

	case SPDU_MAA: 
	    If_Reset (SMASK_MAA_SERIAL)
		s -> s_errno = SC_PROTOCOL;
	    break;

	case SPDU_RS: 
	    If_Reset (SMASK_RS_TYPE)
		s -> s_errno = SC_PROTOCOL;
	    If_Reset (SMASK_RS_SSN)
		s -> s_errno = SC_PROTOCOL;
	    break;

	case SPDU_RA: 
	    If_Reset (SMASK_RA_SSN)
		s -> s_errno = SC_PROTOCOL;
	    break;

	case SPDU_PR: 
	    If_Reset (SMASK_PR_TYPE)
		s -> s_errno = SC_PROTOCOL;
	    break;

	case SPDU_ED: 
	    If_Reset (SMASK_ED_REASON)
		s -> s_errno = SC_PROTOCOL;
	    break;

	case SPDU_AS: 
	    If_Reset (SMASK_AS_ID)
		s -> s_errno = SC_PROTOCOL;
	    break;

	case SPDU_AR: 
	    If_Reset (SMASK_AR_OID)
		s -> s_errno = SC_PROTOCOL;
	    If_Reset (SMASK_AR_SSN)
		s -> s_errno = SC_PROTOCOL;
	    If_Reset (SMASK_AR_ID)
		s -> s_errno = SC_PROTOCOL;
	    break;
    }

#ifdef	DEBUG
    if (ssap_log -> ll_events & LLOG_PDUS)
	spkt2text (ssap_log, s, 1);
#endif

    return s;
}

/*  */

struct ssapkt *newspkt (code)
int	code;
{
    register struct ssapkt *s;

    s = (struct ssapkt *) calloc (1, sizeof *s);
    if (s == NULL)
	return NULL;

    s -> s_code = code;
    s -> s_qbuf.qb_forw = s -> s_qbuf.qb_back = &s -> s_qbuf;

    return s;
}


int	freespkt (s)
register struct ssapkt *s;
{
    if (s == NULL)
	return;

    switch (s -> s_code) {
	case SPDU_RF: 
	    if (s -> s_rdata)
		free (s -> s_rdata);/* and fall... */

	default: 
	    if (s -> s_udata)
		free (s -> s_udata);
	    QBFREE (&s -> s_qbuf);
	    break;
    }

    free ((char *) s);
}
