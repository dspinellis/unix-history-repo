/* tsaperror.c - print out TPKT error codes */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/tsaperror.c,v 7.3 91/02/22 09:47:26 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/tsaperror.c,v 7.3 91/02/22 09:47:26 mrose Interim $
 *
 *
 * $Log:	tsaperror.c,v $
 * Revision 7.3  91/02/22  09:47:26  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/11  07:09:22  mrose
 * jpo
 * 
 * Revision 7.1  90/11/21  11:31:34  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  22:30:43  mrose
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
#include "tsap.h"
#include "tp4.h"
#ifdef	SUNLINK_7_0
#include <netosi/osi_layer.h>
#include <netosi/cons_impl.h>
#endif

/*  */

static char *disc_err0[] = {
    "Reason not specified",
    "Congestion at TSAP",
    "Session entity not attached to TSAP",
    "Address unknown"
};

static int  disc_err0_cnt = sizeof disc_err0 / sizeof disc_err0[0];


static char *disc_err8[] = {
    "Normal disconnect initiated by session entity",
    "Remote transport entity congestion at connect time",
    "Connection negotiation failed (proposed class(es) not supported)",
    "Duplicate source reference detected for the same pair of NSAPs",
    "Mismatched references",
    "Protocol error",
    "Not used",
    "Reference overflow",
    "Connect request refused on this network connection",
    "Not used",
    "Header or parameter length invalid",
    "Network disconnect",
    "Invalid parameter",
    "Invalid operation",
    "Timer expired",
    "Indications waiting"
};

static int  disc_err8_cnt = sizeof disc_err8 / sizeof disc_err8[0];

/*  */

#ifdef	SUNLINK_7_0
static char *cons_err0 = 
	"Unspecified (undefined) CONS error";

/* CONS Error 224 through 255 */
static char *cons_err1[] = {
	"CONS provider: undefined",	/* R_CP_OSI_NETWORK_SERVICE_PROBLEM  */
	"CONS provider: disconnection-transient",
		 		/* R_CP_DIS_TRANS */
	"CONS provider: disconnection-permanent",
				/* R_CP_DIS_PERM */
	"CONS provider: connection rejection - reason unspecified (transient)",
				/* R_CP_CON_REJ_UNSPEC_TRANS */
	"CONS provider: connection rejection - reason unspecified (permanent)",
				/* R_CP_CON_REJ_UNSPEC_PERM */
	"CONS provider: connection rejection - QOS not available (transient)",
				/* R_CP_CON_REJ_NO_QOS_TRANS */
	"CONS provider: connection rejection - QOS not available (permanent)",
				/* R_CP_CON_REJ_NO_QOS_PERM */
	"CONS provider: connection rejection - NSAP unreachable (transient)",
				/* R_CP_CON_REJ_NSAP_UNREACH_TRANS */
	"CONS provider: connection rejection - NSAP unreachable (permanent)",
				/* R_CP_CON_REJ_NSAP_UNREACH_PERM */
	"CONS provider: RESET - reason unspecified",
				/* R_CP_RESET_UNSPEC */
	"CONS provider: RESET - congestion",
				/* R_CP_RESET_CONGESTION */
	"CONS provider: connection rejection - NSAP address unknown (permanent)",
				/* R_CP_CON_REJ_NSAP_UNKNOWN_PERM */
	"CONS provider: 236",
				/* R_CP_X25_236 */
	"CONS provider: 237",
				/* R_CP_X25_237 */
	"CONS provider: 238",
				/* R_CP_X25_238 */
	"CONS provider: 239",
				/* R_CP_X25_239 */
 	"CONS user: undefined",
				/* R_CU_HIGHER_LEVEL_INITIATED = 240 */
	"CONS user: disconnection - normal",
				/* R_CU_DIS_NORMAL */
	"CONS user: disconnection - abnormal",
				/* R_CU_DIS_ABNORMAL */
	"CONS user: 243",
				/* R_CU_DIS_INCOMPAT */
	"CONS user: connection rejection - transient",
				/* R_CU_CON_REJ_UNSPEC_TRANS */
	"CONS user: connection rejection - permanent",
				/* R_CU_CON_REJ_UNSPEC_PERM */
	"CONS user: connection rejection - QOS not available (transient)",
				/* R_CU_CON_REJ_NO_QOS_TRANS */
	"CONS user: connection rejection - QOS not available (permanent)",
				/* R_CU_CON_REJ_NO_QOS_PERM */
	"CONS user: connection rejection - incompatible info in NS-user-data",
				/* R_CU_CON_REJ_INCOMPAT */
	"CONS user: 249",
				/* R_CU_CON_UNREC_PROTO */
	"CONS user: RESET - user resynchronization",
				/* R_CU_RESET_USER_RESYNCH */
	"CONS user: 251",
				/* R_CU_X25_251 */
	"CONS user: 252",
				/* R_CU_X25_252 */
	"CONS user: 253",
				/* R_CU_X25_253 */
	"CONS user: 254",
				/* R_CU_X25_254 */
	"CONS user: 255", 		
				/* R_CU_X25_255 */
};
#endif

/*  */

char *TErrString(code)
register int	code;
{
    register int    fcode;
    static char buffer[60];

#ifdef	SUNLINK_7_0
    if (code > 0xff) {
	    /* CONS error code */
	    code -= 0x100;
	    if (code == R_CONS_UNDEFINED) 
		    return cons_err0;
	    if (code >= R_CP_OSI_NETWORK_SERVICE_PROBLEM && 
		code <= R_CU_X25_255) 
		    return cons_err1[code - R_CP_OSI_NETWORK_SERVICE_PROBLEM];

	    (void) sprintf (buffer, "unknown CONS error code 0x%x", code);
	    return buffer;
    }
#endif

    code &= 0xff;
    if (code & DR_BASE) {
	if ((fcode = code & ~DR_BASE) < disc_err8_cnt)
	    return disc_err8[fcode];
    }
    else
	if (code < disc_err0_cnt)
	    return disc_err0[code];

    (void) sprintf (buffer, "unknown error code 0x%x", code);
    return buffer;
}
