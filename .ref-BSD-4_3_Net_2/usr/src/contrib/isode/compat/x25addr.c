/* x25addr.c - X.25 level generic <-> interface address munging */

#ifndef lint
static char *rcsid = "$Header: /f/osi/compat/RCS/x25addr.c,v 7.6 91/02/22 09:16:14 mrose Interim $";
#endif

/*
 * $Header: /f/osi/compat/RCS/x25addr.c,v 7.6 91/02/22 09:16:14 mrose Interim $
 *
 * Contributed by George Michaelson, Julian Onions, and John Pavel
 *
 *
 * $Log:	x25addr.c,v $
 * Revision 7.6  91/02/22  09:16:14  mrose
 * Interim 6.8
 * 
 * Revision 7.5  91/01/14  13:33:56  mrose
 * loader
 * 
 * Revision 7.4  91/01/07  12:40:04  mrose
 * update
 * 
 * Revision 7.3  90/07/09  14:32:30  mrose
 * sync
 * 
 * Revision 7.2  89/12/11  16:21:42  mrose
 * comments
 * 
 * Revision 7.1  89/12/11  01:36:14  mrose
 * SUN_X25_HACK
 * 
 * Revision 7.0  89/11/23  21:23:49  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

/*
 * for *really* generic address translation
 */

#include <errno.h>
#include <stdio.h>
#include "general.h"
#include "manifest.h"

#ifdef X25
#include "tailor.h"
#include "tpkt.h"
#include <sys/file.h>
#include "x25.h"

#ifndef	DEBUG
#define	DEBUG
#endif

/*  */

/*
 * convert from the generic X25 structure to interface specific
 */
/* ARGSUSED */
CONN_DB *gen2if (generic, specific, context)
struct NSAPaddr *generic;
CONN_DB         *specific;
int             context;
{
    int     dtelen;
    char    dte[NSAP_DTELEN + 1];
#ifdef CAMTEC_CCL
    struct iovec *iov;
#endif

    if (generic == NULLNA
	    || specific == (CONN_DB *) 0
	    || generic -> na_stack != NA_X25)
	return (CONN_DB *)0;

    if (x25_dnic_prefix && *x25_dnic_prefix) {
		/* need DNIC on local calls? */
	register int    i;

	if ( strncmp(generic -> na_dte, x25_dnic_prefix,
	    i = strlen(x25_dnic_prefix)) == 0 )
	    {
	    if (x25_strip_dnic) bcopy(generic -> na_dte + i, dte, dtelen =
		generic -> na_dtelen - i);
	    else bcopy (generic -> na_dte, dte, dtelen = generic -> na_dtelen);
	    }
	else
	    if (x25_intl_zero)
		{
		bcopy(generic -> na_dte, dte + 1, dtelen = generic-> na_dtelen);
		*dte = '0', dtelen++;
		}
	    else bcopy(generic -> na_dte, dte, dtelen = generic -> na_dtelen);

    }
    else bcopy (generic -> na_dte, dte, dtelen = generic -> na_dtelen);
    dte[dtelen] = NULL;

#ifdef	SUN_X25_HACK
	/*	
	 * If your X.25 provider expects to receive the subaddress alone
	 * on listen requests, and you are using SunLink X.25, you may need
	 * to enable SUN_X25_HACK in your config file. This will allow you
	 * to use x25_local_dte in isotailor to specify a dte mask to be
	 * stripped when listening, and thus use full DTE strings in
	 * isoentities and QUIPU EDB files. You will also have to use the 
	 * tsapd -a <dte> option to specify the listen address in
	 * /etc/rc.local and other tsapd startups since by default this equals
	 * x25_local_dte and thus will be masked to <null> unless overridden
	 * with full DTE + subaddress. 
	 */

	/* 
	 * in ADDR_LISTEN context, it may be neccessary to only listen
	 * on the sub-address, because certain PTT-provided networks
	 * remove the local DTE from incoming CR packets. 
	 *
	 * SunLink X.25 listen asserts whatever DTE it is given as a simple
	 * string-compare, and will never receive inbound calls that bear
	 * only the sub-address if you assert the full DTE.
	 *
	 * this behaviour is orthogonal to any requirements to remove DNIC
	 * or add a leading 0 on outbound calls, and so needs a separate
	 * test. It uses tailor variable x25_local_dte to assert the local 
	 * DTE *without* subaddress which should be tested for and stripped 
	 * when detected. 
	 */

    if ((context == ADDR_LISTEN) && x25_local_dte && *x25_local_dte) 
	{
	register int    i;

	if ( strncmp(generic -> na_dte, x25_local_dte,
	    i = strlen(x25_local_dte)) == 0 ) 
	    {
	    bcopy(generic -> na_dte + i, dte, dtelen =
		generic -> na_dtelen - i);
    	    dte[dtelen] = NULL;
	    }
	}
#endif

    DLOG (x25_log, LLOG_DEBUG,
	   ("gen2if %s -> %s, %d octets; PID %s",
	    generic -> na_dte, dte, dtelen,
	    sel2str (generic -> na_pid, (int) generic -> na_pidlen,1)));


#ifndef CAMTEC_CCL
    bzero ((char *)specific, sizeof *specific);
#endif

#ifdef UBC_X25
    if ((specific -> xaddr_len = dtelen) != 0)  {
	bcopy (dte, specific -> xaddr_addr,
	       dtelen);
	specific -> xaddr_len = dtelen;
	specific -> xaddr_facilities = 0;
	bcopy (generic -> na_pid, specific -> xaddr_proto,
	       generic -> na_pidlen);
	bcopy (generic -> na_cudf, specific -> xaddr_userdata,
	       generic -> na_cudflen);
    }
#endif

#ifdef SUN_X25
    specific -> hostlen = char2bcd (dte, specific -> host);

    /* Zero PID */
    if (generic -> na_pidlen) { /* non-null PID */
	if (generic -> na_pidlen > NPSIZE) {
	    SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
		  ("PID too long (%d > %d)", generic -> na_pidlen, NPSIZE));
	    return (CONN_DB *)0;
	} else {
	    bzero((char *)specific -> data, NPSIZE);
	    bcopy (generic -> na_pid, (char *)specific -> data,
		   generic -> na_pidlen);
	    bcopy (generic -> na_cudf, (char *) specific -> data + NPSIZE,
		   generic -> na_cudflen);
	    specific -> datalen = generic -> na_pidlen + generic -> na_cudflen;
	}
    } else { /* Null PID (just copy in CUDF, the first four octets of which
		will be the PID in any case) */
	 bcopy (generic -> na_cudf, (char *)specific -> data,
		generic -> na_cudflen);
	 specific -> datalen = generic -> na_cudflen;
    }
#endif

#ifdef CAMTEC_CCL
    switch (context) {
	case ADDR_REMOTE:
	    iov = &(specific -> ccl_iovec[0]);
	    if (x25_outgoing_port == '#') {
		char *a, *b;
		int i;

		iov -> iov_len = dtelen + 4;
		bzero(iov -> iov_base, iov -> iov_len + 1);
		a = iov -> iov_base;
		b = dte;
		*a++ = '#';
		*a++ = '[';
		for (i = 0; i < dtelen; i++) {
			if (i == 2) *a++ = ':';
			else if (i == 14) *a++ = ']';
			*a++ = *b++;
		}
	    }
	    else {
		iov -> iov_len = dtelen+1;
		bcopy(dte, (iov -> iov_base)+1, dtelen);
		*(iov -> iov_base) = x25_outgoing_port;
	    }
	    break;

	case ADDR_LOCAL:
	    iov = &(specific -> ccl_iovec[0]);
	    strncpy(iov -> iov_base, generic -> na_dte, generic -> na_dtelen);
	    iov -> iov_base[generic -> na_dtelen] = '\0';
	    return (specific);

	case ADDR_LISTEN:
	    iov = &(specific -> ccl_iovec[0]);
	    if (generic -> na_pidlen)
		{                       /* listen on a PID */
		register int i;
		iov -> iov_base[0] = 'C';
		bcopy(generic -> na_pid, iov -> iov_base + 1,
		    i = generic -> na_pidlen);
		iov -> iov_len = i + 1;
		}
	    else
	    if (generic -> na_dtelen < 6)
		{           /* listen on a subaddress */
		register int i;
		iov -> iov_base[0] = 'S';
		bcopy(generic -> na_dte, iov -> iov_base + 1,
		    i = generic -> na_dtelen);
		iov -> iov_len = i + 1;
		}
	    else    /* full DTE */
		bcopy(dte, iov -> iov_base,
		    iov -> iov_len = dtelen);
	    return (specific);
    }
    /*
     * CUDF & PID must be merged. malloc initailly PIDsize space
     * and bzero it. this may be UK net specific action which
     * ensures we do NOT fall foul of listeners which use pid
     * to match as well as "true" cudf & DTE.
     */

    (iov = &(specific -> ccl_iovec[2])) -> iov_len = 0;
    if (generic -> na_faclen != 0)
	bcopy (generic -> na_fac, iov -> iov_base,
	    iov -> iov_len = min( generic -> na_faclen, FACSIZE) );
    iov++;
    if ( (iov -> iov_len = generic -> na_pidlen) != 0)
	bcopy (generic -> na_pid, iov -> iov_base, generic -> na_pidlen);

    /*
     * if there is any other user data add that in now...
     * actually cudf is a variable length field so this is
     * all very suspect.
     */

    if (generic -> na_cudflen != 0)
	bcopy(generic -> na_cudf, iov -> iov_base + iov -> iov_len,
	    generic -> na_cudflen), iov -> iov_len += generic -> na_cudflen;
#endif

    return(specific);
}

/*  */

/*
 * convert from interface specific format to generic X.25 structure
 */
/* ARGSUSED */
struct NSAPaddr *if2gen (generic, specific, context)
struct NSAPaddr *generic;
CONN_DB         *specific;
int             context;
{
    int     dtelen;
    char    dte[NSAP_DTELEN + 1];
#ifdef CAMTEC_CCL
    struct iovec *iov;
#endif

    if (generic == NULLNA || specific == (CONN_DB *) 0)
	return NULLNA;
    bzero ((char *)generic, sizeof *generic);
    bzero (dte, sizeof dte);
    dtelen = 0;

    generic -> na_stack = NA_X25;
    generic -> na_community = ts_comm_x25_default;

#ifdef UBC_X25
    if (specific -> xaddr_len  != 0) {
	bcopy (specific -> xaddr_addr, dte, specific -> xaddr_len);
	dtelen = specific -> xaddr_len;
	bcopy (specific -> xaddr_proto, generic -> na_pid,
				sizeof(specific -> xaddr_proto));
	generic -> na_pidlen = sizeof specific -> xaddr_proto;
	bcopy (specific -> xaddr_userdata, generic -> na_cudf,
				sizeof(specific -> xaddr_userdata));
	generic -> na_cudflen = sizeof specific -> xaddr_userdata;
    }
#endif

#ifdef SUN_X25
    dtelen = bcd2char (specific -> host, dte, (int) specific -> hostlen);

    if (specific -> datalen > NPSIZE) { /* have some real user data after the PID */
	bcopy((char *)specific -> data, generic -> na_pid,
	      generic -> na_pidlen = NPSIZE);
	bcopy((char *) specific -> data + generic -> na_pidlen,
	      generic -> na_cudf,
	      generic -> na_cudflen = specific -> datalen - generic -> na_pidlen);
    }
    else { /* PID only */
	bcopy((char *)specific -> data, generic -> na_pid,
	      generic -> na_pidlen = specific -> datalen);
	generic -> na_cudflen = 0;
    }

#endif

#ifdef CAMTEC_CCL
    switch (context) {
    case ADDR_REMOTE:

	iov = &(specific -> ccl_iovec[1]);
	if (iov -> iov_len) {
		if (*(iov->iov_base) == '#') {
			char *a;

			a = iov -> iov_base;
			while (*a && iov -> iov_len) {
				if (*a == ']') {
					iov -> iov_len--;
					a++;
					break;
				}
				iov -> iov_len--;
				a++;
			}
			if (*a == 0 || iov -> iov_len == 0)
				dtelen = 0;
			else {
				dtelen = iov -> iov_len;
				bcopy (a, dte, dtelen);
			}
		}
		else {
			dtelen = iov -> iov_len - 1;
			bcopy ((iov -> iov_base)+1, dte,
				dtelen);
		}
	}
	else dtelen = 0;
	break;

    case ADDR_LOCAL:
	iov = &(specific -> ccl_iovec[0]);
	if (iov -> iov_len) {
		dtelen = iov -> iov_len -1;
		bcopy ((iov -> iov_base)+1, dte,
			dtelen);
	}
	else dtelen = 0;
	break;

    case ADDR_LISTEN:
	return NULLNA;
    }

    if ( (iov = &(specific -> ccl_iovec[2])) -> iov_len )
	bcopy( iov -> iov_base, generic -> na_fac,
	    generic -> na_faclen = min( iov -> iov_len, FACSIZE));

    if ( ++iov -> iov_len)
	{
	bcopy( iov -> iov_base, generic -> na_pid,
	    generic -> na_pidlen = min( iov -> iov_len, NPSIZE));
	if ( iov -> iov_len > NPSIZE)
	    bcopy( iov -> iov_base + NPSIZE, generic -> na_cudf,
		generic -> na_cudflen = min(iov -> iov_len - NPSIZE, CUDFSIZE));
	}
#endif

    if (x25_dnic_prefix && *x25_dnic_prefix) {
	register int    i;

	i = 0;
	if (x25_intl_zero && dte[0] == '0' && dte[1] != '0')
	    i = 1;
	else
	    if (x25_dnic_prefix
		    && *x25_dnic_prefix
		    && x25_strip_dnic
		    && dtelen < 12)   /* local call... */
		bcopy (x25_dnic_prefix, generic -> na_dte,
		       generic -> na_dtelen = strlen (x25_dnic_prefix));

	bcopy (dte + i, generic -> na_dte + generic -> na_dtelen, dtelen - i);
	generic -> na_dtelen += dtelen - i;
    }
    else
	bcopy (dte, generic -> na_dte, generic -> na_dtelen = dtelen);

    DLOG (x25_log, LLOG_DEBUG,
	   ("if2gen %s -> %s, %d octets; PID %s",
	    dte, generic -> na_dte, generic -> na_dtelen,
	    sel2str (generic -> na_pid, (int) generic -> na_pidlen,1)));

    return(generic);
}

/*  */

elucidate_x25_err (flags, pkt)
int flags;
char * pkt;
{
    char * cp;

    if (flags & (1 << RECV_DIAG)) {
	SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
	      (( flags & (1 << DIAG_TYPE) ) ? "cleared 0x%02x" : "reset 0x%02x",
	       pkt[0] ));

	if ((flags) & (1 << DIAG_TYPE)) /* cleared */
	    switch(pkt[0]) {
		case 0x00:
		    cp = "DTE Clearing";
		    break;

		case 0x01:
		    cp = "Number Busy";
		    break;

		case 0x09:
		    cp = "Out of Order";
		    break;

		case 0x11:
		    cp = "Remote Procedure Error";
		    break;

		case 0x19:
		    cp = "Reverse Charging not subscribed";
		    break;

		case 0x03:
		    cp = "Invalid Facility Request";
		    break;

		case 0x0B:
		    cp = "Access Barred";
		    break;

		case 0x13:
		    cp = "Local Procedure Error";
		    break;

		case 0x05:
		    cp = "Network Congestion";
		    break;

		case 0x0D:
		    cp = "Not Obtainable";
		    break;

		case 0x21:
		    cp = "DTE Incompatible Call";
		    break;

		case 0x29:
		    cp = "Fast Select Acceptance not Subscribed";
		    break;

		default:
		    SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
			  ("clearing cause 0x2%x", pkt[0]));
		    goto next;
	    }
	else /* reset */
	    switch(pkt[0]) {
		case 0x00:
		    cp = "DTE Reset";
		    break;

		case 0x01:
		    cp = "Out of Order (PVC Only)";
		    break;

		case 0x03:
		    cp = "Remote Procedure Error";
		    break;

		case 0x05:
		    cp = "Local Procedure Error";
		    break;

		case 0x07:
		    cp = "Network Congestion";
		    break;

		case 0x09:
		    cp = "Remote DTE Operational (PVC Only)";
		    break;

		case 0x0F:
		    cp = "Network Operational (PVC Only";
		    break;

		default:
		    SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
			  ("resetting cause 0x%2x", pkt[0]));
		    goto next;

	    }
	SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP, ("%s%s",
	    ( flags & (1 << DIAG_TYPE) ) ? "clearing cause " :
	    "resetting cause ", cp));

next: ;
	/* The following may only be applicable to PSS in the UK */
	/* In any case, if someone is keen, they can stuff it all
	   into a text file and read it out */

	switch (pkt[1]) {
	    case 0x00:
		cp = "NO ADDITIONAL INFORMATION";
		break;

	    case 0x01:
		cp = "INVALID P(S)\tRESET";
		break;

	    case 0x02:
		cp = "INVALID P(R)\tRESET";
		break;

	    case 0x11:
		cp = "PACKET TYPE INVALID FOR STATE r1\tRESTART";
		break;

	    case 0x12:
		cp = "PACKET TYPE INVALID FOR STATE r2\tRESTART";
		break;

	    case 0x13:
		cp = "PACKET TYPE INVALID FOR STATE r3\tRESTART";
		break;

	    case 0x14:
		cp = "PACKET TYPE INVALID FOR STATE p1\tCLEAR";
		break;

	    case 0x15:
		cp = "PACKET TYPE INVALID FOR STATE p2\tCLEAR";
		break;

	    case 0x16:
		cp = "PACKET TYPE INVALID FOR STATE p3\tCLEAR";
		break;

	    case 0x17:
		cp = "PACKET TYPE INVALID FOR STATE p4\tCLEAR";
		break;

	    case 0x18:
		cp = "PACKET TYPE INVALID FOR STATE p5\tRESET";
		break;

	    case 0x19:
		cp = "PACKET TYPE INVALID FOR STATE p6\tCLEAR";
		break;

	    case 0x1A:
		cp = "PACKET TYPE INVALID FOR STATE p7\tCLEAR";
		break;

	    case 0x1B:
		cp = "PACKET TYPE INVALID FOR STATE d1\tRESET";
		break;

	    case 0x1C:
		cp = "PACKET TYPE INVALID FOR STATE d2\tRESET";
		break;

	    case 0x1D:
		cp = "PACKET TYPE INVALID FOR STATE d3\tRESET";
		break;

	    case 0x20:
		cp = "PACKET NOT ALLOWED";
		break;

	    case 0x21:
		cp = "UNIDENTIFIABLE PACKET";
		break;

	    case 0x22:
		cp = "CALL ON ONE-WAY LOGICAL CHANNEL\tCLEAR";
		break;

	    case 0x23:
		cp = "INVALID PACKET TYPE ON PVC\tRESET";
		break;

	    case 0x24:
		cp = "PACKET ON UNASSIGNED LCN\tCLEAR";
		break;

	    case 0x25:
		cp = "REJECT NOT SUBSCRIBED TO\tRESET";
		break;

	    case 0x26:
		cp = "PACKET TOO SHORT\tRESET";
		break;

	    case 0x27:
		cp = "PACKET TOO LONG\tRESET";
		break;

	    case 0x28:
		cp = "INVALID GFI\tCLEAR";
		break;

	    case 0x29:
		cp = "RESTART WITH NON-ZERO BITS 5-16";
		break;

	    case 0x2A:
		cp = "PACKET TYPE NOT COMPATIBLE WITH FACILITY\tCLEAR";
		break;

	    case 0x2B:
		cp = "UNAUTHORISED INTERRUPT CONF\tRESET";
		break;

	    case 0x2C:
		cp = "UNAUTHORISED INTERRUPT\tRESET";
		break;

	    case 0x31:
		cp = "TIMER EXPIRED;  INCOMING CALL";
		break;

	    case 0x32:
		cp = "TIMER EXPIRED;\tCLEAR INDICATION";
		break;

	    case 0x33:
		cp = "TIMER EXPIRED;\tRESET INDICATION";
		break;

	    case 0x34:
		cp = "TIMER EXPIRED;\tRESTART IND";
		break;

	    case 0x40:
		cp = "UNSPECIFIED CALL SET-UP PROBLEM CLEAR";
		break;

	    case 0x41:
		cp = "FACILITY CODE NOT ALLOWED\tCLEAR";
		break;

	    case 0x42:
		cp = "FACILITY PARAMETER NOT ALLOWED\tCLEAR";
		break;

	    case 0x43:
		cp = "INVALID CALLED ADDRESS\tCLEAR";
		break;

	    case 0x44:
		cp = "INVALID CALLING ADDRESS\tCLEAR";
		break;

	    case 0x90:
		cp = "DTE/DCE CONGESTION\tRESET";
		break;

	    case 0x91:
		cp = "RECEIVED FAST SELECT CLEAR REQUEST";
		break;

	    case 0x92:
		cp = "LINE RESTARTING BY INMC COMMAND\tRESTART";
		break;

	    case 0xA0:
		cp = "NON-ZERO RESET CAUSE FROM DTE\tRESET";
		break;

	    case 0xA1:
		cp = "DATA PACKET TOO LONG\tRESET";
		break;

	    case 0xA2:
		cp = "INTERRUPT PACKET TOO LONG\tRESET";
		break;

	    case 0xA3:
		cp = "INT PACKET TOO SHORT; NO USER DATA\tRESET";
		break;

	    case 0xA4:
		cp = "INT CONFIRMATION PACKET TOO LONG\tRESET";
		break;

	    case 0xA5:
		cp = "RR PACKET TOO LONG\tRESET";
		break;

	    case 0xA6:
		cp = "RNR PACKET TOO LONG\tRESET";
		break;

	    case 0xA7:
		cp = "RESET PACKET TOO LONG\tRESET";
		break;

	    case 0xA8:
		cp = "RESET CONF PACKET TOO LONG\tRESET";
		break;

	    case 0xA9:
		cp = "INVALID `Q' BIT IN DATA PACKET\tRESET";
		break;

	    case 0xAA:
		cp = "PACKET WINDOW RANGE EXCEEDED\tRESET";
		break;

	    case 0xAB:
		cp = "UNABLE TO TRANSMIT PACKET\tRESET";
		break;

	    case 0xAC:
		cp = "diagnostic `Q' BIT SET IN NON-DATA PACKET\tRESET";
		break;

	    case 0xAD:
		cp = "OUTSTANDING PACKET COUNT LESS THAN ZERO\tRESET";
		break;

	    case 0xAE:
		cp = "RETRANSMISSION ERROR\tRESET";
		break;

	    case 0xAF:
		cp = "RESET PACKET TOO SHORT (NO CAUSE)\tRESET";
		break;

	    case 0xB0:
		cp = "REJECT PACKET TOO LONG\tRESET";
		break;

	    case 0xB1:
		cp = "INVALID 1D PACKET\tRESET";
		break;

	    case 0xB2:
		cp = "UNSUCCESSFUL RECONNECTION RESNC\tCLEAR";
		break;

	    case 0xB3:
		cp = "NON-RECONNECT CALL IN STATE C1\tCLEAR";
		break;

	    case 0xB4:
		cp = "SECOND 1D PACKET FROM DTE\tCLEAR";
		break;

	    case 0xB5:
		cp = "BAD DATA TRANSFER STATE IN RECONNECT\tCLEAR";
		break;

	    case 0xB6:
		cp = "PACKET FORMAT INVALID\tCLEAR";
		break;

	    case 0xB7:
		cp = "FACILITY BYTE COUNT TOO LARGE\tCLEAR";
		break;

	    case 0xB8:
		cp = "INVALID PACKET DETECTED\tCLEAR";
		break;

	    case 0xB9:
		cp = "FACILITY/UTILITY FIELD BYTE COUNT > 63\tCLEAR";
		break;

	    case 0xBA:
		cp = "OUTGOING CALLS BARRED\tCLEAR";
		break;

	    case 0xBB:
		cp = "INCOMING CALLS BARRED\tCLEAR";
		break;

	    case 0xBC:
		cp = "CLEARING OF PVC\tCLEAR";
		break;

	    case 0xBD:
		cp = "CALLED ADDRESS TOO LONG\tCLEAR";
		break;

	    case 0xBE:
		cp = "CALLED ADDRESS TOO SHORT\tCLEAR";
		break;

	    case 0xBF:
		cp = "CALLING ADDRESS TOO LONG\tCLEAR";
		break;

	    case 0xC0:
		cp = "CALLING ADDRESS TOO SHORT\tCLEAR";
		break;

	    case 0xC1:
		cp = "BCD ERROR IN CALL ADDRESS\tCLEAR";
		break;

	    case 0xC2:
		cp = "BCD ERROR IN CALLING ADDRESS\tCLEAR";
		break;

	    case 0xC3:
		cp = "USER DATA FIELD TOO LONG\tCLEAR";
		break;

	    case 0xC4:
		cp = "NO BUFFER AVAILABLE\tCLEAR";
		break;

	    case 0xC5:
		cp = "LOCAL DTE IS NOT ENHANCED\tCLEAR";
		break;

	    case 0xC6:
		cp = "FACILITY NEGOTIATION INVALID\tCLEAR";
		break;

	    case 0xC7:
		cp = "MANDATORY UTILITY NOT INPUT\tCLEAR";
		break;

	    case 0xC8:
		cp = "BUFFER NO AVAILABLE FOR TNIC\tCLEAR";
		break;

	    case 0xC9:
		cp = "OVERFLOW OF TNIC IN BUFFER\tCLEAR";
		break;

	    case 0xCA:
		cp = "DTE LINE CONGESTED\tCLEAR";
		break;

	    case 0xCB:
		cp = "TABLE ERROR IN PACKET PROCEDURES";
		break;

	    case 0xCC:
		cp = "INSERT TABLE OVERFLOW";
		break;

	    case 0xCD:
		cp = "DELETE TABLE OVERFLOW";
		break;

	    case 0xD0:
		cp = "TRUNK LINE RESTART\tRESTART";
		break;

	    case 0xD1:
		cp = "INVALID EVENT IN STATE p2";
		break;

	    case 0xD2:
		cp = "INVALID EVENT IN STATE p3";
		break;

	    case 0xD3:
		cp = "INVALID 1D EVENT IN STATE d1";
		break;

	    case 0xD4:
		cp = "CALL COLLISION ON TRUNK LINE";
		break;

	    case 0xD5:
		cp = "NO BUFFER AVAILABLE";
		break;

	    case 0xD6:

		cp = "CALL COLLISION ON DTE LINE";
		break;

	    case 0xD7:
		cp = "DTE RESTART";
		break;

	    case 0xD8:
		cp = "CALL REQUEST TO TRUNK LINE TIMEOUT";
		break;

	    case 0xD9:
		cp = "RECONNECT SET-UP TIMED OUT";
		break;

	    case 0xDA:
		cp = "INVALID OUTPUT SIDE STATE";
		break;

	    case 0xDB:
		cp = "ERROR DETECTED IN BLINK PACKET QUEUE PROCEDURE";
		break;

	    case 0xDC:
		cp = "RESET INDICATION RETRANSMISSION COUNT EXPIRED";
		break;

	    case 0xDD:
		cp = "INVALID OUTPUT SIDE STATE";
		break;

	    case 0xDE:
		cp = "BLIND BUFFER QUEUE OVERFLOW IN STATE d4";
		break;

	    case 0xDF:
		cp = "BLIND BUFFER QUEUE OVERFLOW IN STATE c1";
		break;

	    case 0xE0:
		cp = "BLIND BUFFER QUEUE OVERFLOW IN STATE c2";
		break;

	    case 0xE1:
		cp = "CLEAR PACKET BYTE COUNT TOO LARGE OR TOO SMALL";
		break;

	    case 0xE2:
		cp = "NON-ZERO\tCLEAR CAUSE";
		break;

	    case 0xE3:
		cp = "CLEAR CONF PACKET BYTE COUNT TOO SMALL OR TOO LARGE";
		break;

	    case 0xE4:
		cp = "CALL COLLISION";
		break;

	    case 0xE5:
		cp = "INVALID TP LOAD REQUEST CALL PKT";
		break;

	    case 0xE6:
		cp = "MAXIMUM HOPCOUNT EXCEEDED";
		break;

	    case 0xE7:
		cp = "ROUTING LOOP DETECTED";
		break;

	    case 0xE8:
		cp = "PVC CALL REQUEST FAILURE";
		break;

	    case 0xE9:
		cp = "RECONNECT CALL REQUEST FAILED";
		break;

	    case 0xEA:
		cp = "NO LC AVAILABLE ON OUTPUT SIDE";
		break;

	    case 0xEB:
		cp = "NO BUFFER AVAILABLE";
		break;

	    case 0xEC:
		cp = "CALL REDIRECTION CLEAR";
		break;

	    case 0xED:
		cp = "NO PATH ROUTE CALL";
		break;

	    case 0xEE:
		cp = "CALL ROUTED TO DTE LINE";
		break;

	    case 0xEF:
		cp = "CALL CANNOT BE REROUTED";
		break;

	    case 0xF0:
		cp = "ADDRESS NOT IN ROUTING TABLES";
		break;

	    case 0xF1:
		cp = "ROUTING TABLE CHANGE DURING CALL ROUTING";
		break;

	    case 0xF2:
		cp = "NO LC AVAILABLE ON FAKE TRUNK";
		break;

	    case 0xF3:
		cp = "REMOTE DTE DOWN ON A PVC";
		break;

	    case 0xF4:
		cp = "INVALID EVENT DETECTED";
		break;

	    case 0xF5:
		cp = "INVALID PACKET RECEIVED; STATE d4";
		break;

	    case 0xF6:
		cp = "INVALID PACKET RECEIVED; STATE d5";
		break;

	    case 0xF7:
		cp = "INVALID PACKET RECEIVED; STATE p8";
		break;

	    case 0xF8:
		cp = "INTERNAL PROCESSING FAILURE";
		break;

	    case 0xF9:
		cp = "INVALID RESTART INDICATION";
		break;

	    case 0xFA:
		cp = "LINE STATUS CHANGE IN STATE r4";
		break;

	    case 0xFB:
		cp = "INVALID PACKET RECEIVED; STATE r4";
		break;

	    case 0xFC:
		cp = "INVALID PACKET RECEIVED; STATE r3";
		break;

	    case 0xFD:
		cp = "LINE STATUS CHANGE IN STATE r2";
		break;

	    case 0xFE:
		cp = "LINE STATUS CHANGE IN STATE r1";
		break;

	    case 0xFF:
		cp = "LINE STATUS CHANGE IN STATE r0";
		break;

	    default:
		SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
		      ("diagnostic: 0x%2x", pkt[1]));
		goto done;
	}
	SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP, ("diagnostic %s", cp));
    }
    else        /* Not RECV_DIAG */
	if (flags)
	    SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
		  ("diag flags: 0x%02x", flags));

done: ;
    return OK;
}

/*  */

#ifdef  SUN_X25
static int  char2bcd (s, d)
register char   *s;
register u_char *d;
{
    register int    c,
		    i;

    for (i = 0; *s; i++) {
	if ((c = *s++) >= 'a' && c <= 'f')
	    c -= 'a' + 0x0a;
	else
	    if (c >= 'A' && c <= 'F')
		c -= 'A' + 0x0a;
	    else
		if (c >= '0' && c <= '9')
		    c -= '0';
		else
		    c = 0;

	if (i & 1)
	    *d++ |= c & 0xf;
	else
	    *d = (c & 0xf) << 4;
    }

    return i;
}

/*  */

static int     bcd2char (s, d, len)
register    u_char *s;
register char  *d;
int     len;
{
    register int    i,
		    g;

    for (i = 0; i < len; i++) {
	g = s[i >> 1];
	if ((i & 1) == 0)
	    g >>= 4;
	g &= 0xf;

	if (g < 0x0a)
	    *d++ = g + '0';
	else
	    *d++ = g + 'a' - 0x0a;
    }

    *d = NULL;

    return len;
}
#endif
#else
int	_x25addr_stub () {};
#endif
