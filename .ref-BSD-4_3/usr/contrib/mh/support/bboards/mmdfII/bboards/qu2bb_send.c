#ifndef	POP
/* qu2bb_send.c - manager for qu --> bb */
#else	POP
/* qu2po_send.c - manager for qu --> po */
#endif	POP


#include "util.h"
#include "mmdf.h"
#include "phs.h"
#include "ch.h"

/*  */

extern LLog *logptr;
extern char *supportaddr;

struct rp_construct rp_hend  = { /* end of host list */
    RP_NOOP,
    'e', 'n', 'd', ' ', 'o', 'f', ' ', 'h', 'o', 's', 't', ' ',
    'i', 'g', 'n', 'o', 'r', 'e', 'd', 
    NULL
};

struct rp_construct rp_aend = {	/* end of address list */
    RP_OK,
#ifndef	POP
    'b', 'b', 'o', 'a', 'r', 'd', 's', ' ', 'e', 'n', 'd', ' ',
#else	POP
    'p', 'o', 'p', ' ', 'e', 'n', 'd', ' ',
#endif	POP
    'o', 'f', ' ', 'a', 'd', 'd', 'r', ' ', 'l', 'i', 's', 't',
    NULL
};

struct rp_construct rp_badr = {	/* no such bboard */
    RP_USER,
    'u', 'n', 'k', 'n', 'o', 'w', 'n', ' ',
#ifndef	POP
    'b', 'b', 'o', 'a', 'r', 'd',
#else	POP
    'p', 'o', 'p', ' ', 's', 'u', 'b', 's', 'c', 'r', 'i', 'b', 'e', 'r',
#endif	POP
    NULL
};

struct rp_construct rp_err = {	/* error, retry later */
    RP_AGN,
    'u', 'n', 'k', 'n', 'o', 'w', 'n', ' ', 'e', 'r', 'r', 'o', 'r',
    NULL
};

/*  */

qu2bb_send (chanptr)
Chan *chanptr;
{
    short   result;
    char    info[LINESIZE],
            sender[LINESIZE];

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "qu2bb_send()");
#endif

    if (rp_isbad (result = qu_pkinit ()))
	return result;
    if (rp_isbad (result = bb_sbinit ()))
	return result;

    while (rp_gval ((result =
	    qu_rinit (info, sender, chanptr -> ch_apout))) == RP_OK) {
#ifdef	DEBUG
	ll_log (logptr, LLOGGEN, "info=%s sender=%s", info, sender);
#endif
	if (rp_isbad (result = bb_winit (info, sender)))
	    return result;
	if (rp_isbad (result = qu2bb_each (sender)))
	    return result;
	qu_rend();
    }
    qu_rend();

    if (rp_gval (result) != RP_DONE) {
	ll_log (logptr, LLOGTMP, "not DONE [%s]", rp_valstr (result));
	return RP_RPLY;
    }

    qu_pkend ();
    bb_sbend ();

    return result;
}

/*  */
LOCFUN
qu2bb_each (sender)
char   *sender;
{
    short   result;
    char    adr[LINESIZE],
            host[LINESIZE];
    RP_Buf replyval;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "qu2bb_each(sender='%s')", sender);
#endif

    FOREVER {			/* loop through the addresses */
	if (rp_isbad (result = qu_radr (host, adr)))
	    return result;
#ifdef	RP_HOK
	if (rp_gval (result) == RP_HOK)	{/* no-op the sub-list indication */
	    qu_wrply ((struct rp_bufstruct *) &rp_hend, rp_conlen (rp_hend));
	    continue;
	}
#endif	RP_HOK
	if (rp_gval (result) == RP_DONE) {
	    qu_wrply ((RP_Buf *) &rp_aend, rp_conlen (rp_aend));
	    return RP_OK;
	}

	switch (replyval.rp_val = bb_wtadr (host, adr)) {
	    case RP_AOK: 
	    case RP_OK: 
		switch (replyval.rp_val = bb_txtcpy ()) {
		    case RP_MOK: 
			replyval.rp_line[0] = NULL;
			break;
		    default: 
			ll_log (logptr, LLOGFAT, "unknown error [%s]",
				rp_valstr (replyval.rp_val));
			blt (&rp_err, (char *) &replyval, sizeof rp_err);
			break;
		}
		break;

	    case RP_USER: 
#ifndef	POP
		ll_log (logptr, LLOGFAT, "unknown bboard '%s'", adr);
#else	POP
		ll_log (logptr, LLOGFAT, "unknown pop subscriber '%s'", adr);
#endif	POP
		blt (&rp_badr, (char *) &replyval, sizeof rp_badr);
		break;

	    default: 
		ll_log (logptr, LLOGFAT, "unknown error [%s]",
			rp_valstr (replyval.rp_val));
		blt (&rp_err, (char *) &replyval, sizeof rp_err);
		break;
	}

	qu_wrply (&replyval,
		(sizeof replyval.rp_val) + strlen (replyval.rp_line));
    }
}
