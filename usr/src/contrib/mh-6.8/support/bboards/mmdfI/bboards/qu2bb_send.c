/* qu2bb_send.c - manager for qu --> bb */

#include "util.h"
#include "mmdf.h"

/*  */

extern struct ll_struct *logptr;

struct rp_construct rp_aend = {	/* end of address list */
    RP_OK,
    'b', 'b', 'o', 'a', 'r', 'd', 's', ' ', 'e', 'n', 'd', ' ',
    'o', 'f', ' ', 'a', 'd', 'd', 'r', ' ', 'l', 'i', 's', 't',
    NULL
};

struct rp_construct rp_badr = {	/* no such bboard */
    RP_USER,
    'u', 'n', 'k', 'n', 'o', 'w', 'n', ' ', 'b', 'b', 'o', 'a', 'r', 'd',
    NULL
};

struct rp_construct rp_err = {	/* error, retry later */
    RP_AGN,
    'u', 'n', 'k', 'n', 'o', 'w', 'n', ' ', 'e', 'r', 'r', 'o', 'r',
    NULL
};

/*  */

qu2bb_send () {
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

    while (rp_gval ((result = qu_rinit (info, sender))) == RP_OK) {
#ifdef	DEBUG
	ll_log (logptr, LLOGGEN, "info=%s sender=%s", info, sender);
#endif
	if (rp_isbad (result = bb_winit (info, sender)))
	    return result;
	if (rp_isbad (result = qu2bb_each (sender)))
	    return result;
    }

    if (rp_gval (result) != RP_DONE) {
	ll_log (logptr, LLOGTMP, "not DONE [%s]", rp_valstr (result));
	return RP_RPLY;
    }

    qu_pkend ();
    bb_sbend ();

    return result;
}

/*  */

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
	if (rp_gval (result) == RP_DONE) {
	    qu_wrply ((RP_Buf *) & rp_aend, rp_conlen (rp_aend));
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
			blt (&rp_err, (char *) & replyval, sizeof rp_err);
			break;
		}
		break;

	    case RP_USER: 
		ll_log (logptr, LLOGFAT, "unknown bboard '%s'", adr);
		blt (&rp_badr, (char *) & replyval, sizeof rp_badr);
		break;

	    default: 
		ll_log (logptr, LLOGFAT, "unknown error [%s]",
			rp_valstr (replyval.rp_val));
		blt (&rp_err, (char *) & replyval, sizeof rp_err);
		break;
	}

	qu_wrply (&replyval,
		(sizeof replyval.rp_val) + strlen (replyval.rp_line));
    }
}
