#define	MFDEBUG			/* temporarily */
#ifndef	lint
static char Id[] = "@(#)$Id: qu2uu_send.c,v 1.2 1993/08/25 17:32:12 jromine Exp $";
#endif

/* qu2uu_send.c - manager for qu --> uu */

#include "util.h"
#include "mmdf.h"

/*  */

extern char *qu_msgfile,
            sitesignature[],
            supportaddr[];

extern struct ll_struct *logptr;


struct rp_construct rp_aend = {	/* end of address list */
    RP_OK,
    'u', 'u', 'c', 'p', ' ', 'e', 'n', 'd', ' ', 'o', 'f', ' ',
    'a', 'd', 'd', 'r', ' ', 'l', 'i', 's', 't', NULL
};

struct rp_construct rp_bhost = {/* no such host */
    RP_USER,
    'b', 'a', 'd', ' ', 'h', 'o', 's', 't', ' ', 'n', 'a', 'm', 'e', NULL
};

struct rp_construct rp_err = {	/* error, you lose */
    RP_NO,
    'u', 'n', 'k', 'n', 'o', 'w', 'n', ' ', 'e', 'r', 'r', 'o', 'r', NULL
};


char   *index (), *strdup ();

/*  */

qu2uu_send () {
    short   result;
    char    info[LINESIZE],
            sender[LINESIZE];

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "qu2uu_send ()");
#endif

    if (rp_isbad (result = qu_pkinit ()))
	return result;
    if (rp_isbad (result = uu_sbinit ()))
	return result;

    while (rp_gval ((result = qu_rinit (info, sender))) == RP_OK) {
#ifdef	DEBUG
	ll_log (logptr, LLOGGEN, "info=%s sender=%s", info, sender);
#endif
	if (rp_isbad (result = qu2uu_each (sender)))
	    return result;
    }

    if (rp_gval (result) != RP_DONE) {
	ll_log (logptr, LLOGTMP, "not DONE [%s]", rp_valstr (result));
	return RP_RPLY;
    }

    qu_pkend ();
    uu_sbend ();

    return result;
}

/*  */

qu2uu_each (sender)
char   *sender;
{
    short   result;
    char    adr[LINESIZE],
            host[LINESIZE];
    RP_Buf replyval;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "qu2uu_each(sender='%s')", sender);
#endif

    mf_rinit (sender, qu_fileno ());

/*  */

    FOREVER
    {
	if (rp_isbad (result = qu_radr (host, adr))) {
	    mf_rend ();
	    return result;
	}
	if (rp_gval (result) == RP_DONE) {
	    qu_wrply ((RP_Buf *) & rp_aend, rp_conlen (rp_aend));
	    mf_rend ();
	    return RP_OK;
	}

	switch (replyval.rp_val = uu_wtadr (host, adr)) {
	    case RP_OK: 
	    case RP_AOK: 
		replyval.rp_val = uu_txtcpy ();
		break;

	    case RP_USER: 
		ll_log (logptr, LLOGFAT, "host (%s) not in table", host);
		blt (&rp_bhost, (char *) & replyval, sizeof rp_bhost);
		break;

	    default: 
		ll_log (logptr, LLOGFAT,
			"unknown return from uu_wtadr() [%s]",
			rp_valstr (replyval.rp_val));
		blt (&rp_err, (char *) & replyval, sizeof rp_err);
		replyval.rp_val = RP_NO;
		break;
	}
	if (replyval.rp_val == RP_MOK)
	    switch (replyval.rp_val = uu_wttend ()) {
		case RP_OK: 
		case RP_MOK: 
		    replyval.rp_line[0] = NULL;
		    break;

		case RP_USER: 
		case RP_LIO: 
		    ll_log (logptr, LLOGFAT, "host (%s) not in table", host);
		    blt (&rp_bhost, (char *) & replyval, sizeof rp_bhost);
		    replyval.rp_val = RP_USER;
		    break;

		default: 
		    ll_log (logptr, LLOGFAT,
			    "unknown return from uu_wttend() [%s]",
			    rp_valstr (replyval.rp_val));
		    blt (&rp_err, (char *) & replyval, sizeof rp_err);
		    break;
	    }

	qu_wrply (&replyval,
		(sizeof replyval.rp_val) + strlen (replyval.rp_line));
    }
}

/*  */

/* ******************** (mf_) MAIL-FILTERING HANDLING ******************** */

#include "mf.h"


static int  mf_fd;

static char mf_from[BUFSIZ];

/*  */

int     mf_rinit (addr, fd)
char   *addr;
int     fd;
{
    long    timenow;
    char   *sender,
            buffer[BUFSIZ];
    struct adrx *adrxp;

    mf_from[0] = NULL;

    if ((mf_fd = mf_get_msg (fd)) == NOTOK) {
	adrxp = getadrx (addr);
	sender = adrxp -> err ? addr
	    : lexequ (adrxp -> host, LocalName ())
	    || lexequ (adrxp -> host, SystemName ()) ? adrxp -> mbox
	    : sprintf (buffer, "%s@%s", adrxp -> mbox, adrxp -> host);
	while (getadrx (NULL))
	    continue;
	sender = strdup (sender);
	lowerfy (sender);
	sprintf (mf_from, "From %s %.24s remote from %s\n",
		sender, ctime (&timenow), SystemName ());
	free (sender);
    }

    return RP_OK;
}

/*  */

int     mf_rend () {
    if (mf_fd != NOTOK)
	close (mf_fd);
    mf_fd = NOTOK;
    mf_from[0] = NULL;

    return RP_OK;
}


mf_rtinit (pos)
long    pos;
{
    if (mf_fd == NOTOK)
	qu_rtinit (pos);
    else
	lseek (mf_fd, (off_t) pos, 0);
}


int     mf_rtxt (buffer, len)
char   *buffer;
int    *len;
{
    if (mf_fd == NOTOK)
	if (mf_from[0]) {
	    strcpy (buffer, mf_from);
	    buffer[*len = strlen (mf_from)] = NULL;
	    mf_from[0] = NULL;
	    return RP_OK;
	}
	else
	    return qu_rtxt (buffer, len);

    switch (*len = read (mf_fd, buffer, BUFSIZE)) {
	case NOTOK: 
	    return RP_LIO;

	case OK: 
	    return RP_DONE;

	default: 
	    buffer[*len] = NULL;
	    return RP_OK;
    }
}

/*  */

static int  mf_get_msg (md)
int     md;
{
    int     i,
            fd,
            qd;
    char    buffer[BUFSIZ],
            tmpfil[BUFSIZ];
#ifdef	MFDEBUG
    FILE * fp;
#endif	MFDEBUG

    lseek (md, (off_t)0, 0);
    if ((qd = dup (md)) == NOTOK)
	return NOTOK;

    strcpy (tmpfil, "/tmp/qu2uuXXXXXX");
    unlink (mktemp (tmpfil));
    if ((fd = creat (tmpfil, 0600)) == NOTOK) {
	close (qd);
	return NOTOK;
    }
    close (fd);
    if ((fd = open (tmpfil, 2)) == NOTOK) {
	close (qd);
	return NOTOK;
    }
    unlink (tmpfil);

    if ((i = mmdf_to_uucp (qd, fd, TRUE)) != OK) {
	close (fd);

	sprintf (buffer, "ch_uucp(%d) filtering for %s failed (%d)\n",
		getpid (), qu_msgfile, i);
	if (ml_1adr (NO, NO, sitesignature, "MF Failure", supportaddr)
		!= OK)
	    goto ml_err;
	ml_txt (buffer);
#ifdef	MFDEBUG
	lseek (md, (off_t)0, 0);
	if ((fd = dup (md)) == NOTOK)
	    ml_txt ("unable to dup() descriptor for message copy\n");
	else
	    if ((fp = fdopen (fd, "r")) == NULL) {
		ml_txt ("unable to fdopen() descriptor for message copy\n");
		close (fd);
	    }
	    else {
		ml_txt ("\n  --Message Follows--\n");
		ml_file (fp);
		fclose (fp);
	    }
#endif	MFDEBUG
	if (ml_end (OK) != OK) {
	    char   *cp;

    ml_err: ;
	    if (cp = index (buffer, '\n'))
		*cp = NULL;
	    ll_log (logptr, LLOGFAT, "Unable to post failure notice");
	    ll_log (logptr, LLOGFAT, "info: %s", buffer);
	}

	fd = NOTOK;
    }
    close (qd);

    return fd;
}
