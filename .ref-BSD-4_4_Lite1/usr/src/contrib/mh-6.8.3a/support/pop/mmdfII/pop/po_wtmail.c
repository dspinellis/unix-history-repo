#ifndef	POP
/* bb_wtmail.c - write mail to a BBoard */
#else	POP
/* po_wtmail.c - write mail for a POP subscriber */
#endif	POP
#ifndef	lint
static char Id[] = "@(#)$Id: po_wtmail.c,v 1.3 1993/08/25 17:43:26 jromine Exp $";
#endif


#include "util.h"
#include "mmdf.h"
#include "bboards.h"
#include "cnvtdate.h"
#include "ch.h"
#include "phs.h"
#include <pwd.h>
#include <sys/stat.h>

/*  */

#ifndef	RP_DOK
#define	submitopts	"vmth%s*"
#else	RP_DOK
#define	submitopts	"vkmth%s*"
#endif	RP_DOK

#ifndef	POP
#define	RP_NOPE	RP_AOK

#define	MBXMODE	BBMODE
#else	POP
#define	RP_NOPE	RP_USER

#define	MBXMODE	sentprotect

extern int   sentprotect;
#endif	POP


int	err_fd = NOTOK;

int     ds_address ();

extern int  errno;

int    bbrduid, bbrdgid;

char   *chnlname,
        chnlinfo[LINESIZE];
#ifndef	POP
char	bbrdaddr[LINESIZE],
        bbrdfrom[LINESIZE],
        bbrdheader[LINESIZE],
	bbrdhome[LINESIZE],
        bbrdtime[LINESIZE];
#endif	not POP

extern char *qu_msgfile,
            *delim1,
            *delim2,
	    *lckdfldir,
            *locdomain,
            *locmachine,
            *locname,
	    *sitesignature,
	    *supportaddr;

struct bboard  *curbb;

extern LLog *logptr;

FILE *lk_fopen();

off_t    lseek ();
char   *index (), *rindex (), *sprintf ();
struct passwd  *getpwnam ();

/*  */

bb_init (chanptr)
Chan * chanptr;
{
    int     uid,
            eid;
    struct passwd *pw;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "bb_init(chanptr=%s)", chanptr -> ch_name);
#endif

    chnlname = chanptr -> ch_name;
#ifndef	notdef
    sprintf (chnlinfo, submitopts, chnlname);
#else	notdef			/* the following is probably a BAD idea */
    if (chanptr -> ch_host == NULL)
	chnlinfo[0] = NULL;	/* local delivery ONLY */
    else
	sprintf (chnlinfo, submitopts, chanptr -> ch_host);
#endif	notdef

#ifndef	POP
    if ((pw = getpwnam (BBOARDS)) == NULL)
	err_abrt (RP_BHST, "no passwd entry for '%s'", BBOARDS);
#else	POP
    if ((pw = getpwnam (POPUID)) == NULL)
	err_abrt (RP_BHST, "no passwd entry for '%s'", POPUID);
#endif	POP

    bbrduid = pw -> pw_uid;
    bbrdgid = pw -> pw_gid;
#ifndef	POP
    if (isstr (locmachine))
	sprintf (bbrdfrom, "%s@%s.%s.%s", pw -> pw_name, locmachine, locname,
		locdomain);
    else
	sprintf (bbrdfrom, "%s@%s.%s", pw -> pw_name, locname, locdomain);
#ifdef DEBUG
    ll_log (logptr, LLOGGEN, "distributing as '%s'", bbrdfrom);
#endif
    sprintf (bbrdhome, pw -> pw_dir);
#endif	not POP

#ifndef	POP
    if (!setbbent ())
	err_abrt (RP_BHST, "setbbent() failed");
#else	POP
    if (!setpwinfo (pw, POPDB, 1))
	err_abrt (RP_BHST, "setbbinfo(%s, %s, 1) failed",
		pw -> pw_name, POPDB);
#endif	POP

    getwho (&uid, &eid);
    if (eid != 0)
	err_abrt (RP_BHST, "not running as root");

    return RP_OK;
}


bb_end (result)
short   result;
{
#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "bb_end(result=0%o)", result);
#endif

    return RP_OK;
}

/*  */

bb_sbinit () {
#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "bb_sbinit()");
#endif

    return RP_OK;
}


bb_sbend () {
#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "bb_sbend()");
#endif

    return RP_OK;
}

/*  */

bb_winit (info, sender)
char   *info,
       *sender;
{
#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "bb_winit(info='%s',sender='%s')",
	    info, sender);
#endif

    return RP_OK;
}

/*  */

bb_wtadr (host, adr)
char   *host,
       *adr;
{
    short   count,
            result;
    int     len,
	    md,
            offset,
	    size;
    long    start,
            stop,
            pos;
    char   *cp,
            buffer[BUFSIZ];

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "bb_wtadr(host=%s,adr=%s)", host, adr);
#endif

    if ((cp = index (adr, '@')) != NULL)
	*cp = NULL;
    make_lower (adr, adr);
    if ((curbb = getbbnam (adr)) == NULL)
	return RP_USER;
#ifndef	POP
    if (isstr (locmachine))
	sprintf (bbrdaddr, "local-%s-request@%s.%s.%s", curbb -> bb_name,
		locmachine, locname, locdomain);
    else
	sprintf (bbrdaddr, "local-%s-request@%s.%s", curbb -> bb_name, locname,
		locdomain);
#endif	not POP
#ifdef DEBUG
    ll_log (logptr, LLOGGEN, "=> BBoard %s: file='%s' info='%s'",
	    curbb -> bb_name, curbb -> bb_file, curbb -> bb_info);
#endif

    if (curbb -> bb_file == NULL || *curbb -> bb_file == NULL)
	return RP_NOPE;
#ifdef DEBUG
    ll_log (logptr, LLOGGEN, "begin local delivery...");
#endif
    printx ("\r\nperforming local delivery to file %s...\n",
	    curbb -> bb_file);

    qu_rtinit (0L);

    if ((md = mbx_open (curbb -> bb_file, bbrduid, bbrdgid, MBXMODE)) == NOTOK)
	return RP_FIO;

#ifndef	POP
    if (rp_isbad (result = mbx_init ())) {
	mbx_close (curbb -> bb_file, md);
	return result;
    }
#endif	not POP

    pos = (long) lseek (md, (off_t)0, 1);
    count = strlen (delim1);
    if (write (md, delim1, count) != count) {
	ll_log (logptr, LLOGTMP, "error writing delim1");
	result = NOTOK;
	goto clean_up;
    }
    start = (long) lseek (md, (off_t)0, 1);
    size = 0;

#ifndef	POP
    count = strlen (bbrdheader);
    if (write (md, bbrdheader, count) != count) {
	ll_log (logptr, LLOGTMP, "error writing BBoard information");
	result = NOTOK;
	goto clean_up;
    }
    for (cp = bbrdheader; *cp; cp++, size++)
	if (*cp == '\n')
	    size++;
#endif	not POP

    for (len = BUFSIZ;
	    rp_gval (result = qu_rtxt (buffer, &len)) == RP_OK;
	    len = BUFSIZ) {
	for (offset = 0;
		(offset = strindex (delim1, buffer)) >= 0;
		buffer[offset]++)
	    continue;
	for (offset = 0;
		(offset = strindex (delim2, buffer)) >= 0;
		buffer[offset]++)
	    continue;
	if (write (md, buffer, len) != len) {
	    ll_log (logptr, LLOGTMP, "error writing to file '%s'",
		    curbb -> bb_file);
	    result = NOTOK;
	    goto clean_up;
	}
	for (offset = 0, cp = buffer; offset < len; offset++, size++)
	    if (*cp++ == '\n')
		size++;
    }

    if (result < 0)
	ll_log (logptr, LLOGTMP, "error reading from message file '%s'",
		qu_msgfile);
clean_up: ;

    stop = (long) lseek (md, (off_t)0, 1);
    count = strlen (delim2);
    if (write (md, delim2, count) != count)
	ll_log (logptr, LLOGTMP, "error writing delim2");
    map_write (curbb -> bb_file, md, curbb -> bb_maxima, start, stop, pos,
	size, 0);
#ifdef DEBUG
    ll_log (logptr, LLOGGEN, "end local delivery...");
#endif

    if (result < 0)
	mbx_close (curbb -> bb_file, md);
    else
	result = mbx_close (curbb -> bb_file, md);

    return (result != NOTOK ? RP_OK : RP_FIO);
}

/*  */

bb_txtcpy () {
#ifndef	POP
    short   result;

#ifdef	DEBUG
    ll_log (logptr, LLOGBTR, "bb_txtcpy()");
#endif

    if (curbb -> bb_dist == NULL
	    || *curbb -> bb_dist == NULL
	    || chnlinfo[0] == NULL)
	return RP_MOK;
#ifdef DEBUG
    ll_log (logptr, LLOGGEN, "begin distribution...");
#endif
    if (curbb -> bb_file == NULL || *curbb -> bb_file == NULL)
	printx ("\r\n");
    printx("\rperforming remote distribution\n");

    if (rp_isbad (result = dist_init ())
	    || rp_isbad (result = dist_adrs ())
	    || rp_isbad (result = dist_text ())
	    || rp_isbad (result = dist_end ()))
	return dist_lose (result);
#ifdef DEBUG
    ll_log (logptr, LLOGGEN, "end distribution...");
#endif

    if (err_fd != NOTOK)
	dist_lose (RP_MOK);
    else
	printx ("\rmessage distributed\n");
#endif	not POP

    return RP_MOK;
}

/*  */

#ifndef	POP
/* **************** (dist_)  BBOARD DISTRIBUTION **************** */

dist_init () {
    short   result;
#ifdef	RP_NS
    int	    len;
    struct rp_bufstruct reply;
#endif	RP_NS

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "dist_init()");
#endif

    if (rp_isbad (result = mm_init ()))
	return ds_log (result, LLOGFAT, "mm_init() failed [%s]",
		rp_valstr (result));
    if (rp_isbad (result = mm_sbinit ()))
	return ds_log (result, LLOGFAT, "mm_sbinit() failed [%s]",
		rp_valstr (result));
    if (rp_isbad (result = mm_winit (chnlname, chnlinfo, bbrdaddr)))
	return ds_log (result, LLOGFAT,
		"mm_winit('%s','%s','%s') failed [%s]",
		chnlname, chnlinfo, bbrdaddr, rp_valstr (result));
#ifdef	RP_NS
	if (rp_isbad (result = mm_rrply (&reply, &len)))
	    return ds_log (result, LLOGFAT, "problem with sender address [%s]",
		    rp_valstr (result));
#endif	RP_NS

    return result;
}

/*  */

dist_adrs ()
{
    short   result;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "dist_adrs()");
#endif

    if (getbbdist (curbb, ds_address))
	return ds_log (RP_NO, LLOGTMP, "getbbdist failed: %s", getbberr ());

    if (rp_isbad (result = mm_waend ()))
	return ds_log (result, LLOGFAT, "mm_waend() failed [%s]",
		rp_valstr (result));

    return result;
}

/*  */

ds_address (addr, host)
char *addr,			/* local part */
     *host;			/* rest */
{
    short   result;
    int     len;
    struct rp_bufstruct reply;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "ds_address(addr='%s',host='%s')", addr, host);
#endif

    printx ("\rperforming distribution to %s@%s...\n", addr, host);
    if (rp_isbad (result = mm_wadr (host, addr))) {
	ds_log (result, LLOGFAT, "mm_wadr('%s','%s') failed [%s]",
		host, addr, rp_valstr (result));
	return NOTOK;
    }
    if (rp_isbad (result = mm_rrply (&reply, &len))) {
	ds_log (result, LLOGFAT,
		"mm_rrply() failed [%s] getting status of '%s@%s'",
		rp_valstr (result), addr, host);
	return NOTOK;
    }

    switch (rp_gval (reply.rp_val)) {
	case RP_AOK:
#ifdef	RP_DOK
	case RP_DOK:
#endif	RP_DOK
#ifdef DEBUG
	    ll_log (logptr, LLOGGEN, "address '%s@%s' [%s] -- %s",
		    addr, host, rp_valstr (reply.rp_val), reply.rp_line);
#endif
	    return OK;

	case RP_NO:
#ifdef	RP_NS
	case RP_NS:
#endif	RP_NS
	case RP_USER:
	case RP_NDEL:
	case RP_AGN:
	case RP_NOOP:
	    ds_log (reply.rp_val, LLOGTMP, "address '%s@%s' [%s] -- %s",
		    addr, host, rp_valstr (reply.rp_val), reply.rp_line);
	    return OK;		/* fail-soft */

	default:
	    ds_log (reply.rp_val, LLOGFAT, "unexpected reply [%s] -- %s",
		    rp_valstr (reply.rp_val), reply.rp_line);
	    return NOTOK;
    }
}

/*  */

dist_text ()
{
    short   result;
    int     len;
    char    buffer[BUFSIZ];

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "dist_text()");
#endif

    qu_rtinit (0L);
    for (len = BUFSIZ;
	    rp_gval (result = qu_rtxt (buffer, &len)) == RP_OK;
	    len = BUFSIZ)
	if (rp_isbad (result = mm_wtxt (buffer, len)))
	    return ds_log (result, LLOGFAT, "mm_wtxt() failed [%s]",
		    rp_valstr (result));

    if (result < 0)
	return ds_log (RP_FIO, LLOGTMP,
		"error reading from message file '%s'", qu_msgfile);

    if (rp_isbad (result = mm_wtend ()))
	return ds_log (result, LLOGFAT, "mm_wtend() failed [%s]",
		rp_valstr (result));

    return result;
}

/*  */

dist_end ()
{
    short   result;
    int     len;
    struct rp_bufstruct reply;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "dist_end()");
#endif

    if (rp_isbad (result = mm_rrply (&reply, &len)))
	return ds_log (result, LLOGFAT,
		"mm_rrply() failed [%s] getting final status",
		rp_valstr (result));

    switch (rp_gval (reply.rp_val)) {
	case RP_OK:
	case RP_MOK:
#ifdef DEBUG
	    ll_log (logptr, LLOGGEN, "message [%s] -- %s",
		    rp_valstr (reply.rp_val), reply.rp_line);
#endif
	    mm_sbend ();
	    mm_end (OK);
	    return result;

	case RP_NO:
	case RP_NDEL:
	case RP_AGN:
	case RP_NOOP:
	    return ds_log (RP_NO, LLOGTMP, "not delivered [%s] -- %s",
		    rp_valstr (reply.rp_val), reply.rp_line);

	default:
	    return ds_log (RP_RPLY, LLOGFAT,
		    "unexpected final reply [%s] -- %s",
		    rp_valstr (reply.rp_val), reply.rp_line);
    }
}

/*  */

dist_lose (result)
short   result;
{
    int     i;
    char   *cp,
            intro[BUFSIZ],
            buffer[BUFSIZ];

#ifdef	DEBUG
    ll_log (logptr, LLOGBTR, "dist_lose(result=0%o)", result);
#endif	DEBUG

    mm_end (NOTOK);

    printx ("\rerrors during distribution: ");
    if (domsg)
	(void) fflush (stdout);
    (void) sprintf (intro, "bboards%d distribution for %s failed [%s]\n",
	    getpid (), curbb -> bb_name, rp_valstr (result));
    if (loseaux (bbrdaddr, bbrdfrom, intro) != OK
	    && loseaux (bbrdfrom, (char *) 0, intro) != OK) {
	printx ("unable to post advisory.\n");
	ll_log (logptr, LLOGFAT, "unable to post failure notice");
	if (err_fd != NOTOK) {
	    (void) lseek (err_fd, (off_t)0, 0);
	    if ((i = read (err_fd, buffer, sizeof buffer)) > 0) {
		buffer[i] = NULL;
		if (cp = index (buffer, '\n'))
		    *cp = NULL;
		ll_log (logptr, LLOGFAT, "info: %s", buffer);
	    }
	}
	if (loseaux (supportaddr, (char *) 0, intro) != NOTOK)
	    ll_log (logptr, LLOGFAT, "unable to advise %s of failure!",
		    supportaddr);
    }
    else
	printx ("advisory posted.\n");
    if (domsg)
	(void) fflush (stdout);

    if (err_fd != NOTOK) {
	close (err_fd);
	err_fd = NOTOK;
    }
    return RP_MOK;
}

/*  */

int	loseaux (to, cc, intro)
char   *to,
       *cc,
       *intro;
{
    int     i;
    char    buffer[BUFSIZ];

    if (ml_init (NO, NO, sitesignature, "Re-distribution Failure") != OK
	    || ml_adr (to) != OK)
	return NOTOK;
    if (cc && (ml_cc () != OK || ml_adr (cc) != OK))
	return NOTOK;
    if (ml_aend () != OK || ml_tinit () != OK)
	return NOTOK;

    ml_txt (intro);
    if (err_fd != NOTOK) {
	lseek (err_fd, (off_t)0, 0);
	while ((i = read (err_fd, buffer, sizeof buffer)) > 0) {
	    buffer[i] = NULL;
	    ml_txt (buffer);
	}
    }
    encap ();

    return ml_end (OK);
}

/*  */

/* very similar to sbr/cpydgst.c */

#define	S1	0
#define	S2	1

#define	output(c)	if (bp >= dp) {flush (); *bp++ = c;} else *bp++ = c
#define	flush()		if (bp - outbuf) \
			    *bp = NULL, ml_txt (outbuf), bp = outbuf

static  encap () {
    register int    state;
    short   result;
    int     len,
	    init;
    register char  *cp,
                   *ep;
    char    buffer[BUFSIZ];
    register char  *bp,
                   *dp;
    char    outbuf[BUFSIZ];

    qu_rtinit (0L);

    dp = (bp = outbuf) + sizeof outbuf;
    init = 0;
    for (state = S1, len = BUFSIZ;
	    rp_gval (result = qu_rtxt (buffer, &len)) == RP_OK;
	    len = BUFSIZ)
	for (ep = (cp = buffer) + len; cp < ep; cp++) {
	    if (*cp == NULL)
		continue;
	    switch (state) {
		case S1: 
		    if (*cp == '-') {
			if (init == 0) {
			    ml_txt ("\n------- Forwarded Message\n\n");
			    init++;
			}
			output ('-');
			output (' ');
		    }
		    state = S2;	/* fall */

		case S2: 
		    if (init == 0) {
			ml_txt ("\n------- Forwarded Message\n\n");
			init++;
		    }
		    output (*cp);
		    if (*cp == '\n')
			state = S1;
		    break;
	    }
	}

    flush ();

    if (result < 0) {
	ll_log (logptr, LLOGTMP, "error reading message when noting failure");
	if (init)
	    ml_txt ("\n------- End of Forwarded Message\n\n");
	ml_txt ("[ error reading message ]\n");
    }
    else
	if (init)
	    ml_txt ("\n------- End of Forwarded Message\n\n");
	else {
	    ll_log (logptr, LLOGTMP, "message empty when noting failure");
	    ml_txt ("[ message empty ]\n");
	}
}

/*  */

/* VARARGS3 */

ds_log (result, level, fmt, a, b, c, d, e)
short   result;
int     level;
char   *fmt,
       *a,
       *b,
       *c,
       *d,
       *e;
{
    int     i;
    char    buffer[BUFSIZ],
	    tmpfil[BUFSIZ];

    ll_log (logptr, level, fmt, a, b, c, d, e);

    sprintf (buffer, fmt, a, b, c, d, e);
    strcat (buffer, "\n");

    printx ("\rerror: %s", buffer);

    if (err_fd == NOTOK) {
	unlink (mktemp (strcpy (tmpfil, "/tmp/bboardsXXXXXX")));
	if ((err_fd = creat (tmpfil, 0600)) == NOTOK)
	    return result;
	close (err_fd);
	if ((err_fd = open (tmpfil, 2)) == NOTOK)
	    return result;
	unlink (tmpfil);
	lseek (err_fd, (off_t)0, 0);
    }
    i = strlen (buffer);
    write (err_fd, buffer, i);

    return result;
}
#endif	not POP

/*  */

/* mbx_	    local mailbox routines */

#ifndef	POP
mbx_init () {
    int     fd,
            clear;
    char    name[BUFSIZ];
    FILE * fp;

    if ((fd = mbx_Xopen (curbb -> bb_info, bbrduid, bbrdgid, MBXMODE, &clear))
	    == NOTOK) {
	if (errno == ETXTBSY) {
	    printx ("\runable to lock %s\n", curbb -> bb_info);
	    ll_err (logptr, LLOGTMP, "unable to lock %s",
		    curbb -> bb_info);
	    return RP_LOCK;
	}
	printx ("\runable to open '%s'", curbb -> bb_info);
	ll_log (logptr, LLOGTMP, "unable to open '%s'", curbb -> bb_info);
	return RP_FOPN;
    }
    if ((fp = fdopen (fd, "w")) == (FILE *) NULL) {
	printx ("\runable to fdopen '%s'", curbb -> bb_info);
	ll_err (logptr, LLOGTMP, "unable to fdopen '%s'", curbb -> bb_info);
	mbx_close (curbb -> bb_info, fd);
	return RP_LIO;
    }

    strcpy (name, curbb -> bb_name);
    if ((curbb = getbbnam (name)) == (struct bboard *) NULL) {
	printx ("\runable to get information on BBoard %s\n", name);
	ll_err (logptr, LLOGFAT, "unable to get info on %s", name);
	lkfclose (fp, curbb -> bb_info);
	return RP_LIO;
    }
    sprintf (bbrdheader, "BBoard-ID: %d\nBB-Posted: %s\n",
	    ++curbb -> bb_maxima, cnvtdate (TIMREG, bbrdtime));
    fprintf (fp, "%d\n%s\n", curbb -> bb_maxima, bbrdtime);

    lkfclose (fp, curbb -> bb_info);

    return RP_OK;
}
#endif	not POP
