/* bb_wtmail.c - write mail to a BBoard */
#ifndef	lint
static char Id[] = "@(#)$Id: bb_wtmail.c,v 1.3 1993/08/25 17:43:26 jromine Exp $";
#endif

#include "util.h"
#include "mmdf.h"
#include "ch.h"
#include "bboards.h"
#include "tws.h"
#include <pwd.h>
#include <sys/stat.h>

/*  */

int	err_fd = NOTOK;

int     dist_address ();

extern int  errno;

int	bbrduid, bbrdgid;

char   *channelname,
        channelinfo[LINESIZE],
	bbrdaddr[LINESIZE],
        bbrdfrom[LINESIZE],
        bbrdheader[LINESIZE],
	bbrdhome[LINESIZE],
        bbrdtime[LINESIZE];

extern char *qu_msgfile,
            delim1[],
            delim2[],
            locname[],
	    sitesignature[];

struct bboard  *curbb;

extern struct ll_struct *logptr;


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
    ll_log (logptr, LLOGBTR, "bb_init(chanptr=%s)", chanptr -> ch_spec);
#endif

    channelname = chanptr -> ch_spec;
#ifndef	notdef
    sprintf (channelinfo, "vmth%s*", channelname);
#else	notdef			/* the following is probably a BAD idea */
    if (chanptr -> ch_host == NULL)
	channelinfo[0] = NULL;	/* local delivery ONLY */
    else
	sprintf (channelinfo, "vmth%s*", chanptr -> ch_host);
#endif	notdef

    if ((pw = getpwnam (BBOARDS)) == NULL)
	err_abrt (RP_BHST, "no passwd entry for '%s'", BBOARDS);
    bbrduid = pw -> pw_uid;
    bbrdgid = pw -> pw_gid;
    sprintf (bbrdfrom, "%s@%s", pw -> pw_name, locname);
#ifdef DEBUG
    ll_log (logptr, LLOGGEN, "distributing as '%s'", bbrdfrom);
#endif
    sprintf (bbrdhome, pw -> pw_dir);

    if (!setbbent ())
	err_abrt (RP_BHST, "setbbent() failed");

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
    int     i,
	    md,
            offset,
            qd,
	    size;
    long    start,
            stop,
            pos;
    char   *cp,
	    buffer[BUFSIZ];

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "bb_wtadr(host=%s,adr=%s)", host, adr);
#endif

    make_lower (adr, adr);
    if ((curbb = getbbnam (adr)) == NULL)
	return RP_USER;
    sprintf (bbrdaddr, "local-%s-request@%s", curbb -> bb_name, locname);
#ifdef DEBUG
    ll_log (logptr, LLOGGEN, "=> BBoard %s: file='%s' info='%s addr='%s'",
	    curbb -> bb_name, curbb -> bb_file, curbb -> bb_info, bbrdaddr);
#endif

    if (curbb -> bb_file == NULL || *curbb -> bb_file == NULL)
	return RP_AOK;
#ifdef DEBUG
    ll_log (logptr, LLOGGEN, "begin local delivery...");
#endif
    printx ("\r\nperforming local delivery to file %s...\n",
	    curbb -> bb_file);

    qu_rtinit (0L);
    if ((qd = dup (qu_fileno ())) == NOTOK) {
	ll_log (logptr, LLOGFAT, "unable to dup qu_fileno()");
	return RP_LIO;
    }

    if ((md = mbx_open (curbb -> bb_file, bbrduid, bbrdgid, BBMODE)) == NOTOK) {
	close (qd);
	return RP_FOPEN;
    }

    if (rp_isbad (result = mbx_init ())) {
	close (qd);
	mbx_close (curbb -> bb_file, md);
	return result;
    }

    pos = (long) lseek (md, (off_t)0, 1);
    count = strlen (delim1);
    if (write (md, delim1, count) != count) {
	ll_log (logptr, LLOGTMP, "error writing delim1");
	i = NOTOK;
	goto clean_up;
    }
    start = (long) lseek (md, (off_t)0, 1);
    size = 0;

    count = strlen (bbrdheader);
    if (write (md, bbrdheader, count) != count) {
	ll_log (logptr, LLOGTMP, "error writing BBoard information");
	i = NOTOK;
	goto clean_up;
    }
    for (cp = bbrhdheader; *cp; cp++, size++)
	if (*cp == '\n')
	    size++;

/*  */

    while ((i = read (qd, buffer, sizeof buffer)) > 0) {
	for (offset = 0;
		(offset = strindex (delim1, buffer)) >= 0;
		buffer[offset]++)
	    continue;
	for (offset = 0;
		(offset = strindex (delim2, buffer)) >= 0;
		buffer[offset]++)
	    continue;
	if (write (md, buffer, i) != i) {
	    ll_log (logptr, LLOGTMP, "error writing to file '%s'",
		    curbb -> bb_file);
	    i = NOTOK;
	    goto clean_up;
	}
	for (offset = 0, cp = buffer; offset < i; offset++, size++)
	    if (*cp++ == '\n')
		size++;
    }

    if (i < 0)
	ll_log (logptr, LLOGTMP, "error reading from message file '%s'",
		qu_msgfile);
clean_up: ;
    close (qd);

    stop = (long) lseek (md, (off_t)0, 1);
    count = strlen (delim2);
    if (write (md, delim2, count) != count)
	ll_log (logptr, LLOGTMP, "error writing delim2");
    map_write (curbb -> bb_file, md, curbb -> bb_maxima, start, stop, pos,
	size, 0);

#ifdef DEBUG
    ll_log (logptr, LLOGGEN, "end local delivery...");
#endif

    result = mbx_close (curbb -> bb_file, md);
    return (i < 0 ? RP_FIO : result);
}

/*  */

bb_txtcpy () {
    short   result;

#ifdef	DEBUG
    ll_log (logptr, LLOGBTR, "bb_txtcpy()");
#endif

    if (curbb -> bb_dist == NULL
	    || *curbb -> bb_dist == NULL
	    || channelinfo[0] == NULL)
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

    return RP_MOK;
}

/*  */

/* dist_    BBoard distribution routines */

dist_init () {
    short   result;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "dist_init()");
#endif

    if (rp_isbad (result = mm_init ()))
	return dist_log (result, LLOGFAT, "mm_init() failed [%s]",
		rp_valstr (result));
    if (rp_isbad (result = mm_sbinit ()))
	return dist_log (result, LLOGFAT, "mm_sbinit() failed [%s]",
		rp_valstr (result));
    if (rp_isbad (result = mm_winit (chnlname, chnlinfo, bbrdaddr)))
	return dist_log (result, LLOGFAT,
		"mm_winit('%s','%s','%s') failed [%s]",
		chnlname, chnlinfo, bbrdaddr, rp_valstr (result));

    return result;
}

/*  */

dist_adrs () {
    short   result;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "dist_adrs()");
#endif

    if (getbbdist (curbb, dist_address))
	return dist_log (RP_NO, LLOGTMP, "getbbdist failed: %s", getbberr ());

    if (rp_isbad (result = mm_waend ()))
	return dist_log (result, LLOGFAT, "mm_waend() failed [%s]",
		rp_valstr (result));

    return result;
}

/*  */

dist_address (addr, host)
char *addr,
     *host;
{
    short   result,
            len;
    struct rp_bufstruct reply;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "dist_address(addr='%s',host='%s')", addr, host);
#endif

    printx ("\rperforming distribution to %s@%s...\n", addr, host);
    if (rp_isbad (result = mm_wadr (host, addr))) {
	dist_log (result, LLOGFAT, "mm_wadr('%s','%s') failed [%s]",
		host, addr, rp_valstr (result));
	return NOTOK;
    }
    if (rp_isbad (result = mm_rrply (&reply, &len))) {
	dist_log (result, LLOGFAT,
		"mm_rrply() failed [%s] getting status of '%s@%s'",
		rp_valstr (result), addr, host);
	return NOTOK;
    }

    switch (rp_gval (reply.rp_val)) {
	case RP_AOK:
#ifdef DEBUG
	    ll_log (logptr, LLOGGEN, "address '%s@%s' [%s] -- %s",
		    addr, host, rp_valstr (reply.rp_val), reply.rp_line);
#endif
	    return OK;

	case RP_NO:
	case RP_USER:
	case RP_NDEL:
	case RP_AGN:
	case RP_NOOP:
	    dist_log (reply.rp_val, LLOGTMP, "address '%s@%s' [%s] -- %s",
		    addr, host, rp_valstr (reply.rp_val), reply.rp_line);
	    return OK;		/* fail-soft */

	default:
	    dist_log (reply.rp_val, LLOGFAT, "unexpected reply [%s] -- %s",
		    rp_valstr (reply.rp_val), reply.rp_line);
	    return NOTOK;
    }
}

/*  */

dist_text () {
    short   result;
    int     i,
            qd;
    char    buffer[BUFSIZ];

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "dist_text()");
#endif

    qu_rtinit (0L);
    if ((qd = dup (qu_fileno ())) == NOTOK)
	return dist_log (RP_LIO, LLOGFAT, "unable to dup qu_fileno()");

    while ((i = read (qd, buffer, sizeof buffer)) > 0)
	if (rp_isbad (result = mm_wtxt (buffer, i)))
	    return dist_log (result, LLOGFAT, "mm_wtxt() failed [%s]",
		    rp_valstr (result));

    close (qd);
    if (i < 0)
	return dist_log (RP_FIO, LLOGTMP,
		"error reading from message file '%s'", qu_msgfile);

    if (rp_isbad (result = mm_wtend ()))
	return dist_log (result, LLOGFAT, "mm_wtend() failed [%s]",
		rp_valstr (result));

    return result;
}

/*  */

dist_end () {
    short   result,
            len;
    struct rp_bufstruct reply;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "dist_end()");
#endif

    if (rp_isbad (result = mm_rrply (&reply, &len)))
	return dist_log (result, LLOGFAT,
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
	    return dist_log (RP_NO, LLOGTMP, "not delivered [%s] -- %s",
		    rp_valstr (reply.rp_val), reply.rp_line);

	default:
	    return dist_log (RP_RPLY, LLOGFAT,
		    "unexpected final reply [%s] -- %s",
		    rp_valstr (reply.rp_val), reply.rp_line);
    }
}

/*  */

dist_lose (result)
short   result;
{
    int     qd;
    char    buffer[BUFSIZ];
    FILE * qp;

#ifdef	DEBUG
    ll_log (logptr, LLOGBTR, "dist_lose(result=0%o)", result);
#endif	DEBUG
    mm_end (NOTOK);

    printx ("\rerrors during distribution: ");
    if (domsg)
	fflush (stdout);
    sprintf (buffer, "ch_bboards(%d) distribution for %s failed [%s]\n",
	    getpid (), curbb -> bb_name, rp_valstr (result));
    if (ml_init (NO, NO, sitesignature, "Re-distribution Failure") != OK
	    || ml_adr (bbrdaddr) != OK
	    || ml_cc () != OK
	    || ml_adr (bbrdfrom) != OK
	    || ml_aend () != OK
	    || ml_tinit () != OK)
	goto ml_err;
    ml_txt (buffer);

    if (err_fd != NOTOK) {
	lseek (err_fd, (off_t)0, 0);
	if ((qp = fdopen (err_fd, "r")) == NULL) {
	    ml_txt ("unable to fdopen() for diagnostic copy\n");
	    close (err_fd);
	}
	else {
	    ml_file (qp);
	    fclose (qp);
	}
	err_fd = NOTOK;
    }

    qu_rtinit (0L);
    if ((qd = dup (qu_fileno ())) == NOTOK)
	ml_txt ("unable to dup qu_fileno() for message copy\n");
    else
	if ((qp = fdopen (qd, "r")) == NULL) {
	    ml_txt ("unable to fdopen() for message copy\n");
	    close (qd);
	}
	else {
	    ml_txt ("\n  --Message Follows--\n");
	    ml_file (qp);
	    fclose (qp);
	}

    if (ml_end (OK) != OK) {
	char   *cp;

ml_err: ;
	if (cp = index (buffer, '\n'))
	    *cp = NULL;
	printx ("unable to post advisory.\n");
	ll_log (logptr, LLOGFAT, "unable to post failure notice");
	ll_log (logptr, LLOGFAT, "info: %s", buffer);
    }
    else
	printx ("advisory posted.\n");
    if (domsg)
	fflush (stdout);

    return RP_MOK;
}

/*  */

/* VARARGS3 */

dist_log (result, level, fmt, a, b, c, d, e)
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

/*  */

/* mbx_	    local mailbox routines */

mbx_init () {
    int	    fd,
	    clear;
    char    name[BUFSIZ];
    FILE  *fp;

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "mbx_init()");
#endif

    if ((fd = mbx_Xopen (curbb -> bb_info, bbrduid, bbrdgid, BBMODE, &clear))
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
    if ((curbb = getbbnam (name)) == NULL) {
	printx ("\runable to get information on BBoard %s\n", name);
	ll_err (logptr, LLOGFAT, "unable to get info on %s", name);
	lkfclose (fp, curbb -> bb_info);
	return RP_LIO;
    }

    strcpy (bbrdtime, dtimenow ());
    sprintf (bbrdheader, "BBoard-ID: %d\nBB-Posted: %s\n",
	    ++curbb -> bb_maxima, bbrdtime);

    fprintf (fp, "%d\n%s\n", curbb -> bb_maxima, bbrdtime);
    lkfclose (fp, curbb -> bb_info);

    return RP_OK;
}
