/* uu_wtmail.c - write mail to UUCP */

#include "util.h"
#include "mmdf.h"
#include "ch.h"
#include <signal.h>

/*  */

extern int  errno;

int     broken_pipe;
void	pipeser ();

extern struct ll_struct *logptr;

FILE * uucpf;

Chan * curchan;


char   *index ();
FILE * popen ();

/*  */

uu_init (chanptr)
Chan * chanptr;
{
#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "uu_init (chanptr=%s)", chanptr -> ch_spec);
#endif

    curchan = chanptr;

    return RP_OK;
}


uu_end (result)
short   result;
{
#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "uu_end (result=0%o)", result);
#endif

    return RP_OK;
}

/*  */

uu_sbinit () {
#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "uu_sbinit ()");
#endif

    return RP_OK;
}

uu_sbend () {
#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "uu_sbend ()");
#endif

    return RP_OK;
}

/*  */

uu_wtadr (host, adr, sender)
char   *host,
       *adr,
       *sender;
{
    char   *p,
            linebuf[LINESIZE],
            nextnode[LINESIZE],
            who[LINESIZE];

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, "uu_wtadr(host='%s',adr='%s')", host, adr);
#endif

    if (host == NULL || host[0] == NULL)
	strcpy (who, adr);
    else {
	if (!ch_h2adr (curchan, TRUE, host, nextnode))
	    return RP_USER;	/* No such host */
	sprintf (who, nextnode, adr);
    }

    if ((p = index (who, '!')) != NULL) {
	*p++ = NULL;
	strcpy (nextnode, who);
	strcpy (who, p);
	lowerfy (nextnode);
    }
    else
	strcpy (nextnode, "");

    printx ("Queuing UUCP mail for %s via %s...\n", who, nextnode);
    sprintf (linebuf, "uux -p %s!rmail \\(%s\\)", nextnode, who);
    if ((uucpf = popen (linebuf, "w")) == NULL) {
	ll_log (logptr, LLOGFAT, "unable to popen() UUX (errno %d)", errno);
	return RP_AGN;
    }

    return RP_OK;
}

/*  */

uu_txtcpy () {
    short   result;
    int     len;
    int     (*pstat) ();
    char    buffer[BUFSIZ];

#ifdef DEBUG
    ll_log (logptr, LLOGBTR, " uu_txtcpy()");
#endif

    mf_rtinit (0L);
    broken_pipe = 0;
    pstat = signal (SIGPIPE, pipeser);

    while (rp_gval (result = mf_rtxt (buffer, &len)) == RP_OK && !broken_pipe)
	if (fwrite (buffer, sizeof *buffer, len, uucpf) != len) {
	    ll_log (logptr, LLOGFAT, "write on pipe lost (errno %d)", errno);
	    ll_log (logptr, LLOGFAT, "pclose() returns %d", pclose (uucpf));
	    signal (SIGPIPE, pstat);
	    return (broken_pipe ? RP_USER : RP_LIO);
	}

    fflush (uucpf);
    if (broken_pipe) {
	ll_log (logptr, LLOGFAT, "pipe to UUX broke -- probably bad host");
	ll_log (logptr, LLOGFAT, "pclose() returns %d", pclose (uucpf));
	signal (SIGPIPE, pstat);
	return RP_USER;
    }
    signal (SIGPIPE, pstat);

    return (rp_gval (result) == RP_DONE ? RP_MOK : result);
}

/*  */

uu_wttend () {
    short   result;
    int     (*pstat) ();

    pstat = signal (SIGPIPE, pipeser);
    result = pclose (uucpf) ? (broken_pipe ? RP_USER : RP_LIO) : RP_MOK;
    signal (SIGPIPE, pstat);

    return result;
}

/*  */

lowerfy (s)
char   *s;
{
    while (*s = uptolow (*s))
	s++;
}


void pipeser (i)
int     i;
{
    broken_pipe++;
    signal (SIGPIPE, SIG_IGN);
}
