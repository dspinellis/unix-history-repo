/* uumm.c - routines to filter UUCP to MMDF mailboxes */

#include "mf.h"
#include "../tws/tws.h"
#include <stdio.h>
#include "../mts/mts.h"
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

/*  */

static struct header {
    char   *h_name;
    int     h_type;
}                       headers[] = {
                            "From", HFROM,
                            "Sender", HSNDR,
                            "Reply-To", HADDR,
                            "To", HADDR,
                            "cc", HADDR,
                            "Bcc", HADDR,
                            "Resent-From", HADDR,
                            "Resent-Sender", HADDR,
                            "Resent-Reply-To", HADDR,
                            "Resent-To", HADDR,
                            "Resent-cc", HADDR,
                            "Resent-Bcc", HADDR,
                            "Date", HDATE,
                            "Resent-Date", HDATE,
                            NULL, HOTHR
};

static char buffer[BUFSIZ],
            tmpbuf[BUFSIZ];

char   *shrink ();

long    time ();

/*  */

/*
 *    uucp2mmdf() - given a file descriptor to a uucp mailbox, filter
 *    its contents to the file descriptor for a mmdf mailbox.  Returns
 *    non-zero on error (see mf.h for values)
 *
 *    It is assumed that the caller will have made sure that the necessary
 *    locking has been performed on the output fd.
 */

int	uucp2mmdf (infd, outfd, nodelim)
int     infd,
        outfd,
        nodelim;
{
    int     fd,
            result;
    struct stat st;
    FILE * in, *out;

    if (fstat (infd, &st) == NOTOK || fstat (outfd, &st) == NOTOK)
	return MFPRM;
    if ((in = fdopen (infd, "r")) == NULL
	    || (out = fdopen (outfd, "w")) == NULL)
	return MFSIO;

    result = uumm (in, out, nodelim);

    /* for STDIO - free up some fp:s */
    fd = dup (fileno (in));
    fclose (in);
    dup2 (fd, infd);
    close (fd);

    fd = dup (fileno (out));
    fclose (out);
    dup2 (fd, outfd);
    close (fd);

    return result;
}

/*  */

int     uumm (in, out, nodelim)
FILE   *in,
       *out;
int     nodelim;
{
    int     i,
            tmp_fd;
    char    from[LINESIZ],
            date[LINESIZ];
    FILE   *tmp;

    for (tmp_fd = NOTOK;;) {
	if ((i = uucp_file (&tmp_fd, in, &tmp, nodelim)) == DONE)
	    break;
	else
	    if (i != OK)
		return i;
	if ((i = uucp_from (from, date, tmp)) != OK)
	    return uucp_die (i, tmp, in, out, nodelim);
	if ((i = uucp_headers (from, date, tmp, out, nodelim)) != OK)
	    return uucp_die (i, tmp, in, out, nodelim);
	if ((i = uucp_text (tmp, out, nodelim)) != OK)
	    return uucp_die (i, tmp, in, out, nodelim);
    }

    fflush (out);

    return (ferror (in) || ferror (out) ? MFERR : MFOK);
}

/*  */

static int  uucp_file (tmp_fd, in, tmp, nodelim)
int    *tmp_fd,
        nodelim;
FILE * in, **tmp;
{
    int     done,
            fd;
    char    tmpfil[LINESIZ];
    FILE * out;

    if (nodelim)
	if (*tmp_fd != NOTOK)
	    return DONE;
	else
	    if ((*tmp_fd = dup (fileno (in))) == NOTOK)
		return MFERR;
	    else
		if ((*tmp = fdopen (*tmp_fd, "r")) == NULL) {
		    close (*tmp_fd);
		    return MFERR;
		}
		else
		    return OK;

    if (*tmp_fd == NOTOK && fgets (tmpbuf, sizeof tmpbuf, in) == NULL)
	return DONE;
    else
	if (feof (in))
	    return DONE;

    strcpy (tmpfil, "/tmp/uummXXXXXX");
    unlink (mktemp (tmpfil));
    if ((fd = creat (tmpfil, TMPMODE)) == NOTOK)
	return MFERR;
    close (fd);

    if ((fd = open (tmpfil, 2)) == NOTOK)
	return MFERR;
    if ((out = fdopen (fd, "w")) == NULL) {
	close (fd);
	return MFERR;
    }
    unlink (tmpfil);

    if ((*tmp_fd = dup (fd)) == NOTOK) {
	close (fd);
	return MFERR;
    }
    if ((*tmp = fdopen (*tmp_fd, "r")) == NULL) {
	close (fd);
	close (*tmp_fd);
	return MFERR;
    }

/*  */

    for (done = FALSE;;) {
	if (done)
	    if (isfrom (tmpbuf))
		break;
	    else
		putc ('\n', out);
	done = tmpbuf[0] == '\n';
	if (!done)
	    fputs (tmpbuf, out);
	if (fgets (tmpbuf, sizeof tmpbuf, in) == NULL)
	    break;
    }

    fclose (out);
    fseek (*tmp, 0L, 0);
    return OK;
}

/*  */

/* We might want to attempt recovery here.  Forget it. */

static int  uucp_from (from, date, in)
char   *from,
       *date;
FILE * in;
{
    char   *cp,
           *pp,
            fromwhom[LINESIZ];
    struct adrx *adrxp;

    if (fgets (buffer, sizeof buffer, in) == NULL || !isfrom (buffer))
	return MFROM;
    if (buffer[strlen (buffer) - 1] == '\n')
	buffer[strlen (buffer) - 1] = NULL;
    if ((cp = index (buffer, ' ')) == NULL)
	return MFROM;
    pp = ++cp;
    if ((cp = index (cp, ' ')) == NULL)
	return MFROM;
    *cp++ = NULL;
    strcpy (fromwhom, pp);
    while (isspace (*cp))
	cp++;
    sprintf (date, "%.24s", cp);

    for (;;) {
	if ((cp = index (cp + 1, 'r')) == NULL) {
	    if (index (fromwhom, '!') || index (fromwhom, '@'))
		strcpy (from, fromwhom);
	    else
		sprintf (from, "%s!%s", SystemName (), fromwhom);
	    break;
	}
	if (strncmp (cp, "remote from ", 12) == 0) {
	    *cp = NULL;
	    sprintf (from, "%s!%s", cp + 12, fromwhom);
	    break;
	}
    }

    if ((adrxp = seekadrx (from)) == NULL)
	return MFROM;
    addr_convert (adrxp, from, TRUE);
    while (seekadrx (NULL))
	continue;
    if (from[0] == NULL)
	return MFROM;
    date_convert (date, date);
    return (date[0] != NULL ? OK : MFROM);
}

/*  */

static int  uucp_headers (from, date, in, out, nodelim)
int     nodelim;
char   *from,
       *date;
FILE * in, *out;
{
    int     i,
            seen_from,
            seen_sender,
            seen_date;

    seen_from = seen_sender = seen_date = 0;
    if (!nodelim)
	fputs (mmdlm1, out);

    fprintf (out, "Munged: from %s to %s; %s\n",
	    SystemName (), LocalName (), dtimenow ());

    for (;;) {
	switch (do_header (&seen_from, &seen_sender, &seen_date, in, out)) {
	    case NOTOK: 
		return MFHDR;

	    case OK: 
		continue;

	    case DONE: 
		break;
	}
	break;
    }
    				/* extra newline separates headers and body */
    fprintf (out, "%sDate: %s\n%s: %s\n\n",
	    seen_date ? "UUCP-" : NULL, date,
	    seen_from ? (seen_sender ? "UUCP-Sender" : "Sender") : "From",
	    from);

    return OK;
}

/*  */

static int  uucp_text (in, out, nodelim)
int     nodelim;
FILE * in, *out;
{
    if (feof (in))		/* probably no body */
	putc ('\n', out);
    else
	while (fgets (buffer, sizeof buffer, in) != NULL) {
	    if (!nodelim && isdlm2 (buffer))
		buffer[0]++;
	    fputs (buffer, out);
	}

    if (!nodelim)
	fputs (mmdlm2, out);
    fclose (in);

    return OK;
}

/*  */

static int  do_header (seen_from, seen_sender, seen_date, in, out)
int    *seen_from,
       *seen_sender,
       *seen_date;
FILE * in, *out;
{
    int     i,
            margin,
            some,
            spat,
            pos;
    char   *bp,
           *cp,
           *pp,
            line[BUFSIZ];
    struct adrx *adrxp;
    struct header  *hl;

    if ((i = mfgets (in, &bp)) != OK)
	return i;

    if ((cp = index (bp, ':')) == NULL) {
	fprintf (out, "Illegal-Field: %s\n", bp);
	return OK;
    }

    *cp = NULL;
    for (hl = &headers[0]; hl -> h_name; hl++)
	if (lequal (hl -> h_name, bp))
	    break;

/*  */

    switch (hl -> h_type) {
	case HDATE: 
	    if (lequal (hl -> h_name, "Date"))
		(*seen_date)++;
	    for (pp = cp + 1; isspace (*pp); pp++)
		continue;
	    date_convert (pp, line);
	    if (line[0] == NULL)
		fprintf (out, "Illegal-Object: %s: %s -- %s\n",
			hl -> h_name, pp, "illegal date construct");
	    else
		fprintf (out, "%s: %s\n", hl -> h_name, line);
	    break;
	    
	case HOTHR: 
	    *cp = ':';
	    fprintf (out, "%s\n", bp);
	    break;

	case HFROM: 
	case HSNDR: 
	    if (hl -> h_type == HFROM)
		(*seen_from)++;
	    else
		(*seen_sender)++;
	case HADDR: 
	    spat = 0;
	    some = FALSE;
	    pp = ++cp;
	    margin = pos = strlen (hl -> h_name) + 2;
	    while (adrxp = seekadrx (pp)) {
		addr_convert (adrxp, line, FALSE);
		if (line[0] != NULL) {
		    if (!spat++)
			fprintf (out, "%s: ", hl -> h_name);
		    if (some++)
			fputs (", ", out), pos += 2;
		    if (pos + strlen (line) >= OWIDTH) {
			fprintf (out, "\n%*s", margin, " ");
			pos = margin;
		    }
		    fputs (line, out);
		    pos += strlen (line);
		}
		else {
		    if (spat)
			putc ('\n', out);
		    fprintf (out, "Illegal-Object: %s: %s -- %s\n",
			    hl -> h_name, adrxp -> text, adrxp -> err);
		    spat = 0;
		    some = FALSE;
		    pos = margin;
		}
	    }
	    if (spat)
		putc ('\n', out);
	    break;

	default: 
	    return NOTOK;
    }

    return OK;
}

/*  */

addr_convert (adrxp, to, notice)
struct adrx *adrxp;
char   *to;
int     notice;
{
    int     addrlen,
            uucplen;
    char   *cp,
           *wp,
            addr[BUFSIZ],
            tmp[LINESIZ],
            uucp[LINESIZ];
    static char path[LINESIZ] = "";

    if (adrxp -> err || !adrxp -> mbox) {
	*to = NULL;
	return;
    }
    if (notice)
	if ((cp = rindex (adrxp -> mbox, '!')) != NULL)
	    sprintf (path, "%.*s!", cp - adrxp -> mbox, adrxp -> mbox);
	else
	    path[0] = NULL;

    sprintf (addr, "%s%s", path, adrxp -> mbox);
    sprintf (uucp, "%s!", SystemName ());
    uucplen = strlen (uucp);
    if ((addrlen = strlen (addr) - uucplen - 1) >= 0)
	for (cp = addr + addrlen; cp >= addr; cp--)
	    if (strncmp (cp, uucp, uucplen) == NULL) {
		if (cp != addr && *(cp - 1) != '!')
		    continue;
		strcpy (addr, cp + uucplen);
		break;
	    }

/*  */

    if (adrxp -> host == NULL) {
	cp = shrink (addr);
#ifdef	MMDFMTS
	sprintf (uucp, "%s%%%s@%s", cp, UucpChan (), LocalName ());
#else	MMDFMTS
	if (wp = index (adrxp -> mbox, '!'))
	    sprintf (uucp, "%s@%.*s.%s",
		wp + 1, wp - adrxp -> mbox, adrxp -> mbox, UucpChan ());
	else
	    sprintf (uucp, "%s@%s.%s",
		adrxp -> mbox, SystemName (), UucpChan ());
#endif	MMDFMTS
	if (strcmp (adrxp -> mbox, cp))
	    sprintf (tmp, "\"%s\" <%s>", adrxp -> mbox, uucp);
	else
	    strcpy (tmp, uucp);
    }
    else
	if ((wp = rindex (adrxp -> mbox, '!')) == NULL)
	    sprintf (tmp, "%s@%s", adrxp -> mbox, adrxp -> host);
	else {
	    sprintf (uucp, "%%%s", UucpChan ());
	    uucplen = strlen (uucp);
	    cp = (lequal (LocalName (), adrxp -> host)
		    && (addrlen = strlen (addr) - uucplen) > 0)
		? addr + addrlen : NULL;
	    if (lequal (uucp, cp))
		sprintf (tmp, "%s@%s", shrink (addr), adrxp -> host);
	    else {
		if (lequal (adrxp -> mbox, ++wp))
		    sprintf (tmp, "%s@%s", wp, adrxp -> host);
		else
		    sprintf (tmp, "\"%s\" <%s@%s>", adrxp -> mbox,
			    wp, adrxp -> host);
	    }
	}

    strcpy (to, tmp);
}

/*  */

static char *shrink (addr)
char   *addr;
{
    int     i,
            j;
    char   *cp,
           *pp,
           *wp,
           *xp;
    static char r1[BUFSIZ],
                r2[BUFSIZ];

    sprintf (r2, "%s!", SystemName ());
    i = strlen (r2);
    if ((j = strlen (addr) - i - 1) >= 0)
	for (cp = &addr[j]; cp >= addr; cp--)
	    if (strncmp (cp, r2, i) == NULL) {
		if (cp != addr && *(cp - 1) != '!')
		    continue;
		strcpy (addr, cp + i);
		break;
	    }

    if ((cp = rindex (addr, '!')) == NULL) {
	sprintf (r1, "%s%s", r2, addr);
	return r1;
    }
    *cp++ = NULL;

    if ((pp = rindex (addr, '!')) == NULL) {
	*--cp = '!';
	strcpy (r1, addr);
	return r1;
    }
    strcpy (r1, cp);

    while ((pp = rindex (addr, '!')) != NULL) {
	for (pp++, xp = addr; (wp = index (xp, '!')) != NULL;) {
	    *wp = NULL;
	    if (strcmp (pp, xp)) {
		*wp++ = '!';
		xp = wp;
	    }
	    else {
		pp = xp;
		break;
	    }
	}
	sprintf (r2, "%s!%s", pp, r1);
	strcpy (r1, r2);
	if (--pp > addr)
	    *pp = NULL;
    }

/*  */

    if ((wp = index (r1, '!')) != NULL) {
	*wp = NULL;
	strcpy (r2, r1);
	*wp = '!';
	if (strcmp (addr, r2)) {
	    sprintf (r2, "%s!%s", addr, r1);
	    strcpy (r1, r2);
	}
    }

    return r1;
}

/*  */

date_convert (from, to)
char    from[],
       *to;
{
    static int  zone = -1,
                flags = TW_NULL;
    char    date[LINESIZ];
    struct tws *tw;

    if (dparsetime (from))	/* might be OK as is */
	strcpy (date, from);
    else
	if (isdigit (from[20])) {
	    if (zone == -1) {
		if (tw = dtwstime ()) {
		    zone = tw -> tw_zone;
		    flags = tw -> tw_flags;
		}
		else
		    zone = 0, flags = TW_NULL;
	    }
	    sprintf (date, "%.3s, %.2s %.3s %.2s %.2s:%.2s:%.2s %s",
		    from + 0, from + 8, from + 4, from + 22, from + 11,
		    from + 14, from + 17, dtimezone (zone, flags));
	}
	else
	    sprintf (date, "%.3s, %.2s %.3s %.2s %.2s:%.2s:%.2s %s",
		    from + 0, from + 8, from + 4, from + 26, from + 11,
		    from + 14, from + 17, from + 20);

    strcpy (to, date);
}

/*  */

static int  uucp_die (error, in1, in2, out, nodelim)
int     error,
        nodelim;
FILE * in1, *in2, *out;
{
    long    clock;
    char    date[LINESIZ];

    if (nodelim) {
	fclose (in1);
	return error;
    }

    switch (error) {
	case MFHDR: 
	    putc ('\n', out);
	case MFTXT: 
	    fprintf (out, "\n%s", mmdlm2);
	    break;
    }

    time (&clock);
    sprintf (date, "%.*s", sizeof date - 1, dtime (&clock));
    fprintf (out, "%sFrom: %s <%s@%s>\nDate: %s\nSubject: %s %s\n\n",
	    mmdlm1, "UUCP to MMDF filter", getusr (), LocalName (), date,
	    "Bad UUCP mailbox - error in",
	    error == MFHDR ? "Header" : error == MFTXT ? "Body" : "Mailbox");

    fprintf (out, "%s: %s\n%s\n--------\n",
	    "Error detected at line", buffer, "Message being processed");
    fseek (in1, 0L, 0);
    while (fgets (buffer, sizeof buffer, in1) != NULL) {
	if (isdlm2 (buffer))
	    buffer[0]++;
	fputs (buffer, out);
    }
    fclose (in1);

    if (!feof (in2)) {
	fprintf (out, "--------\n%s\n--------\n%s",
		"Remainder of unfiltered mailbox follows", tmpbuf);
	while (fgets (buffer, sizeof buffer, in2) != NULL) {
	    if (isdlm2 (buffer))
		buffer[0]++;
	    fputs (buffer, out);
	}
    }

    fprintf (out, "--------\n%s", mmdlm2);
    fflush (out);

    return error;
}
