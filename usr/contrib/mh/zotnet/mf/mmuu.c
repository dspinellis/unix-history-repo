/* mmuu.c - routines to filter MMDF to UUCP mailboxes */

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

long    time ();
char   *ctime ();

/*  */

/* 
 *    mmdf2uucp() - given a file descriptor to a mmdf mailbox, filter
 *    its contents to the file descriptor for a mmdf mailbox.  Returns
 *    non-zero on error (see mf.h for values)
 *
 *    It is assumed that the caller will have made sure that the necessary
 *    locking has been performed on the output fd.
 */

int	mmdf2uucp (infd, outfd, nodelim)
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

    result = mmuu (in, out, nodelim);

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

static int  mmuu (in, out, nodelim)
FILE   *in,
       *out;
int     nodelim;
{
    int     i,
            tmp_fd;
    FILE   *tmp;

    for (tmp_fd = NOTOK;;) {
	if ((i = mmdf_file (&tmp_fd, in, &tmp, nodelim)) == DONE)
	    break;
	else
	    if (i != OK)
		return i;
	if ((i = mmdf_headers (tmp, out, nodelim)) != OK)
	    return mmdf_die (i, tmp, in, out, nodelim);
	if ((i = mmdf_text (tmp, out, nodelim)) != OK)
	    return mmdf_die (i, tmp, in, out, nodelim);
    }

    fflush (out);

    return (ferror (in) || ferror (out) ? MFERR : MFOK);
}

/*  */

static int  mmdf_file (tmp_fd, in, tmp, nodelim)
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

    if (fgets (tmpbuf, sizeof tmpbuf, in) == NULL)
	return DONE;
    if (!isdlm1 (tmpbuf))
	return MFDLM;

    strcpy (tmpfil, "/tmp/mmuuXXXXXX");
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
	if (fgets (tmpbuf, sizeof tmpbuf, in) == NULL)
	    return MFDLM;
	if (done && isdlm2 (tmpbuf))
	    break;
	done = tmpbuf[strlen (tmpbuf) - 1] == '\n';
	fputs (tmpbuf, out);
    }

    fclose (out);
    fseek (*tmp, 0L, 0);
    return OK;
}

/*  */

static int  mmdf_headers (in, out, nodelim)
            FILE * in, *out;
int     nodelim;
{
    int     fd,
            i,
            tmp_fd;
    char   *cp,
            line[BUFSIZ],
            from[LINESIZ],
            date[LINESIZ],
            tmpfil[LINESIZ];
    FILE * tmp;

    *from = *date = NULL;

    strcpy (tmpfil, "/tmp/mmuuXXXXXX");
    unlink (mktemp (tmpfil));
    if ((fd = creat (tmpfil, TMPMODE)) == NOTOK)
	return MFERR;
    close (fd);
    if ((tmp_fd = open (tmpfil, 2)) == NOTOK)
	return MFERR;
    unlink (tmpfil);

    if ((fd = dup (tmp_fd)) == NOTOK) {
	close (tmp_fd);
	return MFERR;
    }
    if ((tmp = fdopen (fd, "w")) == NULL) {
	close (tmp_fd);
	close (fd);
	return MFERR;
    }

    for (;;) {
	switch (do_header (from, date, in, tmp)) {
	    case NOTOK: 
		close (tmp_fd);
		fclose (tmp);
		return MFHDR;

	    case OK: 
		continue;

	    case DONE: 
		fclose (tmp);
		break;
	}
	break;
    }

/*  */

    if (*date == NULL || *from == NULL) {
	if (*date)
	    strcpy (buffer, "No (valid) From: field found in message\n");
	else
	    if (*from)
		strcpy (buffer, "No (valid) Date: field found in message\n");
	    else
		strcpy (buffer,
			"No (valid) From: or Date: fields found in message\n");
	if (nodelim) {
	    if (*date == NULL) {
		long clock;

		time (&clock);
		sprintf (date, "%.24s", ctime (&clock));
	    }
	    if (*from == NULL)
		sprintf (from, "%s!%s", SystemName (), getusr ());
	}
	else
	    return MFHDR;
    }
    else
	buffer[0] = NULL;
	
    if (nodelim && (cp = index (from, '!')) != NULL) {
	*cp++ = NULL;
	fprintf (out, "From %s %s remote from %s\n", cp, date, from);
    }
    else
	fprintf (out, "From %s %s\n", from, date);

    fprintf (out, "Munged: from %s to %s; %s\n",
	    LocalName (), SystemName (), dtimenow ());
    if (buffer[0])
	fprintf (out, "Illegal-Field: %s", buffer);
    
    if ((tmp = fdopen (tmp_fd, "r")) == NULL) {
	close (tmp_fd);
	return MFERR;
    }
    fseek (tmp, 0L, 0);

    while ((i = fread (line, sizeof *line, sizeof line, tmp)) > 0)
	fwrite (line, sizeof *line, i, out);
    putc ('\n', out);		/* separate headers from body */
    fclose (tmp);

    return OK;
}

/*  */

static int  mmdf_text (in, out, nodelim)
int     nodelim;
FILE * in, *out;
{
    int     i;

    if (feof (in))		/* probably no body */
	putc ('\n', out);
    else
	while ((i = fread (buffer, sizeof *buffer, sizeof buffer, in)) > 0)
	    fwrite (buffer, sizeof *buffer, i, out);

    if (!nodelim)
	putc ('\n', out);
    fclose (in);

    return OK;
}

/*  */

static int  do_header (from, date, in, out)
char   *from,
       *date;
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
	case HOTHR: 
	    *cp = ':';
	    fprintf (out, "%s\n", bp);
	    break;

	case HDATE: 
	    pp = ++cp;
	    if (*date != NULL || !lequal (hl -> h_name, "Date"))
		return OK;
	    date_convert (pp, date);
	    if (*date == NULL)
		fprintf (out,
			"Illegal-Object: %s: %s -- illegal date construct\n",
			hl -> h_name, pp);
	    break;

	case HFROM: 
	    pp = ++cp;
	    if (*from != NULL)
		return OK;
	    if ((adrxp = getadrx (pp)) == NULL) {
		fprintf (out, "Illegal-Object: %s: %s -- %s\n",
			hl -> h_name, pp, "no address");
		return OK;	/* catch errors later (possibly) */
	    }
	    addr_convert (adrxp, from, TRUE);
	    if (*from == NULL)
		fprintf (out, "Illegal-Object: %s: %s -- %s\n",
			hl -> h_name, adrxp -> text, adrxp -> err);
	    while (getadrx (NULL))
		continue;
	    break;

	case HADDR: 
	case HSNDR: 
	    spat = 0;
	    some = FALSE;
	    pp = ++cp;
	    margin = pos = strlen (hl -> h_name) + 2;
	    while (adrxp = getadrx (pp)) {
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

static addr_convert (adrxp, to, notice)
struct adrx *adrxp;
char   *to;
int     notice;
{
    int     mboxlen,
            uucplen;
    char   *cp,
            tmp[LINESIZ],
            uucp[LINESIZ];
    static char path[LINESIZ] = "";

    if (path[0] == NULL)
	strcpy (path, LocalName ());

    if (adrxp -> err || !adrxp -> mbox) {
	*to = NULL;
	return;
    }
    if (notice)
	strcpy (path, adrxp -> host ? adrxp -> host : LocalName ());

    if (adrxp -> host == NULL)
	if (index (adrxp -> mbox, '!') != NULL)
	    strcpy (tmp, adrxp -> mbox);
	else
	    if (lequal (path, LocalName ()))
		sprintf (tmp, "%s!%s", SystemName (), adrxp -> mbox);
	    else
		sprintf (tmp, "%s!%s@%s", SystemName (), adrxp -> mbox, path);
    else
	if (index (adrxp -> mbox, '!') == NULL)
	    sprintf (tmp, "%s!%s@%s",
		    SystemName (), adrxp -> mbox, adrxp -> host);
	else {
	    sprintf (uucp, "%%%s", UucpChan ());
	    uucplen = strlen (uucp);
	    cp = (lequal (LocalName (), adrxp -> host)
		    && (mboxlen = strlen (adrxp -> mbox) - uucplen) > 0)
		? (adrxp -> mbox) + mboxlen : NULL;
	    if (lequal (uucp, cp))
		sprintf (tmp, "%.*s", mboxlen, adrxp -> mbox);
	    else
		if ((cp = index (adrxp -> host, '.'))
			&& lequal (UucpChan (), cp + 1))
		    sprintf (tmp, "%.*s!%s",
			cp - adrxp -> host, adrxp -> host, adrxp -> mbox);
	    else
		if (lequal (adrxp -> host, UucpChan ()))
		    strcpy (tmp, adrxp -> mbox);
	    else {
		sprintf (uucp, "%s!", SystemName ());
		uucplen = strlen (uucp);
		if (strncmp (uucp, adrxp -> mbox, uucplen))
		    sprintf (tmp, "%s!%s@%s",
			    SystemName (), adrxp -> mbox, adrxp -> host);
		else
		    sprintf (tmp, "%s@%s", adrxp -> mbox, adrxp -> host);
	    }
	}

    strcpy (to, tmp);
}

/*  */

static  date_convert (from, to)
char   *from,
       *to;
{
    char   *cp;

    if ((cp = dctime (dparsetime (from))) != NULL)
	sprintf (to, "%.24s", cp);
    else
	*to = NULL;
}

/*  */

static int  mmdf_die (error, in1, in2, out, nodelim)
int     error,
        nodelim;
FILE * in1, *in2, *out;
{
    int     i;
    long    clock;
    char    date[LINESIZ];

    if (nodelim) {
	fclose (in1);
	return error;
    }

    switch (error) {
	case MFTXT: 
	    putc ('\n', out);
	    break;
    }

    time (&clock);
    sprintf (date, "%.24s", ctime (&clock));
    fprintf (out, "From %s %s\nSubject: %s %s\n\n",
	    getusr (), date, "Bad MMDF mailbox - error in",
	    error == MFHDR ? "Header" : error == MFTXT ? "Body" : "Mailbox");

    fprintf (out, "%s: %s\n%s\n--------\n",
	    "Error detected at line", buffer, "Message being processed");
    fseek (in1, 0L, 0);
    while ((i = fread (buffer, sizeof *buffer, sizeof buffer, in1)) > 0)
	fwrite (buffer, sizeof *buffer, i, out);
    fclose (in1);

    if (!feof (in2)) {
	fprintf (out, "--------\n%s\n--------\n%s",
		"Remainder of unfiltered mailbox follows", tmpbuf);
	while ((i = fread (buffer, sizeof *buffer, sizeof buffer, in2)) > 0)
	    fwrite (buffer, sizeof *buffer, i, out);
    }

    fprintf (out, "--------\n\n");
    fflush (out);

    return error;
}
