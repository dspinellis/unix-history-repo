#define	MFDEBUG			/* temporarily */
#ifndef	lint
static char Id[] = "@(#)$Id: rmail.c,v 1.2 1993/08/25 17:32:49 jromine Exp $";
#endif

#include "util.h"
#include "mmdf.h"
#include <pwd.h>
#include <signal.h>
#include "mf.h"
#include "tws.h"

/*
 *				R M A I L . C
 *
 *	Developed from the Berkeley mail program of the same name
 *	by Mike Obrien at RAND to run with the MMDF mail system.
 *	Rewritten by Doug Kingston, US Army Ballistics Research Laboratory
 *	Hacked a lot by Steve Bellovin (smb@unc)
 *
 *	This program runs SETUID to root so that it can set effective and
 *	real [ug]ids to mmdflogin.
 *
 *    27-Oct-82	Marshall T. Rose <mrose%uci@rand-relay>
 *		Support proper munging by using the UCI mail filtering
 *		routines (enabled by #ifdef MF)
 *		Also use ll_log() package (enabled by #ifdef LOG)
 *
 *    17-Oct-83 Marshall T. Rose <mrose%uci@rand-relay>
 *		New interfacing.  Remove the #ifdef:s
 */


#define NAMESZ  64      /* Limit on component name size         */

#define	ADDROK	0
#define	UUCP	1
#define	RETURN	2

int	Syscpy = 1;
int	Tmpmode = 0600;
char   *Fromtmp = "/tmp/rml.f.XXXXXX";
char   *Msgtmp = "/tmp/rml.m.XXXXXX";
char   *Errtmp = "/tmp/rml.e.XXXXXX";
char   *Okhosts = "/usr/mmdf/table/rmail.ok";
char   *Okdests = "/usr/mmdf/table/rmail.okdests";

extern  char	cmddfldir[];
extern  char    logdfldir[];
extern	char	mmdflogin[];
extern  char    pathsubmit[];
extern	char	sitesignature[];
extern	char	supportaddr[];
extern  struct ll_struct chanlog;

char   *dupfpath (), *index(), *rindex();
struct  passwd *getpwnam(), *getpwuid();
FILE   *popen();

/*  */

struct ll_struct *logptr = &chanlog;

FILE *fromf;		/* temporary out for colon-less UUCP "From" lines */
FILE *msgf;		/* temporary out for message text */
FILE *mmdf;		/* filtered mail file */
FILE *pipef;		/* output to "submit" or "uux" */
char date[LINESIZE];	/* date of origination from uucp header */
char from[LINESIZE];	/* accumulated path of sender */
char origsys[NAMESZ];	/* originating system */
char origpath[LINESIZE];/* path from us to originating system */
char usrfrm[LINESIZE];
char Mailsys[LINESIZE];
int pbroke;		/* broken-pipe flag */
int rtnflag;		/* note was sent back */

int	brpipe();

/*  */

main(argc, argv)
char **argv;
{
	char fromwhom[NAMESZ];		/* user on remote system */
	char *fromptr;
	char linebuf[LINESIZE];		/* scratchpad */
	char sys[NAMESZ];		/* an element in the uucp path */
	char *cp;
	struct passwd *pw;
	int  error;
	int cpyback;

	if (argc < 2) {
		fprintf(stderr, "Usage: rmail user [user ...]\n");
		exit(1);
	}
	umask (0);

	ll_hdinit (logptr, "RM");
	logptr -> ll_file = dupfpath (logptr -> ll_file, logdfldir);

	if ((pw = getpwnam (mmdflogin)) == NULL) {
		fprintf (stderr, "Cannot find mmdflogin\n");
		exit (99);
	}
	setgid (pw->pw_gid);
	setuid (pw->pw_uid);

	sprintf (Mailsys, "%s <%s@%s>",
	    sitesignature, mmdflogin, LocalName ());

/*  */

    {				/* BE VERY SURE... */
	int     i;
    
	for (i = fileno (stdout); i <= HIGHFD; i++)
	    close (i);
    }

	/* create file to hold stderr output.  We first open some */
	/* null file to make sure stdout is taken.  If stdin isn't */
	/* open either, we've got so much trouble it isn't even worth */
	/* worrying about a little more */
	open("/dev/null", 0);
	mktemp(Errtmp);
	if (freopen(Errtmp, "w", stderr) == NULL) {
		fprintf(stderr, "Can't create %s\n", Errtmp);
		ll_log (logptr, LLOGFAT, "Unable to create '%s'",
			Errtmp);
		exit(1);
	}
	dup2 (fileno (stderr), fileno (stdout));

	/* Create temp file for rest of message */
	mktemp (Msgtmp);
	if ((msgf=fdopen(creat(Msgtmp, Tmpmode), "w")) == NULL) {
		fprintf(stderr, "Can't create %s\n", Msgtmp);
		ll_log (logptr, LLOGFAT, "Unable to create '%s'",
			Msgtmp);
		exit(1);
	}

	/* create temp file for colon-less UUCP "From" lines */
	mktemp (Fromtmp);
	if ((fromf=fdopen(creat(Fromtmp, Tmpmode), "w")) == NULL) {
		fprintf(stderr, "Can't create %s\n", Fromtmp);
		ll_log (logptr, LLOGFAT, "Unable to create '%s'",
			Fromtmp);
		exit(1);
	}

/*  */

	for (;;) {
		if( fgets(linebuf, sizeof linebuf, stdin) == NULL )
			break;
		if(   strncmp(linebuf, "From ", 5)
		   && strncmp(linebuf, ">From ", 6) )
			break;

		if (linebuf[0] != '>')
			fputs (">", fromf);
		fputs(linebuf, fromf);	/* Save, we may forward via UUCP */
		cp = index (linebuf, ' ');	/* start of name */
		fromptr = ++cp;
		cp = index (cp, ' ');		/* cp at end of name */
		*cp++ = 0;			/* term. name, cp at date */
		strcpy (fromwhom, fromptr);
		strncpy (date, cp, 24);		/* Mon Nov 10 23:12:09 1981 */

		for (;;) {
			cp = index(cp+1, 'r');
			if (cp == NULL) {
				cp = rindex(fromwhom, '!');
				if (cp != NULL) {
					char *p;
					*cp = '\0';
					p = rindex(fromwhom, '!');
					if (p != NULL) strcpy(origsys, p+1);
					else strcpy(origsys, fromwhom);
					strcat(from, fromwhom);
					strcat(from, "!");
					strcpy(fromwhom, cp+1);
					goto out;
				}
				strcpy (sys, SystemName ());
				strcat (from, sys);
				strcpy (origsys, sys);
				strcat (from, "!");
				goto out;
			}
			if (strncmp(cp, "remote from ", 12) == 0)
				break;
		}

		sscanf(cp, "remote from %s", sys);
		strcat(from, sys);
		strcpy(origsys, sys);		/* Save for quick ref. */
		strcat(from, "!");
out:;
	}
	if( fromwhom[0] == '\0' )		/* No from line, illegal */
		exit(99);

/*  */

	strcpy (origpath, from);
	strcat (from, fromwhom);
        mf_get_addr (from, usrfrm);
	if ((cp = rindex (usrfrm, '<')) != NULL) {
	    strcpy (usrfrm, ++cp);/* sigh */
	    if ((cp = rindex (usrfrm, '>')) != NULL)
		*cp = NULL;
	    }
	if (usrfrm[0] == NULL)
	    sprintf (usrfrm, "%s!%s%%%s@%s",
		SystemName (), from, UucpChan (), LocalName ());
	ll_log (logptr, LLOGGEN, "Rmail from '%s' (%s)", from, usrfrm);
	fputs (linebuf, msgf);
	if (rp_isbad (txtcpy (stdin, msgf)))
	    fputs ("\n  *** Problem during receipt from UUCP ***\n", msgf);

	freopen (Msgtmp, "r", msgf);
	freopen (Fromtmp, "r", fromf);
	unlink (Msgtmp);
	unlink (Fromtmp);
	mmdf = NULL;

	cpyback = 0;
	for (argv++; --argc > 0; ) {
		rewind (fromf);
		rewind (msgf);
		if (mmdf != NULL)
		    rewind (mmdf);
		pbroke = 0;
		rtnflag = 0;
		signal(SIGPIPE, brpipe);
		if (rp_isbad(deliver(*argv++)) && !rtnflag)
			cpyback++;
	}

	/* Send back a copy if something nasty happened.  For now, we use */
	/* a real kludge -- we see if we noted some error, or if we find */
	/* anything written to stderr....  */
	fflush(stderr);
	fflush (stdout);

	if (cpyback) {rcpy();zcpy();}

	unlink(Errtmp);
	ll_close (logptr);
	exit (0);
}

/*  */

/*
 *	deliver() -- Handle all deliveries be they returns, automatic
 *			copies, or the real thing.  Based on the address
 *			the letter is accepted or returned with a copy
 *			to the system administrators
 *
 *			main() has set up the "from" string and the
 *			"date" string.
 */
char	rtnend[] =
	"  --------------- End of Returned Mail ---------------\n";

deliver(to)
char *to;
{
	int	replyval;
	int	i;
	char	linebuf[LINESIZE];
	char	tmpbuf[LINESIZE];

	switch (adrcheck (to)) {
	case ADDROK:
		ll_log (logptr, LLOGGEN, "Rmail to '%s' via MMDF", to);
		if (rp_isbad (replyval = 
			xsubmit (NULL, usrfrm, NULL, NULL, to)))
		    break;
		if (mmdf == NULL)
		    if (mf_get_msg () == NOTOK)
			mmdf = msgf;
		replyval = txtcpy (mmdf, pipef);
#ifndef RUNALON
		i = (pclose(pipef) >> 8 ) & 0xff;
		if (rp_isgood(replyval)) replyval = i;
#endif
		break;

	case UUCP:
		ll_log (logptr, LLOGGEN, "Rmail to '%s' via UUCP", to);
		if (rp_isbad (replyval = xuucp(from, to)))
			break;
		replyval = txtcpy(msgf, pipef);
#ifndef RUNALON
		i = (pclose(pipef) >> 8 ) & 0xff;
		if (rp_isgood(replyval)) replyval = (i == 0 ? RP_OK : RP_LIO);
#endif
		break;

/*  */

	case RETURN:
		rtnflag = 1;
		ll_log (logptr, LLOGGEN, "Illegal Rmail to '%s'", to);
		switch (adrcheck (from)) {
		case ADDROK:
		case RETURN:
			replyval = xsubmit (dtimenow (), Mailsys,
					from, supportaddr, from);
			rtnmesg(to);
			txtcpy(fromf, pipef);
			txtcpy(msgf, pipef);
			fputs (rtnend, pipef);
#ifndef RUNALON
			i = (pclose(pipef) >> 8 ) & 0xff;
			if (rp_isgood(replyval)) replyval = i;
#endif
			break;

		case UUCP:
			replyval = xuucp (mmdflogin, from);
			if (rp_isbad (replyval))
				break;
			fprintf (pipef, "To: %s\n", from);
			fprintf (pipef, "Cc: %s\n", supportaddr);
			rtnmesg(to);
			txtcpy(fromf, pipef);
			txtcpy(msgf, pipef);
			fputs (rtnend, pipef);
#ifndef RUNALON
			i = (pclose(pipef) >> 8 ) & 0xff;
			if (rp_isgood(replyval))
			    replyval = (i == 0 ? RP_OK : RP_LIO);
#endif
			break;
		}

		/* And now for the mail overseer's copy */
		if (Syscpy) {
		        ll_log (logptr, LLOGGEN, "Notifying %s", supportaddr);
			rewind (fromf);
			rewind (msgf);

			replyval = xsubmit (dtimenow (), Mailsys,
					usrfrm, supportaddr, supportaddr);
			if (rp_isbad (replyval))
				break;
			rtnmesg(to);
			txtcpy (fromf, pipef);
			txtcpy (msgf, pipef);
			fputs (rtnend, pipef);
#ifndef RUNALON
			i = (pclose(pipef) >> 8 ) & 0xff;
			if (rp_isgood(replyval)) replyval = i;
#endif
		}
	}
	return (replyval);
}

/*  */

adrcheck (adr)                             /* Gateway to Arpanet? */
char *adr;
{
    char   *cp,
            err[BUFSIZ],
            host[BUFSIZ],
            mbox[BUFSIZ];
    struct adrx *adrxp;

    if ((adrxp = seekadrx (adr)) == NULL)
	return RETURN;
    strcpy (err, adrxp -> err ? adrxp -> err : "");
    strcpy (host, adrxp -> host ? adrxp -> host : "");
    strcpy (mbox, adrxp -> mbox ? adrxp -> mbox : "");
    while (seekadrx (NULL))
	continue;

    if (err[0] || mbox[0] == NULL)
	return RETURN;
    if (index (mbox, '!') || host[0] == NULL)
	return UUCP;
    if (rp_isgood (lookup (origsys, Okhosts)))
	return ADDROK;
    if (index (host, '@') || rp_isbad (okhost (host)))
	return RETURN;

    return ADDROK;
}


okhost(host)             /* Host permitted to use mail facilities? */
char *host;
{
	if (rp_isgood (lookup (origsys, Okhosts)))
		return (RP_OK);		/* Fully privledged originator */
	if (rp_isgood (lookup (host, Okhosts)))
		return (RP_OK);		/* Fully privledged dest */
	if (rp_isgood (lookup (host, Okdests)))
		return (RP_OK);		/* Unrestricted Dest. Host, OK */
	return(RP_NO);			/* Not permitted; must be bad */
}

/*  */

/*
 *	lookup()  --	This lookup function looks for strings which
 *			must be the first string on a line.  Sorry Dave (dhc)
 *			but the MMDF channel functions are too specific
 *			to be easily used here without much kludging.
 */

/*****************************************
****	Can this be a call to a MMDF function??
****	Remember I have the RHOSTs table and the OKHOSTS table.
******************************************/

lookup (what, where)
char *what, *where;
{
	FILE *lookf;
	char entry[LINESIZE];
	char *cp;

	if ((lookf = fopen (where, "r")) == NULL)
		return (RP_NO);		/* Unknown problem */
	while (fgets (entry, sizeof entry, lookf) != NULL) {
		cp = entry;
		while (*cp != '\n' && *cp != ' ' && *cp != '\t')
			cp++;
		*cp = 0;
		if (lexequ (what, entry)) {
			fclose (lookf);
			return (RP_OK);
		}
	}
	fclose (lookf);
	return (RP_NO);
}


/*  */

char *rtnmessage[] = {
	"	Your message has been intercepted trying to access\n",
	"a restricted access host (e.g. an ARPANET host).  A copy\n",
	"of your message has been sent to the system administrators.\n",
	"The text of your message follows.\n\n",
	"  --------------- Returned Mail Follows --------------\n",
	0
};

rtnmesg (badadr)
char *badadr;
{
	char **cpp;

	fprintf (pipef, "Subject:  Illegal Address (%s)\n\n", badadr);
	for (cpp = rtnmessage; *cpp; cpp++)
		fputs (*cpp, pipef);
}

txtcpy (frm, to)
FILE *frm, *to;
{
	char buffer[BUFSIZ];
	int nread;

	while (!pbroke && (nread = fread (buffer, sizeof (*buffer), BUFSIZ, frm)) > 0)
		fwrite (buffer, sizeof (*buffer), nread, to);
	return (ferror (frm) ? RP_LIO : RP_OK );
}

/*  */

xsubmit (date, from, to, cc, realto)
char *date, *from, *to, *cc, *realto;
{
    char    cmdstr[LINESIZE],
            submit[LINESIZE];

    getfpath (pathsubmit, cmddfldir, submit);
    sprintf (cmdstr, "%s '-mlti%s*'", submit, UucpChan ());

#ifndef RUNALON
    if ((pipef = popen (cmdstr, "w")) == NULL)
	return (RP_NO);
#else
    pipef = stdout;
    printf ("%s\n", cmdstr);
#endif

    fprintf (pipef, "%s\n%s\n!\n", from, realto);

    if (date) {
	fprintf (pipef, "Date:     %s\n", date);
	fprintf (pipef, "From:     %s\n", from);
    }
    if (to) {
	fprintf (pipef, "To:       %s", to);
	if (index (to, '@'))
	    fputc ('\n', pipef);/* Explicit host specified */
	else
	    fprintf (pipef, "@%s\n", LocalName ());
    }
    if (cc) {
	fprintf (pipef, "Cc:       %s\n", cc);
    }

    return (RP_OK);
}

/*  */

xuucp (from, to)
char *from, *to;
{
	char	cmdstr[LINESIZE];

	sprintf (cmdstr, "/etc/delivermail -r%s -ep -m -s -i %s", from, to);

#ifndef RUNALON
	if ((pipef = popen (cmdstr, "w")) == NULL)
		return (RP_NO);
#else
	pipef = stdout;
	printf ("%s\n", cmdstr);
#endif

	return (RP_OK);
}

/*  */

brpipe()	/* catch broken-pipe signals */
{
	signal(SIGPIPE, SIG_IGN);
	pbroke = 1;
}

char *oopsmessage[] = {
    "\n\n\tThe system administrators (%s) have been informed of the\n",
    "problem, but have not been given a copy of your message.\n",
    NULL
};

/*  */

rcpy () {
    int     i;
    char    buffer[BUFSIZ],
	    message[BUFSIZ];
    FILE * fp;

    ll_log (logptr, LLOGGEN, "Advising %s of failure as %s...", from, usrfrm);

    sprintf (buffer, "Problems sending mail:\n\n");
    if (ml_1adr (NO, NO, sitesignature, "Problems sending mail", usrfrm)
	    != OK)
	goto ml_err;
    ml_txt (buffer);

    if ((fp = fopen (Errtmp, "r")) != NULL) {
	ml_file (fp);
	if (ftell (fp) == 0L)
	    fprintf (pipef, "\tunknown problem\n");
	fclose (fp);
    }
    else
	ml_txt ("\tunknown problem\n");

    for (i = 0; oopsmessage[i]; i++) {
	sprintf (message, oopsmessage[i], supportaddr);
	ml_txt (message);
    }
    fprintf (pipef, "\n\nReturned message follows:\n\n---------------\n\n");
    rewind (fromf);
    ml_file (fromf);
    rewind (msgf);
    ml_file (msgf);

    if (ml_end (OK) != OK) {
	char   *cp;

ml_err: ;
	if (cp = index (buffer, '\n'))
	    *cp = NULL;
	ll_log (logptr, LLOGFAT, "Unable to post failure notice");
	ll_log (logptr, LLOGFAT, "info: %s", buffer);
    }
}

/*  */

zcpy () {
    char    buffer[BUFSIZ];
    FILE * fp;

    ll_log (logptr, LLOGGEN, "Advising %s of failure...", supportaddr);

    sprintf (buffer, "Problems sending mail for %s (aka %s):\n\n",
	    from, usrfrm);
    if (ml_1adr (NO, NO, sitesignature, "Problems sending mail", supportaddr)
	    != OK)
	goto ml_err;
    ml_txt (buffer);

    if ((fp = fopen (Errtmp, "r")) != NULL) {
	ml_file (fp);
	if (ftell (fp) == 0L)
	    fprintf (pipef, "\tunknown problem\n");
	fclose (fp);
    }
    else
	ml_txt ("\tunable to open error file\n");

    if (ml_end (OK) != OK) {
	char   *cp;

ml_err: ;
	if (cp = index (buffer, '\n'))
	    *cp = NULL;
	ll_log (logptr, LLOGFAT, "Unable to post failure notice");
	ll_log (logptr, LLOGFAT, "info: %s", buffer);
    }
}

/*  */

mf_get_msg () {
    int     i,
            fd,
            md,
            td;
    char    buffer[BUFSIZ],
            tmpfil[LINESIZE],
            mmdfil[LINESIZE];
#ifdef	MFDEBUG
    FILE * fp;
#endif	MFDEBUG
    FILE * out;

    strcpy (tmpfil, "/tmp/rmailXXXXXX");
    unlink (mktemp (tmpfil));
    if ((fd = creat (tmpfil, Tmpmode)) == NOTOK)
	return NOTOK;
    close (fd);
    if ((fd = open (tmpfil, 2)) == NOTOK)
	return NOTOK;
    if ((out = fdopen (fd, "w")) == NULL) {
	close (fd);
	return NOTOK;
    }
    if ((td = dup (fd)) == NOTOK) {
	close (fd);
	return NOTOK;
    }

    fprintf (out, "From %s %s\n", from, date);
    if (rp_isbad (txtcpy (msgf, out))) {
	close (fd);
	close (td);
	return NOTOK;
    }
    fclose (out);
    lseek (td, (off_t)0, 0);

    strcpy (mmdfil, "/tmp/mmdfXXXXXX");
    unlink (mktemp (mmdfil));
    if ((fd = creat (mmdfil, Tmpmode)) == NOTOK) {
	close (td);
	unlink (tmpfil);
	return NOTOK;
    }
    if ((fd = open (mmdfil, 2)) == NOTOK) {
	close (td);
	unlink (tmpfil);
	return NOTOK;
    }
    if ((md = dup (fd)) == NOTOK) {
	close (td);
	unlink (tmpfil);
	close (fd);
	return NOTOK;
    }

/*  */

    switch (i = uucp_to_mmdf (td, fd, TRUE)) {
	case OK: 
	    lseek (md, (off_t)0, 0);
	    if ((mmdf = fdopen (md, "r")) != NULL)
		break;

	default: 
	    close (md);

	    sprintf (buffer, "rmail(%d) filtering failed(%d)\n",
		    getpid (), i);
	    if (ml_1adr (NO, NO, sitesignature, "MF Failure", supportaddr)
		    != OK)
		goto ml_err;
	    ml_txt (buffer);
#ifdef	MFDEBUG
	    lseek (td, (off_t)0, 0);
	    if ((md = dup (td)) == NOTOK)
		ml_txt ("unable to dup() descriptor for message copy\n");
	    else
		if ((fp = fdopen (md, "r")) == NULL) {
		    ml_txt ("unable to fdopen() descriptor for message copy\n");
		    close (md);
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

	    md = NOTOK;
	    break;
    }
    close (td);
    unlink (tmpfil);
    close (fd);
    unlink (mmdfil);

    return md;
}

/*  */

mf_get_addr (from, to)
char *from,
     *to;
{
    struct adrx *adrxp,
               *seekadrx ();

    *to = NULL;
    if ((adrxp = seekadrx (from)) == NULL)
	return;
    addr_convert (adrxp, to, TRUE);
    while (seekadrx (NULL))
	continue;
    return;
}
