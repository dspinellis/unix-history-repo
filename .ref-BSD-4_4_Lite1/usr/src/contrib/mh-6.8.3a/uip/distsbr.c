/* distsbr.c - routines to do additional "dist-style" processing */
#ifndef	lint
static char ident[] = "@(#)$Id: distsbr.c,v 1.6 1993/08/25 22:26:24 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <ctype.h>
#include <stdio.h>
#include <sys/types.h>	/* for off_t */

static 		ready_msg();

static int  hdrfd = NOTOK;
static int  txtfd = NOTOK;


off_t	lseek ();

/*  */

#define	BADHDR	"please re-edit %s to remove the ``%s'' header!"
#define	BADTXT	"please re-edit %s to consist of headers only!"
#define	BADMSG	"please re-edit %s to include a ``Resent-To:''!"
#define	BADRFT	"please re-edit %s and fix that header!"

int	distout (drft, msgnam, backup)
register char   *drft,
       *msgnam,
       *backup;
{
    int     state;
    register char  *dp,
                   *resent;
    char    name[NAMESZ],
            buffer[BUFSIZ];
    register    FILE *ifp,
		     *ofp;

    if (rename (drft, strcpy (backup, m_backup (drft))) == NOTOK)
	adios (backup, "unable to rename %s to",drft);
    if ((ifp = fopen (backup, "r")) == NULL)
	adios (backup, "unable to read");

    if ((ofp = fopen (drft, "w")) == NULL)
	adios (drft, "unable to create temporary file");
    (void) chmod (drft, m_gmprot ());

    ready_msg (msgnam);
    (void) lseek (hdrfd, (off_t)0, 0);	/* msgnam not accurate */
    cpydata (hdrfd, fileno (ofp), msgnam, drft);

/*  */

    for (state = FLD, resent = NULL;;)
	switch (state =
		m_getfld (state, name, buffer, sizeof buffer, ifp)) {
	    case FLD: 
	    case FLDPLUS: 
	    case FLDEOF: 
		if (uprf (name, "distribute-"))
		    (void) sprintf (name, "%s%s", "Resent", &name[10]);
		if (uprf (name, "distribution-"))
		    (void) sprintf (name, "%s%s", "Resent", &name[12]);
		if (!uprf (name, "resent")) {
		    advise (NULLCP, BADHDR, "draft", name);
		    goto leave_bad;
		}
		if (state == FLD)
		    resent = add (":", add (name, resent));
		resent = add (buffer, resent);
		fprintf (ofp, "%s: %s", name, buffer);
		while (state == FLDPLUS) {
		    state = m_getfld (state, name,
			    buffer, sizeof buffer, ifp);
		    resent = add (buffer, resent);
		    fputs (buffer, ofp);
		}
		if (state == FLDEOF)
		    goto process;
		break;

	    case BODY: 
	    case BODYEOF: 
		for (dp = buffer; *dp; dp++)
		    if (!isspace (*dp)) {
			advise (NULLCP, BADTXT, "draft");
			goto leave_bad;
		    }

	    case FILEEOF: 
		goto process;

	    case LENERR: 
	    case FMTERR: 
		advise (NULLCP, BADRFT, "draft");
	leave_bad: ;
		(void) fclose (ifp);
		(void) fclose (ofp);
		(void) unlink (drft);
		if (rename (backup, drft) == NOTOK)
		    adios (drft, "unable to rename %s to", backup);
		return NOTOK;

	    default: 
		adios (NULLCP, "getfld() returned %d", state);
	}
process: ;
    (void) fclose (ifp);
    (void) fflush (ofp);

/*  */

    if (!resent) {
	advise (NULLCP, BADMSG, "draft");
	(void) fclose (ofp);
	(void) unlink (drft);
	if (rename (backup, drft) == NOTOK)
	    adios (drft, "unable to rename %s to", backup);
	return NOTOK;
    }
    free (resent);

    if (txtfd != NOTOK) {
	(void) lseek (txtfd, (off_t)0, 0);	/* msgnam not accurate */
	cpydata (txtfd, fileno (ofp), msgnam, drft);
    }

    (void) fclose (ofp);

    return OK;
}

/*  */

static	ready_msg (msgnam)
register char   *msgnam;
{
    int     state,
            out;
    char    name[NAMESZ],
            buffer[BUFSIZ],
            tmpfil[BUFSIZ];
    register    FILE *ifp,
		     *ofp;

    if (hdrfd != NOTOK)
	(void) close (hdrfd), hdrfd = NOTOK;
    if (txtfd != NOTOK)
	(void) close (txtfd), txtfd = NOTOK;

    if ((ifp = fopen (msgnam, "r")) == NULL)
	adios (msgnam, "unable to open message");

    (void) strcpy (tmpfil, m_tmpfil ("dist"));
    if ((hdrfd = creat (tmpfil, 0600)) == NOTOK)
	adios (tmpfil, "unable to create temporary file");
    (void) close (hdrfd);
    if ((hdrfd = open (tmpfil, 2)) == NOTOK)
	adios (tmpfil, "unable to re-open temporary file");
    if ((out = dup (hdrfd)) == NOTOK
	    || (ofp = fdopen (out, "w")) == NULL)
	adios (NULLCP, "no file descriptors -- you lose big");
    (void) unlink (tmpfil);

/*  */

    for (state = FLD;;)
	switch (state =
		m_getfld (state, name, buffer, sizeof buffer, ifp)) {
	    case FLD: 
	    case FLDPLUS: 
	    case FLDEOF: 
		if (uprf (name, "resent"))
		    fprintf (ofp, "Prev-");
		fprintf (ofp, "%s: %s", name, buffer);
		while (state == FLDPLUS) {
		    state = m_getfld (state, name,
			    buffer, sizeof buffer, ifp);
		    fputs (buffer, ofp);
		}
		if (state == FLDEOF)
		    goto process;
		break;

	    case BODY: 
	    case BODYEOF: 
		(void) fclose (ofp);

		(void) strcpy (tmpfil, m_tmpfil ("dist"));
		if ((txtfd = creat (tmpfil, 0600)) == NOTOK)
		    adios (tmpfil, "unable to create temporary file");
		(void) close (txtfd);
		if ((txtfd = open (tmpfil, 2)) == NOTOK)
		    adios (tmpfil, "unable to re-open temporary file");
		if ((out = dup (txtfd)) == NOTOK
			|| (ofp = fdopen (out, "w")) == NULL)
		    adios (NULLCP, "no file descriptors -- you lose big");
		(void) unlink (tmpfil);
		fprintf (ofp, "\n%s", buffer);
		while (state == BODY) {
		    state = m_getfld (state, name,
			    buffer, sizeof buffer, ifp);
		    fputs (buffer, ofp);
		}
	    case FILEEOF: 
		goto process;

	    case LENERR: 
	    case FMTERR: 
		adios (NULLCP, "format error in message %s", msgnam);

	    default: 
		adios (NULLCP, "getfld() returned %d", state);
	}
process: ;
    (void) fclose (ifp);
    (void) fclose (ofp);
}
