/* whatnowsbr.c - the WhatNow shell */
#ifndef	lint
static char ident[] = "@(#)$Id: whatnowsbr.c,v 1.21 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef	MIME
#define	MIMEminc(a)	(a)
#else
#define	MIMEminc(a)	0
#endif

static int	editfile(), copyf(), sendfile(), sendit(), whomfile();
#ifdef	MIME
static int	mhnfile();
#endif
/*  */

static struct swit whatnowswitches[] = {
#define	DFOLDSW	0
    "draftfolder +folder", 0,
#define	DMSGSW	1
    "draftmessage msg", 0,
#define	NDFLDSW	2
    "nodraftfolder", 0,

#define	EDITRSW	3
    "editor editor", 0,
#define	NEDITSW	4
    "noedit", 0,

#define	PRMPTSW	5
    "prompt string", 4,

#define	HELPSW	6
    "help", 4,

    NULL, 0
};

/*  */

static struct swit aleqs[] = {
#define	DISPSW	0
    "display [<switches>]", 0,
#define	EDITSW	1
    "edit [<editor> <switches>]", 0,
#define	LISTSW	2
    "list [<switches>]", 0,
#define	PUSHSW	3
    "push [<switches>]", 0,
#define	QUITSW	4
    "quit [-delete]", 0,
#define	REFILEOPT 5
    "refile [<switches>] +folder", 0,
#define	SENDSW	6
    "send [<switches>]", 0,
#define	WHOMSW	7
    "whom [<switches>]", 0,

    NULL, 0
};

/*  */

static char *myprompt = "\nWhat now? ";

/*  */

/* ARGSUSED */

int	WhatNow (argc, argv)
int	argc;
char  **argv;
{
    int     isdf = 0,
	    nedit = 0,
	    use = 0;
    char   *cp,
           *dfolder = NULL,
           *dmsg = NULL,
           *ed = NULL,
           *drft = NULL,
           *msgnam = NULL,
            buf[100],
	    prompt[BUFSIZ],
          **ap,
          **argp,
           *arguments[MAXARGS];
    struct stat st;

    invo_name = r1bindex (argv[0], '/');
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, whatnowswitches)) {
		case AMBIGSW: 
		    ambigsw (cp, whatnowswitches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [switches] [file]", invo_name);
		    help (buf, whatnowswitches);
		    done (1);

		case DFOLDSW: 
		    if (dfolder)
			adios (NULLCP, "only one draft folder at a time!");
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    dfolder = path (*cp == '+' || *cp == '@' ? cp + 1 : cp,
			    *cp != '@' ? TFOLDER : TSUBCWF);
		    continue;
		case DMSGSW: 
		    if (dmsg)
			adios (NULLCP, "only one draft message at a time!");
		    if (!(dmsg = *argp++) || *dmsg == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case NDFLDSW: 
		    dfolder = NULL;
		    isdf = NOTOK;
		    continue;

		case EDITRSW: 
		    if (!(ed = *argp++) || *ed == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    nedit = 0;
		    continue;
		case NEDITSW: 
		    nedit++;
		    continue;

		case PRMPTSW:
		    if (!(myprompt = *argp++) || *myprompt == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
	    }
	if (drft)
	    adios (NULLCP, "only one draft at a time!");
	else
	    drft = cp;
    }

/*  */

    if (drft == NULL && (drft = getenv ("mhdraft")) == NULL || *drft == 0)
	drft = getcpy (m_draft (dfolder, dmsg, 1, &isdf));
    msgnam = (cp = getenv ("mhaltmsg")) && *cp ? getcpy (cp) : NULLCP;
    if (ed == NULL && ((ed = getenv ("mheditor")) == NULL || *ed == 0))
	ed = NULL, nedit++;
    if ((cp = getenv ("mhuse")) && *cp)
	use = atoi (cp);
    if (!nedit
	    && editfile (&ed, NULLVP, drft, use, NULLMP, msgnam, NULLCP) < 0)
	done (1);

/*  */

    (void) sprintf (prompt, myprompt, invo_name);
    for (;;) {
	if (!(argp = getans (prompt, aleqs))) {
	    (void) unlink (LINK);
	    done (1);
	}
	switch (smatch (*argp, aleqs)) {
	    case DISPSW: 
		if (msgnam)
		    (void) showfile (++argp, msgnam);
		else
		    advise (NULLCP, "no alternate message to display");
		break;

	    case EDITSW: 
		if (*++argp)
		    ed = *argp++;
		if (editfile (&ed, argp, drft, NOUSE, NULLMP, msgnam, NULLCP)
			== NOTOK)
		    done (1);
		break;

	    case LISTSW: 
		(void) showfile (++argp, drft);
		break;

	    case WHOMSW: 
		(void) whomfile (++argp, drft);
		break;

	    case QUITSW: 
		if (*++argp && (*argp[0] == 'd' ||
			    ((*argp)[0] == '-' && (*argp)[1] == 'd'))) {
		    if (unlink (drft) == NOTOK)
			adios (drft, "unable to unlink");
		}
		else
		    if (stat (drft, &st) != NOTOK)
			advise (NULLCP, "draft left on %s", drft);
		done (1);

	    case PUSHSW: 
		if (sendfile (++argp, drft, 1))
		    done (1);
		break;

	    case SENDSW: 
		(void) sendfile (++argp, drft, 0);
		break;

	    case REFILEOPT: 
		if (refile (++argp, drft) == 0)
		    done (0);
		break;

	    default: 
		advise (NULLCP, "say what?");
		break;
	}
    }
    /*NOTREACHED*/
}

/*    EDIT */

static int  reedit = 0;
static char *edsave = NULL;


/* ARGSUSED */

static	int editfile (ed, arg, file, use, mp, altmsg, cwd)
register struct msgs   *mp;
register char **ed,
              **arg,
               *file,
               *altmsg,
               *cwd;
register int    use;
{
    int     pid,
            status;
    register int    vecp;
    register char  *cp;
    char    altpath[BUFSIZ],
            linkpath[BUFSIZ],
           *vec[MAXARGS];
    struct stat st;
#ifdef BSD42
#ifdef	notdef
    int     oumask;	/* PJS: for setting permissions on symlinks. */
#endif
    int	    slinked;
#endif	/* BSD42 */

    if (!reedit) {		/* set initial editor */
	if (*ed == NULL && (*ed = m_find ("editor")) == NULL)
	    *ed = sysed;
    }
    else
	if (!*ed) {		/* no explicit editor */
	    *ed = edsave;
	    if ((cp = r1bindex (*ed, '/')) == NULL)
		cp = *ed;
	    cp = concat (cp, "-next", NULLCP);
	    if ((cp = m_find (cp)) != NULL)
		*ed = cp;
	}

    if (altmsg) {
	if (mp == NULL || *altmsg == '/' || cwd == NULL)
	    (void) strcpy (altpath, altmsg);
	else
	    (void) sprintf (altpath, "%s/%s", mp -> foldpath, altmsg);
	if (cwd == NULL)
	    (void) strcpy (linkpath, LINK);
	else
	    (void) sprintf (linkpath, "%s/%s", cwd, LINK);
    }

    if (altmsg) {
	(void) unlink (linkpath);
#ifdef BSD42
	if (link (altpath, linkpath) == NOTOK) {
#ifdef	notdef	/* I don't think permission on symlinks matters /JLR */
	    oumask = umask(0044);	/* PJS: else symlinks are world 'r' */
#endif
	    (void) symlink (altpath, linkpath);
#ifdef	notdef
	    umask(oumask);		/* PJS: else symlinks are world 'r' */
#endif
	    slinked = 1;
	}
	else
	    slinked = 0;
#else	/*  not BSD42 */
	(void) link (altpath, linkpath);
#endif	/* not BSD42 */
    }

    m_update ();
    (void) fflush (stdout);

    switch (pid = vfork ()) {
	case NOTOK: 
	    advise ("fork", "unable to");
	    status = NOTOK;
	    break;

	case OK: 
	    if (cwd)
		(void) chdir (cwd);
	    if (altmsg) {
		if (mp)
		    (void) m_putenv ("mhfolder", mp -> foldpath);
		(void) m_putenv ("editalt", altpath);
	    }

	    vecp = 0;
	    vec[vecp++] = r1bindex (*ed, '/');
	    if (arg)
		while (*arg)
		    vec[vecp++] = *arg++;
	    vec[vecp++] = file;
	    vec[vecp] = NULL;

	    execvp (*ed, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (*ed);
	    _exit (-1);

	default: 
	    if (status = pidwait (pid, NOTOK)) {
#ifdef	ATTVIBUG
		if ((cp = r1bindex (*ed, '/'))
			&& strcmp (cp, "vi") == 0
			&& (status & 0x00ff) == 0)
		    status = 0;
		else {
#endif
		if (((status & 0xff00) != 0xff00)
			&& (!reedit || (status & 0x00ff)))
		    if (!use && (status & 0xff00) &&
			    (rename (file, cp = m_backup (file)) != NOTOK)) {
			advise (NULLCP, "problems with edit--draft left in %s",
				cp);
		    }
		    else
			advise (NULLCP, "problems with edit--%s preserved",
				file);
		status = -2;	/* maybe "reedit ? -2 : -1"? */
		break;
#ifdef	ATTVIBUG
		}
#endif
	    }

	    reedit++;
#ifdef BSD42
	    if (altmsg
		    && mp
		    && (!mp -> msgflags & READONLY)
		    && (slinked
		           ? lstat (linkpath, &st) != NOTOK
				&& (st.st_mode & S_IFMT) == S_IFREG
				&& copyf (linkpath, altpath) == NOTOK
		           : stat (linkpath, &st) != NOTOK
				&& st.st_nlink == 1
				&& (unlink (altpath) == NOTOK
					|| link (linkpath, altpath) == NOTOK)))
		advise (linkpath, "unable to update %s from", altmsg);
#else	/* not BSD42 */
	    if (altmsg
		    && mp
		    && (!mp -> msgflags & READONLY)
		    && stat (linkpath, &st) != NOTOK
		    && st.st_nlink == 1
		    && (unlink (altpath) == NOTOK
			|| link (linkpath, altpath) == NOTOK))
		advise (linkpath, "unable to update %s from", altmsg);
#endif	/* not BSD42 */
    }

    edsave = getcpy (*ed);
    *ed = NULL;
    if (altmsg)
	(void) unlink (linkpath);

    return status;
}

/*  */

#ifdef	BSD42
static int  copyf (ifile, ofile)
register char   *ifile,
		*ofile;
{
    register int    i;
    int     in,
            out;
    char    buffer[BUFSIZ];

    if ((in = open (ifile, 0)) == NOTOK)
	return NOTOK;
    if ((out = open (ofile, 1)) == NOTOK || ftruncate (out, 0) == NOTOK) {
	if (out != NOTOK) {
	    admonish (ofile, "unable to truncate");
	    (void) close (out);
	}
	(void) close (in);
	return NOTOK;
    }

    while ((i = read (in, buffer, sizeof buffer)) > OK)
	if (write (out, buffer, i) != i) {
	    advise (ofile, "may have damaged");
	    i = NOTOK;
	    break;
	}

    (void) close (in);
    (void) close (out);

    return i;
}
#endif	/* BSD42 */

/*    SEND */

static  sendfile (arg, file, pushsw)
register char **arg,
               *file;
int     pushsw;
{
    register int    child_id,
                    i,
                    vecp;
    char *cp,
	 *sp,
	 *vec[MAXARGS];

#ifdef	MIME
    if ((cp = m_find ("automhnproc"))
	    && !getenv ("NOMHNPROC")
	    && mhnfile (file)
	    && (i = editfile (&cp, NULLVP, file, NOUSE, NULLMP, NULLCP,
			      NULLCP)))
	return 0;
#endif

    if (strcmp (sp = r1bindex (sendproc, '/'), "send") == 0) {
	cp = invo_name;
	sendit (invo_name = sp, arg, file, pushsw);
	invo_name = cp;
	return 1;
    }

    m_update ();
    (void) fflush (stdout);

    for (i = 0; (child_id = vfork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK: 
	    advise (NULLCP, "unable to fork, so sending directly...");
	case OK: 
	    vecp = 0;
	    vec[vecp++] = invo_name;
	    if (pushsw)
		vec[vecp++] = "-push";
	    if (arg)
		while (*arg)
		    vec[vecp++] = *arg++;
	    vec[vecp++] = file;
	    vec[vecp] = NULL;

	    execvp (sendproc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (sendproc);
	    _exit (-1);

	default: 
	    if (pidwait (child_id, OK) == 0)
		done (0);
	    return 1;
    }
}

/*  */

#ifdef MIME
#include "../h/mhn.h"


static int  mhnfile (msgnam)
char   *msgnam;
{
    int	    state;
    char    buf[BUFSIZ],
	    name[NAMESZ];
    FILE   *fp;

    if ((fp = fopen (msgnam, "r")) == NULL)
	return 0;
    for (state = FLD;;)
	switch (state = m_getfld (state, name, buf, sizeof buf, fp)) {
	    case FLD:
	    case FLDPLUS:
	    case FLDEOF:
	        if (uprf (name, XXX_FIELD_PRF)) {
		    (void) fclose (fp);
		    return 0;
		}
		while (state == FLDPLUS)
		    state = m_getfld (state, name, buf, sizeof buf, fp);
		break;

	    case BODY:
	        do {
		    register char *bp;

		    for (bp = buf; *bp; bp++)
			if (*bp != ' ' && *bp != '\t' && *bp != '\n') {
			    (void) fclose (fp);
			    return 1;
			}

		    state = m_getfld (state, name, buf, sizeof buf, fp);
		} while (state == BODY);
		/* and fall... */

	    default:
		(void) fclose (fp);
		return 0;
	}
}
#endif /* MIME */

/*  */

static struct swit  sendswitches[] = {
#define	ALIASW	0
    "alias aliasfile", 0,

#define	DEBUGSW	1
    "debug", -5,

#define	ENCRSW	2
    "encrypt",
#ifndef	TMA
    -7,
#else	/* TMA */
    0,
#endif	/* TMA */
#define	NENCRSW	3
    "noencrypt",
#ifndef	TMA
    -9,
#else	/* TMA */
    0,
#endif	/* TMA */

#define	FILTSW	4
    "filter filterfile", 0,
#define	NFILTSW	5
    "nofilter", 0,

#define	FRMTSW	6
    "format", 0,
#define	NFRMTSW	7
    "noformat", 0,

#define	FORWSW	8
    "forward", 0,
#define	NFORWSW	9
    "noforward", 0,

#define	MIMESW	10
    "mime", MIMEminc(-4),
#define	NMIMESW	11
    "nomime", MIMEminc(-6),

#define	MSGDSW	12
    "msgid", 0,
#define	NMSGDSW	13
    "nomsgid", 0,

#define	SPSHSW	14
    "push", 0,
#define	NSPSHSW	15
    "nopush", 0,

#define	SPLITSW	16
    "split seconds", MIMEminc(-5),

#define	UNIQSW	17
    "unique", -6,
#define	NUNIQSW	18
    "nounique", -8,

#define	VERBSW	19
    "verbose", 0,
#define	NVERBSW	20
    "noverbose", 0,

#define	WATCSW	21
    "watch", 0,
#define	NWATCSW	22
    "nowatch", 0,

#define	WIDTHSW	23
    "width columns", 0,

#define	SHELPSW	24
    "help", 4,

#define	MAILSW	25
    "mail", -4,
#define	SAMLSW	26
    "saml", -4,
#define	SSNDSW	27
    "send", -4,
#define	SOMLSW	28
    "soml", -4,

#define	CLIESW	29
    "client host", -6,
#define	SERVSW	30
    "server host", -6,
#define	SNOOPSW	31
    "snoop", -5,

#define SDRFSW 32
    "draftfolder +folder", -6,
#define SDRMSW 33
    "draftmessage msg", -6,
#define SNDRFSW 34
    "nodraftfolder", -3,

    NULL, 0
};

/*  */

extern int debugsw;		/* from sendsbr.c */
extern int forwsw;
extern int inplace;
extern int pushsw;
extern int splitsw;
extern int unique;
extern int verbsw;

extern char *altmsg;		/*  .. */
extern char *annotext;
extern char *distfile;

/*  */

static  sendit (sp, arg, file, pushed)
register char  *sp,
	      **arg,
               *file;
int     pushed;
{
#ifndef	lint
    int	    distsw = 0;
#endif	/* not lint */
    int	    vecp = 1;
    char   *cp,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *vec[MAXARGS];
    struct stat st;
#ifdef	UCI
    FILE   *fp;
#endif	/* UCI */

    if (arg)
	(void) copyip (arg, vec);
    if ((cp = m_find (sp)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    if (arg)
	(void) copyip (vec, ap);
    argp = arguments;

    debugsw = 0, forwsw = 1, inplace = 0, unique = 0;
    altmsg = annotext = distfile = NULL;
    vec[vecp++] = "-library";
    vec[vecp++] = getcpy (m_maildir (""));

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, sendswitches)) {
		case AMBIGSW: 
		    ambigsw (cp, sendswitches);
		    return;
		case UNKWNSW: 
		    advise (NULLCP, "-%s unknown\n", cp);
		    return;
		case SHELPSW: 
		    (void) sprintf (buf, "%s [switches]", sp);
		    help (buf, sendswitches);
		    return;

		case SPSHSW: 
		    pushed++;
		    continue;
		case NSPSHSW: 
		    pushed = 0;
		    continue;

		case SPLITSW: 
		    if (!(cp = *argp++) || sscanf (cp, "%d", &splitsw) != 1) {
			advise (NULLCP, "missing argument to %s", argp[-2]);
			return;
		    }
		    continue;

		case UNIQSW: 
		    unique++;
		    continue;
		case NUNIQSW: 
		    unique = 0;
		    continue;
		case FORWSW: 
		    forwsw++;
		    continue;
		case NFORWSW: 
		    forwsw = 0;
		    continue;

		case VERBSW: 
		    verbsw++;
		    vec[vecp++] = --cp;
		    continue;
		case NVERBSW:
		    verbsw = 0;
		    vec[vecp++] = --cp;
		    continue;

		case DEBUGSW: 
		    debugsw++;	/* fall */
		case NFILTSW: 
		case FRMTSW: 
		case NFRMTSW: 
		case MIMESW: 
		case NMIMESW: 
		case MSGDSW: 
		case NMSGDSW: 
		case WATCSW: 
		case NWATCSW: 
		case MAILSW: 
		case SAMLSW: 
		case SSNDSW: 
		case SOMLSW: 
		case ENCRSW: 
		case NENCRSW: 
		case SNOOPSW: 
		    vec[vecp++] = --cp;
		    continue;

		case ALIASW: 
		case FILTSW: 
		case WIDTHSW: 
		case CLIESW: 
		case SERVSW: 
		    vec[vecp++] = --cp;
		    if (!(cp = *argp++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", argp[-2]);
			return;
		    }
		    vec[vecp++] = cp;
		    continue;

		case SDRFSW: 
		case SDRMSW: 
		    if (!(cp = *argp++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", argp[-2]);
			return;
		    }
		case SNDRFSW: 
		    continue;
	    }
	advise (NULLCP, "usage: %s [switches]", sp);
	return;
    }
    if (cp = m_find ("Aliasfile")) {	/* allow Aliasfile: profile entry */
	char *dp = NULL;

	for (ap = brkstring(dp = getcpy(cp), " ", "\n"); ap && *ap; ap++) {
	    vec[vecp++] = "-alias";
	    vec[vecp++] = *ap;
	}
    }

/*  */

#ifdef	TMA
    if ((cp = getenv ("KDS")) == NULL || *cp == NULL)
	if ((cp = m_find ("kdsproc")) && *cp)
	    (void) m_putenv ("KDS", cp);
    if ((cp = getenv ("TMADB")) == NULL || *cp == NULL)
	if ((cp = m_find ("tmadb")) && *cp)
	    (void) m_putenv ("TMADB", m_maildir (cp));
#endif	/* TMA */

    if ((cp = getenv ("SIGNATURE")) == NULL || *cp == 0)
	if ((cp = m_find ("signature")) && *cp)
	    (void) m_putenv ("SIGNATURE", cp);
#ifdef	UCI
	else {
	    (void) sprintf (buf, "%s/.signature", mypath);
	    if ((fp = fopen (buf, "r")) != NULL
		&& fgets (buf, sizeof buf, fp) != NULL) {
		    (void) fclose (fp);
		    if (cp = index (buf, '\n'))
			*cp = 0;
		    (void) m_putenv ("SIGNATURE", buf);
	    }
	}
#endif	/* UCI */

    if ((annotext = getenv ("mhannotate")) == NULL || *annotext == 0)
	annotext = NULL;
    if ((altmsg = getenv ("mhaltmsg")) == NULL || *altmsg == 0)
	altmsg = NULL;
    if (annotext && ((cp = getenv ("mhinplace")) != NULL && *cp != 0))
	inplace = atoi (cp);

    if ((cp = getenv ("mhdist"))
	    && *cp
#ifndef	lint
	    && (distsw = atoi (cp))
#endif	/* not lint */
	    && altmsg) {
	vec[vecp++] = "-dist";
	distfile = getcpy (m_scratch (altmsg, invo_name));
	if (link (altmsg, distfile) == NOTOK)
	    adios (distfile, "unable to link %s to", altmsg);
    }
    else
	distfile = NULL;

    if (altmsg == NULL || stat (altmsg, &st) == NOTOK)
	st.st_mtime = 0, st.st_dev = 0, st.st_ino = 0;
    if (pushsw = pushed)
	push ();

    vec[0] = r1bindex (postproc, '/');
    closefds (3);

    if (sendsbr (vec, vecp, file, &st) == OK)
	done (0);
}

/*    WHOM */

static	int whomfile (arg, file)
register char **arg,
               *file;
{
    int     pid;
    register int    vecp;
    char   *vec[MAXARGS];

    m_update ();
    (void) fflush (stdout);

    switch (pid = vfork ()) {
	case NOTOK: 
	    advise ("fork", "unable to");
	    return 1;

	case OK: 
	    vecp = 0;
	    vec[vecp++] = r1bindex (whomproc, '/');
	    vec[vecp++] = file;
	    if (arg)
		while (*arg)
		    vec[vecp++] = *arg++;
	    vec[vecp] = NULL;

	    execvp (whomproc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (whomproc);
	    _exit (-1);		/* NOTREACHED */

	default: 
	    return (pidwait (pid, NOTOK) & 0377 ? 1 : 0);
    }
}
