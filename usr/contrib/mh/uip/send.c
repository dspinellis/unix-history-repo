/* send.c - send a composed message */

#include "../h/mh.h"
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

/*  */

static struct swit switches[] = {
#define	ALIASW	0
    "alias aliasfile", 0,

#define	DEBUGSW	1
    "debug", -5,

#define	DRAFTSW	2
    "draft", 0,

#define	DFOLDSW	3
    "draftfolder +folder", 6,
#define	DMSGSW	4
    "draftmessage msg", 6,
#define	NDFLDSW	5
    "nodraftfolder", 0,

#define	ENCRSW	6
    "encrypt",
#ifndef	TMA
	-7,
#else	TMA
	0,
#endif	TMA
#define	NENCRSW	7
    "noencrypt",
#ifndef	TMA
	-9,
#else	TMA
	0,
#endif	TMA

#define	FILTSW	8
    "filter filterfile", 0,
#define	NFILTSW	9
    "nofilter", 0,

#define	FRMTSW	10
    "format", 0,
#define	NFRMTSW	11
    "noformat", 0,

#define	FORWSW	12
    "forward", 0,
#define	NFORWSW	13
    "noforward", 0,

#define	MSGDSW	14
    "msgid", 0,
#define	NMSGDSW	15
    "nomsgid", 0,

#define	PUSHSW	16
    "push", 0,
#define	NPUSHSW	17
    "nopush", 0,

#define	UNIQSW	18
    "unique", -6,
#define	NUNIQSW	19
    "nounique", -8,

#define	VERBSW	20
    "verbose", 0,
#define	NVERBSW	21
    "noverbose", 0,

#define	WATCSW	22
    "watch", 0,
#define	NWATCSW	23
    "nowatch", 0,

#define	WIDTHSW	24
    "width columns", 0,

#define	HELPSW	25
    "help", 4,

#define	MAILSW	26
    "mail", -4,
#define	SAMLSW	27
    "saml", -4,
#define	SENDSW	28
    "send", -4,
#define	SOMLSW	29
    "soml", -4,

#define	CLIESW	30
    "client host", -6,
#define	SERVSW	31
    "server host", -6,
#define	SNOOPSW	32
    "snoop", -5,

    NULL, NULL
};

static struct swit anyl[] = {
#define	NOSW	0
    "no", 0,
#define	YESW	1
    "yes", 0,
#define	LISTDSW	2
    "list", 0,

    NULL, NULL
};

/*  */

extern int debugsw;		/* from sendsbr.c */
extern int forwsw;
extern int inplace;
extern int pushsw;
extern int unique;

extern char *altmsg;		/*  .. */
extern char *annotext;
extern char *distfile;

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int     msgp = 0,
	    distsw = 0,
            vecp = 1,
            isdf = 0,
            msgnum,
            status;
    char   *cp,
           *dfolder = NULL,
           *maildir,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *msgs[MAXARGS],
           *vec[MAXARGS];
    struct msgs *mp;
    struct stat st;
#ifdef	UCI
    FILE   *fp;
#endif	UCI

    invo_name = r1bindex (argv[0], '/');
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

    vec[vecp++] = "-library";
    vec[vecp++] = getcpy (m_maildir (""));

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown\n", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [file] [switches]", invo_name);
		    help (buf, switches);
		    done (1);	/* thanks, phyl */

		case DRAFTSW: 
		    msgs[msgp++] = draft;
		    continue;

		case DFOLDSW: 
		    if (dfolder)
			adios (NULLCP, "only one draft folder at a time!");
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    dfolder = path (*cp == '+' || *cp == '@' ? cp + 1 : cp,
			    *cp != '@' ? TFOLDER : TSUBCWF);
		    continue;
		case DMSGSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    msgs[msgp++] = cp;
		    continue;
		case NDFLDSW: 
		    dfolder = NULL;
		    isdf = NOTOK;
		    continue;

		case PUSHSW: 
		    pushsw++;
		    continue;
		case NPUSHSW: 
		    pushsw = 0;
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

		case DEBUGSW: 
		    debugsw++;	/* fall */
		case NFILTSW: 
		case FRMTSW: 
		case NFRMTSW: 
		case MSGDSW: 
		case NMSGDSW: 
		case VERBSW: 
		case NVERBSW: 
		case WATCSW: 
		case NWATCSW: 
		case MAILSW: 
		case SAMLSW: 
		case SENDSW: 
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
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    vec[vecp++] = cp;
		    continue;
	    }
	else
	    msgs[msgp++] = cp;
    }

/*  */

    if (dfolder == NULL) {
	if (msgp == 0) {
#ifdef	WHATNOW
	    if ((cp = getenv ("mhdraft")) && *cp) {
		msgs[msgp++] = cp;
		goto go_to_it;
	    }
#endif	WHATNOW
	    msgs[msgp++] = getcpy (m_draft (NULLCP, NULLCP, 1, &isdf));
	    if (stat (msgs[0], &st) == NOTOK)
		adios (msgs[0], "unable to stat draft file");
	    cp = concat ("Use \"", msgs[0], "\"? ", NULLCP);
	    for (status = LISTDSW; status != YESW;) {
		if (!(argp = getans (cp, anyl)))
		    done (1);
		switch (status = smatch (*argp, anyl)) {
		    case NOSW: 
			done (0);
		    case YESW: 
			break;
		    case LISTDSW: 
			(void) showfile (++argp, msgs[0]);
			break;
		    default:
			advise (NULLCP, "say what?");
			break;
		}
	    }
	}
	else
	    for (msgnum = 0; msgnum < msgp; msgnum++)
		msgs[msgnum] = getcpy (m_maildir (msgs[msgnum]));
    }
    else {
	if (!m_find ("path"))
	    free (path ("./", TFOLDER));

	if (!msgp)
	    msgs[msgp++] = "cur";
	maildir = m_maildir (dfolder);

	if (chdir (maildir) == NOTOK)
	    adios (maildir, "unable to change directory to");
	if (!(mp = m_gmsg (dfolder)))
	    adios (NULLCP, "unable to read folder %s", dfolder);
	if (mp -> hghmsg == 0)
	    adios (NULLCP, "no messages in %s", dfolder);

	for (msgnum = 0; msgnum < msgp; msgnum++)
	    if (!m_convert (mp, msgs[msgnum]))
		done (1);
	m_setseq (mp);

	for (msgp = 0, msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED) {
		msgs[msgp++] = getcpy (m_name (msgnum));
#ifdef	notdef
		mp -> msgstats[msgnum] |= DELETED;
#endif	notdef
		mp -> msgstats[msgnum] &= ~EXISTS;
	    }
	mp -> msgflags |= SEQMOD;

	m_sync (mp);
    }
#ifdef	WHATNOW
go_to_it: ;
#endif	WHATNOW

/*  */

#ifdef	TMA
    if ((cp = getenv ("KDS")) == NULL || *cp == NULL)
	if ((cp = m_find ("kdsproc")) && *cp)
	    (void) putenv ("KDS", cp);
    if ((cp = getenv ("TMADB")) == NULL || *cp == NULL)
	if ((cp = m_find ("tmadb")) && *cp)
	    (void) putenv ("TMADB", m_maildir (cp));
#endif	TMA

    if ((cp = getenv ("SIGNATURE")) == NULL || *cp == NULL)
	if ((cp = m_find ("signature")) && *cp)
	    (void) putenv ("SIGNATURE", cp);
#ifdef	UCI
	else {
	    (void) sprintf (buf, "%s/.signature", mypath);
	    if ((fp = fopen (buf, "r")) != NULL
		&& fgets (buf, sizeof buf, fp) != NULL) {
		    (void) fclose (fp);
		    if (cp = index (buf, '\n'))
			*cp = NULL;
		    (void) putenv ("SIGNATURE", buf);
	    }
	}
#endif	UCI

    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (stat (msgs[msgnum], &st) == NOTOK)
	    adios (msgs[msgnum], "unable to stat draft file");

    if ((annotext = getenv ("mhannotate")) == NULL || *annotext == NULL)
	annotext = NULL;
    if (annotext
	    && ((altmsg = getenv ("mhaltmsg")) == NULL || *altmsg == NULL))
	altmsg = NULL;
    if (annotext && ((cp = getenv ("mhinplace")) != NULL && *cp != NULL))
	inplace = atoi (cp);

    if ((cp = getenv ("mhdist"))
	    && *cp
	    && (distsw = atoi (cp))
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
    if (pushsw)
	push ();

    status = 0;
    vec[0] = r1bindex (postproc, '/');
    closefds (3);

    for (msgnum = 0; msgnum < msgp; msgnum++)
	switch (sendsbr (vec, vecp, msgs[msgnum], &st)) {
	    case DONE: 
		done (++status);

	    case NOTOK: 
		status++;	/* fall */
	    case OK:
		break;
	}

    m_update ();

    done (status);
}
