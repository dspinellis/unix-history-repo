/* mshcmds.c - command handlers in msh */
#ifndef	lint
static char ident[] = "@(#)$Id: mshcmds.c,v 1.28 1993/12/01 03:50:31 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/dropsbr.h"
#include "../h/formatsbr.h"
#include "../h/scansbr.h"
#include "../zotnet/tws.h"
#ifdef	_AIX		/* AIX 1.2.1 <stdio.h> declares getws() */
#define getws _getws
#endif
#include <stdio.h>
#ifdef	_AIX
#undef getws
#endif
#include "../zotnet/mts.h"
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "../h/mshsbr.h"

/*  */

extern int errno;

				/* BURST */
static char delim3[] = "-------";/* from burst.c */


				/* SHOW */
static int  mhlnum;
static FILE *mhlfp;

void clear_screen ();
static int     eom_action ();
static FP	mhl_action ();
#ifdef	MIME
static int     nontext();
#endif


static burst(), forw(), rmm(), show(), ask(), copy_message(), copy_digest();
static int	process();
				/* SORTM */
static int	msgsort (), subsort();
static int	getws ();
static char    *sosmash ();

#if	defined(NNTP) && defined(MPOP)
#undef	MPOP
#endif
#ifdef	MPOP
#ifdef	BPOP
extern	int	pmsh;
extern	char	response[];
#endif
#endif /* MPOP */

/*  */

forkcmd (args, pgm)
char  **args,
       *pgm;
{
    int     child_id;
    char   *vec[MAXARGS];

    vec[0] = r1bindex (pgm, '/');
    (void) copyip (args, vec + 1);

    if (fmsh) {
	(void) m_delete (pfolder);
	m_replace (pfolder, fmsh);
	m_sync (mp);
	m_update ();
    }
    (void) fflush (stdout);
    switch (child_id = fork ()) {
	case NOTOK: 
	    advise ("fork", "unable to");
	    return;

	case OK: 
	    closefds (3);
	    (void) signal (SIGINT, istat);
	    (void) signal (SIGQUIT, qstat);

	    execvp (pgm, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (cmd_name);
	    _exit (1);

	default: 
	    (void) pidXwait (child_id, NULLCP);
	    break;
    }
    if (fmsh) {			/* assume the worst case */
	mp -> msgflags |= MODIFIED;
	modified++;
    }
}

/*  */

static struct swit distswit[] = {
#define	DIANSW	0
    "annotate", 0,
#define	DINANSW	1
    "noannotate", 0,
#define	DIDFSW	2
    "draftfolder +folder", 0,
#define	DIDMSW	3
    "draftmessage msg", 0,
#define	DINDFSW	4
    "nodraftfolder", 0,
#define	DIEDTSW	5
    "editor editor", 0,
#define	DINEDSW	6
    "noedit", 0,
#define	DIFRMSW	7
    "form formfile", 0,
#define	DIINSW	8
    "inplace", 0,
#define	DININSW	9
    "noinplace", 0,
#define	DIWHTSW	10
    "whatnowproc program", 0,
#define	DINWTSW	11
    "nowhatnowproc", 0,
#define	DIHELP	12
    "help", 4,

    NULL, 0
};

/*  */

distcmd (args)
char  **args;
{
    int     vecp = 1;
    char   *cp,
           *msg = NULL,
            buf[BUFSIZ],
           *vec[MAXARGS];

    if (fmsh) {
	forkcmd (args, cmd_name);
	return;
    }

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, distswit)) {
		case AMBIGSW: 
		    ambigsw (cp, distswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case DIHELP: 
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, distswit);
		    return;

		case DIANSW:	/* not implemented */
		case DINANSW: 
		case DIINSW: 
		case DININSW: 
		    continue;

		case DINDFSW:
		case DINEDSW:
		case DINWTSW:
		    vec[vecp++] = --cp;
		    continue;

		case DIEDTSW: 
		case DIFRMSW: 
		case DIDFSW:
		case DIDMSW:
		case DIWHTSW:
		    vec[vecp++] = --cp;
		    if (!(cp = *args++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    vec[vecp++] = cp;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    if (msg) {
		advise (NULLCP, "only one message at a time!");
		return;
	    }
	    else
		msg = cp;
    }

    vec[0] = cmd_name;
    vec[vecp++] = "-file";
    vec[vecp] = NULL;
    if (!msg)
	msg = "cur";
    if (!m_convert (mp, msg))
	return;
    m_setseq (mp);

    if (mp -> numsel > 1) {
	advise (NULLCP, "only one message at a time!");
	return;
    }
    (void) process (mp -> hghsel, cmd_name, vecp, vec);
    m_setcur (mp, mp -> hghsel);
}

/*  */

static struct swit explswit[] = {
#define	EXINSW	0
    "inplace", 0,
#define	EXNINSW	1
    "noinplace", 0,
#define	EXQISW	2
    "quiet", 0,
#define	EXNQISW	3
    "noquiet", 0,
#define	EXVBSW	4
    "verbose", 0,
#define	EXNVBSW	5
    "noverbose", 0,
#define	EXHELP	6
    "help", 4,

    NULL, 0
};

/*  */

explcmd (args)
char  **args;
{
    int     inplace = 0,
            quietsw = 0,
            verbosw = 0,
            msgp = 0,
            hi,
            msgnum;
    char   *cp,
            buf[BUFSIZ],
           *msgs[MAXARGS];
    struct Msg *smsgs;

    if (fmsh) {
	forkcmd (args, cmd_name);
	return;
    }

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, explswit)) {
		case AMBIGSW: 
		    ambigsw (cp, explswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case EXHELP: 
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, explswit);
		    return;

		case EXINSW: 
		    inplace++;
		    continue;
		case EXNINSW: 
		    inplace = 0;
		    continue;
		case EXQISW: 
		    quietsw++;
		    continue;
		case EXNQISW: 
		    quietsw = 0;
		    continue;
		case EXVBSW: 
		    verbosw++;
		    continue;
		case EXNVBSW: 
		    verbosw = 0;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    msgs[msgp++] = cp;
    }

    if (!msgp)
	msgs[msgp++] = "cur";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    return;
    m_setseq (mp);

    smsgs = (struct Msg *)
		calloc ((unsigned) (MAXFOLDER + 2), sizeof *smsgs);
    if (smsgs == NULL)
	adios (NULLCP, "unable to allocate folder storage");

    hi = mp -> hghmsg + 1;
    interrupted = 0;
    for (msgnum = mp -> lowsel;
	    msgnum <= mp -> hghsel && !interrupted;
	    msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED)
	    if (burst (smsgs, msgnum, inplace, quietsw, verbosw) != OK)
		break;

    free ((char *) smsgs);

    if (inplace)
	m_setcur (mp, mp -> lowsel);
    else
	if (hi <= mp -> hghmsg)
	    m_setcur (mp, hi);

    mp -> msgflags |= MODIFIED;
    modified++;
}

/*  */

static  burst (smsgs, msgnum, inplace, quietsw, verbosw)
struct Msg *smsgs;
int     msgnum,
        inplace,
        quietsw,
        verbosw;
{
    int     i,
            j,
            ld3,
	    wasdlm,
            msgp;
    long    pos;
    char    c,
	    cc,
            buffer[BUFSIZ];
    register FILE *zp;

    ld3 = strlen (delim3);

    if (Msgs[msgnum].m_scanl) {
	free (Msgs[msgnum].m_scanl);
	Msgs[msgnum].m_scanl = NULL;
    }

    pos = ftell (zp = msh_ready (msgnum, 1));
    for (msgp = 0; msgp <= MAXFOLDER;) {
	while (fgets (buffer, sizeof buffer, zp) != NULL
		&& buffer[0] == '\n'
		&& pos < Msgs[msgnum].m_stop)
	    pos += (long) strlen (buffer);
	if (feof (zp) || pos >= Msgs[msgnum].m_stop)
	    break;
	(void) fseek (zp, pos, 0);
	smsgs[msgp].m_start = pos;

	for (c = 0;
		pos < Msgs[msgnum].m_stop
		&& fgets (buffer, sizeof buffer, zp) != NULL;
		c = buffer[0])
	    if (strncmp (buffer, delim3, ld3) == 0
		    && (msgp == 1 || c == '\n')
		    && peekc (zp) == '\n')
		break;
	    else
		pos += (long) strlen (buffer);

	wasdlm = strncmp (buffer, delim3, ld3) == 0;
	if (smsgs[msgp].m_start != pos)
	    smsgs[msgp++].m_stop = (c == '\n' && wasdlm) ? pos - 1 : pos;
	if (feof (zp) || pos >= Msgs[msgnum].m_stop) {
	    if (wasdlm)
		smsgs[msgp - 1].m_stop -= ((long) strlen (buffer) + 1);
	    break;
	}
	pos += (long) strlen (buffer);
    }

    switch (msgp--) {		/* toss "End of XXX Digest" */
	case 0: 
	    adios (NULLCP, "burst() botch -- you lose big");

	case 1: 
	    if (!quietsw)
		printf ("message %d not in digest format\n", msgnum);
	    return OK;

	default: 
	    if (verbosw)
		printf ("%d message%s exploded from digest %d\n",
			msgp, msgp != 1 ? "s" : "", msgnum);
	    break;
    }

    if ((i = msgp + mp -> hghmsg) > MAXFOLDER) {
	advise (NULLCP, "more than %d messages", MAXFOLDER);
	return NOTOK;
    }
    if ((mp = m_remsg (mp, 0, i)) == NULL)
	adios (NULLCP, "unable to allocate folder storage");

    j = mp -> hghmsg;
    mp -> hghmsg += msgp;
    mp -> nummsg += msgp;
    if (mp -> hghsel > msgnum)
	mp -> hghsel += msgp;

    if (inplace)
	for (i = mp -> hghmsg; j > msgnum; i--, j--) {
	    if (verbosw)
		printf ("message %d becomes message %d\n", j, i);

	    Msgs[i].m_bboard_id = Msgs[j].m_bboard_id;
	    Msgs[i].m_top = Msgs[j].m_top;
	    Msgs[i].m_start = Msgs[j].m_start;
	    Msgs[i].m_stop = Msgs[j].m_stop;
	    Msgs[i].m_scanl = NULL;
	    if (Msgs[j].m_scanl) {
		free (Msgs[j].m_scanl);
		Msgs[j].m_scanl = NULL;
	    }
	    mp -> msgstats[i] = mp -> msgstats[j];
	}

    if (Msgs[msgnum].m_bboard_id == 0)
	(void) readid (msgnum);

    mp -> msgstats[msgnum] &= ~SELECTED;
    i = inplace ? msgnum + msgp : mp -> hghmsg;
    for (j = msgp; j >= (inplace ? 0 : 1); i--, j--) {
	if (verbosw && i != msgnum)
	    printf ("message %d of digest %d becomes message %d\n",
		    j, msgnum, i);

	Msgs[i].m_bboard_id = Msgs[msgnum].m_bboard_id;
	Msgs[i].m_top = Msgs[j].m_top;
	Msgs[i].m_start = smsgs[j].m_start;
	Msgs[i].m_stop = smsgs[j].m_stop;
	Msgs[i].m_scanl = NULL;
	mp -> msgstats[i] = mp -> msgstats[msgnum];
    }

    return OK;
}

/*  */

static struct swit fileswit[] = {
#define	FIDRFT	0
    "draft", 0,
#define	FILINK	1
    "link", 0,
#define	FINLINK	2
    "nolink", 0,
#define	FIPRES	3
    "preserve", 0,
#define FINPRES	4
    "nopreserve", 0,
#define	FISRC	5
    "src +folder", 0,
#define	FIFILE	6
    "file file", 0,
#define	FIPROC	7
    "rmmproc program", 0,
#define	FINPRC	8
    "normmproc", 0,
#define	FIHELP	9
    "help", 4,

    NULL, 0
};

/*  */

filecmd (args)
char  **args;
{
    int	    linksw = 0,
	    msgp = 0,
            vecp = 1,
	    i,
            msgnum;
    char   *cp,
            buf[BUFSIZ],
           *msgs[MAXARGS],
           *vec[MAXARGS];

    if (fmsh) {
	forkcmd (args, cmd_name);
	return;
    }

    while (cp = *args++) {
	if (*cp == '-')
	    switch (i = smatch (++cp, fileswit)) {
		case AMBIGSW: 
		    ambigsw (cp, fileswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case FIHELP: 
		    (void) sprintf (buf, "%s +folder... [msgs] [switches]",
			    cmd_name);
		    help (buf, fileswit);
		    return;

		case FILINK:
		    linksw++;
		    continue;
		case FINLINK: 
		    linksw = 0;
		    continue;

		case FIPRES: 
		case FINPRES: 
		    continue;

		case FISRC: 
		case FIDRFT:
		case FIFILE: 
		case FIPROC:
		case FINPRC:
		    advise (NULLCP, "sorry, -%s not allowed!", fileswit[i].sw);
		    return;
	    }
	if (*cp == '+' || *cp == '@')
	    vec[vecp++] = cp;
	else
	    msgs[msgp++] = cp;
    }

    vec[0] = cmd_name;
    vec[vecp++] = "-file";
    vec[vecp] = NULL;
    if (!msgp)
	msgs[msgp++] = "cur";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    return;
    m_setseq (mp);

    interrupted = 0;
    for (msgnum = mp -> lowsel;
	    msgnum <= mp -> hghsel && !interrupted;
	    msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED)
	    if (process (msgnum, fileproc, vecp, vec)) {
		mp -> msgstats[msgnum] &= ~SELECTED;
		mp -> numsel--;
	    }

    if (mp -> numsel != mp -> nummsg || linksw)
	m_setcur (mp, mp -> hghsel);
    if (!linksw)
	rmm ();
}

/*  */

int	filehak (args)
char  **args;
{
    int	    result,
	    vecp = 0;
    char   *cp,
	   *cwd,
           *vec[MAXARGS];

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, fileswit)) {
		case AMBIGSW: 
		case UNKWNSW: 
		case FIHELP: 
		    return NOTOK;

		case FILINK:
		case FINLINK: 
		case FIPRES: 
		case FINPRES: 
		    continue;

		case FISRC: 
		case FIDRFT:
		case FIFILE: 
		    return NOTOK;
	    }
	if (*cp == '+' || *cp == '@')
	    vec[vecp++] = cp;
    }
    vec[vecp] = NULL;

    result = NOTOK;
    cwd = NULL;
    for (vecp = 0; (cp = vec[vecp]) && result == NOTOK; vecp++) {
	if (cwd == NULL)
	    cwd = getcpy (pwd ());
	(void) chdir (m_maildir (""));
	cp = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	if (access (m_maildir (cp), 0) == NOTOK)
	    result = OK;
	free (cp);
    }
    if (cwd)
	(void) chdir (cwd);

    return result;
}

/*  */

static struct swit foldswit[] = {
#define	FLALSW	0
    "all", 0,
#define	FLFASW	1
    "fast", 0,
#define	FLNFASW	2
    "nofast", 0,
#define	FLHDSW	3
    "header", 0,
#define	FLNHDSW	4
    "noheader", 0,
#define	FLPKSW	5
    "pack", 0,
#define	FLNPKSW	6
    "nopack", 0,
#define	FLRCSW	7
    "recurse", 0,
#define	FLNRCSW	8
    "norecurse", 0,
#define	FLTLSW	9
    "total", 0,
#define	FLNTLSW	10
    "nototal", 0,
#define	FLPRSW	11
    "print", 0,
#define	FLPUSW	12
    "push", 0,
#define	FLPOSW	13
    "pop", 0,
#define	FLLISW	14
    "list", 0,
#define	FLHELP	15
    "help", 4,

    NULL, 0
};

/*  */

foldcmd (args)
char  **args;
{
    int     fastsw = 0,
            headersw = 0,
	    packsw = 0,
	    hole,
	    msgnum;
    char   *cp,
           *folder = NULL,
           *msg = NULL,
            buf[BUFSIZ],
	  **vec = args;

    if (args == NULL)
	goto fast;

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, foldswit)) {
		case AMBIGSW: 
		    ambigsw (cp, foldswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case FLHELP: 
		    (void) sprintf (buf, "%s [+folder] [msg] [switches]",
			    cmd_name);
		    help (buf, foldswit);
		    return;

		case FLALSW:	/* not implemented */
		case FLRCSW: 
		case FLNRCSW: 
		case FLTLSW: 
		case FLNTLSW: 
		case FLPRSW:
		case FLPUSW:
		case FLPOSW:
		case FLLISW:
		    continue;

		case FLFASW: 
		    fastsw++;
		    continue;
		case FLNFASW: 
		    fastsw = 0;
		    continue;
		case FLHDSW: 
		    headersw++;
		    continue;
		case FLNHDSW: 
		    headersw = 0;
		    continue;
		case FLPKSW: 
		    packsw++;
		    continue;
		case FLNPKSW: 
		    packsw = 0;
		    continue;
	    }
	if (*cp == '+' || *cp == '@')
	    if (folder) {
		advise (NULLCP, "only one folder at a time!\n");
		return;
	    }
	    else
		folder = fmsh ? path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF)
			    : cp + 1;
	else
	    if (msg) {
		advise (NULLCP, "only one message at a time!\n");
		return;
	    }
	    else
		msg = cp;
    }

    if (folder) {
	if (*folder == 0) {
	    advise (NULLCP, "null folder names are not permitted");
	    return;
	}
	if (fmsh) {
	    if (access (m_maildir (folder), 04) == NOTOK) {
		advise (folder, "unable to read");
		return;
	    }
	}
	else {
	    (void) strcpy (buf, folder);
	    if (expand (buf) == NOTOK)
		return;
	    folder = buf;
	    if (access (folder, 04) == NOTOK) {
		advise (folder, "unable to read");
		return;
	    }
	}
	m_reset ();

	if (fmsh)
	    fsetup (folder);
	else
	    setup (folder);
	readids (0);
	display_info (0);
    }

    if (msg) {
	if (!m_convert (mp, msg))
	    return;
	m_setseq (mp);

	if (mp -> numsel > 1) {
	    advise (NULLCP, "only one message at a time!");
	    return;
	}
	m_setcur (mp, mp -> hghsel);
    }

    if (packsw) {
	if (fmsh) {
	    forkcmd (vec, cmd_name);
	    return;
	}

	if (mp -> lowmsg > 1 && (mp = m_remsg (mp, 1, mp -> hghmsg)) == NULL)
	    adios (NULLCP, "unable to allocate folder storage");
	for (msgnum = mp -> lowmsg, hole = 1; msgnum <= mp -> hghmsg; msgnum++)
	    if (mp -> msgstats[msgnum] & EXISTS) {
		if (msgnum != hole) {
		    Msgs[hole].m_bboard_id = Msgs[msgnum].m_bboard_id;
		    Msgs[hole].m_top = Msgs[msgnum].m_top;
		    Msgs[hole].m_start = Msgs[msgnum].m_start;
		    Msgs[hole].m_stop = Msgs[msgnum].m_stop;
		    Msgs[hole].m_scanl = NULL;
		    if (Msgs[msgnum].m_scanl) {
			free (Msgs[msgnum].m_scanl);
			Msgs[msgnum].m_scanl = NULL;
		    }
		    mp -> msgstats[hole] = mp -> msgstats[msgnum];
		    if (mp -> curmsg == msgnum)
			m_setcur (mp, hole);
		}
		hole++;
	    }
	if (mp -> nummsg > 0) {
	    mp -> lowmsg = 1;
	    mp -> hghmsg = hole - 1;
	}
	mp -> msgflags |= MODIFIED;
	modified++;
    }

fast: ;
    if (fastsw)
	printf ("%s\n", fmsh ? fmsh : mp -> foldpath);
    else {
	if (headersw)
	    printf ("\t\tFolder  %*s# of messages (%*srange%*s); cur%*smsg\n",
		DMAXFOLDER, "", DMAXFOLDER - 2, "", DMAXFOLDER - 2, "",
		DMAXFOLDER - 2, "");
	printf (args ? "%22s  " : "%s ", fmsh ? fmsh : mp -> foldpath);
	if (mp -> hghmsg == 0)
	    printf ("has   no messages%*s",
		    mp -> msgflags & OTHERS ? DMAXFOLDER * 2 + 4 : 0, "");
	else {
	    printf ("has %*d message%s (%*d-%*d)",
		    DMAXFOLDER, mp -> nummsg, mp -> nummsg != 1 ? "s" : "",
		    DMAXFOLDER, mp -> lowmsg, DMAXFOLDER, mp -> hghmsg);
	    if (mp -> curmsg >= mp -> lowmsg
		    && mp -> curmsg <= mp -> hghmsg)
		printf ("; cur=%*d", DMAXFOLDER, mp -> curmsg);
	}
	printf (".\n");
    }
}

/*  */

#ifndef	MIME
#define	MIMEminc(a)	(a)
#else	/* MIME */
#define	MIMEminc(a)	0
#endif	/* MIME */

static struct swit forwswit[] = {
#define	FOANSW	0
    "annotate", 0,
#define	FONANSW	1
    "noannotate", 0,
#define	FODFSW	2
    "draftfolder +folder", 0,
#define	FODMSW	3
    "draftmessage msg", 0,
#define	FONDFSW	4
    "nodraftfolder", 0,
#define	FOEDTSW	5
    "editor editor", 0,
#define	FONEDSW	6
    "noedit", 0,
#define	FOFTRSW	7
    "filter filterfile", 0,
#define	FOFRMSW	8
    "form formfile", 0,
#define	FOFTSW	9
    "format", 5,
#define	FONFTSW	10
    "noformat", 7,
#define	FOINSW	11
    "inplace", 0,
#define	FONINSW	12
    "noinplace", 0,
#define	FOMISW	13
    "mime", MIMEminc(-4),
#define	FONMISW	14
    "nomime", MIMEminc(-6),
#define	FOWHTSW	15
    "whatnowproc program", 0,
#define	FONWTSW	16
    "nowhatnow", 0,
#define	FOHELP	17
    "help", 4,

    NULL, 0
};

/*  */

forwcmd (args)
char  **args;
{
    int	    msgp = 0,
            vecp = 1,
            msgnum;
    char   *cp,
           *filter = NULL,
            buf[BUFSIZ],
           *msgs[MAXARGS],
           *vec[MAXARGS];

    if (fmsh) {
	forkcmd (args, cmd_name);
	return;
    }

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, forwswit)) {
		case AMBIGSW: 
		    ambigsw (cp, forwswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case FOHELP: 
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, forwswit);
		    return;

		case FOANSW:	/* not implemented */
		case FONANSW: 
		case FOINSW: 
		case FONINSW: 
		case FOMISW: 
		case FONMISW: 
		    continue;

		case FONDFSW:
		case FONEDSW:
		case FONWTSW:
		    vec[vecp++] = --cp;
		    continue;

		case FOEDTSW: 
		case FOFRMSW: 
		case FODFSW:
		case FODMSW:
		case FOWHTSW:
		    vec[vecp++] = --cp;
		    if (!(cp = *args++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    vec[vecp++] = cp;
		    continue;
		case FOFTRSW: 
		    if (!(filter = *args++) || *filter == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    continue;
		case FOFTSW: 
		    if (access (filter = myfilter, 04) == NOTOK) {
			advise (filter, "unable to read default filter file");
			return;
		    }
		    continue;
		case FONFTSW: 
		    filter = NULL;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    msgs[msgp++] = cp;
    }

					/* foil search of .mh_profile */
    (void) sprintf (buf, "%sXXXXXX", invo_name);
    vec[0] = (char *)mktemp (buf);
    vec[vecp++] = "-file";
    vec[vecp] = NULL;
    if (!msgp)
	msgs[msgp++] = "cur";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    return;
    m_setseq (mp);

    if (filter) {
	(void) strcpy (buf, filter);
	if (expand (buf) == NOTOK)
	    return;
	if (access (filter = getcpy (libpath (buf)), 04) == NOTOK) {
	    advise (filter, "unable to read");
	    free (filter);
	    return;
	}
    }
    forw (cmd_name, filter, vecp, vec);
    m_setcur (mp, mp -> hghsel);
    if (filter)
	free (filter);
}

/*  */

static	forw (proc, filter, vecp, vec)
int     vecp;
char   *proc,
       *filter,
      **vec;
{
    int     i,
            child_id,
            msgnum,
            msgcnt;
    char    tmpfil[80],
           *args[MAXARGS];
    FILE   *out;

    (void) strcpy (tmpfil, m_tmpfil (invo_name));
    interrupted = 0;
    if (filter)
	switch (child_id = fork ()) {
	    case NOTOK: 
		advise ("fork", "unable to");
		return;

	    case OK: 		/* "trust me" */
		if (freopen (tmpfil, "w", stdout) == NULL) {
		    fprintf (stderr, "unable to create ");
		    perror (tmpfil);
		    _exit (1);
		}
		args[0] = r1bindex (mhlproc, '/');
		i = 1;
		args[i++] = "-forwall";
		args[i++] = "-form";
		args[i++] = filter;
		for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
		    if (mp -> msgstats[msgnum] & SELECTED)
			args[i++] = getcpy (m_name (msgnum));
		args[i] = NULL;
		(void) mhlsbr (i, args, mhl_action);
		m_eomsbr ((int (*) ()) 0);
		(void) fclose (stdout);
		_exit (0);

	    default: 
		if (pidXwait (child_id, NULLCP))
		    interrupted++;
		break;
	}
    else {
	if ((out = fopen (tmpfil, "w")) == NULL) {
	    advise (tmpfil, "unable to create temporary file");
	    return;
	}

	msgcnt = 1;
	for (msgnum = mp -> lowsel;
		msgnum <= mp -> hghsel && !interrupted;
		msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED) {
		fprintf (out, "\n\n-------");
		if (msgnum == mp -> lowsel)
		    fprintf (out, " Forwarded Message%s",
			    mp -> numsel > 1 ? "s" : "");
		else
		    fprintf (out, " Message %d", msgcnt);
		fprintf (out, "\n\n");
		copy_digest (msgnum, out);
		msgcnt++;
	    }

	fprintf (out, "\n\n------- End of Forwarded Message%s\n",
		mp -> numsel > 1 ? "s" : "");
	(void) fclose (out);
    }

    (void) fflush (stdout);
    if (!interrupted)
	switch (child_id = fork ()) {
	    case NOTOK: 
		advise ("fork", "unable to");
		break;

	    case OK: 
		closefds (3);
		(void) signal (SIGINT, istat);
		(void) signal (SIGQUIT, qstat);

		vec[vecp++] = tmpfil;
		vec[vecp] = NULL;

		execvp (proc, vec);
		fprintf (stderr, "unable to exec ");
		perror (proc);
		_exit (1);

	    default: 
		(void) pidXwait (child_id, NULLCP);
		break;
	}

    (void) unlink (tmpfil);
}

/*  */

static char *hlpmsg[] = {
    "The %s program emulates many of the commands found in the Rand MH",
    "system.  Instead of operating on MH folders, commands to %s concern",
    "a single file.",
    "",
    "To see the list of commands available, just type a ``?'' followed by",
    "the RETURN key.  To find out what switches each command takes, type",
    "the name of the command followed by ``-help''.  To leave %s, use the",
    "``quit'' command.",
    "",
    "Although a lot of MH commands are found in %s, not all are fully",
    "implemented.  %s will always recognize all legal switches for a",
    "given command though, and will let you know when you ask for an",
    "option that it is unable to perform.",
    "",
    "Running %s is fun, but using MH from your shell is far superior.",
    "After you have familiarized yourself with the MH style by using %s,",
    "you should try using MH from the shell.  You can still use %s for",
    "message files that aren't in MH format, such as BBoard files.",
    NULL
};


/* ARGSUSED */

helpcmd (args)
char  **args;
{
    int     i;

    for (i = 0; hlpmsg[i]; i++) {
	printf (hlpmsg[i], invo_name);
	(void) putchar ('\n');
    }
}

/*  */

static struct swit markswit[] = {
#define	MADDSW	0
    "add", 0,
#define	MDELSW	1
    "delete", 0,
#define	MLSTSW	2
    "list", 0,
#define	MSEQSW	3
    "sequence name", 0,
#define	MPUBSW	4
    "public", 0,
#define	MNPUBSW	5
    "nopublic", 0,
#define	MZERSW	6
    "zero", 0,
#define	MNZERSW	7
    "nozero", 0,
#define	MHELP	8
    "help", 4,
#define	MDBUGSW	9
    "debug", -5,

    NULL, 0
};

/*  */

markcmd (args)
char  **args;
{
    int     addsw = 0,
            deletesw = 0,
            debugsw = 0,
            listsw = 0,
            zerosw = 0,
            seqp = 0,
            msgp = 0,
            i,
            msgnum;
    char   *cp,
            buf[BUFSIZ],
           *seqs[NATTRS + 1],
           *msgs[MAXARGS];

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, markswit)) {
		case AMBIGSW: 
		    ambigsw (cp, markswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case MHELP: 
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, markswit);
		    return;

		case MADDSW: 
		    addsw++;
		    deletesw = listsw = 0;
		    continue;
		case MDELSW: 
		    deletesw++;
		    addsw = listsw = 0;
		    continue;
		case MLSTSW: 
		    listsw++;
		    addsw = deletesw = 0;
		    continue;

		case MSEQSW: 
		    if (!(cp = *args++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    if (seqp < NATTRS)
			seqs[seqp++] = cp;
		    else {
			advise (NULLCP, "only %d sequences allowed!", NATTRS);
			return;
		    }
		    continue;

		case MPUBSW: 	/* not implemented */
		case MNPUBSW: 
		    continue;

		case MDBUGSW: 
		    debugsw++;
		    continue;

		case MZERSW: 
		    zerosw++;
		    continue;
		case MNZERSW: 
		    zerosw = 0;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    msgs[msgp++] = cp;
    }

    if (!addsw && !deletesw && !listsw)
	if (seqp)
	    addsw++;
	else
	    if (debugsw)
		listsw++;
	    else {
		seqs[seqp++] = "unseen";
		deletesw++;
		zerosw = 0;
		if (!msgp)
		    msgs[msgp++] = "all";
	    }

    if (!msgp)
	msgs[msgp++] = listsw ? "all" :"cur";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    return;

    if (debugsw) {
	printf ("invo_name=%s mypath=%s defpath=%s\n",
		invo_name, mypath, defpath);
	printf ("ctxpath=%s context flags=%s\n",
		ctxpath, sprintb (buf, (unsigned) ctxflags, DBITS));
	printf ("foldpath=%s flags=%s\n",
		mp -> foldpath,
		sprintb (buf, (unsigned) mp -> msgflags, FBITS));
	printf ("hghmsg=%d lowmsg=%d nummsg=%d curmsg=%d\n",
		mp -> hghmsg, mp -> lowmsg, mp -> nummsg, mp -> curmsg);
	printf ("lowsel=%d hghsel=%d numsel=%d\n",
		mp -> lowsel, mp -> hghsel, mp -> numsel);
#ifndef	MTR
	printf ("lowoff=%d hghoff=%d\n",
		mp -> lowoff, mp -> hghoff);
#else	/* MTR */
	printf ("lowoff=%d hghoff=%d msgbase=0x%x msgstats=0x%x\n",
		mp -> lowoff, mp -> hghoff, mp -> msgbase, mp -> msgstats);
#endif	/* MTR */
    }

    if (seqp == 0 && (addsw || deletesw)) {
	advise (NULLCP, "-%s requires at least one -sequence argument",
		addsw ? "add" : "delete");
	return;
    }
    seqs[seqp] = NULL;

    if (addsw)
	for (seqp = 0; seqs[seqp]; seqp++) {
	    if (zerosw && !m_seqnew (mp, seqs[seqp], 0))
		return;
	    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
		if (mp -> msgstats[msgnum] & SELECTED)
		    if (!m_seqadd (mp, seqs[seqp], msgnum, 0))
			return;
	}

    if (deletesw)
	for (seqp = 0; seqs[seqp]; seqp++) {
	    if (zerosw)
		for (msgnum = mp -> lowmsg; msgnum <= mp -> hghmsg; msgnum++)
		    if (mp -> msgstats[msgnum] & EXISTS)
			if (!m_seqadd (mp, seqs[seqp], msgnum, 0))
			    return;
	    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
		if (mp -> msgstats[msgnum] & SELECTED)
		    if (!m_seqdel (mp, seqs[seqp], msgnum))
			return;
	}

    if (listsw) {
	int     bits = FFATTRSLOT;

	if (seqp == 0)
	    for (i = 0; mp -> msgattrs[i]; i++)
		printf ("%s%s: %s\n", mp -> msgattrs[i],
			mp -> attrstats & (1 << (bits + i))
			? " (private)" : "",
			m_seq (mp, mp -> msgattrs[i]));
	else
	    for (seqp = 0; seqs[seqp]; seqp++)
		printf ("%s%s: %s\n", seqs[seqp], m_seq (mp, seqs[seqp]));

	interrupted = 0;
	if (debugsw)
	    for (msgnum = mp -> lowsel;
		    msgnum <= mp -> hghsel && !interrupted;
		    msgnum++)
		if (mp -> msgstats[msgnum] & SELECTED) {
		    printf ("%*d: id=%d top=%d start=%ld stop=%ld %s\n",
			    DMAXFOLDER, msgnum,
			    Msgs[msgnum].m_bboard_id, Msgs[msgnum].m_top,
			    Msgs[msgnum].m_start, Msgs[msgnum].m_stop,
			    sprintb (buf, (unsigned) mp -> msgstats[msgnum],
				m_seqbits (mp)));
		    if (Msgs[msgnum].m_scanl)
			printf ("%s", Msgs[msgnum].m_scanl);
		}			    
    }
}

/*  */

#ifdef MIME
static struct swit mhnswit[] = {
#define	MHNAUTOSW	  0
    "auto", 0,
#define	MHNNAUTOSW	  1
    "noauto", 0,
#define	MHNDEBUGSW	  2
    "debug", -5,
#define	MHNEBCDICSW 	 3
    "ebcdicsafe", 0,
#define	MHNNEBCDICSW	 4
    "noebcdicsafe", 0,
#define	MHNFORMSW	  5
    "form formfile", 4,
#define	MHNHEADSW	  6
    "headers", 0,
#define	MHNNHEADSW	  7
    "noheaders", 0,
#define	MHNLISTSW	  8
    "list", 0,
#define	MHNNLISTSW	  9
    "nolist", 0,
#define	MHNPARTSW	 10
    "part number", 0,
#define	MHNSIZESW	 11
    "realsize", 0,
#define	MHNNSIZESW	 12
    "norealsize", 0,
#define	MHNRFC934SW	 13
    "rfc934mode", 0,
#define	MHNNRFC934SW	 14
    "norfc934mode", 0,
#define	MHNSERIALSW	 15
    "serialonly", 0,
#define	MHNNSERIALSW	 16
    "noserialonly", 0,
#define	MHNSHOWSW	 17
    "show", 0,
#define	MHNNSHOWSW	 18
    "noshow", 0,
#define	MHNSTORESW	 19
    "store", 0,
#define	MHNNSTORESW	 20
    "nostore", 0,
#define	MHNTYPESW	 21
    "type content", 0,
#define	MHNVERBSW	 22
    "verbose", 0,
#define	MHNNVERBSW	 23
    "noverbose", 0,
#define	MHNHELPSW	 24
    "help", 4,
#define	MHNPROGSW	 25
    "moreproc program", -4,
#define	MHNNPROGSW	 26
    "nomoreproc", -3,
#define	MHNLENSW	 27
    "length lines", -4,
#define	MHNWIDSW	 28
    "width columns", -4,

    NULL, 0
};

/*  */

mhncmd (args)
char  **args;
{
    int     msgp = 0,
	    vecp = 1,
	    i,
	    msgnum;
    char   *cp,
            buf[BUFSIZ],
	   *msgs[MAXARGS],
           *vec[MAXARGS];

    if (fmsh) {
	forkcmd (args, cmd_name);
	return;
    }

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, mhnswit)) {
		case AMBIGSW: 
		    ambigsw (cp, mhnswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case MHNHELPSW:
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, mhnswit);
		    return;

		case MHNAUTOSW:
		case MHNNAUTOSW:
		case MHNDEBUGSW:
		case MHNEBCDICSW:
		case MHNNEBCDICSW:
		case MHNHEADSW:
		case MHNNHEADSW:
		case MHNLISTSW:
		case MHNNLISTSW:
		case MHNSIZESW:
		case MHNNSIZESW:
		case MHNRFC934SW:
		case MHNNRFC934SW:
		case MHNSERIALSW:
		case MHNNSERIALSW:
		case MHNSHOWSW:
		case MHNNSHOWSW:
		case MHNSTORESW:
		case MHNNSTORESW:
		case MHNVERBSW:
		case MHNNVERBSW:
		case MHNNPROGSW:
		    vec[vecp++] = --cp;
		    continue;

		case MHNFORMSW:
		case MHNPARTSW:
		case MHNTYPESW:
		case MHNPROGSW:
		case MHNLENSW:
		case MHNWIDSW:
		    vec[vecp++] = --cp;
		    if (!(cp = *args++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    vec[vecp++] = cp;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    msgs[msgp++] = cp;
    }

    vec[0] = cmd_name;
    vec[vecp++] = "-file";
    vec[vecp] = NULL;
    if (!msgp)
	msgs[msgp++] = "cur";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    return;
    m_setseq (mp);

    interrupted = 0;
    for (msgnum = mp -> lowsel;
	    msgnum <= mp -> hghsel && !interrupted;
	    msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED)
	    if (process (msgnum, cmd_name, vecp, vec)) {
		mp -> msgstats[msgnum] &= ~SELECTED;
		mp -> numsel--;
	    }

    m_setcur (mp, mp -> hghsel);
}

/*  */

#endif /* MIME */
static struct swit packswit[] = {
#define	PAFISW	0
    "file name", 0,

#define	PAHELP	1
    "help", 4,

    NULL, 0
};

/*  */

packcmd (args)
char  **args;
{
    int     msgp = 0,
            md,
            msgnum;
    char   *cp,
           *file = NULL,
            buf[BUFSIZ],
           *msgs[MAXARGS];
    struct stat st;

    if (fmsh) {
	forkcmd (args, cmd_name);
	return;
    }

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, packswit)) {
		case AMBIGSW: 
		    ambigsw (cp, packswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case PAHELP: 
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, packswit);
		    return;

		case PAFISW: 
		    if (!(file = *args++) || *file == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    msgs[msgp++] = cp;
    }

    if (!file)
	file = "./msgbox";
    file = path (file, TFILE);
    if (stat (file, &st) == NOTOK) {
	if (errno != ENOENT) {
	    advise (file, "error on file");
	    goto done_pack;
	}
	md = getanswer (cp = concat ("Create file \"", file, "\"? ", NULLCP));
	free (cp);
	if (!md)
	    goto done_pack;
    }

    if (!msgp)
	msgs[msgp++] = "all";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    goto done_pack;
    m_setseq (mp);

    if ((md = mbx_open (file, getuid (), getgid (), m_gmprot ())) == NOTOK) {
	advise (file, "unable to open");
	goto done_pack;
    }
    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED)
	    if (pack (file, md, msgnum) == NOTOK)
		break;
    (void) mbx_close (file, md);

    if (mp -> hghsel != mp -> curmsg)
	m_setcur (mp, mp -> lowsel);

done_pack: ;
    free (file);
}

/*  */

int	pack (mailbox, md, msgnum)
char   *mailbox;
int     md,
        msgnum;
{
    register FILE *zp;

    if (Msgs[msgnum].m_bboard_id == 0)
	(void) readid (msgnum);

    zp = msh_ready (msgnum, 1);
    return mbx_write (mailbox, md, zp, Msgs[msgnum].m_bboard_id,
	    0L, ftell (zp), Msgs[msgnum].m_stop, 1, 1);
}

/*  */

int	packhak (args)
char  **args;
{
    int	    result;
    char   *cp,
	   *file = NULL;

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, packswit)) {
		case AMBIGSW: 
		case UNKWNSW: 
		case PAHELP: 
		    return NOTOK;

		case PAFISW: 
		    if (!(file = *args++) || *file == '-') 
			return NOTOK;
		    continue;
	    }
	if (*cp == '+' || *cp == '@')
	    return NOTOK;
    }

    file = path (file ? file : "./msgbox", TFILE);
    result = access (file, 0) == NOTOK ? OK : NOTOK;
    free (file);

    return result;
}

/*  */

static struct swit pickswit[] = {
#define	PIANSW	0
    "and", 0,
#define	PIORSW	1
    "or", 0,
#define	PINTSW	2
    "not", 0,
#define	PILBSW	3
    "lbrace", 0,
#define	PIRBSW	4
    "rbrace", 0,

#define	PICCSW	5
    "cc  pattern", 0,
#define	PIDASW	6
    "date  pattern", 0,
#define	PIFRSW	7
    "from  pattern", 0,
#define	PISESW	8
    "search  pattern", 0,
#define	PISUSW	9
    "subject  pattern", 0,
#define	PITOSW	10
    "to  pattern", 0,
#define	PIOTSW	11
    "-othercomponent  pattern", 15,
#define	PIAFSW	12
    "after date", 0,
#define	PIBFSW	13
    "before date", 0,
#define	PIDFSW	14
    "datefield field", 5,
#define	PISQSW	15
    "sequence name", 0,
#define	PIPUSW	16
    "public", 0,
#define	PINPUSW	17
    "nopublic", 0,
#define	PIZRSW	18
    "zero", 0,
#define	PINZRSW	19
    "nozero", 0,
#define	PILISW	20
    "list", 0,
#define	PINLISW	21
    "nolist", 0,
#define	PIHELP	22
    "help", 4,

    NULL, 0
};

/*  */

pickcmd (args)
char  **args;
{
    int     zerosw = 1,
            msgp = 0,
            seqp = 0,
            vecp = 0,
            hi,
            lo,
            msgnum;
    char   *cp,
            buf[BUFSIZ],
           *msgs[MAXARGS],
           *seqs[NATTRS],
           *vec[MAXARGS];
    register FILE *zp;

    while (cp = *args++) {
	if (*cp == '-') {
	    if (*++cp == '-') {
		vec[vecp++] = --cp;
		goto pattern;
	    }
	    switch (smatch (cp, pickswit)) {
		case AMBIGSW: 
		    ambigsw (cp, pickswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case PIHELP: 
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, pickswit);
		    return;

		case PICCSW: 
		case PIDASW: 
		case PIFRSW: 
		case PISUSW: 
		case PITOSW: 
		case PIDFSW: 
		case PIAFSW: 
		case PIBFSW: 
		case PISESW: 
		    vec[vecp++] = --cp;
pattern: ;
		    if (!(cp = *args++)) {/* allow -xyz arguments */
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    vec[vecp++] = cp;
		    continue;
		case PIOTSW: 
		    advise (NULLCP, "internal error!");
		    return;
		case PIANSW: 
		case PIORSW: 
		case PINTSW: 
		case PILBSW: 
		case PIRBSW: 
		    vec[vecp++] = --cp;
		    continue;

		case PISQSW: 
		    if (!(cp = *args++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    if (seqp < NATTRS)
			seqs[seqp++] = cp;
		    else {
			advise (NULLCP, "only %d sequences allowed!", NATTRS);
			return;
		    }
		    continue;
		case PIZRSW: 
		    zerosw++;
		    continue;
		case PINZRSW: 
		    zerosw = 0;
		    continue;

		case PIPUSW: 	/* not implemented */
		case PINPUSW: 
		case PILISW: 
		case PINLISW: 
		    continue;
	    }
	}
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    msgs[msgp++] = cp;
    }
    vec[vecp] = NULL;

    if (!msgp)
	msgs[msgp++] = "all";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    return;
    m_setseq (mp);

    interrupted = 0;
    if (!pcompile (vec, NULLCP))
	return;

    lo = mp -> lowsel;
    hi = mp -> hghsel;

    for (msgnum = mp -> lowsel;
	    msgnum <= mp -> hghsel && !interrupted;
	    msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED) {
	    zp = msh_ready (msgnum, 1);
	    if (pmatches (zp, msgnum, fmsh ? 0L : Msgs[msgnum].m_start,
			fmsh ? 0L : Msgs[msgnum].m_stop)) {
		if (msgnum < lo)
		    lo = msgnum;
		if (msgnum > hi)
		    hi = msgnum;
	    }
	    else {
		mp -> msgstats[msgnum] &= ~SELECTED;
		mp -> numsel--;
	    }
	}

    if (interrupted)
	return;

    mp -> lowsel = lo;
    mp -> hghsel = hi;

    if (mp -> numsel <= 0) {
	advise (NULLCP, "no messages match specification");
	return;
    }

    seqs[seqp] = NULL;
    for (seqp = 0; seqs[seqp]; seqp++) {
	if (zerosw && !m_seqnew (mp, seqs[seqp], 0))
	    return;
	for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED)
		if (!m_seqadd (mp, seqs[seqp], msgnum, 0))
		    return;
    }

    printf ("%d hit%s\n", mp -> numsel, mp -> numsel == 1 ? "" : "s");
}

/*  */

static struct swit replswit[] = {
#define	REANSW	0
    "annotate", 0,
#define	RENANSW	1
    "noannotate", 0,
#define	RECCSW	2
    "cc type", 0,
#define	RENCCSW	3
    "nocc type", 0,
#define	REDFSW	4
    "draftfolder +folder", 0,
#define	REDMSW	5
    "draftmessage msg", 0,
#define	RENDFSW	6
    "nodraftfolder", 0,
#define	REEDTSW	7
    "editor editor", 0,
#define	RENEDSW	8
    "noedit", 0,
#define	REFCCSW	9
    "fcc +folder", 0,
#define	REFLTSW	10
    "filter filterfile", 0,
#define	REFRMSW	11
    "form formfile", 0,
#define	REINSW	12
    "inplace", 0,
#define	RENINSW	13
    "noinplace", 0,
#define	REQUSW	14
    "query", 0,
#define	RENQUSW	15
    "noquery", 0,
#define	REWHTSW	16
    "whatnowproc program", 0,
#define	RENWTSW	17
    "nowhatnow", 0,
#define	REWIDSW	19
    "width columns", 0,
#define	REHELP	20
    "help", 4,

    NULL, 0
};

/*  */

replcmd (args)
char  **args;
{
    int     vecp = 1;
    char   *cp,
           *msg = NULL,
            buf[BUFSIZ],
           *vec[MAXARGS];

    if (fmsh) {
	forkcmd (args, cmd_name);
	return;
    }

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, replswit)) {
		case AMBIGSW: 
		    ambigsw (cp, replswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case REHELP: 
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, replswit);
		    return;

		case REANSW:	/* not implemented */
		case RENANSW: 
		case REINSW: 
		case RENINSW: 
		    continue;

		case REQUSW:
		case RENQUSW:
		case RENDFSW:
		case RENEDSW:
		case RENWTSW:
		    vec[vecp++] = --cp;
		    continue;

		case RECCSW: 
		case RENCCSW: 
		case REEDTSW: 
		case REFCCSW: 
		case REFLTSW:
		case REFRMSW: 
		case REWIDSW: 
		case REDFSW:
		case REDMSW:
		case REWHTSW:
		    vec[vecp++] = --cp;
		    if (!(cp = *args++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    vec[vecp++] = cp;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    if (msg) {
		advise (NULLCP, "only one message at a time!");
		return;
	    }
	    else
		msg = cp;
    }

    vec[0] = cmd_name;
    vec[vecp++] = "-file";
    vec[vecp] = NULL;
    if (!msg)
	msg = "cur";
    if (!m_convert (mp, msg))
	return;
    m_setseq (mp);

    if (mp -> numsel > 1) {
	advise (NULLCP, "only one message at a time!");
	return;
    }
    (void) process (mp -> hghsel, cmd_name, vecp, vec);
    m_setcur (mp, mp -> hghsel);
}

/*  */

static struct swit rmmswit[] = {
#define	RMHELP	0
    "help", 4,

    NULL, 0
};

/*  */

rmmcmd (args)
char  **args;
{
    int	    msgp = 0,
            msgnum;
    char   *cp,
            buf[BUFSIZ],
           *msgs[MAXARGS];

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, rmmswit)) {
		case AMBIGSW: 
		    ambigsw (cp, rmmswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case RMHELP: 
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, rmmswit);
		    return;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    msgs[msgp++] = cp;
    }

    if (!msgp)
	msgs[msgp++] = "cur";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    return;
    m_setseq (mp);

    rmm ();
}

/*  */

static  rmm () {
    register int    msgnum,
                    vecp;
    register char  *cp;
    char    buffer[BUFSIZ],
	   *vec[MAXARGS];

    if (fmsh) {
	if (rmmproc) {
	    if (mp -> numsel > MAXARGS - 1) {
		advise (NULLCP, "more than %d messages for %s exec",
			MAXARGS - 1, rmmproc);
		return;
	    }
	    vecp = 0;
	    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
		if (mp -> msgstats[msgnum] & SELECTED)
		    vec[vecp++] = getcpy (m_name (msgnum));
	    vec[vecp] = NULL;
	    forkcmd (vec, rmmproc);
	    for (vecp = 0; vec[vecp]; vecp++)
		free (vec[vecp]);
	}
	else
	    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
		if (mp -> msgstats[msgnum] & SELECTED) {
		    (void) strcpy (buffer, m_backup (cp = m_name (msgnum)));
		    if (rename (cp, buffer) == NOTOK)
			admonish (buffer, "unable to rename %s to", cp);
		}
    }

    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED) {
	    mp -> msgstats[msgnum] |= DELETED;
	    mp -> msgstats[msgnum] &= ~EXISTS;
#ifdef	MPOP
#ifdef	BPOP
	    if (pmsh && pop_dele (msgnum) != OK)
		fprintf (stderr, "%s", response);
#endif
#endif /* MPOP */
	}

    if ((mp -> nummsg -= mp -> numsel) <= 0) {
	if (fmsh)
	    admonish (NULLCP, "no messages remaining in +%s", fmsh);
	else
	    admonish (NULLCP, "no messages remaining in %s", mp -> foldpath);
	mp -> lowmsg = mp -> hghmsg = mp -> nummsg = 0;
    }
    if (mp -> lowsel == mp -> lowmsg) {
	for (msgnum = mp -> lowmsg + 1; msgnum <= mp -> hghmsg; msgnum++)
	    if (mp -> msgstats[msgnum] & EXISTS)
		break;
	mp -> lowmsg = msgnum;
    }
    if (mp -> hghsel == mp -> hghmsg) {
	for (msgnum = mp -> hghmsg - 1; msgnum >= mp -> lowmsg; msgnum--)
	    if (mp -> msgstats[msgnum] & EXISTS)
		break;
	mp -> hghmsg = msgnum;
    }

    mp -> msgflags |= MODIFIED;
    modified++;
}

/*  */

static struct swit scanswit[] = {
#define	SCCLR	0
    "clear", 0,
#define	SCNCLR	1
    "noclear", 0,
#define	SCFORM	2
    "form formatfile", 0,
#define	SCFMT	3
    "format string", 5,
#define	SCHEAD	4
    "header", 0,
#define SCNHEAD	5
    "noheader", 0,
#define	SCWID	6
    "width columns", 0,
#define	SCHELP	7
    "help", 4,

    NULL, 0
};

/*  */

scancmd (args)
char  **args;
{
#define	equiv(a,b)	(a ? b && !strcmp (a, b) : !b)

    int     clearsw = 0,
            headersw = 0,
	    width = 0,
            msgp = 0,
            msgnum,
	    optim,
	    state;
    char   *cp,
	   *form = NULL,
	   *format = NULL,
            buf[BUFSIZ],
	   *nfs,
           *msgs[MAXARGS];
    register FILE *zp;
#ifdef	MPOP
#ifdef	BPOP
    static int p_optim = 0;
#endif
#endif /* MPOP */
    static int s_optim = 0;
    static char *s_form = NULL,
		*s_format = NULL;

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, scanswit)) {
		case AMBIGSW: 
		    ambigsw (cp, scanswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case SCHELP: 
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, scanswit);
		    return;

		case SCCLR: 
		    clearsw++;
		    continue;
		case SCNCLR: 
		    clearsw = 0;
		    continue;
		case SCHEAD: 
		    headersw++;
		    continue;
		case SCNHEAD: 
		    headersw = 0;
		    continue;
		case SCFORM: 
		    if (!(form = *args++) || *form == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    format = NULL;
		    continue;
		case SCFMT: 
		    if (!(format = *args++) || *format == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    form = NULL;
		    continue;
		case SCWID: 
		    if (!(cp = *args++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    width = atoi (cp);
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    msgs[msgp++] = cp;
    }

    if (!msgp)
	msgs[msgp++] = "all";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    return;
    m_setseq (mp);

    nfs = new_fs (form, format, FORMAT);
    if (scanl) {		/* force scansbr to (re)compile format */
	(void) free (scanl);
	scanl = NULL;
    }

    if (s_optim == 0) {
	s_optim = optim = 1;
	s_form = form ? getcpy (form) : NULL;
	s_format = format ? getcpy (format) : NULL;

#ifdef	MPOP
#ifdef	BPOP
	if (pmsh) {
	    int	    i;
	    char   *dp,
		   *ep,
		   *fp;

	    if (width == 0)
		width = sc_width ();

	    for (dp = nfs, i = 0; *dp; dp++, i++)
		if (*dp == '\\' || *dp == '"' || *dp == '\n')
		    i++;
	    i++;
	    if ((ep = malloc ((unsigned) i)) == NULL)
		adios (NULLCP, "out of memory");
	    for (dp = nfs, fp = ep; *dp; dp++) {
		if (*dp == '\n') {
		    *fp++ = '\\', *fp++ = 'n';
		    continue;
		}
		if (*dp == '"' || *dp == '\\')
		    *fp++ = '\\';
		*fp++ = *dp;
	    }
	    *fp = NULL;

	    if (pop_command ("XTND SCAN %d \"%s\"", width, ep) == OK)
		p_optim = 1;

	    free (ep);
	}
#endif
#endif	/* MPOP */
    }
    else
	optim = equiv (s_form, form) && equiv (s_format, format);

#ifdef	MPOP
#ifdef	BPOP
    if (p_optim && optim) {
	for (msgnum = mp -> lowmsg; msgnum <= mp -> hghmsg; msgnum++)
	    if (!(mp -> msgstats[msgnum] & SELECTED) || Msgs[msgnum].m_scanl)
		break;
	if (msgnum > mp -> hghmsg && pop_command ("LIST") == OK) {
	    fprintf (stderr, "Stand-by...");
	    fflush (stderr);

	    for (;;) {
		int	size;

		switch (pop_multiline ()) {
		    case NOTOK:
		        fprintf (stderr, "%s", response);
		        /* and fall... */
		    case DONE:
		        fprintf (stderr,"\n");
		        break;

		    case OK:
			if (sscanf (response, "%d %d", &msgnum, &size) == 2
			        && mp -> lowmsg <= msgnum
			        && msgnum <= mp -> hghmsg
			        && (cp = index (response, '#'))
			        && *++cp)
			    Msgs[msgnum].m_scanl = concat (cp, "\n", NULLCP);
			continue;
		}
		break;
	    }
	}
    }
#endif
#endif /* MPOP */

    interrupted = 0;
    for (msgnum = mp -> lowsel;
	    msgnum <= mp -> hghsel && !interrupted;
	    msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED) {
	    if (optim && Msgs[msgnum].m_scanl)
		printf ("%s", Msgs[msgnum].m_scanl);
	    else {
#ifdef	MPOP
#ifdef	BPOP
		if (p_optim
		        && optim
			&& (mp -> msgstats[msgnum] & VIRTUAL)
		        && pop_command ("LIST %d", msgnum) == OK
			&& (cp = index (response, '#'))
		        && *++cp) {
		    Msgs[msgnum].m_scanl = concat (cp, "\n", NULLCP);
		    printf ("%s", Msgs[msgnum].m_scanl);		    
		    continue;
		}
#endif
#endif /* MPOP */

		zp = msh_ready (msgnum, 0);
		switch (state = scan (zp, msgnum, 0, nfs, width,
			msgnum == mp -> curmsg,
			mp -> msgstats[msgnum] & UNSEEN,	/* ?? */
			headersw ? (fmsh ? fmsh : mp -> foldpath) : (char *)0,
			fmsh ? 0L : (long) (Msgs[msgnum].m_stop - Msgs[msgnum].m_start),
			1)) {
		    case SCNMSG:
		    case SCNENC:
		    case SCNERR:
			if (optim)
			    Msgs[msgnum].m_scanl = getcpy (scanl);
			break;

		    default:
			advise (NULLCP, "scan() botch (%d)", state);
			return;

		    case SCNEOF:
			printf ("%*d  empty\n", DMAXFOLDER, msgnum);
			break;
		    }
	    }
	    headersw = 0;
	}

    if (clearsw)
	clear_screen ();
}

/*  */

static struct swit showswit[] = {
#define	SHDRAFT	0
    "draft", 5,
#define	SHFORM	1
    "form formfile", 4,
#define	SHPROG	2
    "moreproc program", 4,
#define	SHNPROG	3
    "nomoreproc", 3,
#define	SHLEN	4
    "length lines", 4,
#define	SHWID	5
    "width columns", 4,
#define	SHSHOW	6
    "showproc program", 4,
#define	SHNSHOW	7
    "noshowproc", 3,
#define	SHHEAD	8
    "header", 4,
#define SHNHEAD	9
    "noheader", 3,
#define	SHHELP	10
    "help", 4,

    NULL, 0
};

/*  */

showcmd (args)
char  **args;
{
    int	    headersw = 1,
            nshow = 0,
            msgp = 0,
            vecp = 1,
            mhl = 0,
            seen = 0,
            mode = 0,
	    i,
            msgnum;
    char   *cp,
           *proc = showproc,
            buf[BUFSIZ],
           *msgs[MAXARGS],
           *vec[MAXARGS];

    if (uleq (cmd_name, "next"))
	mode = 1;
    else
	if (uleq (cmd_name, "prev"))
	    mode = -1;
    while (cp = *args++) {
	if (*cp == '-')
	    switch (i = smatch (++cp, showswit)) {
		case AMBIGSW: 
		    ambigsw (cp, showswit);
		    return;
		case UNKWNSW: 
		case SHNPROG:
		    vec[vecp++] = --cp;
		    continue;
		case SHHELP: 
		    (void) sprintf (buf,
			    "%s %s[switches] [switches for showproc]",
			    cmd_name, mode ? NULL : "[msgs] ");
		    help (buf, showswit);
		    return;

		case SHFORM: 
		case SHPROG:
		case SHLEN:
		case SHWID:
		    vec[vecp++] = --cp;
		    if (!(cp = *args++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    vec[vecp++] = cp;
		    continue;
		case SHHEAD: 
		    headersw++;
		    continue;
		case SHNHEAD: 
		    headersw = 0;
		    continue;
		case SHSHOW: 
		    if (!(proc = *args++) || *proc == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    nshow = 0;
		    continue;
		case SHNSHOW: 
		    nshow++;
		    continue;

		case SHDRAFT: 
		    advise (NULLCP, "sorry, -%s not allowed!", showswit[i].sw);
		    return;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    if (mode) {
		fprintf (stderr,
			"usage: %s [switches] [switches for showproc]\n",
			cmd_name);
		return;
	    }
	    else
		msgs[msgp++] = cp;
    }
    vec[vecp] = NULL;

    if (!msgp)
	msgs[msgp++] = mode > 0 ? "next" : mode < 0 ? "prev" : "cur";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    return;
    m_setseq (mp);

#ifdef	MIME
    if (!nshow && !getenv ("NOMHNPROC"))
	for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	    if ((mp -> msgstats[msgnum] & SELECTED) && nontext (msgnum)) {
		proc = (cp = m_find ("mhnproc")) ? cp : "mhn";
		vec[vecp++] = "-show";
		vec[vecp++] = "-file";
		vec[vecp] = NULL;
		goto finish;
	    }
#endif	/* MIME */

    if (nshow)
	proc = "cat";
    else
	if (strcmp (showproc, "mhl") == 0) {
	    proc = mhlproc;
	    mhl++;
	}

finish: ;
    seen = m_seqflag (mp, "unseen");
    vec[0] = r1bindex (proc, '/');
    if (mhl) {
	msgp = vecp;
	for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED) {
		vec[vecp++] = getcpy (m_name (msgnum));
		if (seen)
		    (void) m_seqdel (mp, "unseen", msgnum);
	    }
	vec[vecp] = NULL;
	if (mp -> numsel == 1 && headersw)
	    show (mp -> lowsel);
	(void) mhlsbr (vecp, vec, mhl_action);
	m_eomsbr ((int (*)()) 0);
	while (msgp < vecp)
	    free (vec[msgp++]);
    }
    else {
	interrupted = 0;
	for (msgnum = mp -> lowsel;
		msgnum <= mp -> hghsel && !interrupted;
		msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED) {
		switch (ask (msgnum)) {
		    case NOTOK: /* QUIT */
			break;

		    case OK: 	/* INTR */
			continue;

		    default:
			if (mp -> numsel == 1 && headersw)
			    show (msgnum);
			if (nshow)
			    copy_message (msgnum, stdout);
			else
			    (void) process (msgnum, proc, vecp, vec);

			if (seen)
			    (void) m_seqdel (mp, "unseen", msgnum);
			continue;
		}
		break;
	    }
    }

    m_setcur (mp, mp -> hghsel);
}

/*  */

static  show (msgnum)
int     msgnum;
{
    if (Msgs[msgnum].m_bboard_id == 0)
	(void) readid (msgnum);

    printf ("(Message %d", msgnum);
    if (Msgs[msgnum].m_bboard_id > 0)
	printf (", %s: %d", BBoard_ID, Msgs[msgnum].m_bboard_id);
    printf (")\n");
}


/* ARGSUSED */

static	int eom_action (c)
int     c;
{
    return (ftell (mhlfp) >= Msgs[mhlnum].m_stop);
}


static	FP mhl_action (name)
char   *name;
{
    int     msgnum;

    if ((msgnum = m_atoi (name)) < mp -> lowmsg
	    || msgnum > mp -> hghmsg
	    || !(mp -> msgstats[msgnum] & EXISTS))
	return NULL;
    mhlnum = msgnum;

    mhlfp = msh_ready (msgnum, 1);
    if (!fmsh)
	m_eomsbr (eom_action);

    return mhlfp;
}


/*  */

static  ask (msgnum)
int     msgnum;
{
    char    buf[BUFSIZ];

    if (mp -> numsel == 1 || !interactive || redirected)
	return DONE;

    if (SOprintf ("Press <return> to list \"%d\"...", msgnum)) {
	if (mp -> lowsel != msgnum)
	    printf ("\n\n\n");
	printf ("Press <return> to list \"%d\"...", msgnum);
    }
    (void) fflush (stdout);
    buf[0] = 0;
#ifndef	BSD42
    (void) read (fileno (stdout), buf, sizeof buf);
#else	/* BSD42 */
    switch (setjmp (sigenv)) {
	case OK: 
	    should_intr = 1;
	    (void) read (fileno (stdout), buf, sizeof buf);/* fall... */

	default: 
	    should_intr = 0;
	    break;
    }
#endif	/* BSD42 */
    if (index (buf, '\n') == NULL)
	(void) putchar ('\n');

    if (told_to_quit) {
	told_to_quit = interrupted = 0;
	return NOTOK;
    }
    if (interrupted) {
	interrupted = 0;
	return OK;
    }

    return DONE;
}

/*  */

#ifdef	MIME
#include "../h/mhn.h"


static int  nontext (msgnum)
int	msgnum;
{
    int	    result,
	    state;
    register char   *bp,
		    *dp;
    char   *chset,
	   *cp,
	    buf[BUFSIZ],
	    name[NAMESZ];
    FILE   *fp;

    if (Msgs[msgnum].m_flags & MHNCHK)
	return (Msgs[msgnum].m_flags & MHNYES);
    Msgs[msgnum].m_flags |= MHNCHK;

    fp = msh_ready (msgnum, 1);

    if (!(chset = getenv ("MM_CHARSET")))
	chset = "us-ascii";

    for (state = FLD;;)
	switch (state = m_getfld (state, name, buf, sizeof buf, fp)) {
	    case FLD:
	    case FLDPLUS:
	    case FLDEOF:
	        if (uleq (name, TYPE_FIELD)) {
		    int	    passno;
		    char    c;

		    cp = add (buf, NULLCP);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, fp);
			cp = add (buf, cp);
		    }
		    bp = cp;
		    passno = 1;

again: ;
		    for (; isspace (*bp); bp++)
			continue;
		    if (*bp == '(') {
			int	i;

			for (bp++, i = 0;;) {
			    switch (*bp++) {
				case '\0':
invalid: ;
				    result = 0;
				    goto out;
				case '\\':
				    if (*bp++ == '\0')
					goto invalid;
    				    continue;
				case '(':
    				    i++;
    				    /* and fall... */
    				default:
    				    continue;
    				case ')':
    				    if (--i < 0)
					break;
				continue;
			    }
			    break;
			}
		    }
		    if (passno == 2) {
			if (*bp != '/')
			    goto invalid;
			bp++;
			passno = 3;
			goto again;
		    }
		    for (dp = bp; istoken (*dp); dp++)
			continue;
		    c = *dp, *dp = NULL;
		    if (*bp == NULL)
			goto invalid;
		    if (passno > 1) {
			if (result = !uleq (bp, "plain"))
			    goto out;
			*dp = c;
			for (dp++; isspace (*dp); dp++)
			    continue;
			if (result = !uprf (dp, "charset"))
			    goto out;
			dp += sizeof "charset" - 1;
			while (isspace (*dp))
			    dp++;
			if (*dp++ != '=')
			    goto invalid;
			while (isspace (*dp))
			    dp++;
			if (*dp == '"') {
			    if (bp = index (++dp, '"'))
				*bp = NULL;
			}
			else
			    for (bp = dp; *bp; bp++)
				if (isspace (*bp)) {
				    *bp = NULL;
				    break;
				}
			if ((result = !uleq (dp, chset))
			        && uleq (dp, "us-ascii")
			        && uleq (chset, "iso-8859-1"))
			    result = 0;
		    }
		    else
			if (!(result = !uleq (bp, "text"))) {
			    *dp = c;
			    bp = dp;
			    passno = 2;
			    goto again;
			}

out: ;
		    free (cp);

		    if (result) {
			Msgs[msgnum].m_flags |= MHNYES;
			return result;
		    }
		    break;
		}
	        if (uleq (name, ENCODING_FIELD)) {
		    cp = add (buf, NULLCP);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, fp);
			cp = add (buf, cp);
		    }
		    for (bp = cp; isspace (*bp); bp++)
			continue;
		    for (dp = bp; istoken (*dp); dp++)
			continue;
		    *dp = NULL;
		    result = !uleq (bp, "7bit")
				&& !uleq (bp, "8bit")
				&& !uleq (bp, "binary");

		    free (cp);
		    if (result) {
			Msgs[msgnum].m_flags |= MHNYES;
			return result;
		    }
		    break;
		}
		while (state == FLDPLUS)
		    state = m_getfld (state, name, buf, sizeof buf, fp);
		break;

	    default:
		return 0;
	}
}
#endif	/* MIME */

/*  */

static struct swit sortswit[] = {
#define	SODATE	0
    "datefield field", 0,
#define	SOSUBJ	1
    "textfield field", 0,
#define	SONSUBJ	2
    "notextfield", 0,
#define	SOLIMT	3
    "limit days", 0,
#define	SONLIMT	4
    "nolimit", 0,
#define	SOVERB	5
    "verbose", 0,
#define	SONVERB	6
    "noverbose", 0,
#define	SOHELP	7
    "help", 4,

    NULL, 0
};

/*  */

sortcmd (args)
char  **args;
{
    int     msgp = 0,
            msgnum;
    char   *cp,
           *datesw = NULL,
    	   *subjsw = NULL,
            buf[BUFSIZ],
           *msgs[MAXARGS];
    struct tws  tb,
               *tw;

    if (fmsh) {
	forkcmd (args, cmd_name);
	return;
    }

    while (cp = *args++) {
	if (*cp == '-')
	    switch (smatch (++cp, sortswit)) {
		case AMBIGSW: 
		    ambigsw (cp, sortswit);
		    return;
		case UNKWNSW: 
		    fprintf (stderr, "-%s unknown\n", cp);
		    return;
		case SOHELP: 
		    (void) sprintf (buf, "%s [msgs] [switches]", cmd_name);
		    help (buf, sortswit);
		    return;

		case SODATE: 
		    if (datesw) {
			advise (NULLCP, "only one date field at a time!");
			return;
		    }
		    if (!(datesw = *args++) || *datesw == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    continue;

		case SOSUBJ:
		    if (subjsw) {
			advise (NULLCP, "only one text field at a time!");
			return;
		    }
		    if (!(subjsw = *args++) || *subjsw == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		    continue;
		case SONSUBJ:
		    subjsw = (char *)0;
		    continue;

		case SOLIMT:		/* too hard */
		    if (!(cp = *args++) || *cp == '-') {
			advise (NULLCP, "missing argument to %s", args[-2]);
			return;
		    }
		case SONLIMT:
		case SOVERB: 		/* not implemented */
		case SONVERB: 
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    advise (NULLCP, "sorry, no folders allowed!");
	    return;
	}
	else
	    msgs[msgp++] = cp;
    }

    if (!msgp)
	msgs[msgp++] = "all";
    if (!datesw)
	datesw = "Date";
    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    return;
    m_setseq (mp);

    twscopy (&tb, dtwstime ());

    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++) {
	if (Msgs[msgnum].m_scanl) {
	    free (Msgs[msgnum].m_scanl);
	    Msgs[msgnum].m_scanl = NULL;
	}
	if (mp -> msgstats[msgnum] & SELECTED) {
	    if (getws (datesw, subjsw, msgnum, &Msgs[msgnum]))
		twscopy (&Msgs[msgnum].m_tb,
			msgnum != mp -> lowsel ? &Msgs[msgnum - 1].m_tb : &tb);
	}
	else			/* m_scaln is already NULL */
	    twscopy (&Msgs[msgnum].m_tb, &tb);
	Msgs[msgnum].m_stats = mp -> msgstats[msgnum];
	if (mp -> curmsg == msgnum)
	    Msgs[msgnum].m_stats |= CUR;
    }

    qsort ((char *) &Msgs[mp -> lowsel], mp -> hghsel - mp -> lowsel + 1,
	    sizeof (struct Msg),
	    subjsw ? subsort : msgsort);

    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++) {
	if (subjsw && Msgs[msgnum].m_scanl) {
	    free (Msgs[msgnum].m_scanl);	/* from subjsort */
	    Msgs[msgnum].m_scanl = NULL;
	}
	mp -> msgstats[msgnum] = Msgs[msgnum].m_stats & ~CUR;
	if (Msgs[msgnum].m_stats & CUR)
	    m_setcur (mp, msgnum);
    }
	    
    mp -> msgflags |= MODIFIED;
    modified++;
}

/*  */

/* 
 * getws - parse message, and get date and subject if needed.  We'll use
 * the msgp->m_tb tws struct for the date, and overload the msgp->m_scanl
 * field with our subject string.
 */
static int   getws (datesw, subjsw, msgnum, msgp)
char   *datesw,
       *subjsw;
int	msgnum;
struct	Msg	*msgp;
{
    int	    state,
	    gotdate = 0;
    char   *bp,
            buf[BUFSIZ],
            name[NAMESZ];
    struct tws *tw = (struct tws *)0;
    register FILE *zp;

    zp = msh_ready (msgnum, 0);
    for (state = FLD;;) {
	switch (state = m_getfld (state, name, buf, sizeof buf, zp)) {
	    case FLD: 
	    case FLDEOF: 
	    case FLDPLUS: 
		if (uleq (name, datesw)) {
		    bp = getcpy (buf);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, zp);
			bp = add (buf, bp);
		    }
		    if ((tw = dparsetime (bp)) == NULL)
			admonish (NULLCP,
				"unable to parse %s field in message %d",
				datesw, msgnum);
		    else
			twscopy (&(msgp->m_tb), tw);
		    free (bp);
		    if (!subjsw)	/* not using this, or already done */
			break;		/* all done! */
		    gotdate++;
		}
		else if (subjsw && uleq(name, subjsw)) {
		    bp = getcpy (buf);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, zp);
			bp = add (buf, bp);
		    }
		    msgp->m_scanl = sosmash(subjsw, bp);
		    if (gotdate)
			break;		/* date done so we're done */
		    else
			subjsw = (char *)0;/* subject done, need date */
		} else {
		    while (state == FLDPLUS)	/* flush this one */
			state = m_getfld (state, name, buf, sizeof buf, zp);
		}
		continue;

	    case BODY: 
	    case BODYEOF: 
	    case FILEEOF: 
		break;

	    case LENERR: 
	    case FMTERR: 
		admonish (NULLCP, "format error in message %d", msgnum);
		if (msgp->m_scanl) {	/* this might need free'd */
		    free (msgp->m_scanl); /* probably can't use subj anyway */
		    msgp->m_scanl = NULL;
		}
		return NOTOK;

	    default: 
		adios (NULLCP, "internal error -- you lose");
	}
	break;
    }
    if (tw)
	return OK;	/* not an error if subj not found */

    admonish (NULLCP, "no %s field in message %d", datesw, msgnum);
    return NOTOK;	/* NOTOK means use some other date */
}

/* sort routines */

static int  msgsort (a, b)
struct Msg *a,
           *b;
{
    return twsort (&a -> m_tb, &b -> m_tb);
}

static int  subsort (a, b)
struct Msg *a,
           *b;
{
	register int i;

	if (a->m_scanl && b->m_scanl)
	    if (i = strcmp (a->m_scanl, b->m_scanl))
		return (i);

	return twsort (&a -> m_tb, &b -> m_tb);
}

/*
 * try to make the subject "canonical": delete leading "re:", everything
 * but letters & smash letters to lower case. 
 */
static char *
sosmash (subj, s)
char *subj;
register char *s;
{
    register char  *cp,
		   *dp,
		    c;
    if (s) {
	cp = s;
	dp = s;	/* dst pointer */
	if (uleq (subj, "subject"))
	    while (c = *cp) {
		if (! isspace(c)) {
		    if(uprf(cp, "re:"))
			cp += 2;
		    else {
			if (isalnum(c))
			    *dp++ = isupper(c) ? tolower(c) : c;
			break;
		    }
		}
		cp++;
	    }
	while (c = *cp++) {
	    if (isalnum(c))
		*dp++ = isupper(c) ? tolower(c) : c;

	}
	*dp = '\0';
    }
    return s;
}

/*  */

static int  process (msgnum, proc, vecp, vec)
int     msgnum,
        vecp;
char   *proc,
      **vec;
{
    int	    child_id,
	    status;
    char    tmpfil[80];
    FILE   *out;

    if (fmsh) {
	(void) strcpy (tmpfil, m_name (msgnum));
	(void) m_delete (pfolder);
	m_replace (pfolder, fmsh);
	m_sync (mp);
	m_update ();
	goto ready;
    }

    (void) strcpy (tmpfil, m_scratch ("", invo_name));
    if ((out = fopen (tmpfil, "w")) == NULL) {
	int     olderr;
	extern int  errno;
	char    newfil[80];

	olderr = errno;
	(void) strcpy (newfil, m_tmpfil (invo_name));
	if ((out = fopen (newfil, "w")) == NULL) {
	    errno = olderr;
	    advise (tmpfil, "unable to create temporary file");
	    return NOTOK;
	}
	else
	    (void) strcpy (tmpfil, newfil);
    }
    copy_message (msgnum, out);
    (void) fclose (out);

ready: ;
    (void) fflush (stdout);
    switch (child_id = fork ()) {
	case NOTOK: 
	    advise ("fork", "unable to");
	    status = NOTOK;
	    break;
	    
	case OK: 
	    closefds (3);
	    (void) signal (SIGINT, istat);
	    (void) signal (SIGQUIT, qstat);

	    vec[vecp++] = tmpfil;
	    vec[vecp] = NULL;

	    execvp (proc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (proc);
	    _exit (1);

	default: 
	    status = pidXwait (child_id, NULLCP);
	    break;
    }

    if (!fmsh)
	(void) unlink (tmpfil);
    return status;
}

/*  */

static  copy_message (msgnum, out)
int     msgnum;
FILE * out;
{
    long    pos;
    static char buffer[BUFSIZ];
    register    FILE * zp;

    zp = msh_ready (msgnum, 1);
    if (fmsh) {
	while (fgets (buffer, sizeof buffer, zp) != NULL) {
	    fputs (buffer, out);
	    if (interrupted && out == stdout)
		break;
	}
    }
    else {
	pos = ftell (zp);
	while (fgets (buffer, sizeof buffer, zp) != NULL
		&& pos < Msgs[msgnum].m_stop) {
	    fputs (buffer, out);
	    pos += (long) strlen (buffer);
	    if (interrupted && out == stdout)
		break;
	}
    }
}


static  copy_digest (msgnum, out)
int     msgnum;
FILE * out;
{
    char    c;
    long    pos;
    static char buffer[BUFSIZ];
    register FILE *zp;

    c = '\n';
    zp = msh_ready (msgnum, 1);
    if (!fmsh)
	pos = ftell (zp);
    while (fgets (buffer, sizeof buffer, zp) != NULL
	    && !fmsh && pos < Msgs[msgnum].m_stop) {
	if (c == '\n' && *buffer == '-')
	    (void) fputc (' ', out);
	fputs (buffer, out);
	c = buffer[strlen (buffer) - 1];
	if (!fmsh)
	    pos += (long) strlen (buffer);
	if (interrupted && out == stdout)
	    break;
    }
}
