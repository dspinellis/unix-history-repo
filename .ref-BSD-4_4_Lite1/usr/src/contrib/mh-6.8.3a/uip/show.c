/* show.c - list messages */
#ifndef	lint
static char ident[] = "@(#)$Id: show.c,v 1.14 1993/10/26 20:09:21 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <ctype.h>
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	DRFTSW	0
    "draft", 5,

#define	FORMSW	1
    "form formfile", 4,
#define	PROGSW	2
    "moreproc program", 4,
#define	NPROGSW	3
    "nomoreproc", 3,
#define	LENSW	4
    "length lines", 4,
#define	WIDSW	5
    "width columns", 4,

#define	SHOWSW	6
    "showproc program", 4,
#define	NSHOWSW	7
    "noshowproc", 3,

#define	HEADSW	8
    "header", 4,
#define	NHEADSW	9
    "noheader", 3,

#define	FILESW	10
    "file file", -4,		/* interface from showfile */

#define	HELPSW	11
    "help", 4,

    NULL, 0
};

#ifdef	MIME
static int  nontext();
#endif

#define	SHOW	0
#define	NEXT	1
#define	PREV	2

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    int     draftsw = 0,
            headersw = 1,
            nshow = 0,
            msgp = 0,
            vecp = 1,
	    procp = 1,
	    isdf = 0,
	    mode = SHOW,
            msgnum;
    char   *cp,
           *maildir,
           *file = NULL,
           *folder = NULL,
           *proc,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *msgs[MAXARGS],
           *vec[MAXARGS];
    struct msgs *mp;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    if (uleq (invo_name, "next"))
	mode = NEXT;
    else
	if (uleq (invo_name, "prev"))
	    mode = PREV;
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
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		case NPROGSW:
		    vec[vecp++] = --cp;
		    continue;
		case HELPSW: 
		    (void) sprintf (buf,
			   "%s [+folder] %s[switches] [switches for showproc]",
			    invo_name, mode == SHOW ? "[msgs] ": "");
		    help (buf, switches);
		    done (1);

		case DRFTSW: 
		    if (file)
			adios (NULLCP, "only one file at a time!");
		    draftsw++;
		    if (mode == SHOW)
			continue;
	    usage:  ;
		    adios (NULLCP,
			    "usage: %s [+folder] [switches] [switches for showproc]",
			    invo_name);
		case FILESW: 
		    if (mode != SHOW)
			goto usage;
		    if (draftsw || file)
			adios (NULLCP, "only one file at a time!");
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    file = path (cp, TFILE);
		    continue;

		case HEADSW: 
		    headersw++;
		    continue;
		case NHEADSW: 
		    headersw = 0;
		    continue;

		case FORMSW:
		    vec[vecp++] = --cp;
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    vec[vecp++] = getcpy (libpath(cp));
		    continue;

		case PROGSW:
		case LENSW:
		case WIDSW:
		    vec[vecp++] = --cp;
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    vec[vecp++] = cp;
		    continue;

		case SHOWSW: 
		    if (!(showproc = *argp++) || *showproc == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    nshow = 0;
		    continue;
		case NSHOWSW: 
		    nshow++;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    if (folder)
		adios (NULLCP, "only one folder at a time!");
	    else
		folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	}
	else
	    if (mode != SHOW)
		goto usage;
	    else
		msgs[msgp++] = cp;
    }
    procp = vecp;

/*  */

    if (!m_find ("path"))
	free (path ("./", TFOLDER));

    if (draftsw || file) {
	if (msgp)
	    adios (NULLCP, "only one file at a time!");
	vec[vecp++] = draftsw
	    ? getcpy (m_draft (folder, msgp ? msgs[0] : NULL, 1, &isdf))
	    : file;
	goto go_to_it;
    }

#ifdef	WHATNOW
    if (!msgp && !folder && mode == SHOW && (cp = getenv ("mhdraft")) && *cp) {
	draftsw++;
	vec[vecp++] = cp;
	goto go_to_it;
    }
#endif	/* WHATNOW */

    if (!msgp)
	msgs[msgp++] = mode == NEXT ? "next" : mode == PREV ? "prev" : "cur";
    if (!folder)
	folder = m_getfolder ();
    maildir = m_maildir (folder);

    if (chdir (maildir) == NOTOK)
	adios (maildir, "unable to change directory to");
    if (!(mp = m_gmsg (folder)))
	adios (NULLCP, "unable to read folder %s", folder);
    if (mp -> hghmsg == 0)
	adios (NULLCP, "no messages in %s", folder);

    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    done (1);
    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED)
	    mp -> msgstats[msgnum] |= UNSEEN;
    m_setseq (mp);
    m_setvis (mp, 1);

    if (mp -> numsel > MAXARGS - 2)
	adios (NULLCP, "more than %d messages for show exec", MAXARGS - 2);
    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED)
	    vec[vecp++] = getcpy (m_name (msgnum));

    m_replace (pfolder, folder);
    if (mp -> hghsel != mp -> curmsg)
	m_setcur (mp, mp -> hghsel);
    m_sync (mp);
    m_update ();

    if (vecp == 2 && headersw)
	printf ("(Message %s:%s)\n", folder, vec[1]);

/*  */

go_to_it: ;
    (void) fflush (stdout);

    vec[vecp] = NULL;

#ifdef	MIME
    if (!nshow && !getenv ("NOMHNPROC")) {
	if (!(cp = m_find ("mhnproc")))
	    cp = "mhn";

	if (draftsw || file) {
	    if (nontext (vec[vecp - 1])) {
		vec[vecp] = vec[vecp - 1];
		vec[vecp - 1] = "-file";
		vecp++;
		goto use_mmp;
	    }
	}
	else
	    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
		if ((mp -> msgstats[msgnum] & SELECTED)
		        && nontext (m_name (msgnum))) {
use_mmp: ;
		    proc = cp;
		    vec[vecp++] = "-show";
		    vec[vecp] = NULL;
		    goto finish;
		}
    }
#endif	/* MIME */
    if (nshow)
	proc = "/bin/cat";
    else {
	if (folder && !draftsw && !file)
	    (void) m_putenv ("mhfolder", folder);
	if (strcmp (r1bindex (showproc, '/'), "mhl") == 0) {
	    vec[0] = "mhl";
	    (void) mhl (vecp, vec);
	    done (0);
	}
	proc = showproc;
    }

    if (!draftsw
	    && !file
	    && chdir (maildir = concat (m_maildir (""), "/", NULLCP))
	    != NOTOK) {
	mp -> foldpath = concat (mp -> foldpath, "/", NULLCP);
	cp = ssequal (maildir, mp -> foldpath)
	    ? mp -> foldpath + strlen (maildir)
	    : mp -> foldpath;
	for (msgnum = procp; msgnum < vecp; msgnum++)
	    vec[msgnum] = concat (cp, vec[msgnum], NULLCP);
    }

finish:;
    vec[0] = r1bindex (proc, '/');
    execvp (proc, vec);
    adios (proc, "unable to exec");
}

/*  */

/* Cheat:  we are loaded with adrparse, which wants a routine called
   OfficialName().  We call adrparse:getm() with the correct arguments
   to prevent OfficialName() from being called.  Hence, the following
   is to keep the loader happy.
 */

char   *OfficialName (name)
register char  *name;
{
    return name;
}

/*  */

#ifdef MIME
#include "../h/mhn.h"


static int  nontext (msgnam)
char   *msgnam;
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

    if ((fp = fopen (msgnam, "r")) == NULL)
	return 0;

    if (!(chset = getenv ("MM_CHARSET")))
	chset = "us-ascii";

    for (state = FLD;;)
	switch (state = m_getfld (state, name, buf, sizeof buf, fp)) {
	    case FLD:
	    case FLDPLUS:
	    case FLDEOF:
	        if (uleq (name, TYPE_FIELD)) {
		    int	    passno;
		    char     c;

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
			(void) fclose (fp);
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
			(void) fclose (fp);
			return result;
		    }
		    break;
		}
		while (state == FLDPLUS)
		    state = m_getfld (state, name, buf, sizeof buf, fp);
		break;

	    default:
		(void) fclose (fp);
		return 0;
	}
}
#endif	/* MIME */
