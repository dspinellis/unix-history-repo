/* refile.c - file messages away */
#ifndef	lint
static char ident[] = "@(#)$Id: refile.c,v 1.10 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	DRAFTSW	0
    "draft", 0,

#define	LINKSW	1
    "link", 0,
#define	NLINKSW	2
    "nolink", 0,

#define	PRESSW	3
    "preserve", 0,
#define	NPRESSW	4
    "nopreserve", 0,

#define	SRCSW	5
    "src +folder", 0,

#define	FILESW	6
    "file file", 0,

#define	RPROCSW	7
    "rmmproc program", 0,
#define	NRPRCSW 8
    "normmproc", 0,

#define	HELPSW	9
    "help", 4,

    NULL, 0
};

/*  */

extern int  errno;


static char maildir[BUFSIZ];


struct st_fold {
    char   *f_name;
    struct msgs *f_mp;
};

static opnfolds(), clsfolds(), removeit();
/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    int	    linkf = 0,
            prsrvf = 0,
	    filep = 0,
            foldp = 0,
            msgp = 0,
	    isdf = 0,
	    i,
            msgnum;
    char   *cp,
           *folder = NULL,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
	   *filevec[NFOLDERS + 2],
          **files = &filevec[1],	/* leave room for removeit:vec[0] */
           *msgs[MAXARGS];
    struct st_fold   folders[NFOLDERS + 1];
    struct msgs *mp;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
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
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown\n", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [msgs] [switches] +folder ...",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case LINKSW: 
		    linkf++;
		    continue;
		case NLINKSW: 
		    linkf = 0;
		    continue;

		case PRESSW: 
		    prsrvf++;
		    continue;
		case NPRESSW: 
		    prsrvf = 0;
		    continue;

		case SRCSW: 
		    if (folder)
			adios (NULLCP, "only one source folder at a time!");
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    folder = path (*cp == '+' || *cp == '@' ? cp + 1 : cp,
				   *cp != '@' ? TFOLDER : TSUBCWF);
		    continue;
		case DRAFTSW:
		    if (filep > NFOLDERS)
			adios (NULLCP, "only %d files allowed!", NFOLDERS);
		    isdf = 0;
		    files[filep++] = getcpy (m_draft (NULLCP, NULLCP, 1, &isdf));
		    continue;
		case FILESW: 
		    if (filep > NFOLDERS)
			adios (NULLCP, "only %d files allowed!", NFOLDERS);
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    files[filep++] = path (cp, TFILE);
		    continue;

		case RPROCSW: 
		    if (!(rmmproc = *argp++) || *rmmproc == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case NRPRCSW: 
		    rmmproc = (char *)0;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    if (foldp > NFOLDERS)
		adios (NULLCP, "only %d folders allowed!", NFOLDERS);
	    folders[foldp++].f_name =
		    path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	}
	else
	    msgs[msgp++] = cp;
    }

/*  */

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    if (foldp == 0)
	adios (NULLCP, "no folder specified");

#ifdef	WHATNOW
    if (!msgp && !foldp && !filep && (cp = getenv ("mhdraft")) && *cp)
	files[filep++] = cp;
#endif	/* WHATNOW */

    if (filep > 0) {
	if (folder || msgp)
	    adios (NULLCP, "use -file or some messages, not both");
	opnfolds (folders, foldp);
	for (i = 0; i < filep; i++)
	    if (m_file (files[i], folders, foldp, prsrvf))
		done (1);
	if (!linkf)
	    removeit (NULLMP, filep, filevec);
	done (0);
    }

    if (!msgp)
	msgs[msgp++] = "cur";
    if (!folder)
	folder = m_getfolder ();
    (void) strcpy (maildir, m_maildir (folder));

    if (chdir (maildir) == NOTOK)
	adios (maildir, "unable to change directory to");
    if (!(mp = m_gmsg (folder)))
	adios (NULLCP, "unable to read folder %s", folder);
    if (mp -> hghmsg == 0)
	adios (NULLCP, "no messages in %s", folder);

    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    done (1);
    m_setseq (mp);

    opnfolds (folders, foldp);
    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED) {
	    cp = getcpy (m_name (msgnum));
	    if (m_file (cp, folders, foldp, prsrvf))
		done (1);
	    free (cp);
	    if (!linkf) {
#ifdef	notdef
		mp -> msgstats[msgnum] |= DELETED;
#endif	/* notdef */
		mp -> msgstats[msgnum] &= ~EXISTS;
	    }
	}
    if (!linkf)
	mp -> msgflags |= SEQMOD;
    clsfolds (folders, foldp);

    m_replace (pfolder, folder);
    if (mp -> hghsel != mp -> curmsg
	    && (mp -> numsel != mp -> nummsg || linkf))
	m_setcur (mp, mp -> hghsel);
    m_sync (mp);
    m_update ();

    if (!linkf)
	removeit (mp, filep, filevec);

    done (0);
}

/*  */

static	opnfolds (folders, nfolders)
register struct st_fold *folders;
int	nfolders;
{
    register char  *cp;
    char    nmaildir[BUFSIZ];
    register struct st_fold *fp,
                            *ep;
    register struct msgs   *mp;
    struct stat st;

    for (ep = (fp = folders) + nfolders; fp < ep; fp++) {
	(void) chdir (m_maildir (""));
	(void) strcpy (nmaildir, m_maildir (fp -> f_name));

	if (stat (nmaildir, &st) == NOTOK) {
	    if (errno != ENOENT)
		adios (nmaildir, "error on folder");
	    cp = concat ("Create folder \"", nmaildir, "\"? ", NULLCP);
	    if (!getanswer (cp))
		done (1);
	    free (cp);
	    if (!makedir (nmaildir))
		adios (NULLCP, "unable to create folder %s", nmaildir);
	}

	if (chdir (nmaildir) == NOTOK)
	    adios (nmaildir, "unable to change directory to");
	if (!(mp = m_gmsg (fp -> f_name)))
	    adios (NULLCP, "unable to read folder %s", fp -> f_name);
	mp -> curmsg = 0;

	fp -> f_mp = mp;

	(void) chdir (maildir);
    }
}

/*  */

static	clsfolds (folders, nfolders)
register struct st_fold *folders;
int	nfolders;
{
    register struct st_fold *fp,
                           *ep;
    register struct msgs   *mp;

    for (ep = (fp = folders) + nfolders; fp < ep; fp++) {
	mp = fp -> f_mp;
	m_setseq (mp);
	m_sync (mp);
    }
}

/*  */

static	removeit (mp, filep, files)
register struct msgs *mp;
register int filep;
register char **files;
{
    register int    i,
                    vecp;
    register char  *cp,
                  **vec;

    if (rmmproc) {
	if (filep > 0) {
	    vec = files++;	/* filevec[1] */
	    files[filep] = NULL;
	}
	else {
	    if (mp -> numsel > MAXARGS - 2)
		adios (NULLCP, "more than %d messages for %s exec",
			MAXARGS - 2, rmmproc);
	    vec = (char **) calloc ((unsigned) (mp -> numsel + 2), sizeof *vec);
	    if (vec == NULL)
		adios (NULLCP, "unable to allocate exec vector");
	    vecp = 1;
	    for (i = mp -> lowsel; i <= mp -> hghsel; i++)
		if (mp -> msgstats[i] & SELECTED)
		    vec[vecp++] = getcpy (m_name (i));
	    vec[vecp] = NULL;
	}

	(void) fflush (stdout);
	vec[0] = r1bindex (rmmproc, '/');
	execvp (rmmproc, vec);
	adios (rmmproc, "unable to exec");
    }

    if (filep > 0) {
	files++;	/* filevec[1] */
	for (i = 0; i < filep; i++)
	    if (unlink (files[i]) == NOTOK)
		admonish (files[i], "unable to unlink");
    }
    else
	for (i = mp -> lowsel; i <= mp -> hghsel; i++)
	    if (mp -> msgstats[i] & SELECTED)
		if (unlink (cp = m_name (i)) == NOTOK)
		    admonish (cp, "unable to unlink");
}

/*  */

m_file (msg, folders, nfolders, prsrvf)
register char  *msg;
struct st_fold  *folders;
int	nfolders,
	prsrvf;
{
    int     in,
            out,
            linkerr,
            msgnum;
    register char  *nmsg;
    char    newmsg[BUFSIZ];
    register struct st_fold *fp,
			    *ep;
    register struct msgs *mp;
    struct stat st,
                s1;

    for (ep = (fp = folders) + nfolders; fp < ep; fp++) {
	mp = fp -> f_mp;
	if (prsrvf && (msgnum = m_atoi (nmsg = msg)) > 0) {
	    if (msgnum >= mp -> hghoff)
		if (mp = m_remsg (mp, 0, msgnum + MAXFOLDER))
		    fp -> f_mp = mp;
		else
		    adios (NULLCP, "unable to allocate folder storage");
	    if (!(mp -> msgstats[msgnum] & EXISTS)) {
		mp -> msgstats[msgnum] |= EXISTS;
#ifdef	notdef
		mp -> msgstats[msgnum] &= ~DELETED;
#endif	/* notdef */
		mp -> nummsg++;
	    }
	    mp -> msgstats[msgnum] |= SELECTED;		    
	    if (msgnum > mp -> hghmsg)
		mp -> hghmsg = msgnum;
	}
	else {
	    if (mp -> hghmsg >= mp -> hghoff)
		if (mp = m_remsg (mp, 0, mp -> hghoff + MAXFOLDER))
		    fp -> f_mp = mp;
		else
		    adios (NULLCP, "unable to allocate folder storage");

	    nmsg = m_name (msgnum = ++mp -> hghmsg);
	    mp -> nummsg++;
	    mp -> msgstats[msgnum] |= EXISTS | SELECTED;
	}
	if (mp -> lowmsg == 0)
	    mp -> lowmsg = msgnum;
	if (mp -> lowsel == 0 || msgnum < mp -> lowsel)
	    mp -> lowsel = msgnum;
	if (msgnum > mp -> hghsel)
	    mp -> hghsel = msgnum;

/*  */

	(void) sprintf (newmsg, "%s/%s", mp -> foldpath, nmsg);
	if (link (msg, newmsg) == NOTOK) {
#ifndef	EISREMOTE
	    linkerr = errno;
#else	/* EISREMOTE */
	    if ((linkerr = errno) == EISREMOTE)
		linkerr = EXDEV;
#endif	/* EISREMOTE */
	    if (linkerr == EEXIST
		    || (linkerr == EXDEV && stat (newmsg, &st) != NOTOK)) {
		if (linkerr != EEXIST
			|| stat (msg, &s1) == NOTOK
			|| stat (newmsg, &st) == NOTOK
			|| s1.st_ino != st.st_ino) {
		    advise (NULLCP, "message %s:%s already exists",
			    fp -> f_name, newmsg);
		    return 1;
		}
		continue;
	    }
	    if (linkerr == EXDEV) {
		if ((in = open (msg, 0)) == NOTOK) {
		    advise (msg, "unable to open message %s");
		    return 1;
		}
		(void) fstat (in, &st);
		if ((out = creat (newmsg, (int) st.st_mode & 0777))
			== NOTOK) {
		    advise (newmsg, "unable to create");
		    (void) close (in);
		    return 1;
		}
		cpydata (in, out, msg, newmsg);
		(void) close (in);
		(void) close (out);
	    }
	    else {
		advise (newmsg, "error linking %s to", msg);
		return 1;
	    }
	}
    }

    return 0;
}
