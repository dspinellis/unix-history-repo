/* folder(s).c - report on folders */
#ifndef	lint
static char ident[] = "@(#)$Id: folder.c,v 2.11 1993/08/27 23:23:06 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/local.h"
#include <errno.h>
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif

static		dodir(), addir(), addfold(), dother();
static int	pfold(), sfold(), compare();
/*  */

static struct swit switches[] = {
#define	ALLSW	0
    "all", 0,

#define	CREATSW	1
    "create", 0,
#define	NCREATSW 2
    "nocreate", 0,

#define	FASTSW	3
    "fast", 0,
#define	NFASTSW	4
    "nofast", 0,

#define	HDRSW	5
    "header", 0,
#define	NHDRSW	6
    "noheader", 0,

#define	PACKSW	7
    "pack", 0,
#define	NPACKSW	8
    "nopack", 0,
#define	VERBSW	9
    "verbose", 0,
#define	NVERBSW	10
    "noverbose", 0,

#define	RECURSW	11
    "recurse", 0,
#define	NRECRSW	12
    "norecurse", 0,

#define	TOTALSW	13
    "total", 0,
#define	NTOTLSW	14
    "nototal", 0,

#define	PRNTSW	15
    "print", 0,
#define	NPRNTSW	16
    "noprint", -4,
#define	LISTSW	17
    "list", 0,
#define	NLISTSW	18
    "nolist", 0,
#define	PUSHSW	19
    "push", 0,
#define	POPSW	20
    "pop", 0,

#define	HELPSW	21
    "help", 4,

    NULL, 0
};

/*  */

extern int errno;

static int  fshort = 0;
static int  fcreat = 0;
static int  fpack = 0;
static int  fverb = 0;
static int  fheader = 0;
static int  frecurse = 0;
static int  ftotonly = 0;
static int  msgtot = 0;
static int  foldtot = 0;
static int  start = 0;
static int  foldp = 0;

static char *mhdir;
static char *stack = "Folder-Stack";
static char folder[BUFSIZ];
static char *folds[NFOLDERS + 1];

struct msgs *tfold ();

/*  */

/* ARGSUSED */

main (argc, argv)
char   *argv[];
{
    int     all = 0,
            printsw = 0,
            listsw = 0,
            pushsw = 0,
            popsw = 0;
    char   *cp,
           *dp,
           *msg = NULL,
           *argfolder = NULL,
          **ap,
          **argp,
            buf[100],
           *arguments[MAXARGS];
    struct stat st;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    if (argv[0][strlen (argv[0]) - 1] == 's')
	all++;
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
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [+folder] [msg] [switches]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case ALLSW: 
		    all++;
		    continue;

		case CREATSW: 
		    fcreat = 1;
		    continue;
		case NCREATSW: 
		    fcreat = -1;
		    continue;

		case FASTSW: 
		    fshort++;
		    continue;
		case NFASTSW: 
		    fshort = 0;
		    continue;

		case HDRSW: 
		    fheader = -1;
		    continue;
		case NHDRSW: 
		    fheader++;
		    continue;

		case PACKSW: 
		    fpack++;
		    continue;
		case NPACKSW: 
		    fpack = 0;
		    continue;

		case VERBSW:
		    fverb++;
		    continue;
		case NVERBSW:
		    fverb = 0;
		    continue;

		case RECURSW: 
		    frecurse++;
		    continue;
		case NRECRSW: 
		    frecurse = 0;
		    continue;

		case TOTALSW: 
		    all++;
		    ftotonly++;
		    continue;
		case NTOTLSW: 
		    if (ftotonly)
			all = 0;
		    ftotonly = 0;
		    continue;

		case PRNTSW: 
		    printsw++;
		    continue;
		case NPRNTSW: 
		    printsw = 0;
		    continue;

		case LISTSW: 
		    listsw++;
		    continue;
		case NLISTSW: 
		    listsw = 0;
		    continue;

		case PUSHSW: 
		    pushsw++;
		    listsw++;
		    popsw = 0;
		    continue;
		case POPSW: 
		    popsw++;
		    listsw++;
		    pushsw = 0;
		    continue;
	    }
	if (*cp == '+' || *cp == '@')
	    if (argfolder)
		adios (NULLCP, "only one folder at a time!");
	    else
		argfolder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	else
	    if (msg)
		adios (NULLCP, "only one (current) message at a time!");
	    else
		msg = cp;
    }

/*  */

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    mhdir = concat (m_maildir (""), "/", NULLCP);

    if (pushsw == 0 && popsw == 0 && listsw == 0)
	printsw++;
    if (pushsw) {
	if (!argfolder) {
	    if ((cp = m_find (stack)) == NULL
		    || (ap = brkstring (dp = getcpy (cp), " ", "\n")) == NULL
		    || (argfolder = *ap++) == NULL)
		adios (NULLCP, "no other folder");
	    for (cp = getcpy (m_getfolder ()); *ap; ap++)
		cp = add (*ap, add (" ", cp));
	    free (dp);
	    m_replace (stack, cp);
	}
	else
	    m_replace (stack,
		    (cp = m_find (stack))
		    ? concat (m_getfolder (), " ", cp, NULLCP)
		    : getcpy (m_getfolder ()));
    }
    if (popsw) {
	if (argfolder)
	    adios (NULLCP, "sorry, no folders allowed with -pop");
	if ((cp = m_find (stack)) == NULL
		|| (ap = brkstring (dp = getcpy (cp), " ", "\n")) == NULL
		|| (argfolder = *ap++) == NULL)
	    adios (NULLCP, "folder stack empty");
	for (cp = NULL; *ap; ap++)
	    cp = cp ? add (*ap, add (" ", cp)) : getcpy (*ap);
	free (dp);
	if (cp)
	    m_replace (stack, cp);
	else
	    (void) m_delete (stack);
    }
    if (pushsw || popsw) {
	if (access (cp = m_maildir (argfolder), 0) == NOTOK)
	    adios (cp, "unable to find folder");
	m_replace (pfolder, argfolder);
	m_update ();
	argfolder = NULL;
    }
    if (listsw) {
	printf ("%s", argfolder ? argfolder : m_getfolder ());
	if (cp = m_find (stack)) {
	    for (ap = brkstring (dp = getcpy (cp), " ", "\n"); *ap; ap++)
		printf (" %s", *ap);
	    free (dp);
	}
	printf ("\n");

	if (!printsw)
	    done (0);
    }

/*  */

    if (all) {
	fheader = 0;
	if (argfolder) {
	    (void) strcpy (folder, argfolder);
	    if (pfold (argfolder, msg)) {
		m_replace (pfolder, argfolder);
		m_update ();
	    }
	    if (!frecurse)	/* recurse not done in pfold(), */
		dodir (folder);	/* so just list all level-1 sub-folders */
	}
	else {
	    if (msg)
		admonish (NULLCP, "no folder given for message %s", msg);
	    dother ();

	    (void) strcpy (folder, (cp = m_find (pfolder)) ? cp : "");
	    dodir (".");
	}

	if (!fshort) {
	    if (!ftotonly)
		printf ("\n\t\t     ");
	    printf ("TOTAL= %*d message%c in %d folder%s.\n",
		    DMAXFOLDER, msgtot, msgtot != 1 ? 's' : ' ',
		    foldtot, foldtot != 1 ? "s" : "");
	}
    }
    else {
	fheader++;

	(void) strcpy (folder, argfolder ? argfolder : m_getfolder ());
	if (stat (strcpy (buf, m_maildir (folder)), &st) == NOTOK) {
	    if (errno != ENOENT)
		adios (buf, "error on folder");
	    switch (fcreat) {
		case 0:			/* ask before create */
		    cp = concat ("Create folder \"", buf, "\"? ", NULLCP);
		    if (!getanswer (cp))
			done (1);
		    free (cp);
		    break;
		case -1:		/* do not create */
		    done (1);
		    break;
	    }
	    if (!makedir (buf))
		adios (NULLCP, "unable to create folder %s", buf);
	}

	if (pfold (folder, msg) && argfolder)
	    m_replace (pfolder, argfolder);
    }

    m_update ();

    done (0);
}

/*  */

static	dodir (dir)
register char   *dir;
{
    int     i;
    int     os = start;
    int     of = foldp;
    char    buffer[BUFSIZ];

    start = foldp;
    if (chdir (mhdir) == NOTOK)
	adios (mhdir, "unable to change directory to");

    addir (strcpy (buffer, dir));
    for (i = start; i < foldp; i++)
	(void) pfold (folds[i], NULLCP), (void) fflush (stdout);

    start = os;
    foldp = of;
}

/*  */

static int  pfold (fold, msg)
register char   *fold,
		*msg;
{
    int	    hack,
	    others,
            retval = 1;
    register char *mailfile;
    register struct msgs   *mp = NULL;

    mailfile = m_maildir (fold);
    if (chdir (mailfile) == NOTOK) {
	if (errno != EACCES)
	    admonish (mailfile, "unable to change directory to");
	else
	    printf ("%22s%c unreadable\n",
		    fold, strcmp (folder, fold) ? ' ' : '+');
	return 0;
    }

    if (fshort) {
	printf ("%s\n", fold);

	if (!msg && !fpack) {
	    if (frecurse)
		dodir (fold);
	    return retval;
	}
    }

    if (!(mp = m_gmsg (fold))) {
	admonish (NULLCP, "unable to read folder %s", fold);
	return 0;
    }

    if (msg && !sfold (mp, msg))
	retval = 0;
    if (fpack)
	mp = tfold (mp);

    if (fshort)
	goto out;
    foldtot++;
    msgtot += mp -> nummsg;

    if (ftotonly)
	goto out;

    if (!fheader++)
	printf ("\t\tFolder  %*s# of messages (%*srange%*s); cur%*smsg  (other files)\n",
	    DMAXFOLDER, "", DMAXFOLDER - 2, "", DMAXFOLDER - 2, "",
	    DMAXFOLDER - 2, "");

    printf ("%22s%c ", fold, strcmp (folder, fold) ? ' ' : '+');

    hack = 0;
    if (mp -> hghmsg == 0)
	printf ("has   no messages%*s",
		mp -> msgflags & OTHERS ? DMAXFOLDER * 2 + 4 : 0, "");
    else {
	printf ("has %*d message%s (%*d-%*d)",
		DMAXFOLDER, mp -> nummsg, (mp -> nummsg == 1) ? " " : "s",
		DMAXFOLDER, mp -> lowmsg, DMAXFOLDER, mp -> hghmsg);
	if (mp -> curmsg >= mp -> lowmsg && mp -> curmsg <= mp -> hghmsg)
	    printf ("; cur=%*d", DMAXFOLDER, hack = mp -> curmsg);
    }

    if (mp -> msgflags & OTHERS)
	printf (";%*s (others)", hack ? 0 : DMAXFOLDER + 6, "");
    printf (".\n");

out: ;
    others = mp -> msgflags & OTHERS;
    m_fmsg (mp);

    if (frecurse && others)
	dodir (fold);

    return retval;
}

/*  */

static int  sfold (mp, msg)
register struct msgs   *mp;
char   *msg;
{
    if (!m_convert (mp, msg))
	return 0;

    if (mp -> numsel > 1) {
	admonish (NULLCP, "only one message at a time!");
	return 0;
    }
    m_setseq (mp);
    m_setcur (mp, mp -> lowsel);
    m_sync (mp);
    m_update ();

    return 1;
}


struct msgs *tfold (mp)
register struct msgs   *mp;
{
    register int    hole,
                    msgnum;
    char    newmsg[BUFSIZ],
            oldmsg[BUFSIZ];

    if (mp -> lowmsg > 1 && (mp = m_remsg (mp, 1, mp -> hghmsg)) == NULL)
	adios (NULLCP, "unable to allocate folder storage");

    for (msgnum = mp -> lowmsg, hole = 1; msgnum <= mp -> hghmsg; msgnum++)
	if (mp -> msgstats[msgnum] & EXISTS) {
	    if (msgnum != hole) {
		(void) strcpy (newmsg, m_name (hole));
		(void) strcpy (oldmsg, m_name (msgnum));
		if (fverb)
		    printf ("message %s becomes %s\n", oldmsg, newmsg);
		if (rename (oldmsg, newmsg) == NOTOK)
		    adios (newmsg, "unable to rename %s to", oldmsg);
		if (msgnum == mp -> curmsg)
		    m_setcur (mp, mp -> curmsg = hole);
		mp -> msgstats[hole] = mp -> msgstats[msgnum];
		mp -> msgflags |= SEQMOD;
		if (msgnum == mp -> lowsel)
		    mp -> lowsel = hole;
		if (msgnum == mp -> hghsel)
		    mp -> hghsel = hole;
	    }
	    hole++;
	}
    if (mp -> nummsg > 0) {
	mp -> lowmsg = 1;
	mp -> hghmsg = hole - 1;
    }
    m_sync (mp);
    m_update ();

    return mp;
}

/*  */

static	addir (name)
register char   *name;
{
    register char  *base,
                   *cp;
    struct stat st;
#ifdef SYS5DIR
    register struct dirent *dp;
#else /* SYS5DIR */
    register struct direct *dp;
#endif /* SYS5DIR */
    register    DIR * dd;

    cp = name + strlen (name);
    *cp++ = '/';
    *cp = '\0';

    base = strcmp (name, "./") ? name : name + 2;/* hack */

    if ((dd = opendir (name)) == NULL) {
	admonish (name, "unable to read directory ");
	return;
    }
    while (dp = readdir (dd))
	if (strcmp (dp -> d_name, ".") && strcmp (dp -> d_name, "..")) {
#ifdef SYS5DIR
	    if (cp + dp -> d_reclen + 2 >= name + BUFSIZ)
#else /* SYS5DIR */
	    if (cp + strlen (dp -> d_name) + 2 >= name + BUFSIZ)
#endif /* SYS5DIR */
		continue;
	    (void) strcpy (cp, dp -> d_name);
	    if (stat (name, &st) != NOTOK && (st.st_mode & S_IFMT) == S_IFDIR)
		addfold (base);
	}
    closedir (dd);

    *--cp = '\0';
}

/*  */

static	addfold (fold)
register char   *fold;
{
    register int    i,
                    j;
    register char  *cp;

    if (foldp > NFOLDERS)
	adios (NULLCP, "more than %d folders to report on", NFOLDERS);

    cp = getcpy (fold);
    for (i = start; i < foldp; i++)
	if (compare (cp, folds[i]) < 0) {
	    for (j = foldp - 1; j >= i; j--)
		folds[j + 1] = folds[j];
	    foldp++;
	    folds[i] = cp;
	    return;
	}

    folds[foldp++] = cp;
}

/*  */

static int  compare (s1, s2)
register char   *s1,
		*s2;
{
    register int    i;

    while (*s1 || *s2)
	if (i = *s1++ - *s2++)
	    return i;

    return 0;
}

/*  */

static	dother () {
    int	    atrlen;
    char    atrcur[BUFSIZ];
    register struct node   *np;

    (void) sprintf (atrcur, "atr-%s-", current);
    atrlen = strlen (atrcur);

    m_getdefs ();
    for (np = m_defs; np; np = np -> n_next)
	if (ssequal (atrcur, np -> n_name)
		&& !ssequal (mhdir, np -> n_name + atrlen))
	    (void) pfold (np -> n_name + atrlen, NULLCP);
}
