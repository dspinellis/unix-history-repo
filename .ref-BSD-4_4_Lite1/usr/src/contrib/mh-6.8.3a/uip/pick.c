/* pick.c - select messages by content */
#ifndef lint
static char ident[] = "@(#)$Id: pick.c,v 1.4 1992/12/15 00:20:22 jromine Exp $";
#endif	/*  lint */

#include "../h/mh.h"
#include "../zotnet/tws.h"
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	ANDSW	0
    "and", 0,
#define	ORSW	1
    "or", 0,
#define	NOTSW	2
    "not", 0,
#define	LBRSW	3
    "lbrace", 0,
#define	RBRSW	4
    "rbrace", 0,

#define	CCSW	5
    "cc  pattern", 0,
#define	DATESW	6
    "date  pattern", 0,
#define	FROMSW	7
    "from  pattern", 0,
#define	SRCHSW	8
    "search  pattern", 0,
#define	SUBJSW	9
    "subject  pattern", 0,
#define	TOSW	10
    "to  pattern", 0,
#define	OTHRSW	11
    "-othercomponent  pattern", 0,
#define	AFTRSW	12
    "after date", 0,
#define	BEFRSW	13
    "before date", 0,
#define	DATFDSW	14
    "datefield field", 5,

#define	SEQSW	15
    "sequence name", 0,
#define	PUBLSW	16
    "public", 0,
#define	NPUBLSW	17
    "nopublic", 0,
#define	ZEROSW	18
    "zero", 0,
#define	NZEROSW	19
    "nozero", 0,

#define	LISTSW	20
    "list", 0,
#define	NLISTSW	21
    "nolist", 0,

#define	HELPSW	22
    "help", 4,

    NULL, 0
};

/*  */

static int  listsw = 0;

/*  */

/* ARGSUSED */

main (argc, argv)
char   *argv[];
{
    int     publicsw = -1,
            zerosw = 1,
            msgp = 0,
            seqp = 0,
            vecp = 0,
	    lo,
	    hi,
            msgnum;
    char   *maildir,
           *folder = NULL,
            buf[100],
           *cp,
          **ap,
          **argp,
           *arguments[MAXARGS],
           *msgs[MAXARGS],
           *seqs[NATTRS + 1],
           *vec[MAXARGS];
    struct msgs *mp;
    register FILE *fp;

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
	if (*cp == '-') {
	    if (*++cp == '-') {
		vec[vecp++] = --cp;
		goto pattern;
	    }
	    switch (smatch (cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [+folder] [msgs] [switches]",
			    invo_name);
		    help (buf, switches);
		    listsw = 0;	/* HACK */
		    done (1);

		case CCSW: 
		case DATESW: 
		case FROMSW: 
		case SUBJSW: 
		case TOSW: 
		case DATFDSW: 
		case AFTRSW: 
		case BEFRSW: 
		case SRCHSW: 
		    vec[vecp++] = --cp;
	    pattern: ;
		    if (!(cp = *argp++))/* allow -xyz arguments */
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    vec[vecp++] = cp;
		    continue;
		case OTHRSW: 
		    adios (NULLCP, "internal error!");

		case ANDSW:
		case ORSW:
		case NOTSW:
		case LBRSW:
		case RBRSW:
		    vec[vecp++] = --cp;
		    continue;

		case SEQSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if (seqp < NATTRS)
			seqs[seqp++] = cp;
		    else
			adios (NULLCP, "only %d sequences allowed!", NATTRS);
		    listsw = 0;
		    continue;
		case PUBLSW: 
		    publicsw = 1;
		    continue;
		case NPUBLSW: 
		    publicsw = 0;
		    continue;
		case ZEROSW: 
		    zerosw++;
		    continue;
		case NZEROSW: 
		    zerosw = 0;
		    continue;

		case LISTSW: 
		    listsw++;
		    continue;
		case NLISTSW: 
		    listsw = 0;
		    continue;
	    }
	}
	if (*cp == '+' || *cp == '@')
	    if (folder)
		adios (NULLCP, "only one folder at a time!");
	    else
		folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	else
	    msgs[msgp++] = cp;
    }
    vec[vecp] = NULL;

/*  */

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    if (!msgp)
	msgs[msgp++] = "all";
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
    m_setseq (mp);

    if (seqp == 0)
	listsw++;
    if (publicsw == -1)
	publicsw = mp -> msgflags & READONLY ? 0 : 1;
    if (publicsw && (mp -> msgflags & READONLY))
	adios (NULLCP, "folder %s is read-only, so -public not allowed",
		folder);

/*  */

    if (!pcompile (vec, NULLCP))
	done (1);

    lo = mp -> lowsel;
    hi = mp -> hghsel;

    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED) {
	    if ((fp = fopen (cp = m_name (msgnum), "r")) == NULL)
		admonish (cp, "unable to read message");
	    if (fp && pmatches (fp, msgnum, 0L, 0L)) {
		if (msgnum < lo)
		    lo = msgnum;
		if (msgnum > hi)
		    hi = msgnum;
	    }
	    else {
		mp -> msgstats[msgnum] &= ~SELECTED;
		mp -> numsel--;
	    }
	    if (fp)
		(void) fclose (fp);
	}

    mp -> lowsel = lo;
    mp -> hghsel = hi;

    if (mp -> numsel <= 0)
	adios (NULLCP, "no messages match specification");

/*  */

    seqs[seqp] = NULL;
    for (seqp = 0; seqs[seqp]; seqp++) {
	if (zerosw && !m_seqnew (mp, seqs[seqp], publicsw))
	    done (1);
	for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED)
		if (!m_seqadd (mp, seqs[seqp], msgnum, publicsw))
		    done (1);
    }

    if (listsw) {
	for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED)
		printf ("%s\n", m_name (msgnum));
    }
    else
	printf ("%d hit%s\n", mp -> numsel,
		mp -> numsel == 1 ? "" : "s");

    m_replace (pfolder, folder);
    m_sync (mp);
    m_update ();

    done (0);
}

/*  */

void done (status)
int	status;
{
    if (listsw && status && !isatty (fileno (stdout)))
	printf ("0\n");
    exit (status);
}
