/* sortm.c - sort messages in a folder by date/time */
/* 21Apr90 do subject sorts too - from V. Jacobson */
#ifndef	lint
static char ident[] = "@(#)$Id: sortm.c,v 1.19 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../zotnet/tws.h"
#define getws _getws
#include <stdio.h>
#undef getws
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#ifdef LOCALE
#include	<locale.h>
#endif
#if	defined(SYS5) && defined(AUX)
#define u_short ushort
#define u_long  ulong
#endif


static struct swit switches[] = {
#define DATESW  0
     "datefield field", 0,

#define	TEXTSW 1
     "textfield field", 0,
#define	NSUBJSW	2
     "notextfield", 0,

#define SUBJSW 3
     "subject", -3,		/* backward-compatibility */
#define LIMSW 4
     "limit days", 0,
#define	NLIMSW 5
     "nolimit", 0,

#define VERBSW  6
     "verbose", 0,
#define NVERBSW 7
     "noverbose", 0,

#define HELPSW  8
     "help", 4,

     NULL, 0
};

struct smsg {
    int s_msg;
    unsigned long s_clock;
    char *s_subj;
};

static struct smsg *smsgs;
int 	nmsgs;

char   *subjsort = (char *)0;           /* sort on subject if != 0 */
u_long	datelimit = 0;
int	submajor = 0;		/* if true, sort on subject-major */
int 	verbose;

#ifdef __STDC__
static int getws (char *datesw, int msg, struct smsg *smsg);
#else
static int getws ();
#endif

static	int dsort(), read_hdrs (), subsort(), txtsort();
static void rename_chain(), rename_msgs();

/*  */

main (argc, argv)
int argc;
char **argv;
{
    int	    msgp = 0,
	    i,
	    msgnum;
    char   *cp,
	   *maildir,
	   *datesw = NULL,
	   *folder = NULL,
	    buf[100],
	  **ap,
	  **argp,
	   *arguments[MAXARGS],
	   *msgs[MAXARGS];
    struct msgs *mp;
    struct smsg **dlist;

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

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
	    case AMBIGSW:
		ambigsw (cp, switches);
		done (1);
	    case UNKWNSW:
		adios (NULLCP, "-%s unknown", cp);
	    case HELPSW:
		(void) sprintf(buf, "%s [+folder] [msgs] [switches]",
			  invo_name);
		help (buf, switches);
		done (1);

	    case DATESW:
		if (datesw)
		    adios (NULLCP, "only one date field at a time");
		if (!(datesw = *argp++) || *datesw == '-')
		    adios (NULLCP, "missing argument to %s", argp[-2]);
		continue;

	    case TEXTSW:
		if (subjsort)
		    adios (NULLCP, "only one text field at a time");
		if (!(subjsort = *argp++) || *subjsort == '-')
		    adios (NULLCP, "missing argument to %s", argp[-2]);
		continue;

	    case SUBJSW:
		subjsort = "subject";
		continue;
	    case NSUBJSW:
		subjsort = (char *)0;
		continue;

	    case LIMSW:
		if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		while (*cp == '0')
		    cp++;		/* skip any leading zeros */
		if (!*cp) {		/* hit end of string */
		    submajor++;		/* sort subject-major */
		    continue;
		}
		if (!isdigit(*cp) || !(datelimit = atoi(cp)))
		    adios (NULLCP, "impossible limit %s", cp);
		datelimit *= 60*60*24;
		continue;
	    case NLIMSW:
		submajor = 0;	/* use date-major, but */
		datelimit = 0;	/* use no limit */
		continue;

	    case VERBSW:
		verbose++;
		continue;
	    case NVERBSW:
		verbose = 0;
		continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    if (folder)
		adios (NULLCP, "only one folder at a time!");
	    else
		folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	}
	else
	    msgs[msgp++] = cp;
    }

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    if (!msgp)
	msgs[msgp++] = "all";
    if (!datesw)
	datesw = "date";
    if (!folder)
	folder = m_getfolder ();
    maildir = m_maildir (folder);

    if (chdir (maildir) == NOTOK)
	adios (maildir, "unable to change directory to");
    if (!(mp = m_gmsg (folder)))
	adios (NULLCP, "unable to read folder %s", folder);
    if (mp->hghmsg == 0)
	adios (NULLCP, "no messages in %s", folder);

    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    done (1);
    m_setseq (mp);

    if ((nmsgs = read_hdrs (mp, datesw)) <= 0)
	adios (NULLCP, "no messages to sort");

    /*
     * sort a list of pointers to our "messages to be sorted".
     */
    dlist = (struct smsg **) malloc ((nmsgs+1) * sizeof(*dlist));
    if (! dlist)
	adios (NULLCP, "couldn't allocate sort memory");
    for (i = 0; i < nmsgs; i++)
	dlist[i] = &smsgs[i];
    dlist[nmsgs] = 0;

    if (verbose) 	/* announce what we're doing */
	if (subjsort)
	    printf ("sorting by %s-major %s-minor\n", 
		submajor ? subjsort : datesw,
		submajor ? datesw : subjsort);
	else
	    printf ("sorting by datefield %s\n", datesw);

    /* first sort by date, or by subject-major, date-minor */
    qsort ((char *) dlist, nmsgs, sizeof(*dlist), 
	    submajor && subjsort ? txtsort : dsort);

    /*
     * if we're sorting on subject, we need another list
     * in subject order, then a merge pass to collate the
     * two sorts.
     */
    if (!submajor && subjsort) {	/* already date sorted */
	struct smsg 		**slist,
		    		**flist;
	register struct smsg   ***il,
				**fp,
				**dp;

	slist = (struct smsg **) malloc ((nmsgs+1) * sizeof(*slist));
	if (! slist)
	    adios (NULLCP, "couldn't allocate sort memory");
	bcopy ((char *)dlist, (char *)slist, (nmsgs+1)*sizeof(*slist));
	qsort ((char *)slist, nmsgs, sizeof(*slist), subsort);

	/*
	 * make an inversion list so we can quickly find
	 * the collection of messages with the same subj
	 * given a message number.
	 */
	il = (struct smsg ***) calloc (mp->hghsel+1, sizeof(*il));
	if (! il)
	    adios (NULLCP, "couldn't allocate msg list");
	for (i = 0; i < nmsgs; i++)
	    il[slist[i]->s_msg] = &slist[i];
	/*
	 * make up the final list, chronological but with
	 * all the same subjects grouped together.
	 */
	flist = (struct smsg **) malloc ((nmsgs+1) * sizeof(*flist));
	if (! flist)
	    adios (NULLCP, "couldn't allocate msg list");
	fp = flist;
	for (dp = dlist; *dp;) {
	    register struct smsg **s = il[(*dp++)->s_msg];

	    /* see if we already did this guy */
	    if (! s)
		continue;

	    *fp++ = *s++;
	    /*
	     * take the next message(s) if there is one,
	     * its subject isn't null and its subject
	     * is the same as this one and it's not too
	     * far away in time.
	     */
	    while (*s && (*s)->s_subj[0] &&
		   strcmp((*s)->s_subj, s[-1]->s_subj) == 0 &&
		   (datelimit == 0 || 
		   (*s)->s_clock - s[-1]->s_clock <= datelimit)) {
		il[(*s)->s_msg] = 0;
		*fp++ = *s++;
	    }
	}
	*fp = 0;
	(void) free (slist);
	(void) free (dlist);
	dlist = flist;
    }
    rename_msgs (mp, dlist);

    m_replace (pfolder, folder);
    m_sync (mp);
    m_update ();
    done (0);
}

static int 
read_hdrs (mp, datesw)
register struct msgs	*mp;
register char 		*datesw;
{
    int 	msgnum;
    struct tws 	tb;
    register struct smsg *s;

    twscopy (&tb, dtwstime ());

    smsgs = (struct smsg *)
	calloc ((unsigned) (mp->hghsel - mp->lowsel + 2),
	    sizeof *smsgs);
    if (smsgs == NULL)
	adios (NULLCP, "unable to allocate sort storage");

    s = smsgs;
    for (msgnum = mp->lowsel; msgnum <= mp->hghsel; msgnum++) {
	if (mp->msgstats[msgnum] & SELECTED) {
	    if (getws (datesw, msgnum, s)) {
		s->s_msg = msgnum;
		s++;
	    }
	}
    }
    s->s_msg = 0;
    return(s - smsgs);
}

static
getws (datesw, msg, smsg)
register char *datesw;
int msg;
register struct smsg *smsg;
{
    register int state;
    int      compnum;
    char    *msgnam,
	     buf[BUFSIZ],
	     nam[NAMESZ];
    register struct tws *tw;
    register char *datecomp = NULLCP,
		  *subjcomp = NULLCP;
    register FILE *in;

    if ((in = fopen (msgnam = m_name (msg), "r")) == NULL) {
	admonish (msgnam, "unable to read message");
	return (0);
    }
    for (compnum = 1, state = FLD;;) {
	switch (state = m_getfld (state, nam, buf, sizeof buf, in)) {
	case FLD:
	case FLDEOF:
	case FLDPLUS:
	    compnum++;
	    if (uleq (nam, datesw)) {
		datecomp = add (buf, datecomp);
		while (state == FLDPLUS) {
		    state = m_getfld (state, nam, buf, sizeof buf, in);
		    datecomp = add (buf, datecomp);
		}
		if (!subjsort || subjcomp)
		    break;
	    }
	    else if (subjsort && uleq (nam, subjsort)) {
		subjcomp = add (buf, subjcomp);
		while (state == FLDPLUS) {
		    state = m_getfld (state, nam, buf, sizeof buf, in);
		    subjcomp = add (buf, subjcomp);
		}
		if (datecomp)
		    break;
	    }
	    else {
		/* just flush this guy */
		while (state == FLDPLUS)
		    state = m_getfld (state, nam, buf, sizeof buf, in);
	    }
	    continue;

	case BODY:
	case BODYEOF:
	case FILEEOF:
	    break;

	case LENERR:
	case FMTERR:
	    if (state == LENERR || state == FMTERR)
		admonish (NULLCP, "format error in message %d (header #%d)",
		      msg, compnum);
	    if (datecomp)
		free (datecomp);
	    if (subjcomp)
		free (subjcomp);
	    (void) fclose (in);
	    return (0);

	default:
	    adios (NULLCP, "internal error -- you lose");
	}
	break;
    }

    if (!datecomp || (tw = dparsetime (datecomp)) == NULL) {
	struct stat st;

	admonish (NULLCP, "can't parse %s field in message %d",
	      datesw, msg);

	/* use the modify time of the file as its date */
	(void) fstat (fileno (in), &st);
	smsg->s_clock = st.st_mtime;
    }
    else
	smsg->s_clock = twclock (tw);

    if (subjsort) {
	if (subjcomp) {
	    /*
	     * try to make the subject "canonical": delete
	     * leading "re:", everything but letters & smash
	     * letters to lower case. 
	     */
	    register char  *cp,
			   *cp2,
			    c;

	    cp = subjcomp;
	    cp2 = subjcomp;
	    if (strcmp (subjsort, "subject") == 0)
		while (c = *cp) {
		    if (! isspace(c)) {
			if(uprf(cp, "re:"))
			    cp += 2;
			else {
			    if (isalnum(c))
				*cp2++ = isupper(c) ? tolower(c) : c;
			    break;
			}
		    }
		    cp++;
		}
	    while (c = *cp++) {
		if (isalnum(c))
		    *cp2++ = isupper(c) ? tolower(c) : c;

	    }
	    *cp2 = '\0';
	}
	else
	    subjcomp = "";

	smsg->s_subj = subjcomp;
    }
    (void) fclose (in);
    if (datecomp)
	free (datecomp);

    return (1);
}

/*
 * sort on dates.
 */
static int 
dsort (a, b)
register struct smsg **a,
		     **b;
{
    if ((*a)->s_clock < (*b)->s_clock)
	return (-1);
    else if ((*a)->s_clock > (*b)->s_clock)
	return (1);
    else if ((*a)->s_msg < (*b)->s_msg)
	return (-1);
    else
	return (1);
}

/*
 * sort on subjects.
 */
static int 
subsort (a, b)
register struct smsg **a,
		     **b;
{
    register int i;

    if (i = strcmp ((*a)->s_subj, (*b)->s_subj))
	return (i);

    return (dsort (a, b));
}

static int 
txtsort (a, b)
register struct smsg **a,
		     **b;
{
    register int i;

    if (i = strcmp ((*a)->s_subj, (*b)->s_subj))
	return (i);
    else if ((*a)->s_msg < (*b)->s_msg)
	return (-1);
    else
	return (1);
}

static void   rename_chain (mp, mlist, msg, endmsg)
register struct msgs *mp;
struct smsg 	    **mlist;
int 	  msg,
	  endmsg;
{
    int   nxt,
	  old,
	  new;
    char *newname,
	  oldname[BUFSIZ];

    for (;;) {
	nxt = mlist[msg] - smsgs;	/* mlist[msg] is a ptr into smsgs */
	mlist[msg] = (struct smsg *)0;
	old = smsgs[nxt].s_msg;
	new = smsgs[msg].s_msg;
	(void) strcpy (oldname, m_name (old));
	newname = m_name (new);
	if (verbose)
	    printf ("message %d becomes message %d\n", old, new);

	if (rename (oldname, newname) == NOTOK)
	    adios (newname, "unable to rename %s to", oldname);

	mp->msgstats[new] = mp->msgstats[old];
	if (mp->curmsg == old)
	    m_setcur (mp, new);

	if (nxt == endmsg) 
	    break;

	msg = nxt;
    }
/*	if (nxt != endmsg); */
/*	rename_chain (mp, mlist, nxt, endmsg); */
}

static void
rename_msgs (mp, mlist)
register struct msgs *mp;
register struct smsg **mlist;
{
    register int i,
		 j,
		 old,
		 new;
    short        stats;
    char         f1[BUFSIZ],
	         tmpfil[BUFSIZ];
    register struct smsg *sp;

    (void) strcpy (tmpfil, m_name (mp->hghmsg + 1));

    for (i = 0; i < nmsgs; i++) {
	if (! (sp = mlist[i])) 
	    continue;   /* did this one */

	j = sp - smsgs;
	if (j == i)
	    continue;   /* this one doesn't move */

	/*
	 * the guy that was msg j is about to become msg i.
	 * rename 'j' to make a hole, then recursively rename
	 * guys to fill up the hole.
	 */
	old = smsgs[j].s_msg;
	new = smsgs[i].s_msg;
	(void) strcpy (f1, m_name (old));

	if (verbose)
	    printf ("renaming message chain from %d to %d\n", old, new);

	if (rename (f1, tmpfil) == NOTOK)
	    adios (tmpfil, "unable to rename %s to ", f1);
	stats = mp->msgstats[old];

	rename_chain (mp, mlist, j, i);
	if (rename (tmpfil, m_name(new)) == NOTOK)
	    adios (m_name(new), "unable to rename %s to", tmpfil);

	mp->msgstats[new] = stats;
	mp->msgflags |= SEQMOD;
    }
}
