/* burst.c - explode digests into individual messages */
#ifndef	lint
static char ident[] = "@(#)$Id: burst.c,v 1.7 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef LOCALE
#include	<locale.h>
#endif

static	cpybrst(), burst();
/*  */

static struct swit switches[] = {
#define	INPLSW	0
    "inplace", 0,
#define	NINPLSW	1
    "noinplace", 0,

#define	QIETSW	2
    "quiet", 0,
#define	NQIETSW	3
    "noquiet", 0,

#define	VERBSW	4
    "verbose", 0,
#define	NVERBSW	5
    "noverbose", 0,

#define	HELPSW	6
    "help", 4,

    NULL, 0
};

/*  */

static char delim3[] = "-------";


static struct msgs *mp;

struct smsg {
    long    s_start;
    long    s_stop;
};

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    int     inplace = 0,
            quietsw = 0,
            verbosw = 0,
            msgp = 0,
            hi,
            msgnum;
    char   *cp,
           *maildir,
           *folder = NULL,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *msgs[MAXARGS];
    struct smsg *smsgs;

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
		    (void) sprintf (buf, "%s [+folder] [msgs] [switches]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case INPLSW: 
		    inplace++;
		    continue;
		case NINPLSW: 
		    inplace = 0;
		    continue;

		case QIETSW: 
		    quietsw++;
		    continue;
		case NQIETSW: 
		    quietsw = 0;
		    continue;

		case VERBSW: 
		    verbosw++;
		    continue;
		case NVERBSW: 
		    verbosw = 0;
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

/*  */

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    if (!msgp)
	msgs[msgp++] = "cur";
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

    smsgs = (struct smsg   *)
		calloc ((unsigned) (MAXFOLDER + 2), sizeof *smsgs);
    if (smsgs == NULL)
	adios (NULLCP, "unable to allocate burst storage");

    hi = mp -> hghmsg + 1;
    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED)
	    burst (smsgs, msgnum, inplace, quietsw, verbosw);

    free ((char *) smsgs);

    m_replace (pfolder, folder);
    if (inplace) {
	if (mp -> lowsel != mp -> curmsg)
	    m_setcur (mp, mp -> lowsel);
    }
    else
	if (hi <= mp -> hghmsg)
	    m_setcur (mp, hi);
    m_sync (mp);
    m_update ();

    done (0);
}

/*  */

static  burst (smsgs, msgnum, inplace, quietsw, verbosw)
register struct smsg *smsgs;
int     msgnum,
        inplace,
        quietsw,
        verbosw;
{
    int     i,
            j,
            ld3,
	    wasdlm,
            mode,
            msgp;
    register long   pos;
    register char   c,
		    cc,
                   *msgnam;
    char    buffer[BUFSIZ],
            f1[BUFSIZ],
            f2[BUFSIZ],
            f3[BUFSIZ];
    struct stat st;
    register    FILE *in,
		     *out;

    ld3 = strlen (delim3);

    if ((in = fopen (msgnam = m_name (msgnum), "r")) == NULL)
	adios (msgnam, "unable to read message");

    mode = fstat (fileno (in), &st) != NOTOK ? (st.st_mode & 0777)
	: m_gmprot ();
    for (msgp = 0, pos = 0L; msgp <= MAXFOLDER;) {
	while (fgets (buffer, sizeof buffer, in) != NULL
		&& buffer[0] == '\n')
	    pos += (long) strlen (buffer);
	if (feof (in))
	    break;
	(void) fseek (in, pos, 0);
	smsgs[msgp].s_start = pos;

	for (c = 0;
		fgets (buffer, sizeof buffer, in) != NULL;
		c = buffer[0])
	    if (strncmp (buffer, delim3, ld3) == 0
		    && (msgp == 1 || c == '\n')
		    && ((cc = peekc (in)) == '\n' || cc == EOF))
		break;
	    else
		pos += (long) strlen (buffer);

	wasdlm = strncmp (buffer, delim3, ld3) == 0;
	if (smsgs[msgp].s_start != pos)
	    smsgs[msgp++].s_stop = (c == '\n' && wasdlm) ? pos - 1 : pos;
	if (feof (in)) {
#ifdef	notdef
	    if (wasdlm) {
		smsgs[msgp - 1].s_stop -= ((long) strlen (buffer) + 1);
		msgp++;		/* fake "End of XXX Digest" */
	    }
#endif
	    break;
	}
	pos += (long) strlen (buffer);
    }

/*  */

    switch (msgp--) {		/* toss "End of XXX Digest" */
	case 0: 
	    adios (NULLCP, "burst() botch -- you lose big");

	case 1: 
	    if (!quietsw)
		admonish (NULLCP, "message %d not in digest format", msgnum);
	    (void) fclose (in);
	    return;

	default: 
	    if (verbosw)
		printf ("%d message%s exploded from digest %d\n",
			msgp, msgp != 1 ? "s" : "", msgnum);
	    break;
    }
    /* msgp now contains the number of new msgs to be created */

    if ((mp = m_remsg (mp, 0, mp -> hghmsg + msgp)) == NULL)
	adios (NULLCP, "unable to allocate folder storage");

/*  */

    j = mp -> hghmsg;		/* old value */
    mp -> hghmsg += msgp;
    mp -> nummsg += msgp;
    if (mp -> hghsel > msgnum)
	mp -> hghsel += msgp;

    if (inplace)
	for (i = mp -> hghmsg; j > msgnum; i--, j--) {
	    (void) strcpy (f1, m_name (i));
	    (void) strcpy (f2, m_name (j));
	    if (mp -> msgstats[j] & EXISTS) {
		if (verbosw)
		    printf ("message %d becomes message %d\n", j, i);

		if (rename (f2, f1) == NOTOK)
		    admonish (f1, "unable to rename %s to", f2);
		mp -> msgstats[i] = mp -> msgstats[j];
		mp -> msgstats[j] = 0;
		mp -> msgflags |= SEQMOD;
	    }
	}
    
    mp -> msgstats[msgnum] &= ~SELECTED;
    i = inplace ? msgnum + msgp : mp -> hghmsg; /* new hghmsg is hghmsg+msgp */
    for (j = msgp; j >= (inplace ? 0 : 1); i--, j--) {
	(void) strcpy (f1, m_name (i));
	(void) strcpy (f2, m_scratch ("", invo_name));
	if (verbosw && i != msgnum)
	    printf ("message %d of digest %d becomes message %d\n",
		    j, msgnum, i);

	if ((out = fopen (f2, "w")) == NULL)
	    adios (f2, "unable to write message");
	(void) chmod (f2, mode);
	(void) fseek (in, pos = smsgs[j].s_start, 0);
	cpybrst (in, out, msgnam, f2,
		(int) (smsgs[j].s_stop - smsgs[j].s_start));
	(void) fclose (out);

	if (i == msgnum) {
	    (void) strcpy (f3, m_backup (f1));
	    if (rename (f1, f3) == NOTOK)
		admonish (f3, "unable to rename %s to", f1);
	}
	if (rename (f2, f1) == NOTOK)
	    admonish (f1, "unable to rename %s to", f2);
	mp -> msgstats[i] = mp -> msgstats[msgnum];
	mp -> msgflags |= SEQMOD;
    }

    (void) fclose (in);
}


/*  */

#define	S1	0
#define	S2	1
#define	S3	2

static cpybrst (in, out, ifile, ofile, len)
register FILE   *in,
		*out;
register char   *ifile,
		*ofile;
register int	len;
{
    register int    c,
                    state;

    for (state = S1; (c = fgetc (in)) != EOF && len > 0; len--) {
	if (c == 0)
	    continue;
	switch (state) {
	    case S1: 
		switch (c) {
		    case '-': 
			state = S3;
			break;

		    default: 
			state = S2;
		    case '\n': 
			(void) fputc (c, out);
			break;
		}
		break;

	    case S2: 
		switch (c) {
		    case '\n': 
			state = S1;
		    default: 
			(void) fputc (c, out);
			break;
		}
		break;

	    case S3: 
		switch (c) {
		    case ' ': 
			state = S2;
			break;

		    default: 
			state = c == '\n' ? S1 : S2;
			(void) fputc ('-', out);
			(void) fputc (c, out);
			break;
		}
		break;
	}
    }

    if (ferror (in) && !feof (in))
	adios (ifile, "error reading");
    if (ferror (out))
	adios (ofile, "error writing");
}
