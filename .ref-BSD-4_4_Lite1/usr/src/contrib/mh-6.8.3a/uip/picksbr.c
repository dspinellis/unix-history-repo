/* picksbr.c - routines to help pick along... */
#ifndef	lint
static char ident[] = "@(#)$Id: picksbr.c,v 1.8 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../zotnet/tws.h"
#include <stdio.h>

/*  */

static struct swit parswit[] = {
#define	PRAND	0
    "and", 0,
#define	PROR	1
    "or", 0,
#define	PRNOT	2
    "not", 0,
#define	PRLBR	3
    "lbrace", 0,
#define	PRRBR	4
    "rbrace", 0,

#define	PRCC	5
    "cc  pattern", 0,
#define	PRDATE	6
    "date  pattern", 0,
#define	PRFROM	7
    "from  pattern", 0,
#define	PRSRCH	8
    "search  pattern", 0,
#define	PRSUBJ	9
    "subject  pattern", 0,
#define	PRTO	10
    "to  pattern", 0,
#define	PROTHR	11
    "-othercomponent  pattern", 15,

#define	PRAFTR	12
    "after date", 0,
#define	PRBEFR	13
    "before date", 0,
#define	PRDATF	14
    "datefield field", 5,

    NULL, 0
};

/*    DEFINITIONS FOR PATTERN MATCHING */

/* We really should be using re_comp() and re_exec() here.  Unfortunately,
   pick advertises that lowercase characters matches characters of both
   cases.  Since re_exec() doesn't exhibit this behavior, we are stuck
   with this version.  Furthermore, we need to be able to save and restore
   the state of the pattern matcher in order to do things "efficiently".

   The matching power of this algorithm isn't as powerful as the re_xxx()
   routines (no \(xxx\) and \n constructs).  Such is life.
 */


#define	CCHR	2
#define	CDOT	4
#define	CCL	6
#define	NCCL	8
#define	CDOL	10
#define	CEOF	11

#define	STAR	01

#define LBSIZE  1024
#define	ESIZE	256


static char	linebuf[LBSIZE + 1];

static char cc[] = {		/* the magic array for case-independence */
	0000,0001,0002,0003,0004,0005,0006,0007,
	0010,0011,0012,0013,0014,0015,0016,0017,
	0020,0021,0022,0023,0024,0025,0026,0027,
	0030,0031,0032,0033,0034,0035,0036,0037,
	0040,0041,0042,0043,0044,0045,0046,0047,
	0050,0051,0052,0053,0054,0055,0056,0057,
	0060,0061,0062,0063,0064,0065,0066,0067,
	0070,0071,0072,0073,0074,0075,0076,0077,
	0100,0141,0142,0143,0144,0145,0146,0147,
	0150,0151,0152,0153,0154,0155,0156,0157,
	0160,0161,0162,0163,0164,0165,0166,0167,
	0170,0171,0172,0133,0134,0135,0136,0137,
	0140,0141,0142,0143,0144,0145,0146,0147,
	0150,0151,0152,0153,0154,0155,0156,0157,
	0160,0161,0162,0163,0164,0165,0166,0167,
	0170,0171,0172,0173,0174,0175,0176,0177,
};

/*    DEFINITIONS FOR DATE MATCHING */

static struct tws *tws_parse (), *tws_special ();


long   time ();

/*    DEFINITIONS FOR NEXUS */

#define	nxtarg()	(*argp ? *argp++ : NULL)
#define	prvarg()	argp--


#define	padvise		if (!talked++) advise


struct nexus {
    int     (*n_action) ();

    union {
	struct {		/* for {OR,AND,NOT}action */
	    struct nexus   *un_L_child;
	    struct nexus   *un_R_child;
	}       st1;
#define	n_L_child	un.st1.un_L_child
#define	n_R_child	un.st1.un_R_child

	struct {		/* for GREPaction */
	    int     un_header;
	    int     un_circf;
	    char    un_expbuf[ESIZE];
	    char   *un_patbuf;
	}       st2;
#define	n_header	un.st2.un_header
#define	n_circf		un.st2.un_circf
#define	n_expbuf	un.st2.un_expbuf
#define	n_patbuf	un.st2.un_patbuf

	struct {		/* for TWSaction */
	    char   *un_datef;
	    int     un_after;
	    struct tws un_tws;
	}	st3;
#define	n_datef		un.st3.un_datef
#define	n_after		un.st3.un_after
#define	n_tws		un.st3.un_tws

    }	un;
};

static	int   talked;
static	int   pdebug = 0;

static  char *datesw;
static  char **argp;

static  struct nexus *head;

static void	PRaction();
static int	gcompile(), advance(), cclass(), tcompile();

static struct	nexus *parse (), *exp1 (), *exp2 (), *exp3 (), *newnexus ();

static int	ORaction (), ANDaction (), NOTaction (), GREPaction (),
		TWSaction ();

/*  */

int	pcompile (vec, date)
register char  **vec,
	        *date;
{
    register char  *cp;

    if ((cp = getenv ("MHPDEBUG")) && *cp)
	pdebug++;

    argp = vec;
    if ((datesw = date) == NULL)
	datesw = "date";
    talked = 0;

    if ((head = parse ()) == NULL)
	return (talked ? 0 : 1);

    if (*argp) {
	padvise (NULLCP, "%s unexpected", *argp);
	return 0;
    }

    return 1;
}

/*  */

static struct nexus *parse () {
    register char  *cp;
    register struct nexus  *n,
                           *o;

    if ((n = exp1 ()) == NULL || (cp = nxtarg ()) == NULL)
	return n;

    if (*cp != '-') {
	padvise (NULLCP, "%s unexpected", cp);
	return NULL;
    }

    if (*++cp == '-')
	goto header;
    switch (smatch (cp, parswit)) {
	case AMBIGSW: 
	    ambigsw (cp, parswit);
	    talked++;
	    return NULL;
	case UNKWNSW: 
	    fprintf (stderr, "-%s unknown\n", cp);
	    talked++;
	    return NULL;

	case PROR: 
	    o = newnexus (ORaction);
	    o -> n_L_child = n;
	    if (o -> n_R_child = parse ())
		return o;
	    padvise (NULLCP, "missing disjunctive");
	    return NULL;

header: ;
	default: 
	    prvarg ();
	    return n;
    }
}

/*  */

static struct nexus *exp1 () {
    register char  *cp;
    register struct nexus  *n,
                           *o;

    if ((n = exp2 ()) == NULL || (cp = nxtarg ()) == NULL)
	return n;

    if (*cp != '-') {
	padvise (NULLCP, "%s unexpected", cp);
	return NULL;
    }

    if (*++cp == '-')
	goto header;
    switch (smatch (cp, parswit)) {
	case AMBIGSW: 
	    ambigsw (cp, parswit);
	    talked++;
	    return NULL;
	case UNKWNSW: 
	    fprintf (stderr, "-%s unknown\n", cp);
	    talked++;
	    return NULL;

	case PRAND: 
	    o = newnexus (ANDaction);
	    o -> n_L_child = n;
	    if (o -> n_R_child = exp1 ())
		return o;
	    padvise (NULLCP, "missing conjunctive");
	    return NULL;

header: ;
	default: 
	    prvarg ();
	    return n;
    }
}

/*  */

static struct nexus *exp2 () {
    register char  *cp;
    register struct nexus  *n;

    if ((cp = nxtarg ()) == NULL)
	return NULL;

    if (*cp != '-') {
	prvarg ();
	return exp3 ();
    }

    if (*++cp == '-')
	goto header;
    switch (smatch (cp, parswit)) {
	case AMBIGSW: 
	    ambigsw (cp, parswit);
	    talked++;
	    return NULL;
	case UNKWNSW: 
	    fprintf (stderr, "-%s unknown\n", cp);
	    talked++;
	    return NULL;

	case PRNOT: 
	    n = newnexus (NOTaction);
	    if (n -> n_L_child = exp3 ())
		return n;
	    padvise (NULLCP, "missing negation");
	    return NULL;

header: ;
	default: 
	    prvarg ();
	    return exp3 ();
    }
}

/*  */

static struct nexus *exp3 () {
    int     i;
    register char  *cp,
                   *dp;
    char    buffer[BUFSIZ], temp[64];
    register struct nexus  *n;

    if ((cp = nxtarg ()) == NULL)
	return NULL;

    if (*cp != '-') {
	padvise (NULLCP, "%s unexpected", cp);
	return NULL;
    }

    if (*++cp == '-') {
	dp = ++cp;
	goto header;
    }
    switch (i = smatch (cp, parswit)) {
	case AMBIGSW: 
	    ambigsw (cp, parswit);
	    talked++;
	    return NULL;
	case UNKWNSW: 
	    fprintf (stderr, "-%s unknown\n", cp);
	    talked++;
	    return NULL;

	case PRLBR: 
	    if ((n = parse ()) == NULL) {
		padvise (NULLCP, "missing group");
		return NULL;
	    }
	    if ((cp = nxtarg ()) == NULL) {
		padvise (NULLCP, "missing -rbrace");
		return NULL;
	    }
	    if (*cp++ == '-' && smatch (cp, parswit) == PRRBR)
		return n;
	    padvise (NULLCP, "%s unexpected", --cp);
	    return NULL;

	default: 
	    prvarg ();
	    return NULL;

	case PRCC: 
	case PRDATE: 
	case PRFROM: 
	case PRTO: 
	case PRSUBJ: 
	    strncpy(temp, parswit[i].sw, sizeof(temp));
	    temp[sizeof(temp) - 1] = '\0';
	    dp = *brkstring (temp, " ", NULLCP);
    header: ;
	    if (!(cp = nxtarg ())) {/* allow -xyz arguments */
		padvise (NULLCP, "missing argument to %s", argp[-2]);
		return NULL;
	    }
	    n = newnexus (GREPaction);
	    n -> n_header = 1;
	    (void) sprintf (buffer, "^%s[ \t]*:.*%s", dp, cp);
	    dp = buffer;
	    goto pattern;

	case PRSRCH: 
	    n = newnexus (GREPaction);
	    n -> n_header = 0;
	    if (!(cp = nxtarg ())) {/* allow -xyz arguments */
		padvise (NULLCP, "missing argument to %s", argp[-2]);
		return NULL;
	    }
	    dp = cp;
    pattern: ;
	    if (!gcompile (n, dp)) {
		padvise (NULLCP, "pattern error in %s %s", argp[-2], cp);
		return NULL;
	    }
	    n -> n_patbuf = getcpy (dp);
	    return n;

	case PROTHR: 
	    padvise (NULLCP, "internal error!");
	    return NULL;

	case PRDATF: 
	    if (!(datesw = nxtarg ()) || *datesw == '-') {
		padvise (NULLCP, "missing argument to %s", argp[-2]);
		return NULL;
	    }
	    return exp3 ();

	case PRAFTR: 
	case PRBEFR: 
	    if (!(cp = nxtarg ())) {/* allow -xyz arguments */
		padvise (NULLCP, "missing argument to %s", argp[-2]);
		return NULL;
	    }
	    n = newnexus (TWSaction);
	    n -> n_datef = datesw;
	    if (!tcompile (cp, &n -> n_tws, n -> n_after = i == PRAFTR)) {
		padvise (NULLCP, "unable to parse %s %s", argp[-2], cp);
		return NULL;
	    }
	    return n;
    }
}

/*  */

static struct nexus *newnexus (action)
register int	(*action) ();
{
    register struct nexus   *p;

    if ((p = (struct nexus *) calloc ((unsigned) 1, sizeof *p)) == NULL)
	adios (NULLCP, "unable to allocate component storage");

    p -> n_action = action;
    return p;
}

/*  */

#define	args(a)	a, fp, msgnum, start, stop
#define	params	args (n)
#define	plist	\
	    register struct nexus  *n; \
	    register FILE *fp; \
	    int	msgnum; \
	    long    start, \
		    stop;

int	pmatches (fp, msgnum, start, stop)
register FILE *fp;
int	msgnum;
long	start,
	stop;
{
    if (!head)
	return 1;

    if (!talked++ && pdebug)
	PRaction (head, 0);

    return (*head -> n_action) (args (head));
}

/*  */

static	void PRaction (n, level)
register struct nexus *n;
register int	level;
{
    register int    i;

    for (i = 0; i < level; i++)
	fprintf (stderr, "| ");

    if (n -> n_action == ORaction) {
	fprintf (stderr, "OR\n");
	PRaction (n -> n_L_child, level + 1);
	PRaction (n -> n_R_child, level + 1);
	return;
    }
    if (n -> n_action == ANDaction) {
	fprintf (stderr, "AND\n");
	PRaction (n -> n_L_child, level + 1);
	PRaction (n -> n_R_child, level + 1);
	return;
    }
    if (n -> n_action == NOTaction) {
	fprintf (stderr, "NOT\n");
	PRaction (n -> n_L_child, level + 1);
	return;
    }
    if (n -> n_action == GREPaction) {
	fprintf (stderr, "PATTERN(%s) %s\n",
		n -> n_header ? "header" : "body", n -> n_patbuf);
	return;
    }
    if (n -> n_action == TWSaction) {
	fprintf (stderr, "TEMPORAL(%s) %s: %s\n",
		n -> n_after ? "after" : "before", n -> n_datef,
		dasctime (&n -> n_tws, TW_NULL));
	return;
    }
    fprintf (stderr, "UNKNOWN(0x%x)\n", (*n -> n_action));
}

/*  */

static int  ORaction (params)
plist
{
    if ((*n -> n_L_child -> n_action) (args (n -> n_L_child)))
	return 1;
    return (*n -> n_R_child -> n_action) (args (n -> n_R_child));
}


static int  ANDaction (params)
plist
{
    if (!(*n -> n_L_child -> n_action) (args (n -> n_L_child)))
	return 0;
    return (*n -> n_R_child -> n_action) (args (n -> n_R_child));
}


static int  NOTaction (params)
plist
{
    return (!(*n -> n_L_child -> n_action) (args (n -> n_L_child)));
}

/*  */

static	int gcompile (n, astr)
register struct nexus *n;
register char *astr;
{
    register int    c;
    int     cclcnt;
    register char  *ep,
		   *dp,
                   *sp,
                   *lastep;

    dp = (ep = n -> n_expbuf) + sizeof n -> n_expbuf;
    sp = astr;
    if (*sp == '^') {
	n -> n_circf = 1;
	sp++;
    }
    else
	n -> n_circf = 0;
    for (;;) {
	if (ep >= dp)
	    goto cerror;
	if ((c = *sp++) != '*')
	    lastep = ep;
	switch (c) {
	    case '\0': 
		*ep++ = CEOF;
		return 1;

	    case '.': 
		*ep++ = CDOT;
		continue;

	    case '*': 
		if (lastep == 0)
		    goto defchar;
		*lastep |= STAR;
		continue;

	    case '$': 
		if (*sp != '\0')
		    goto defchar;
		*ep++ = CDOL;
		continue;

	    case '[': 
		*ep++ = CCL;
		*ep++ = 0;
		cclcnt = 1;
		if ((c = *sp++) == '^') {
		    c = *sp++;
		    ep[-2] = NCCL;
		}
		do {
		    *ep++ = c;
		    cclcnt++;
		    if (c == '\0' || ep >= dp)
			goto cerror;
		} while ((c = *sp++) != ']');
		lastep[1] = cclcnt;
		continue;

	    case '\\': 
		if ((c = *sp++) == '\0')
		    goto cerror;
	defchar: 
	    default: 
		*ep++ = CCHR;
		*ep++ = c;
	}
    }

cerror: ;
    return 0;
}

/*  */

/* ARGSUSED */

static int  GREPaction (params)
plist
{
    int     c,
            body,
            lf;
    long    pos = start;
    register char  *p1,
                   *p2,
                   *ebp,
                   *cbp;
    char    ibuf[BUFSIZ];

    (void) fseek (fp, start, 0);
    body = 0;
    ebp = cbp = ibuf;
    for (;;) {
	if (body && n -> n_header)
	    return 0;
	p1 = linebuf;
	p2 = cbp;
	lf = 0;
	for (;;) {
	    if (p2 >= ebp) {
		if (fgets (ibuf, sizeof ibuf, fp) == NULL
			|| (stop && pos >= stop)) {
		    if (lf)
			break;
		    return 0;
		}
		pos += (long) strlen (ibuf);
		p2 = ibuf;
		ebp = ibuf + strlen (ibuf);
	    }
	    c = *p2++;
	    if (lf && c != '\n')
		if (c != ' ' && c != '\t') {
		    --p2;
		    break;
		}
		else
		    lf = 0;
	    if (c == '\n')
		if (body)
		    break;
		else {
		    if (lf) {
			body++;
			break;
		    }
		    lf++;
		    c = ' ';
		}
	    if (c && p1 < &linebuf[LBSIZE - 1])
		*p1++ = c;
	}

	*p1++ = 0;
	cbp = p2;
	p1 = linebuf;
	p2 = n -> n_expbuf;

	if (n -> n_circf) {
	    if (advance (p1, p2))
		return 1;
	    continue;
	}

	if (*p2 == CCHR) {
	    c = p2[1];
	    do {
		if (*p1 == c || cc[*p1] == c)
		    if (advance (p1, p2))
			return 1;
	    } while (*p1++);
	    continue;
	}

	do {
	    if (advance (p1, p2))
		return 1;
	} while (*p1++);
    }
}

/*  */

static	int advance (alp, aep)
register char  *alp,
               *aep;
{
    register char  *lp,
                   *ep,
                   *curlp;

    lp = alp;
    ep = aep;
    for (;;)
	switch (*ep++) {
	    case CCHR: 
		if (*ep++ == *lp++ || ep[-1] == cc[lp[-1]])
		    continue;
		return 0;

	    case CDOT: 
		if (*lp++)
		    continue;
		return 0;

	    case CDOL: 
		if (*lp == 0)
		    continue;
		return 0;

	    case CEOF: 
		return 1;

	    case CCL: 
		if (cclass (ep, *lp++, 1)) {
		    ep += *ep;
		    continue;
		}
		return 0;

	    case NCCL: 
		if (cclass (ep, *lp++, 0)) {
		    ep += *ep;
		    continue;
		}
		return 0;

	    case CDOT | STAR: 
		curlp = lp;
		while (*lp++)
		    continue;
		goto star;

	    case CCHR | STAR: 
		curlp = lp;
		while (*lp++ == *ep || cc[lp[-1]] == *ep)
		    continue;
		ep++;
		goto star;

	    case CCL | STAR: 
	    case NCCL | STAR: 
		curlp = lp;
		while (cclass (ep, *lp++, ep[-1] == (CCL | STAR)))
		    continue;
		ep += *ep;
		goto star;

	star: 
		do {
		    lp--;
		    if (advance (lp, ep))
			return (1);
		} while (lp > curlp);
		return 0;

	    default: 
		admonish (NULLCP, "advance() botch -- you lose big");
		return 0;
	}
}

/*  */

static	int cclass (aset, ac, af)
register char   *aset;
{
    register int    n;
    register char   c,
                   *set;

    set = aset;
    if ((c = ac) == 0)
	return (0);

    n = *set++;
    while (--n)
	if (*set++ == c)
	    return (af);

    return (!af);
}

/*  */

static	int tcompile (ap, tb, isafter)
register char   *ap;
register struct tws *tb;
int     isafter;
{
    register struct tws *tw;

    if ((tw = tws_parse (ap, isafter)) == NULL)
	return 0;

    twscopy (tb, tw);
    return 1;
}

/*  */

static struct tws  *tws_parse (ap, isafter)
register char   *ap;
int     isafter;
{
    char    buffer[BUFSIZ];
    register struct tws *tw,
                        *ts;

    if ((tw = tws_special (ap)) != NULL) {
	tw -> tw_sec = tw -> tw_min = isafter ? 59 : 0;
	tw -> tw_hour = isafter ? 23 : 0;
	return tw;
    }
    if ((tw = dparsetime (ap)) != NULL)
	return tw;

    if ((ts = dtwstime ()) == NULL)
	return NULL;

    (void) sprintf (buffer, "%s %s", ap, dtwszone (ts));
    if ((tw = dparsetime (buffer)) != NULL)
	return tw;

    (void) sprintf (buffer, "%s %02d:%02d:%02d %s", ap,
	    ts -> tw_hour, ts -> tw_min, ts -> tw_sec, dtwszone (ts));
    if ((tw = dparsetime (buffer)) != NULL)
	return tw;

    (void) sprintf (buffer, "%02d %s %04d %s",
	    ts -> tw_mday, tw_moty[ts -> tw_mon], ts -> tw_year, ap);
    if ((tw = dparsetime (buffer)) != NULL)
	return tw;

    (void) sprintf (buffer, "%02d %s %04d %s %s",
	    ts -> tw_mday, tw_moty[ts -> tw_mon], ts -> tw_year,
	    ap, dtwszone (ts));
    if ((tw = dparsetime (buffer)) != NULL)
	return tw;

    return NULL;
}

/*  */

static struct tws  *tws_special (ap)
register char   *ap;
{
    int     i;
    long    clock;
    register struct tws *tw;

    (void) time (&clock);
    if (uleq (ap, "Today"))
	return dlocaltime (&clock);
    if (uleq (ap, "Yesterday")) {
	clock -= (long) (60 * 60 * 24);
	return dlocaltime (&clock);
    }
    if (uleq (ap, "Tomorrow")) {
	clock += (long) (60 * 60 * 24);
	return dlocaltime (&clock);
    }

    for (i = 0; tw_ldotw[i]; i++)
	if (uleq (ap, tw_ldotw[i]))
	    break;
    if (tw_ldotw[i]) {
	if ((tw = dlocaltime (&clock)) == NULL)
	    return NULL;
	if ((i -= tw -> tw_wday) > 0)
	    i -= 7;
    }
    else
	if (*ap != '-')
	    return NULL;
	else			/* -ddd days ago */
	    i = atoi (ap);	/* we should error check this */

    clock += (long) ((60 * 60 * 24) * i);
    return dlocaltime (&clock);
}

/*  */

/* ARGSUSED */

static int  TWSaction (params)
plist
{
    int     state;
    register char  *bp;
    char    buf[BUFSIZ],
            name[NAMESZ];
    register struct tws *tw;

    (void) fseek (fp, start, 0);
    for (state = FLD, bp = NULL;;) {
	switch (state = m_getfld (state, name, buf, sizeof buf, fp)) {
	    case FLD: 
	    case FLDEOF: 
	    case FLDPLUS: 
		if (bp != NULL)
		    free (bp), bp = NULL;
		bp = add (buf, NULLCP);
		while (state == FLDPLUS) {
		    state = m_getfld (state, name, buf, sizeof buf, fp);
		    bp = add (buf, bp);
		}
		if (uleq (name, n -> n_datef))
		    break;
		if (state != FLDEOF)
		    continue;

	    case BODY: 
	    case BODYEOF: 
	    case FILEEOF: 
	    case LENERR: 
	    case FMTERR: 
		if (state == LENERR || state == FMTERR)
		    advise (NULLCP, "format error in message %d", msgnum);
		if (bp != NULL)
		    free (bp);
		return 0;

	    default: 
		adios (NULLCP, "internal error -- you lose");
	}
	break;
    }

/*  */

    if ((tw = dparsetime (bp)) == NULL)
	advise (NULLCP, "unable to parse %s field in message %d, matching...",
		n -> n_datef, msgnum), state = 1;
    else
	state = n -> n_after ? (twsort (tw, &n -> n_tws) > 0)
	    : (twsort (tw, &n -> n_tws) < 0);

    if (bp != NULL)
	free (bp);
    return state;
}
