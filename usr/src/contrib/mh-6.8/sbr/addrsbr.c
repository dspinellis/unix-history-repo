/* addrsbr.c - parse addresses 822-style */
#ifndef	lint
static char ident[] = "@(#)$Id: addrsbr.c,v 1.13 1992/10/26 22:44:26 jromine Exp $";
#endif /* lint */

#include "../h/mh.h"
#include "../h/addrsbr.h"
#include "../zotnet/mf.h"
#include <stdio.h>
#ifdef	BERK
#include <ctype.h>
#endif /* BERK */

/* High level parsing of addresses:

   The routines in zotnet/mf/mf.c parse the syntactic representations of
   addresses.  The routines in sbr/addrsbr.c associate semantics with those
   addresses.  

   If #ifdef BERK is in effect, the routines in mf.c aren't called and only
   the most rudimentary syntax parse is done.  The parse is not 822-conformant.
   This causes problems as there is no semantics associated with the address
   at all--it's just a string. (the author of the BERK code disagrees with
   the preceding, of course.  BERK solves problems for incoming mail
   because it will accept damn near any address.  BERK was intended to be
   used when spost is the interface to the mail delivery system which means
   all outgoing address interpretation is left to sendmail.  It is possible,
   though unlikely, for BERK address parsing to interact poorly with 
   "post". - van@monet.berkeley.edu).

   Instead, if #ifdef DUMB is in effect, a full 822-style parser is called
   for syntax recongition.  This breaks each address into its components.
   Note however that no semantics are assumed about the parts or their
   totality.  This means that implicit hostnames aren't made explicit,
   and explicit hostnames aren't expanded to their "official" represenations.

   If neither BERK nor DUMB is in effect, then this module does some
   high-level thinking about what the addresses are.  If #ifdef MF is in
   effect, then MH will deduce UUCP-style addressing masquerading as
   822-style addresses.

   1. for MMDF systems:

	string%<uucp>@<local>	->	string

   2. for non-MMDF systems:

	string@host.<uucp>	->	host!string

   3. for any system, an address interpreted relative to the local host:

	string@<uucp>		->	string

   For cases (1) and (3) above, the leftmost host is extracted.  If it's not
   present, the local host is used.  If #ifdef MF is not in effect or the
   tests above fail, the address is considered to be a real 822-style address.

   If an explicit host is not present, then MH checks for a bang to indicate
   an explicit UUCP-style address.  If so, this is noted.  If not, the host is
   defaulted, typically to the local host.  The lack of an explict host is
   also noted.

   If an explicit 822-style host is present, then MH checks to see if it
   can expand this to the official name for the host.  If the hostname is
   unknown, the address is so typed.

   To summarize, when we're all done, here's what MH knows about the address:

   BERK	-	type: 	local
		nohost:	set if no '@' or '!' in mailbox
		text:	exact copy of address
		mbox:	lowercase version of mailbox

   DUMB	-	type:	local, uucp, or network
		host:	not locally defaulted, not explicitly expanded
		everything else

   other -	type:	local, uucp, network, unknown
		everything else
 */

/*  */

#if	!defined(DUMB) && defined(SENDMTS) && !defined(BANG)
#define	MF
#define	UucpChan()	"UUCP"
#endif /* MF */

#ifdef	BERK
static char *err = NULL;
static char adrtext[BUFSIZ];
#else /* not BERK */
static int  ingrp = 0;

static char *pers = NULL;
static char *mbox = NULL;
static char *host = NULL;
static char *route = NULL;
static char *grp = NULL;
static char *note = NULL;

static char err[BUFSIZ];
#endif /* not BERK */
static char adr[BUFSIZ];


char   *getusr ();

/*  */

char   *getname (addrs)
register char   *addrs;
{
#ifdef	BERK
    /*
     * Berkeley uses a very simple parser since Sendmail does all the work.
     * The only thing that does address parsing if BERK is defined is the
     * routine "ismybox" used by "scan" & "repl" to identify the current
     * users maildrop.
     *
     * This routine does essentially the same address interpretation as the
     * routine "prescan" in "sendmail".  The intent is that MH should 
     * make minimum assumptions about address forms since it doesn't
     * have access to the information in the sendmail config file
     * (God forbid that anything but sendmail has to deal with a sendmail
     * config file) and, therefore, hasn't the faintest idea of what will
     * or won't be a legal address.
     *
     * Since this parse is only used by "ismybox" and repl, it just does
     * two things: split multiple addr on a line into separate addresses and 
     * locate the "mailbox" portion of an address.  The parse uses rfc-822
     * metacharacters and quoting but is much less restrictive that rfc-822.
     * In detail, `,' or eos terminate addresses.  "Empty" addresses
     * (e.g., `,,') are ignored.  Double quote ("), backslash, left & right
     * paren and left and right angle brackets are metacharacters.  Left &
     * right parens must balance as must left & right angle brackets.  Any
     * metacharacter may be escaped by preceding it with a backslash.
     * Any text between parens is considered a comment and ignored (i.e.,
     * only `(', `)' and `\' are metacharacters following a `(').  Text
     * between double quotes is considered escaped (i.e., only `"' and
     * `\' are metacharacters following a `"').  The `mailbox' portion
     * of an address is the non-comment text between angle-brackets if
     * the address contains any angle brackets.  Otherwise, it is all the
     * non-comment text.  Blanks, tabs & newlines will not be included
     * in the mailbox portion of an address unless they are escaped.
     */

/* Scanner states */
#define NORMAL	(0<<8)
#define QS	(1<<8)	/* in quoted string */
#define COM	(2<<8)	/* in comment (...) */
#define ERR	(3<<8)	/* found an error */
#define EOA	(4<<8)	/* end of address */

    static char *saved_addr = NULL;	/* saved address line ptr */
    static char *adr_ptr = NULL;	/* where to start looking for
					   next address on line */
    register char *nxtout = adr;	/* where to put next character of
					   `mailbox' part of address */
    register char c;
    register int state = NORMAL;
    register char *adrcopy = adrtext;	/* where to put next character of
					   address */
    register int lbcnt = 0;		/* number of unmatched "(" */
    register int lpcnt = 0;		/* number of unmatched "<" */

    err = NULL;
    if (! addrs) {
	adr_ptr = NULL;
	return NULL;
    }
    if (adr_ptr)
	addrs = adr_ptr;
    else
	addrs = saved_addr = getcpy(addrs ? addrs : "");

    /* skip any leading whitespace or commas. */
    while ( (c = *addrs++) == ',' || isspace(c))
	;
    
    *nxtout = *adrcopy = '\0';
    while (state != EOA) {
	*adrcopy++ = c;
	if (state != COM)
	    *nxtout++ = (isalpha(c) && isupper (c)) ? tolower (c) : c;
	switch (state+c) {

	case NORMAL+'\n':	/* discard newlines */
	case QS+'\n':
	case COM+'\n':
	case ERR+'\n':
	    --nxtout;
	    --adrcopy;
	    break;

	case NORMAL+' ':	/* skip unquoted whitespace */
	case NORMAL+'\t':
	    --nxtout;
	    break;

	case NORMAL+'"':	/* start quoted string */
	    state = QS;
	    break;

	case QS+'"':		/* end quoted string */
	    state = NORMAL;
	    break;

	case NORMAL+'<':
	    nxtout = adr;	/* start over accumulating address */
	    lbcnt++;
	    break;

	case NORMAL+'>':
	    --lbcnt;
	    if (lbcnt < 0) {
		state = ERR;
		err = "extra >";
	    } else
		*(nxtout-1) = '\0';
	    break;

	case NORMAL+'(':
	    state = COM;
	    --nxtout;
	case COM+'(':
	    lpcnt++;
	    break;

	case COM+')':
	    --lpcnt;
	    if (lpcnt < 0) {
		state = ERR;
		err = "extra )";
	    } else if (lpcnt == 0)
		state = NORMAL;
	    break;

	case NORMAL+'\\':
	case QS+'\\':
	case COM+'\\':
	    if ((c = *addrs++) == '\n' || c == '\0') {
		state = EOA;
		err = "illegal \\";
	    }
	    *adrcopy++ = c;
	    *nxtout++ = (isalpha(c) && isupper (c)) ? tolower (c) : c;
	    break;

	case NORMAL+',':
	case ERR+',':
	case NORMAL+'\0':
	case ERR+'\0':
	    state = EOA;
	    if (lbcnt)
		err = "missing >";
	    break;

	case COM+'\0':
	    state = EOA;
	    err = "missing )";
	    if (nxtout == adr)
		nxtout++;
	    break;

	case QS+'\0':
	    state = EOA;
	    err = "missing \"";
	    break;
	}
	if (c != '\0')
	    c = *addrs++;
    }
    /*
     * at this point adr contains the `mailbox' part of the address
     * in lower case & minus any comment or unquoted whitespace.
     * adrtext contains an exact copy of the address and
     * addr points to where we should start scanning next time.
     */
    *(nxtout-1) = *(adrcopy-1) = '\0';
    if (*adr && !err) {
	adr_ptr = addrs-1;
	return adrtext;
    } else {
	free (saved_addr);
	adr_ptr = NULL;
	return NULL;
    }
#else /* not BERK */
    register struct adrx *ap;

    pers = mbox = host = route = grp = note = NULL;
    err[0] = '\0';

    if ((ap = getadrx (addrs ? addrs : "")) == NULL)
	return NULL;

    (void) strcpy (adr, ap -> text);
    pers = ap -> pers;
    mbox = ap -> mbox;
    host = ap -> host;
    route = ap -> path;
    grp = ap -> grp;
    ingrp = ap -> ingrp;
    note = ap -> note;
    if (ap -> err && *ap -> err)
	(void) strcpy (err, ap -> err);

    return adr;
#endif /* not BERK */
}

/*  */

#ifdef	BERK
/* ARGSUSED */
#endif /* BERK */

struct mailname *getm (str, dfhost, dftype, wanthost, eresult)
register char   *str,
		*eresult;
char   *dfhost;
int     dftype,
	wanthost;
{
#ifndef	BERK
    register char   *pp;
#ifndef	DUMB
    register char   *dp;
#endif /* not DUMB */
#ifdef	MF
    char   *up = UucpChan ();
#endif /* MF */
#endif /* not BERK */
    register struct mailname *mp;

    if (err && err[0]) {
	if (eresult)
	    (void) strcpy (eresult, err);
	else
	    if (wanthost == AD_HOST)
		admonish (NULLCP, "bad address '%s' - %s", str, err);
	return NULL;
    }
#ifdef	BERK
    if (str == NULL || *str == '\0') {
#else /* not BERK */
    if (pers == NULL
	    && mbox == NULL && host == NULL && route == NULL
	    && grp == NULL) {
#endif /* not BERK */
	if (eresult)
	    (void) strcpy (eresult, "null address");
	else
	    if (wanthost == AD_HOST)
		admonish (NULLCP, "null address '%s'", str);
	return NULL;
    }
#ifndef	BERK
    if (mbox == NULL && grp == NULL) {
	if (eresult)
	    (void) strcpy (eresult, "no mailbox in address");
	else
	    if (wanthost == AD_HOST)
		admonish (NULLCP, "no mailbox in address '%s'", str);
	return NULL;
    }

    if (dfhost == NULL) {
	dfhost = LocalName ();
	dftype = LOCALHOST;
    }
#endif /* not BERK */

    mp = (struct mailname  *) calloc ((unsigned) 1, sizeof *mp);
    if (mp == NULL) {
	if (eresult)
	   (void) strcpy (eresult, "insufficient memory to represent address");
	else
	    if (wanthost == AD_HOST)
		adios (NULLCP, "insufficient memory to represent address");
	return NULL;
    }

    mp -> m_next = NULL;
    mp -> m_text = getcpy (str);
#ifdef	BERK
    mp -> m_type = LOCALHOST;
    mp -> m_mbox = getcpy (adr);
    if (!index (adr, '@') && !index (adr, '!'))
	mp -> m_nohost = 1;
#else /* not BERK */
    if (pers)
	mp -> m_pers = getcpy (pers);

    if (mbox == NULL) {
	mp -> m_type = BADHOST;
	mp -> m_nohost = 1;
	mp -> m_ingrp = ingrp;
	mp -> m_gname = getcpy (grp);
	if (note)
	    mp -> m_note = getcpy (note);
	return mp;
    }

/*  */

    if (host) {
#ifdef MF
#ifdef	MMDFMTS
	if (up && uleq (host, LocalName ())
		&& (pp = rindex (mbox, '%'))
		&& uleq (up, pp + 1)) {/* uucpaddr%<uucp>@<local> */
	    *pp = NULL;
	    goto get_uucp;
	}
#else /* not MMDFMTS */
	if (up && (pp = index (host, '.')) 
		&& uleq (up, pp + 1)) {/* uucpaddr@host.<uucp> */
	    *pp = NULL;
	    mp -> m_host = getcpy (host);
	    mp -> m_mbox = getcpy (mbox);
	    mp -> m_type = UUCPHOST;
	    goto got_host;
	}
#endif /* not MMDFMTS */
	if (up && uleq (dfhost, LocalName ())
		&& uleq (up, host)) {/* uucpaddr@<uucp> [local] */
	    if (pp = index (mbox, '!')) {
		*pp++ = NULL;
		mp -> m_host = getcpy (mbox);
		mp -> m_mbox = getcpy (pp);
	    }
	    else {
		mp -> m_host = getcpy (SystemName ());
		mp -> m_mbox = getcpy (mbox);
	    }
	    mp -> m_type = UUCPHOST;
	    goto got_host;
	}
#endif /* MF */
	mp -> m_mbox = getcpy (mbox);
	mp -> m_host = getcpy (host);
    }
    else {
	if (pp = index (mbox, '!')) {
	    *pp++ = '\0';
	    mp -> m_mbox = getcpy (pp);
	    mp -> m_host = getcpy (mbox);
	    mp -> m_type = UUCPHOST;
	}
	else {
	    mp -> m_nohost = 1;
	    mp -> m_mbox = getcpy (mbox);
#ifdef	DUMB
	    if (route == NULL && dftype == LOCALHOST) {
		mp -> m_host = NULLCP;
		mp -> m_type = dftype;
	    }
	    else
#endif /* DUMB */
	    {
		mp -> m_host = route ? NULLCP : getcpy (dfhost);
		mp -> m_type = route ? NETHOST : dftype;
	    }
	}
	goto got_host;
    }

/*  */

    if (wanthost == AD_NHST)
	mp -> m_type = uleq (LocalName (), mp -> m_host)
	    ? LOCALHOST : NETHOST;
#ifdef	DUMB
    else
	mp -> m_type = uleq (LocalName (), mp -> m_host) ? LOCALHOST
		: NETHOST;
#else /* not DUMB */
    else
	if (pp = OfficialName (mp -> m_host)) {
    got_real_host: ;
	    free (mp -> m_host);
	    mp -> m_host = getcpy (pp);
	    mp -> m_type = uleq (LocalName (), mp -> m_host) ? LOCALHOST
		    : NETHOST;
	}
	else {
	    if (dp = index (mp -> m_host, '.')) {
		*dp = NULL;
		if (pp = OfficialName (mp -> m_host))
		    goto got_real_host;
		*dp = '.';
	    }
	    mp -> m_type = BADHOST;
	}
#endif /* not DUMB */

got_host: ;
    if (route)
	mp -> m_path = getcpy (route);
    mp -> m_ingrp = ingrp;
    if (grp)
	mp -> m_gname = getcpy (grp);
    if (note)
	mp -> m_note = getcpy (note);
#endif /* not BERK */

    return mp;
}

/*  */

void	mnfree (mp)
register struct mailname *mp;
{
    if (!mp)
	return;

    if (mp -> m_text)
	free (mp -> m_text);
    if (mp -> m_pers)
	free (mp -> m_pers);
    if (mp -> m_mbox)
	free (mp -> m_mbox);
    if (mp -> m_host)
	free (mp -> m_host);
    if (mp -> m_path)
	free (mp -> m_path);
    if (mp -> m_gname)
	free (mp -> m_gname);
    if (mp -> m_note)
	free (mp -> m_note);
#ifdef	MHMTS
    if (mp -> m_aka)
	free (mp -> m_aka);
#endif /* MHMTS */

    free ((char *) mp);
}

/*  */

char   *auxformat (mp, extras)
register struct mailname   *mp;
int	extras;
{
#ifndef	BERK
#ifdef	MF
    char   *up = UucpChan ();
#endif /* MF */
    static char addr[BUFSIZ];
#endif /* not BERK */
    static char buffer[BUFSIZ];

#ifdef	BERK
    /* this "if" is a crufty hack to handle "visible" aliases */
    if (mp->m_pers && !extras)
	(void) sprintf (buffer, "%s <%s>", mp->m_pers, mp->m_mbox);
    else
	(void) strcpy (buffer, mp -> m_text);
#else /* not BERK */

#ifdef	MF
    if (up && mp -> m_type == UUCPHOST)
#ifdef	MMDFMTS
	(void) sprintf (addr, "%s!%s%%%s@%s", mp -> m_host, mp -> m_mbox,
		up, LocalName ());
#else /* not MMDFMTS */
	(void) sprintf (addr, "%s@%s.%s", mp -> m_mbox, mp -> m_host, up);
#endif /* not MMDFMTS */
    else 
#endif /* MF */

#ifdef	DUMB
	if (mp -> m_nohost)
	    (void) strcpy (addr, mp -> m_mbox ? mp -> m_mbox : "");
	else
#endif /* DUMB */

#ifndef	BANG
	if (mp -> m_type != UUCPHOST)
	    (void) sprintf (addr, mp -> m_host ? "%s%s@%s" : "%s%s",
		mp -> m_path ? mp -> m_path : "", mp -> m_mbox, mp -> m_host);
	else
#endif /* not BANG */
	    (void) sprintf (addr, "%s!%s", mp -> m_host, mp -> m_mbox);

    if (!extras)
	return addr;

    if (mp -> m_pers || mp -> m_path)
	if (mp -> m_note)
	    (void) sprintf (buffer, "%s %s <%s>",
		    legal_person (mp -> m_pers ? mp -> m_pers : mp -> m_mbox),
		    mp -> m_note, addr);
	else
	    (void) sprintf (buffer, "%s <%s>",
		    legal_person (mp -> m_pers ? mp -> m_pers : mp -> m_mbox),
		    addr);
    else
	if (mp -> m_note)
	    (void) sprintf (buffer, "%s %s", addr, mp -> m_note);
	else
	    (void) strcpy (buffer, addr);
#endif /* not BERK */

    return buffer;
}

/*  */

#if	defined(BERK) || (defined(DUMB) && !defined(MMDFMTS) && !defined(SMTP))
#define	REALLYDUMB
#else
#undef	REALLYDUMB
#endif

char   *adrsprintf (local, domain)
char   *local,
       *domain;
{
    static char addr[BUFSIZ];

    if (local == NULL)
#ifdef	REALLYDUMB
	return getusr ();
    else
#endif /* REALLYDUMB */
	local = getusr ();

    if (domain == NULL)
#ifdef	REALLYDUMB
	return local;
    else
#endif /* REALLYDUMB */
	domain = LocalName ();

#ifndef	BANG
    (void) sprintf (addr, "%s@%s", local, domain);
#else /* BANG */
    (void) sprintf (addr, "%s!%s", domain, local);
#endif /* BANG */

    return addr;
}

/*  */

#define	W_NIL	0x0000
#define	W_MBEG	0x0001
#define	W_MEND	0x0002
#define	W_MBOX	(W_MBEG | W_MEND)
#define	W_HBEG	0x0004
#define	W_HEND	0x0008
#define	W_HOST	(W_HBEG | W_HEND)
#define	WBITS	"\020\01MBEG\02MEND\03HBEG\04HEND"


int	ismymbox (np)
register struct mailname *np;
{
    int     oops;
    register int    len,
                    i;
    register char  *cp;
#ifndef	BERK
    register char  *pp;
    char    buffer[BUFSIZ];
#endif /* not BERK */
    register struct mailname   *mp;
    static char *am = NULL;
    static struct mailname  mq={NULL};

    /* if this is the first call, init. alternate mailboxes list */
    if (am == NULL) {
	mq.m_next = NULL;
	mq.m_mbox = getusr ();
	if ((am = m_find ("alternate-mailboxes")) == NULL)
	    am = getusr ();
	else {
	    mp = &mq;
	    oops = 0;
	    while (cp = getname (am))
		if ((mp -> m_next = getm (cp, NULLCP, 0, AD_NAME, NULLCP))
			== NULL)
		    admonish (NULLCP, "illegal address: %s", cp), oops++;
		else {
		    mp = mp -> m_next;
		    mp -> m_type = W_NIL;
#ifdef	BERK
		    /* check for wildcards on the mailbox name and
		       set m_type accordingly. */
		    mp -> m_ingrp = strlen (mp -> m_mbox);
		    if (*(mp -> m_mbox) == '*') {
			mp -> m_type |= W_MBEG;
			mp -> m_mbox++;
			--mp -> m_ingrp;
		    }
		    if (mp -> m_mbox[mp -> m_ingrp - 1] == '*') {
			mp -> m_type |= W_MEND;
			mp -> m_ingrp--;
			mp -> m_mbox[mp -> m_ingrp] = 0;
		    }
#else /* not BERK */
		    /* owing to screwy munging, wildcarding is a great idea
		       even under #ifndef BERK, so... */
		    mp -> m_type = W_NIL;
		    if (*mp -> m_mbox == '*')
			mp -> m_type |= W_MBEG, mp -> m_mbox++;
		    if (*(cp = mp -> m_mbox + strlen (mp -> m_mbox) - 1)
			    == '*')
			mp -> m_type |= W_MEND, *cp = '\0';
		    if (mp -> m_host) {
			if (*mp -> m_host == '*')
			    mp -> m_type |= W_HBEG, mp -> m_host++;
			if (*(cp = mp -> m_host + strlen (mp -> m_host) - 1)
				== '*')
			    mp -> m_type |= W_HEND, *cp = '\0';
		    }
		    if ((cp = getenv ("MHWDEBUG")) && *cp)
			fprintf (stderr, "mbox=\"%s\" host=\"%s\" %s\n",
			    mp -> m_mbox, mp -> m_host,
			    sprintb (buffer, (unsigned) mp -> m_type, WBITS));
#endif /* not BERK */
		}
	    if (oops)
		advise (NULLCP, "please fix the %s: entry in your %s file",
			"alternate-mailboxes", mh_profile);
	}
    }
    if (np == NULL) /* XXX */
	return 0;
    
#ifdef	BERK
    cp = np -> m_mbox;
    if (strcmp (cp, mq.m_mbox) == 0)
	return 1;
#else /* not BERK */
    switch (np -> m_type) {
	case NETHOST:
	    len = strlen (cp = LocalName ());
	    if (!uprf (np -> m_host, cp) || np -> m_host[len] != '.')
		break;
	    goto local_test;

	case UUCPHOST:
	    if (!uleq (np -> m_host, SystemName ()))
		break;		/* fall */
	case LOCALHOST:
local_test: ;
	    if (uleq (np -> m_mbox, mq.m_mbox))
		return 1;
	    break;

	default:
	    break;
    }
#endif /* not BERK */

#ifdef	BERK
    len = strlen (cp);
    for (mp = &mq; mp = mp -> m_next;)
	if (len >= mp -> m_ingrp)
	    switch (mp -> m_type) {
		case W_NIL: 	/* no wildcards */
		    if (strcmp (cp, mp -> m_mbox) == 0)
			return 1;
		    break;

		case W_MBEG: 	/* wildcard at beginning */
		    if (strcmp (&cp[len - mp -> m_ingrp], mp -> m_mbox) == 0)
			return 1;
		    break;

		case W_MEND: 	/* wildcard at end */
		    if (strncmp (cp, mp -> m_mbox, mp -> m_ingrp) == 0)
			return 1;
		    break;

		case W_MBEG | W_MEND: /* wildcard at beginning & end */
		    for (i = 0; i <= len - mp -> m_ingrp; i++)
			if (strncmp (&cp[i], mp -> m_mbox, mp -> m_ingrp) == 0)
			    return 1;
	    }
#else /* not BERK */
    for (mp = &mq; mp = mp -> m_next;) {
	if (np -> m_mbox == NULL)
	    continue;
	if ((len = strlen (cp = np -> m_mbox))
		< (i = strlen (pp = mp -> m_mbox)))
	    continue;
	switch (mp -> m_type & W_MBOX) {
	    case W_NIL: 
		if (!uleq (cp, pp))
		    continue;
		break;
	    case W_MBEG: 
		if (!uleq (cp + len - i, pp))
		    continue;
		break;
	    case W_MEND: 
		if (!uprf (cp, pp))
		    continue;
		break;
	    case W_MBEG | W_MEND: 
		if (stringdex (pp, cp) < 0)
		    continue;
		break;
	}

	if (mp -> m_nohost)
	    return 1;
	if (np -> m_host == NULL)
	    continue;
	if ((len = strlen (cp = np -> m_host))
		< (i = strlen (pp = mp -> m_host)))
	    continue;
	switch (mp -> m_type & W_HOST) {
	    case W_NIL: 
		if (!uleq (cp, pp))
		    continue;
		break;
	    case W_HBEG: 
		if (!uleq (cp + len - i, pp))
		    continue;
		break;
	    case W_HEND: 
		if (!uprf (cp, pp))
		    continue;
		break;
	    case W_HBEG | W_HEND: 
		if (stringdex (pp, cp) < 0)
		    continue;
		break;
	}
	return 1;
    }
#endif /* not BERK */

    return 0;
}
