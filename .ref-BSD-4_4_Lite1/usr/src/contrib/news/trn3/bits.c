/* $Id: bits.c,v 3.0 1992/02/01 03:09:32 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "cache.h"
#include "INTERN.h"
#include "bits.h"
#include "EXTERN.h"
#include "rcstuff.h"
#include "head.h"
#include "util.h"
#include "final.h"
#include "trn.h"
#include "ng.h"
#include "artio.h"
#include "intrp.h"
#include "ngdata.h"
#include "rcln.h"
#include "ndir.h"
#include "nntp.h"
#include "rthread.h"
#include "rt-select.h"

#ifdef DBM_XREFS
#    ifdef NULL
#	undef NULL
#    endif
#    include <dbm.h>
#endif

void
bits_init()
{
    ;
}

void
rc_to_bits()
{
    char *mybuf = buf;			/* place to decode rc line */
    register char *s, *c, *h;
    register long i;
    register ART_NUM unread;
    register ARTICLE *ap;

    /* modify the article flags to reflect what has already been read */

    for (s = rcline[ng] + rcnums[ng]; *s == ' '; s++) ;
					/* find numbers in rc line */
    i = strlen(s);
#ifndef lint
    if (i >= LBUFLEN-2)			/* bigger than buf? */
	mybuf = safemalloc((MEM_SIZE)(i+2));
#endif
    strcpy(mybuf,s);			/* make scratch copy of line */
    mybuf[i++] = ',';			/* put extra comma on the end */
    mybuf[i] = '\0';
    s = mybuf;				/* initialize the for loop below */
    if (strnEQ(s,"1-",2)) {		/* can we save some time here? */
	firstart = atol(s+2)+1;		/* process first range thusly */
	s=index(s,',') + 1;
	for (i = absfirst, ap = article_ptr(i); i < firstart; i++, ap++)
	    ap->flags |= AF_READ;
    } else {
	i = firstart = absfirst;
	ap = article_ptr(i);
    }
    unread = lastart - firstart + 1;	/* assume this range unread */
#ifdef DEBUG
    if (debug & DEB_CTLAREA_BITMAP) {
	printf("\n%s\n",mybuf) FLUSH;
	for (i = absfirst, ap = article_ptr(i); i < firstart; i++, ap++)
	    if (!(ap->flags & AF_READ))
		printf("%ld ",(long)i) FLUSH;
    }
#endif
    for ( ; (c = index(s,',')) != Nullch; s = ++c) {	/* for each range */
	ART_NUM min, max;

	*c = '\0';			/* do not let index see past comma */
	h = index(s,'-');
	min = atol(s);
	if (h)				/* is there a dash? */
	    max = atol(h+1);
	else
	    max = min;
	if (min < firstart)		/* make sure range is in range */
	    min = firstart;
	if (min > lastart)
	    min = lastart+1;
	for (; i < min; i++, ap++)
	    ap->flags &= ~AF_READ;
	if (max > lastart)
	    max = lastart;
	if (min <= max) {		/* non-null range? */
	    unread -= max - min + 1;/* adjust unread count */
	    /* mark all arts in range as read */
	    for (; i <= max; i++, ap++)
		ap->flags |= AF_READ;
	}
#ifdef DEBUG
	if (debug & DEB_CTLAREA_BITMAP) {
	    printf("\n%s\n",s) FLUSH;
	    for (i=absfirst; i <= lastart; i++)
		if (!was_read(i))
		    printf("%ld ",(long)i) FLUSH;
	}
#endif
    }
    for (; i <= lastart; i++, ap++)
	ap->flags &= ~AF_READ;
#ifdef DEBUG
    if (debug & DEB_CTLAREA_BITMAP) {
	fputs("\n(hit CR)",stdout) FLUSH;
	gets(cmd_buf);
    }
#endif
    if (mybuf != buf)
	free(mybuf);
    toread[ng] = unread;
}

/* reconstruct the .newsrc line in a human readable form */

void
bits_to_rc()
{
    register char *s, *mybuf = buf;
    register ART_NUM i;
    ART_NUM count=0;
    int safelen = LBUFLEN - 32;

    strcpy(buf,rcline[ng]);		/* start with the newsgroup name */
    s = buf + rcnums[ng] - 1;		/* use s for buffer pointer */
    *s++ = RCCHAR(rcchar[ng]);		/* put the requisite : or !*/
    for (i=absfirst; i<=lastart; i++)
	if (!was_read(i))
	    break;
    sprintf(s," 1-%ld,",(long)i-1);
    s += strlen(s);
    for (; i<=lastart; i++) {	/* for each article in newsgroup */
	if (s-mybuf > safelen) {	/* running out of room? */
	    safelen *= 2;
	    if (mybuf == buf) {		/* currently static? */
		*s = '\0';
		mybuf = safemalloc((MEM_SIZE)safelen + 32);
		strcpy(mybuf,buf);	/* so we must copy it */
		s = mybuf + (s-buf);
					/* fix the pointer, too */
	    }
	    else {			/* just grow in place, if possible */
		char *newbuf;

		newbuf = saferealloc(mybuf,(MEM_SIZE)safelen + 32);
		s = newbuf + (s-mybuf);
		mybuf = newbuf;
	    }
	}
	if (!was_read(i))		/* still unread? */
	    count++;			/* then count it */
	else {				/* article was read */
	    ART_NUM oldi;

	    sprintf(s,"%ld",(long)i);	/* put out the min of the range */
	    s += strlen(s);		/* keeping house */
	    oldi = i;			/* remember this spot */
	    do i++; while (i <= lastart && was_read(i));
					/* find 1st unread article or end */
	    i--;			/* backup to last read article */
	    if (i > oldi) {		/* range of more than 1? */
		sprintf(s,"-%ld,",(long)i);
					/* then it out as a range */
		s += strlen(s);		/* and housekeep */
	    }
	    else
		*s++ = ',';		/* otherwise, just a comma will do */
	}
    }
    if (*(s-1) == ',')			/* is there a final ','? */
	s--;				/* take it back */
    *s++ = '\0';			/* and terminate string */
#ifdef DEBUG
    if (debug & DEB_NEWSRC_LINE && !panic) {
	printf("%s: %s\n",rcline[ng],rcline[ng]+rcnums[ng]) FLUSH;
	printf("%s\n",mybuf) FLUSH;
    }
#endif
    free(rcline[ng]);			/* return old rc line */
    if (mybuf == buf) {
	rcline[ng] = safemalloc((MEM_SIZE)(s-buf)+1);
					/* grab a new rc line */
	strcpy(rcline[ng], buf);	/* and load it */
    }
    else {
	mybuf = saferealloc(mybuf,(MEM_SIZE)(s-mybuf)+1);
					/* be nice to the heap */
	rcline[ng] = mybuf;
    }
    *(rcline[ng] + rcnums[ng] - 1) = '\0';
    if (rcchar[ng] == NEGCHAR) {	/* did they unsubscribe? */
	printf(unsubto,ngname) FLUSH;
	toread[ng] = TR_UNSUB;		/* make line invisible */
    }
    else
	/*NOSTRICT*/
	toread[ng] = (ART_UNREAD)count;		/* remember how many unread there are */
}

#ifdef USE_NNTP

/* Parse the LISTGROUP output and set anything not mentioned as missing. */

void
setmissingbits()				/* NNTP version */
{
    register ART_NUM num, priornum;
    register ARTICLE *ap;

    if (!nntp_listgroup())
	return;
    for (priornum = absfirst-1, ap = article_ptr(absfirst);; ap++) {
	nntp_gets(ser_line, sizeof ser_line);
	if (*ser_line == '.')
	    break;
	num = atol(ser_line);
	while (++priornum < num)
	    uncache_article(ap++,FALSE);
    }
}

#else /* !USE_NNTP */

/* Scan the directory to find which articles are present. */

void
setfoundbits()
{
    register ART_NUM first = lastart+1;
    register DIR *dirp;
    register struct direct *dp;
    long an;
    char ch;

    if (!(dirp = opendir(".")))
	return;
    
    found_min = absfirst;
    an = (lastart-found_min)/BITSPERBYTE+20;
    found_bits = safemalloc((MEM_SIZE)an);
    bzero(found_bits, an);

    while ((dp = readdir(dirp)) != Null(struct direct *)) {
	if (sscanf(dp->d_name, "%ld%c", &an, &ch) == 1) {
	    if (an <= lastart && an >= found_min) {
		if (an < first)
		    first = an;
		foundart(an);
	    }
	}
    }
    closedir(dirp);
    abs1st[ng] = first;
    if (first > absfirst)
	checkexpired(ng);
    absfirst = first;
}

void
setmissingbits()				/* non-NNTP version */
{
    register ARTICLE *ap;
    register ART_NUM an;

    if (!found_bits)
	return;

    for (an = absfirst, ap = article_ptr(an); an <= lastart; an++, ap++) {
	if (artismissing(an))
	    onemissing(ap);
    }
    free(found_bits);
    found_bits = NULL;
}
#endif /* !USE_NNTP */

/* mark an article unread, keeping track of toread[] */

void
onemore(ap)
ARTICLE *ap;
{
    if (ap->flags & AF_READ) {
	register ART_NUM artnum = article_num(ap);
	check_first(artnum);
	ap->flags &= ~AF_READ;
	++toread[ng];
	ap->flags &= ~AF_DEL;
	if (ap->subj) {
	    if (selected_only) {
		if (ap->subj->flags & sel_mask) {
		    ap->flags |= sel_mask;
		    selected_count++;
		}
	    } else
		ap->subj->flags |= SF_VISIT;
	}
    }
}

/* mark an article read, keeping track of toread[] */

void
oneless(ap)
ARTICLE *ap;
{
    if (!(ap->flags & AF_READ)) {
	ap->flags |= AF_READ;
	/* Keep selected_count accurate */
	if (ap->flags & sel_mask) {
	    selected_count--;
	    ap->flags &= ~sel_mask;
	}
	if (toread[ng] > TR_NONE)
	    --toread[ng];
    }
}

void
onemissing(ap)
ARTICLE *ap;
{
    oneless(ap);
    ap->flags = (ap->flags & ~(AF_HAS_RE|AF_YANKBACK|AF_FROMTRUNCED))
	      | AF_MISSING|AF_CACHED|AF_THREADED;
}

/* mark an article as unread, with possible xref chasing */

void
unmark_as_read()
{
    onemore(article_ptr(art));
#ifdef MCHASE
    chase_xrefs(art,FALSE);
#endif
}

/* Mark an article as read in this newsgroup and possibly chase xrefs.
** Don't call this on missing articles.
*/
void
set_read(ap)
register ARTICLE *ap;
{
    register ART_NUM artnum = article_num(ap);

    oneless(ap);
    if (!olden_days && ap->xrefs != nullstr) {
	if (output_chase_phrase) {
#ifdef VERBOSE
	    IF(verbose)
		fputs("\nChasing xrefs", stdout);
	    ELSE
#endif
#ifdef TERSE
		fputs("\nXrefs", stdout);
#endif
	    output_chase_phrase = 0;
	}
	putchar('.'), fflush(stdout);
	chase_xrefs(artnum, TRUE);
    }
}

/* temporarily mark article as read.  When newsgroup is exited, articles */
/* will be marked as unread.  Called via M command */

void
delay_unmark(ap)
ARTICLE *ap;
{
    if (!(ap->flags & AF_YANKBACK)) {
	ap->flags |= AF_YANKBACK;
	dmcount++;
    }
}

/* mark article as read.  If article is cross referenced to other */
/* newsgroups, mark them read there also. */

void
mark_as_read()
{
    oneless(article_ptr(art));
    checkcount++;			/* get more worried about crashes */
    chase_xrefs(art,TRUE);
}

/* keep firstart pointing at the first unread article */

void
check_first(min)
ART_NUM min;
{
    if (min < absfirst)
	min = absfirst;
    if (min < firstart)
	firstart = min;
}

/* bring back articles marked with M */

void
yankback()
{
    register ARTICLE *ap;

    if (dmcount) {			/* delayed unmarks pending? */
	if (mode == 't')
	    sprintf(buf, "Returned %ld Marked article%s.",(long)dmcount,
		dmcount == 1 ? nullstr : "s");
	else
#ifdef VERBOSE
	    printf("\nReturning %ld Marked article%s...\n",(long)dmcount,
		dmcount == 1 ? nullstr : "s") FLUSH;
#endif
	for (art=absfirst, ap=article_ptr(art); art <= lastart; art++, ap++) {
	    if ((ap->flags & (AF_YANKBACK|AF_MISSING)) == AF_YANKBACK) {
		unmark_as_read();
		if (selected_only)
		    select_article(ap, 0);
		ap->flags &= ~AF_YANKBACK;
	    }
	}
	dmcount = 0;
    }
}
    
/* run down xref list and mark as read or unread */

#ifndef DBM_XREFS
int
chase_xrefs(artnum,markread)	/* The Xref-line-using version */
ART_NUM artnum;
int markread;
{
    bool valid_xref_site();
    register char *xartnum;
    register ART_NUM x;
    char *xref_buf, *curxref;
    char tmpbuf[128];

    xref_buf = fetchcache(artnum, XREF_LINE);
    if (!xref_buf || !*xref_buf)
	return 0;

    xref_buf = savestr(xref_buf);
# ifdef DEBUG
    if (debug & DEB_XREF_MARKER)
	printf("Xref: %s\n",xref_buf) FLUSH;
# endif
    curxref = cpytill(tmpbuf,xref_buf,' ') + 1;
    if (valid_xref_site(artnum,tmpbuf)) {
	while (*curxref) {		/* for each newsgroup */
	    curxref = cpytill(tmpbuf,curxref,' ');
	    xartnum = index(tmpbuf,':');
	    if (!xartnum)
		break;
	    *xartnum++ = '\0';
	    if (!(x = atol(xartnum)))
		continue;
	    if (strEQ(tmpbuf,ngname)) {/* is this the current newsgroup? */
		if (x < absfirst || x > lastart)
		    continue;
		if (markread)
		    oneless(article_ptr(x)); /* take care of old C newses */
#ifdef MCHASE
		else
		    onemore(article_ptr(x));
#endif
	    } else {
		if (markread) {
		    if (addartnum(x,tmpbuf))
			break;
		}
# ifdef MCHASE
		else
		    subartnum(x,tmpbuf);
# endif
	    }
	    while (*curxref && isspace(*curxref))
		curxref++;
	}
    }
    free(xref_buf);
    return 0;
}

/* Make sure the site name on Xref matches what inews thinks the site
 * is.  Check first against last inews_site.  If it matches, fine.
 * If not, fetch inews_site from current Path or Relay-Version line and
 * check again.  This is so that if the new administrator decides
 * to change the system name as known to inews, rn will still do
 * Xrefs correctly--each article need only match itself to be valid.
 */ 
bool
valid_xref_site(artnum, site)
ART_NUM artnum;
char *site;
{
    static char *inews_site = Nullch;
    char *sitebuf, *s;

    if (inews_site && strEQ(site,inews_site))
	return TRUE;

    if (inews_site)
	free(inews_site);
#ifndef ANCIENT_NEWS
    /* Grab the site from the first component of the Path line */
    sitebuf = fetchlines(artnum,PATH_LINE);
    if ((s = index(sitebuf, '!')) != Nullch) {
	*s = '\0';
	inews_site = savestr(sitebuf);
    }
#else /* ANCIENT_NEWS */
    /* Grab the site from the Posting-Version line */
    sitebuf = fetchlines(artnum,RVER_LINE);
    if ((s = instr(sitebuf,"; site ",TRUE)) != Nullch) {
	char *t = index(s+7, '.');
	if (t)
	    *t = '\0';
	inews_site = savestr(s+7);
    }
#endif /* ANCIENT_NEWS */
    else
	inews_site = savestr(nullstr);
    free(sitebuf);

    if (strEQ(site,inews_site))
	return TRUE;

#ifdef DEBUG
    if (debug)
	printf("Xref not from %s--ignoring\n",inews_site) FLUSH;
#endif
    return FALSE;
}

#else /* DBM_XREFS */

int
chase_xrefs(artnum,markread)		/* The DBM version */
ART_NUM artnum;
int markread;
{
    datum lhs, rhs;
    datum fetch();
    register char *idp;
    char *ident_buf;
    static FILE *hist_file = Nullfp;
    long pos;
    register char *xartnum;
    register ART_NUM x;
    char *xref_buf, *curxref;
    char tmpbuf[128];

    xref_buf = fetchcache(artnum, NGS_LINE);
    if (!xref_buf || !*xref_buf)
	return 0;

    xref_buf = safemalloc((MEM_SIZE)BUFSIZ);
    if (hist_file == Nullfp) {	/* Init. file accesses */
#ifdef DEBUG
	if (debug)
	    printf("chase_xref: opening files\n");
#endif
	dbminit(filexp(ARTFILE));
	if ((hist_file = fopen(filexp(ARTFILE), "r")) == Nullfp)
	    return 0;
    }
    ident_buf = fetchlines(artnum,MESSID_LINE);	/* get Message-ID */
#ifdef DEBUG
    if (debug)
	printf ("chase_xref: Message-ID: %s\n", ident_buf);
#endif
    
    if ((idp = index(ident_buf, '@')) != Nullch) {
	while (*++idp)			/* make message-id case insensitive */
	    if (isupper(*idp))
		*idp = tolower(*idp);
    }
    lhs.dptr = ident_buf;		/* look up article by id */
    lhs.dsize = strlen(lhs.dptr) + 1;
    rhs = fetch(lhs);			/* fetch the record */
    if (rhs.dptr == NULL)		/* if null, nothing there */
	goto wild_goose;
    bcopy(rhs.dptr,(char*)&pos, 4);
    fseek(hist_file, pos, 0);	/* datum returned is position in hist file */
    fgets(xref_buf, BUFSIZ, hist_file);
#ifdef DEBUG
    if (debug)
	printf ("Xref from history: %s\n", xref_buf);
#endif
    curxref = cpytill(tmpbuf, xref_buf, '\t') + 1;
    curxref = cpytill(tmpbuf, curxref, '\t') + 1;
#ifdef DEBUG
    if (debug)
	printf ("chase_xref: curxref: %s\n", curxref);
#endif
    while (*curxref) {			/* for each newsgroup */
	curxref = cpytill(tmpbuf,curxref,' ');
	xartnum = index(tmpbuf,'/');
	if (!xartnum)			/* probably an old-style Xref */
	    break;
	*xartnum++ = '\0';
	if (!(x = atol(xartnum)))
	    continue;
	if (strNE(tmpbuf,ngname)) {	/* not the current newsgroup? */
	    if (markread) {
		if (addartnum(x,tmpbuf))
		    goto wild_goose;
	    }
#ifdef MCHASE
	    else
		subartnum(x,tmpbuf);
#endif
	}
	while (*curxref && isspace(*curxref))
	    curxref++;
    }
  wild_goose:
    free(xref_buf);
    free(ident_buf);
    return 0;
}
#endif /* DBM_XREFS */
