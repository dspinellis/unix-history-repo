/* $Id: head.c,v 3.0 1992/02/23 21:25:39 davison Trn $
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
#include "artio.h"
#include "cache.h"
#include "ng.h"
#include "ngdata.h"
#include "util.h"
#include "hash.h"
#include "rthread.h"
#include "rt-process.h"
#include "rt-util.h"
#include "final.h"
#include "nntp.h"
#include "INTERN.h"
#include "head.h"

bool first_one;		/* is this the 1st occurance of this header line? */

static char htypeix[26] =
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

void
head_init()
{
    register int i;

    for (i=HEAD_FIRST+1; i<HEAD_LAST; i++)
	htypeix[*htype[i].ht_name - 'a'] = i;

    headbuf_size = LBUFLEN * 8;
    headbuf = safemalloc(headbuf_size);
}

#ifdef DEBUG
void
dumpheader(where)
char *where;
{
    register int i;

    printf("header: %d %s", parsed_art, where);

    for (i=0; i<HEAD_LAST; i++) {
	printf("%15s %4d %4d %03o\n",htype[i].ht_name,
	    htype[i].ht_minpos,
	    htype[i].ht_maxpos,
	    htype[i].ht_flags) FLUSH;
    }
}
#endif

int
set_line_type(bufptr,colon)
char *bufptr;
register char *colon;
{
    char lc[LONGKEY+3];
    register char *t, *f;
    register int i, len;

    if (colon-bufptr > LONGKEY+2)
	return SOME_LINE;

    for (t=lc,f=bufptr; f<colon; f++, t++) {
	if (isspace(*f))
	/* guard against space before : */
	    break;
	*t = isupper(*f) ? tolower(*f) : *f;
    }
    *t = '\0';
    f = lc;				/* get lc into register */
    len = t - f;

    /* now scan the headtype table, backwards so we don't have to supply an
     * extra terminating value, using first letter as index, and length as
     * optimization to avoid calling subroutine strEQ unnecessarily.  Hauls.
     */
    
    if (islower(*f)) {
	for (i = htypeix[*f - 'a']; *htype[i].ht_name == *f; --i) {
	    if (len == htype[i].ht_length && strEQ(f, htype[i].ht_name)) {
		return i;
	    }
	}
    }
    return SOME_LINE;
}

void
start_header(artnum)
ART_NUM artnum;
{
    register int i;

#ifdef DEBUG
    if (debug & DEB_HEADER)
	dumpheader("start_header\n");
#endif
    for (i=0; i<HEAD_LAST; i++) {
	htype[i].ht_minpos = -1;
	htype[i].ht_maxpos = 0;
    }
    in_header = SOME_LINE;
    first_one = FALSE;
    parsed_art = artnum;
}

void
end_header_line()
{
    if (first_one) {		/* did we just pass 1st occurance? */
	first_one = FALSE;
	/* remember where line left off */
	htype[in_header].ht_maxpos = artpos;
	if (htype[in_header].ht_flags & HT_CACHED) {
	    ARTICLE *ap = article_ptr(parsed_art);
	    if (!get_cached_line(ap, in_header, TRUE)) {
		int start = htype[in_header].ht_minpos
			  + htype[in_header].ht_length + 1;
		MEM_SIZE size;
		char *s;
		while (headbuf[start] == ' ' || headbuf[start] == '\t')
		    start++;
		size = artpos - start + 1 - 1;	/* pre-strip newline */
		if (in_header == SUBJ_LINE)
		    set_subj_line(ap,headbuf+start,size-1);
		else {
		    s = safemalloc(size);
		    *s = '\0';
		    safecat(s,headbuf+start,size);
		    set_cached_line(ap,in_header,s);
		}
	    }
	}
    }
}

bool
parseline(art_buf,newhide,oldhide)
char *art_buf;
int newhide, oldhide;
{
    if (*art_buf == ' ' || *art_buf == '\t') {
					/* header continuation line? */
	return oldhide;
    } else {				/* maybe another header line */
	char *s;
	end_header_line();
	s = index(art_buf,':');
	if (s == Nullch)	/* is it the end of the header? */
	    in_header = PAST_HEADER;
	else {	/* it is a new header line */
	    in_header = set_line_type(art_buf,s);
	    first_one = (htype[in_header].ht_minpos < 0);
	    if (first_one) {
		htype[in_header].ht_minpos = artpos;
		if (in_header == DATE_LINE) {
		    register ARTICLE *ap = article_ptr(parsed_art);
		    if (!ap->date)
			ap->date = parsedate(art_buf+6);
		}
	    }
#ifdef DEBUG
	    if (debug & DEB_HEADER)
		dumpheader(art_buf);
#endif
	    if (htype[in_header].ht_flags & HT_HIDE)
		return newhide;
	}
    }
    return FALSE;			/* don't hide this line */
}

void
end_header()
{
    register ARTICLE *ap = article_ptr(parsed_art);

    end_header_line();
    in_header = PAST_HEADER;	/* just to be sure */

    if (!ap->subj) {
	uncache_article(ap,FALSE);
	return;
    }

#ifndef DBM_XREFS
    if (htype[XREF_LINE].ht_minpos < 0)
	article_ptr(parsed_art)->xrefs = nullstr;
#endif
#ifdef USE_NNTP
    htype[PAST_HEADER].ht_minpos = artpos;    /* remember where body starts */
#else
    htype[PAST_HEADER].ht_minpos = ftell(artfp);
#endif

    if (ThreadedGroup && !(ap->flags & AF_THREADED)) {
	if (valid_article(ap)) {
	    ARTICLE *artp_hold = artp;
	    references = fetchlines(parsed_art, REFS_LINE);
	    thread_article(ap);
	    free(references);
	    artp = artp_hold;
	}
    } else if (!(ap->flags & AF_CACHED))
	cache_article(ap);
    check_poster(ap);
}

/* read the header into memory and parse it if we haven't already */

bool
parseheader(artnum)
ART_NUM artnum;
{
    register char *bp;
    register int len;

    if (parsed_art == artnum)
	return TRUE;
    if (artnum > lastart)
	return FALSE;
    spin(20);
#ifdef USE_NNTP
    if (!nntp_header(artnum)) {
	uncache_article(article_ptr(artnum),FALSE);
	return FALSE;
    }
#else
    if (!artopen(artnum))
	return FALSE;
#endif
    start_header(artnum);
    artpos = 0;
    bp = headbuf;
    while (in_header) {
	if (headbuf_size < artpos + LBUFLEN) {
	    headbuf_size += LBUFLEN * 4;
	    headbuf = saferealloc(headbuf,headbuf_size);
	}
#ifdef USE_NNTP
	nntp_gets(bp,LBUFLEN);
	if (*bp == '.') {
	    if (!bp[1]) {
		*bp++ = '\n';		/* tag the end with an empty line */
		artpos++;
		break;
	    }
	    strcpy(bp,bp+1);
	}
	len = strlen(bp);
	bp[len++] = '\n';
	bp[len] = '\0';
#else
	if (fgets(bp,LBUFLEN,artfp) == Nullch)
	    break;
	len = strlen(bp);
#endif
	parseline(bp,FALSE,FALSE);
	artpos += len;
	bp += len;
    }
    *bp = '\0';   /* this probably isn't needed */
    end_header();
    return TRUE;
}

/* get a header line from an article */

char *
fetchlines(artnum,which_line)
ART_NUM artnum;				/* article to get line from */
int which_line;				/* type of line desired */
{
    char *s, *t;
    register ART_POS firstpos;
    register ART_POS lastpos;
    int size;

    s = fetchcache(artnum,which_line);
    if (s)
	return savestr(s);
    if ((firstpos = htype[which_line].ht_minpos) < 0)
	return savestr(nullstr);

    firstpos += htype[which_line].ht_length + 1;
    lastpos = htype[which_line].ht_maxpos;
    size = lastpos - firstpos;
#ifdef DEBUG
    if (debug && (size < 1 || size > 1000)) {
	printf("Firstpos = %ld, lastpos = %ld\n",(long)firstpos,(long)lastpos);
	gets(cmd_buf);
    }
#endif
    s = safemalloc((MEM_SIZE)size);
    t = headbuf + firstpos;
    while (*t == ' ' || *t == '\t') t++;
    *s = '\0';
    safecat(s,t,size);
    return s;
}

/* prefetch a header line from one or more articles */

#ifdef USE_NNTP
char *
prefetchlines(artnum,which_line,copy)	/* NNTP version */
ART_NUM artnum;				/* article to get line from */
int which_line;				/* type of line desired */
bool_int copy;				/* do you want it savestr()ed? */
{
    register ARTICLE *ap;
    register char *s, *t;
    int size;
    register ART_NUM num, priornum, lastnum;
    bool cached = (htype[which_line].ht_flags & HT_CACHED);

    /* find_article() returns a Nullart if the artnum value is invalid */
    if (!(ap = find_article(artnum)) || (ap->flags & AF_MISSING))
	s = nullstr;
    else if (cached)
	s = get_cached_line(ap, which_line, untrim_cache);
    else
	s = Nullch;
    if (s) {
	if (copy)
	    s = savestr(s);
	return s;
    }

    spin(20);
    if (copy)
	s = safemalloc((MEM_SIZE)(size = LBUFLEN));
    else {
	s = cmd_buf;
	size = sizeof cmd_buf;
    }
    *s = '\0';
    priornum = artnum-1;
    lastnum = artnum + PREFETCH_SIZE - 1;
    if (lastnum > lastart)
	lastnum = lastart;
    if (cached)
	sprintf(ser_line,"XHDR %s %ld-%ld",htype[which_line].ht_name,
		artnum,lastnum);
    else
	sprintf(ser_line,"XHDR %s %ld",htype[which_line].ht_name,artnum);
    nntp_command(ser_line);
    if (nntp_check(TRUE) == NNTP_CLASS_OK) {
	for (;;) {
	    nntp_gets(ser_line, sizeof ser_line);
# ifdef DEBUG
	    if (debug & DEB_NNTP)
		printf("<%s\n", ser_line) FLUSH;
# endif
	    if (ser_line[0] == '.')
		break;
	    if ((t = index(ser_line, '\r')) != Nullch)
		*t = '\0';
	    if (!(t = index(ser_line, ' ')))
		continue;
	    t++;
	    num = atol(ser_line);
	    if (num < artnum || num > lastnum)
		continue;
	    while (++priornum < num)
		uncache_article(ap++,FALSE);
	    if (which_line == SUBJ_LINE)
		set_subj_line(ap++, t, strlen(t));
	    else if (cached)
		set_cached_line(ap++, which_line, savestr(t));
	    if (num == artnum)
		safecat(s,t,size);
	}
    } else {
	fprintf(stderr,"\nUnexpected close of server socket.\n");
	finalize(1);
    }
    while (priornum++ < lastnum)
	uncache_article(ap++,FALSE);
    if (copy)
	s = saferealloc(s, (MEM_SIZE)strlen(s)+1);
    return s;
}

#else  /* !USE_NNTP */
char *
prefetchlines(artnum,which_line,copy)
ART_NUM artnum;				/* article to get line from */
int which_line;				/* type of line desired */
bool_int copy;				/* do you want it savestr()ed? */
{
    char *s, *t;
    register ART_POS firstpos;
    register ART_POS lastpos;
    int size;

    if (copy)
	return fetchlines(artnum,which_line);

    s = fetchcache(artnum,which_line);
    if (s)
	return s;
    if ((firstpos = htype[which_line].ht_minpos) < 0)
	return nullstr;

    firstpos += htype[which_line].ht_length + 1;
    lastpos = htype[which_line].ht_maxpos;
    size = lastpos - firstpos;
    t = headbuf + firstpos;
    while (*t == ' ' || *t == '\t') t++;
    *cmd_buf = '\0';
    safecat(cmd_buf,t,CBUFLEN);		/* hope this is okay--we're */
    return cmd_buf;			/* really scraping for space here */
}
#endif /* !USE_NNTP */
