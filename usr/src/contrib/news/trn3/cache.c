/* $Id: cache.c,v 3.0 1992/02/01 03:09:32 davison Trn $
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
#include "INTERN.h"
#include "cache.h"
#include "EXTERN.h"
#include "intrp.h"
#include "search.h"
#include "ng.h"
#include "trn.h"
#include "ngdata.h"
#include "term.h"
#include "final.h"
#include "artsrch.h"
#include "head.h"
#include "bits.h"
#include "rcstuff.h"
#include "hash.h"
#include "rthread.h"
#include "rt-ov.h"
#include "rt-select.h"
#include "rt-util.h"
#include "util.h"

#ifdef PENDING
#   ifdef ARTSEARCH
	COMPEX srchcompex;		/* compiled regex for searchahead */
#   endif
#endif

HASHTABLE *subj_hash = 0;

int subject_cmp _((char *,int,HASHDATUM));

void
cache_init()
{
    ;
}

NG_NUM cached_ng = -1;
time_t cached_time = 0;
ART_NUM cached_cnt = 0;
ART_NUM cached_absfirst = 0;

void
build_cache()
{
    if (cached_ng == ng && cached_absfirst == absfirst
     && time((time_t*)NULL) < cached_time + 6*60*60L) {
	grow_cache(lastart);
	rc_to_bits();
	thread_grow();
	return;
    }

    close_cache();

    cached_ng = ng;
    cached_absfirst = absfirst;
    cached_time = time((time_t*)NULL);
    cached_cnt = lastart-absfirst+2 + 5;
    article_list = (ARTICLE*)
	safemalloc((MEM_SIZE)(cached_cnt * sizeof (ARTICLE)));
    bzero((char*)article_list, cached_cnt * sizeof (ARTICLE));
    subj_hash = hashcreate(201, subject_cmp);	/*TODO: pick a better size */

    rc_to_bits();			/* sets firstart */
    first_cached = thread_always? absfirst : firstart;
    last_cached = first_cached-1;
    cached_all_in_range = FALSE;
#ifdef PENDING
    subj_to_get = xref_to_get = firstart;
#endif
#ifndef USE_NNTP
    setmissingbits();
#endif

    /* Cache as much data in advance as possible, possibly threading
    ** articles as we go. */
    thread_open();
}

#define FIXPTR(p) (!(p) || (p)<old_list || ((p)-old_list) > (lastart-absfirst)\
		   ? (p) : (article_list + ((p)-old_list)))

void
grow_cache(newlast)
ART_NUM newlast;
{
    ART_NUM new_cnt = newlast-absfirst+2;

    if (new_cnt > cached_cnt) {
	ARTICLE *old_list = article_list;
	new_cnt += 5;
	article_list = (ARTICLE*)saferealloc((char*)article_list,
		(MEM_SIZE)(new_cnt * sizeof (ARTICLE)));
	bzero((char*)(article_list+cached_cnt),
		(new_cnt-cached_cnt) * sizeof (ARTICLE));
	if (article_list != old_list) {
	    register ARTICLE *ap;
	    register SUBJECT *sp;
	    for (sp = first_subject; sp; sp = sp->next) {
		sp->thread = FIXPTR(sp->thread);
		sp->articles = FIXPTR(sp->articles);
		if (sp->thread) {
		    for (ap = sp->thread; ap; ap = bump_art(ap)) {
			ap->child1 = FIXPTR(ap->child1);
			ap->parent = FIXPTR(ap->parent);
			ap->sibling = FIXPTR(ap->sibling);
			ap->subj_next = FIXPTR(ap->subj_next);
		    }
		} else {
		    for (ap = sp->articles; ap; )
			ap = ap->subj_next = FIXPTR(ap->subj_next);
		}
	    }
	    artp = FIXPTR(artp);
	    curr_artp = FIXPTR(curr_artp);
	    recent_artp = FIXPTR(recent_artp);
	}
	cached_cnt = new_cnt;
    }
    cached_time = time((time_t*)NULL);
}

void
close_cache()
{
    SUBJECT *sp, *next;
    ARTICLE *ap;
    ART_NUM i;

    if (subj_hash) {
	hashdestroy(subj_hash);
	subj_hash = 0;
    }
    /* Free all the subjects. */
    for (sp = first_subject; sp; sp = next) {
	next = sp->next;
	free(sp->str);
	free((char*)sp);
    }
    first_subject = last_subject = Nullsubj;
    subject_count = 0;			/* just to be sure */
    parsed_art = 0;

    if (artptr_list) {
	free((char*)artptr_list);
	artptr_list = Null(ARTICLE**);
    }
    artptr = Null(ARTICLE**);
    thread_close();

    if (cached_cnt) {
	for (i = 0, ap = article_list; i < cached_cnt; i++, ap++)
	    clear_article(ap);
	free((char*)article_list);
	cached_cnt = 0;
    }
    cached_ng = -1;
}

/* The article has all it's data in place, so add it to the list of articles
** with the same subject.
*/
void
cache_article(ap)
register ARTICLE *ap;
{
    register ARTICLE *next, *ap2;

    if (!(next = ap->subj->articles) || ap->date < next->date)
	ap->subj->articles = ap;
    else {
	while ((next = (ap2 = next)->subj_next) && next->date <= ap->date)
	    ;
	ap2->subj_next = ap;
    }
    ap->subj_next = next;
    ap->flags |= AF_CACHED;

    if (!(ap->flags & AF_READ) ^ sel_rereading) {
	if (selected_only) {
	    if (ap->subj->flags & sel_mask) {
		ap->flags |= sel_mask;
		selected_count++;
	    }
	} else if (ap->subj->flags & SF_WASSELECTED)
	    select_article(ap, 0);
	else {
	    ap->subj->flags |= SF_VISIT;
	    if (sel_mode == SM_THREAD)
		ap->subj->thread->subj->flags |= SF_VISIT;
	}
    }
}

void
check_poster(ap)
register ARTICLE *ap;
{
    if (auto_select_postings && !(ap->flags & AF_MISSING)) {
	if (ap->flags & AF_FROMTRUNCED) {
	    strcpy(cmd_buf,realname);
	    if (strEQ(ap->from,compress_name(cmd_buf,16))) {
		untrim_cache = TRUE;
		fetchfrom(article_num(ap),FALSE);
		untrim_cache = FALSE;
	    }
	}
	if (!(ap->flags & AF_FROMTRUNCED)) {
	    if (instr(ap->from,phostname,FALSE)) {
		if (instr(ap->from,logname,TRUE))
		    select_subthread(ap,AF_AUTOSELECT);
		else {
#ifdef SLOW_BUT_COMPLETE_POSTER_CHECKING
		    char *reply_buf = fetchlines(article_num(ap),REPLY_LINE);
		    if (instr(reply_buf,logname,TRUE))
			select_subthread(ap,AF_AUTOSELECT);
		    free(reply_buf);
#endif
		}
	    }
	}
    }
}

/* The article turned out to be a duplicate, so remove it from the cached
** list and possibly destroy the subject (should only happen if the data
** was corrupt and the duplicate id got a different subject).
*/
void
uncache_article(ap, remove_empties)
register ARTICLE *ap;
bool_int remove_empties;
{
    register ARTICLE *next, *ap2;

    if (ap->subj) {
	if ((ap->flags & (AF_CACHED|AF_MISSING)) == AF_CACHED) {
	    if ((next = ap->subj->articles) == ap)
		ap->subj->articles = ap->subj_next;
	    else if (next) {
		while (next && (next = (ap2 = next)->subj_next) != ap)
		    ;
		ap2->subj_next = next;
	    }
	}
	if (remove_empties && !ap->subj->articles) {
	    register SUBJECT *sp = ap->subj;
	    if (sp == first_subject)
		first_subject = sp->next;
	    else
		sp->prev->next = sp->next;
	    if (sp == last_subject)
		last_subject = sp->prev;
	    else
		sp->next->prev = sp->prev;
	    free((char*)sp);
	    ap->subj = Nullsubj;
	    subject_count--;
	}
    }
    onemissing(ap);
}

/* get the header line from an article's cache or parse the article trying */

char *
fetchcache(artnum,which_line)
ART_NUM artnum;
int which_line;
{
    register char *s;
    register ARTICLE *ap;
    register bool cached = (htype[which_line].ht_flags & HT_CACHED);

    /* find_article() returns a Nullart if the artnum value is invalid */
    if (!(ap = find_article(artnum)) || (ap->flags & AF_MISSING))
	return nullstr;
    if (cached && (s=get_cached_line(ap,which_line,untrim_cache)) != Nullch)
	return s;
    if (!parseheader(artnum))
	return nullstr;
    if (cached && (s=get_cached_line(ap,which_line,untrim_cache)) != Nullch)
	return s;
    return Nullch;
}

/* Return a pointer to a cached header line for the indicated article.
** Truncated headers (e.g. from a .thread file) are optionally ignored.
*/
char *
get_cached_line(ap, which_line, no_truncs)
register ARTICLE *ap;
int which_line;
bool_int no_truncs;
{
    register char *s;

    switch (which_line) {
    case SUBJ_LINE:
	if (!ap->subj || (no_truncs && (ap->subj->flags & SF_SUBJTRUNCED)))
	    s = Nullch;
	else
	    s = ap->subj->str + ((ap->flags & AF_HAS_RE) ? 0 : 4);
	break;
    case FROM_LINE:
	if (no_truncs && (ap->flags & AF_FROMTRUNCED))
	    s = Nullch;
	else
	    s = ap->from;
	break;
#ifdef DBM_XREFS
    case NGS_LINE:
#else
    case XREF_LINE:
#endif
	s = ap->xrefs;
	break;
    case MESSID_LINE:
	s = ap->msgid;
	break;
    default:
	s = Nullch;
	break;
    }
    return s;
}

void
set_subj_line(ap, s, size)
register ARTICLE *ap;
register char *s;	/* not yet allocated, so we can tweak it first */
register int size;
{
    HASHDATUM data;
    SUBJECT *sp;
    char *s2, *subj_start = get_subject_start(s);

    if (s != subj_start) {
	size -= subj_start - s;
	ap->flags |= AF_HAS_RE;
    }
    if (ap->subj && strnEQ(ap->subj->str+4, subj_start, size))
	return;

    s2 = safemalloc(size + 4 + 1);
    strcpy(s2, "Re:");
    safecat(s2, subj_start, size+5);

    if (ap->subj) {
	/* This only happens when we freshen truncated subjects */
	hashdelete(subj_hash, ap->subj->str+4, strlen(ap->subj->str+4));
	free(ap->subj->str);
	ap->subj->str = s2;
	data.dat_ptr = (char*)ap->subj;
	hashstore(subj_hash, s2 + 4, size, data);
    } else {
	data = hashfetch(subj_hash, s2 + 4, size);
	if (!(sp = (SUBJECT*)data.dat_ptr)) {
	    sp = (SUBJECT*)safemalloc(sizeof (SUBJECT));
	    bzero((char*)sp, sizeof (SUBJECT));
	    subject_count++;
	    if ((sp->prev = last_subject) != NULL)
		sp->prev->next = sp;
	    else
		first_subject = sp;
	    last_subject = sp;
	    sp->str = s2;
	    sp->thread_link = sp;
	    sp->flags = SF_THREAD;

	    data.dat_ptr = (char*)sp;
	    hashstorelast(data);
	} else
	    free(s2);
	ap->subj = sp;
    }
}

void
set_cached_line(ap, which_line, s)
register ARTICLE *ap;
register int which_line;
register char *s;		/* already allocated, ready to save */
{
    /* SUBJ_LINE is handled specially above */
    switch (which_line) {
    case FROM_LINE:
	ap->flags &= ~AF_FROMTRUNCED;
	if (ap->from)
	    free(ap->from);
	ap->from = s;
	break;
#ifdef DBM_XREFS
    case NGS_LINE:
	if (ap->xrefs && ap->xrefs != nullstr)
	    free(ap->xrefs);
	if (!index(s, ',')) {	/* if no comma, no Xref! */
	    free(s);
	    s = nullstr;
	}
	ap->xrefs = s;
	break;
#else
    case XREF_LINE:
	if (ap->xrefs && ap->xrefs != nullstr)
	    free(ap->xrefs);
# ifdef USE_NNTP
	if (strEQ(s, "(none)")) {
	    free(s);
	    s = nullstr;
	}
# endif
	ap->xrefs = s;
	break;
#endif
    case MESSID_LINE:
	if (ap->msgid)
	    free(ap->msgid);
	ap->msgid = s;
	break;
    }
}

int
subject_cmp(key, keylen, data)
char *key;
int keylen;
HASHDATUM data;
{
    /* We already know that the lengths are equal, just compare the strings */
    return bcmp(key, ((SUBJECT*)data.dat_ptr)->str+4, keylen);
}

/* see what we can do while they are reading */

#ifdef PENDING
void
look_ahead()
{
#ifdef ARTSEARCH
    register char *h, *s;

#ifdef DEBUG
    if (debug && srchahead) {
	printf("(%ld)",(long)srchahead);
	fflush(stdout);
    }
#endif
#endif

    if (ThreadedGroup) {
	artp = curr_artp;
	inc_art(selected_only,FALSE);
	if (artp)
	    parseheader(art);
    }
    else
#ifdef ARTSEARCH
    if (srchahead && srchahead < art) {	/* in ^N mode? */
	char *pattern;

	pattern = buf+1;
	strcpy(pattern,": *");
	h = pattern + strlen(pattern);
	interp(h,(sizeof buf) - (h-buf),"%\\s");
	{			/* compensate for notesfiles */
	    register int i;
	    for (i = 24; *h && i--; h++)
		if (*h == '\\')
		    h++;
	    *h = '\0';
	}
#ifdef DEBUG
	if (debug & DEB_SEARCH_AHEAD) {
	    fputs("(hit CR)",stdout);
	    fflush(stdout);
	    gets(buf+128);
	    printf("\npattern = %s\n",pattern);
	}
#endif
	if ((s = compile(&srchcompex,pattern,TRUE,TRUE)) != Nullch) {
				    /* compile regular expression */
	    printf("\n%s\n",s) FLUSH;
	    srchahead = 0;
	}
	if (srchahead) {
	    srchahead = art;
	    for (;;) {
		srchahead++;	/* go forward one article */
		if (srchahead > lastart) { /* out of articles? */
#ifdef DEBUG
		    if (debug)
			fputs("(not found)",stdout);
#endif
		    break;
		}
		if (!was_read(srchahead) &&
		    wanted(&srchcompex,srchahead,0)) {
				    /* does the shoe fit? */
#ifdef DEBUG
		    if (debug)
			printf("(%ld)",(long)srchahead);
#endif
		    parseheader(srchahead);
		    break;
		}
		if (input_pending())
		    break;
	    }
	    fflush(stdout);
	}
    }
    else
#endif /* ARTSEARCH */
    {
	if (art+1 <= lastart)		/* how about a pre-fetch? */
	    parseheader(art+1);		/* look for the next article */
    }
}
#endif /* PENDING */

/* see what else we can do while they are reading */

void
cache_until_key()
{
#ifdef PENDING
    if (!in_ng || input_pending())
	return;

    untrim_cache = TRUE;
    sentinel_artp = curr_artp;

    /* Prioritize our caching based on what mode we're in */
    if (mode == 't') {
	if (cache_subjects())
	    if (cache_xrefs())
		if (ThreadedGroup)
		    cache_all_arts();
		else
		    cache_unread_arts();
    } else {
	if (!ThreadedGroup || cache_all_arts())
	    if (cache_subjects())
		if (cache_unread_arts())
		    cache_xrefs();
    }

    setspin(SPIN_OFF);
    untrim_cache = FALSE;
#endif
}

#ifdef PENDING
bool
cache_subjects()
{
    register ARTICLE *ap;
    register ART_NUM an;

    if (subj_to_get > lastart)
	return TRUE;
    setspin(SPIN_BACKGROUND);
    for (an = subj_to_get, ap = article_ptr(an); an <= lastart; ap++, an++) {
	if (input_pending())
	    break;
	if (!(ap->flags & AF_READ))
	    fetchsubj(an,FALSE);
    }
    subj_to_get = an;
    return subj_to_get > lastart;
}

bool
cache_xrefs()
{
    register ARTICLE *ap;
    register ART_NUM an;

    if (olden_days || xref_to_get > lastart)
	return TRUE;
    setspin(SPIN_BACKGROUND);
    for (an = xref_to_get, ap = article_ptr(an); an <= lastart; ap++, an++) {
	if (input_pending())
	    break;
	if (!(ap->flags & AF_READ))
	    fetchxref(an,FALSE);
    }
    xref_to_get = an;
    return xref_to_get > lastart;
}

bool
cache_all_arts()
{
    if (!cached_all_in_range)
	last_cached = first_cached-1;
    if (last_cached >= lastart && first_cached <= absfirst)
	return TRUE;

    /* turn it on as late as possible to avoid fseek()ing openart */
    setspin(SPIN_BACKGROUND);
    if (last_cached < lastart) {
	if (!art_data(last_cached+1, lastart, TRUE, TRUE))
	    return FALSE;
	cached_all_in_range = TRUE;
    }
    if (first_cached > absfirst) {
#ifdef USE_OV
	if (ov_opened)
	    ov_data(absfirst, first_cached-1, TRUE);
	else
#endif
	    art_data(absfirst, first_cached-1, TRUE, TRUE);
	/* If we got interrupted, make a quick exit */
	if (first_cached > absfirst)
	    return FALSE;
    }
    /* We're all done threading the group, so if the current article is
    ** still in doubt, tell them it's missing. */
    if (curr_artp && !(curr_artp->flags & AF_CACHED) && !input_pending())
	pushchar('\f' | 0200);
    return TRUE;
}

bool
cache_unread_arts()
{
    if (last_cached >= lastart)
	return TRUE;
    setspin(SPIN_BACKGROUND);
    return art_data(last_cached+1, lastart, TRUE, FALSE);
}
#endif

bool
art_data(first, last, cheating, all_articles)
ART_NUM first, last;
bool_int cheating;
bool_int all_articles;
{
    register ARTICLE *ap;
    register ART_NUM i;
    int cachemask = (ThreadedGroup ? AF_THREADED : AF_CACHED)
		  + (all_articles? 0 : AF_READ);

    setspin(cheating? SPIN_BACKGROUND : SPIN_FOREGROUND);
    assert(first >= absfirst && last <= lastart);
    for (i = first, ap = article_ptr(first); i <= last; i++, ap++) {
	if (ap->flags & cachemask)
	    continue;
	if (int_count) {
	    int_count = 0;
	    break;
	}
	if (cheating) {
	    if (input_pending())
		break;
	    /* If the current article is no longer a '?', let them know. */
	    if (curr_artp != sentinel_artp) {
		pushchar('\f' | 0200);
		break;
	    }
	}
	/* This parses the header which will cache/thread the article */
	(void) parseheader(i);
    }
    setspin(SPIN_POP);
    if (--i > last_cached)
	last_cached = i;
    if (i == last) {
	if (first < first_cached)
	    first_cached = first;
	return TRUE;
    }
    return FALSE;
}

bool
cache_range(first,last)
ART_NUM first;
ART_NUM last;
{
    bool success = TRUE;
    bool all_arts = (sel_rereading || thread_always);
    ART_NUM count = 0;

    if (sel_rereading && !cached_all_in_range) {
	first_cached = first;
	last_cached = first-1;
    }
    if (first < first_cached)
	count = first_cached-first;
    if (last > last_cached)
	count += last-last_cached;
    if (!count)
	return TRUE;

    if (first_cached > last_cached) {
	if (sel_rereading) {
	    if (first_subject)
		count -= toread[ng];
	} else if (first == firstart && last == lastart && !all_arts)
	    count = toread[ng];
    }

    printf("\n%sing %ld article%s.", ThreadedGroup? "Thread" : "Cach",
	   (long)count, count==1? nullstr : "s") FLUSH;

    setspin(SPIN_FOREGROUND);

    if (first < first_cached) {
#ifdef USE_OV
	if (ov_opened) {
	    ov_data(absfirst,first_cached-1,FALSE);
	    if ((success = (first_cached == absfirst)) != FALSE)
		ov_close();
	} else
#endif
	{
	    success = art_data(first, first_cached-1, FALSE, all_arts);
	    cached_all_in_range = (all_arts && success);
	}
    }
    if (success && last_cached < last) {
	success = art_data(last_cached+1, last, FALSE, all_arts);
	cached_all_in_range = (all_arts && success);
    }
    setspin(SPIN_POP);
    return success;
}

void
clear_article(ap)
register ARTICLE *ap;
{
    if (ap->from)
	free(ap->from);
    if (ap->msgid)
	free(ap->msgid);
    if (ap->xrefs && ap->xrefs != nullstr)
	free(ap->xrefs);
}
