/* $Id: rthread.c,v 3.0 1992/12/14 00:14:13 davison Trn $
*/

#include "EXTERN.h"
#include "common.h"
#include "intrp.h"
#include "trn.h"
#include "cache.h"
#include "bits.h"
#include "ng.h"
#include "rcln.h"
#include "search.h"
#include "artstate.h"
#include "rcstuff.h"
#include "ngdata.h"
#include "kfile.h"
#include "head.h"
#include "util.h"
#include "hash.h"
#include "nntp.h"
#include "rt-mt.h"
#include "rt-ov.h"
#include "rt-page.h"
#include "rt-process.h"
#include "rt-select.h"
#include "rt-wumpus.h"
#include "INTERN.h"
#include "rthread.h"

HASHTABLE *msgid_hash = 0;

bool try_ov = FALSE;
bool try_mt = FALSE;

void
thread_init()
{
#ifdef USE_OV
    try_ov = ov_init();
#endif
#ifdef USE_MT
    try_mt = mt_init();
#endif
}

/* Generate the thread data we need for this group.  We must call
** thread_close() before calling this again.
*/
void
thread_open()
{
    if (!msgid_hash)
	msgid_hash = hashcreate(201, msgid_cmp); /*TODO: pick a better size */
    if (ThreadedGroup) {
	/* Parse input and use msgid_hash for quick article lookups. */
	/* If cached but not threaded articles exist, set up to thread them. */
	if (first_subject) {
	    first_cached = firstart;
	    last_cached = firstart - 1;
	    parsed_art = 0;
	}
    }

    if (sel_mode == SM_ARTICLE)
	set_selector(sel_mode, sel_artsort);
    else
	set_selector(sel_threadmode, sel_threadsort);

#ifdef USE_MT
    if (try_mt && !first_subject)
	if (!mt_data())
	    return;
#endif
#ifdef USE_OV
    if (try_ov && first_cached > last_cached)
	if (thread_always)
	    (void) ov_data(absfirst, lastart, FALSE);
	else if (firstart > lastart) {
	    /* If no unread articles, see if ov. exists as quick as possible */
	    (void) ov_data(absfirst, absfirst, FALSE);
	    first_cached = last_cached+1;
	} else
	    (void) ov_data(firstart, lastart, FALSE);
#endif
#ifdef USE_NNTP
    if (!ov_opened)
	setmissingbits();
#endif

    if (last_cached > lastart) {
	toread[ng] += (ART_UNREAD)(last_cached-lastart);
	/* ensure getngsize() knows the new maximum */
	ngmax[ng] = lastart = last_cached;
    }
    thread_grow();	/* thread any new articles not yet in the database */
    added_articles = 0;
    sel_page_sp = 0;
    sel_page_app = 0;
}

/* Update the group's thread info.
*/
void
thread_grow()
{
    added_articles = lastart - last_cached;
    if (added_articles > 0 && thread_always)
	cache_range(last_cached + 1, lastart);
    count_subjects(CS_NORM);
    if (artptr_list)
	sort_articles();
    else
	sort_subjects();
}

static void
kill_tmp_arts(data, extra)
HASHDATUM *data;
int extra;
{
    register ARTICLE *ap = (ARTICLE*)data->dat_ptr;

    if (ap)
	free((char*)ap);
}

void
thread_close()
{
    curr_artp = artp = Nullart;
    init_tree();			/* free any tree lines */

    if (msgid_hash) {
	hashwalk(msgid_hash, kill_tmp_arts, 0);
	hashdestroy(msgid_hash);
	msgid_hash = 0;
    }
    sel_page_sp = 0;
    sel_page_app = 0;
    sel_last_ap = 0;
    sel_last_sp = 0;
    selected_only = FALSE;
#ifdef USE_OV
    ov_close();
#endif
}

ARTICLE *
find_article(artnum)
ART_NUM artnum;
{
    if (artp && (artp->flags & AF_TMPMEM) == AF_TMPMEM && !artnum)
	return artp;
    if (artnum < absfirst || artnum > lastart)
	return (artp = Nullart);
    return (artp = article_ptr(artnum));
}

void
top_article()
{
    art = lastart+1;
    artp = Nullart;
    inc_art(selected_only, FALSE);
}

ARTICLE *
first_art(sp)
register SUBJECT *sp;
{
    register ARTICLE *ap = (ThreadedGroup? sp->thread : sp->articles);
    if (ap && (ap->flags & AF_MISSING))
	ap = next_art(ap);
    return ap;
}

ARTICLE *
last_art(sp)
register SUBJECT *sp;
{
    register ARTICLE *ap;

    if (!ThreadedGroup) {
	ap = sp->articles;
	while (ap->subj_next)
	    ap = ap->subj_next;
	return ap;
    }

    ap = sp->thread;
    if (ap) {
	for (;;) {
	    if (ap->sibling)
		ap = ap->sibling;
	    else if (ap->child1)
		ap = ap->child1;
	    else
		break;
	}
	if (ap->flags & AF_MISSING)
	    ap = prev_art(ap);
    }
    return ap;
}

/* Bump art/artp to the next article, wrapping from thread to thread.
** If sel_flag is TRUE, only stops at selected articles.
** If rereading is FALSE, only stops at unread articles.
*/
void
inc_art(sel_flag, rereading)
bool_int sel_flag, rereading;
{
    register ARTICLE *ap = artp;
    int subj_mask = (sel_mode == SM_THREAD? (SF_THREAD|SF_VISIT) : SF_VISIT);

    /* Use the explicit article-order if it exists */
    if (artptr_list) {
	ARTICLE **limit = artptr_list + article_count;
	if (!ap)
	    artptr = artptr_list-1;
	else if (!artptr || *artptr != ap) {
	    for (artptr = artptr_list; artptr < limit; artptr++) {
		if (*artptr == ap)
		    break;
	    }
	}
	do {
	    if (++artptr >= limit)
		break;
	    ap = *artptr;
	} while ((!rereading && (ap->flags & AF_READ))
	      || (sel_flag && !(ap->flags & AF_SEL)));
	if (artptr < limit) {
	    artp = *artptr;
	    art = article_num(artp);
	} else {
	    artp = Nullart;
	    art = lastart+1;
	    artptr = artptr_list;
	}
	return;
    }

    /* Use subject- or thread-order when possible */
    if (ThreadedGroup || srchahead) {
	register SUBJECT *sp;
	bool try_this_group_again = !rereading;
	if (ap)
	    sp = ap->subj;
	else
	    for (sp = first_subject; sp && (sp->flags&subj_mask) != subj_mask;)
		sp = sp->next;
	if (!sp)
	    goto num_inc;
	do {
	    if (ap)
		ap = next_art(ap);
	    else
		ap = first_art(sp);
	    while (!ap) {
		if (try_this_group_again)
		    try_this_group_again = FALSE;
		else {
		    while ((sp = sp->next) != Nullsubj
		        && (sp->flags & subj_mask) != subj_mask)
			;
		    if (!sp)
			break;
		}
		ap = first_art(sp);
	    }
	} while (ap && ((!rereading && (ap->flags & AF_READ))
		     || (sel_flag && !(ap->flags & AF_SEL))));
	if ((artp = ap) != Nullart)
	    art = article_num(ap);
	else
	    art = lastart+1;
	return;
    }

    /* Otherwise, just increment through the art numbers */
  num_inc:
    if (!ap) {
	art = firstart-1;
	ap = article_ptr(art);
    }
    do {
	if (++art > lastart) {
	    ap = Nullart;
	    break;
	}
	ap++;
    } while ((!rereading && (ap->flags & AF_READ))
	  || (sel_flag && !(ap->flags & AF_SEL))
	  || (ap->flags & AF_MISSING));
    artp = ap;
}

/* Bump art/artp to the previous article, wrapping from thread to thread.
** If sel_flag is TRUE, only stops at selected articles.
** If rereading is FALSE, only stops at unread articles.
*/
void
dec_art(sel_flag, rereading)
bool_int sel_flag, rereading;
{
    register ARTICLE *ap = artp;
    int subj_mask = (sel_mode == SM_THREAD? (SF_THREAD|SF_VISIT) : SF_VISIT);

    /* Use the explicit article-order if it exists */
    if (artptr_list) {
	ARTICLE **limit = artptr_list + article_count;
	if (!ap)
	    artptr = limit;
	else if (!artptr || *artptr != ap) {
	    for (artptr = artptr_list; artptr < limit; artptr++) {
		if (*artptr == ap)
		    break;
	    }
	}
	do {
	    if (artptr == artptr_list)
		break;
	    ap = *--artptr;
	} while ((!rereading && (ap->flags & AF_READ))
	      || (sel_flag && !(ap->flags & AF_SEL)));
	artp = *artptr;
	art = article_num(artp);
	return;
    }

    /* Use subject- or thread-order when possible */
    if (ThreadedGroup || srchahead) {
	register SUBJECT *sp;
	if (ap)
	    sp = ap->subj;
	else
	    for (sp = last_subject; sp && (sp->flags&subj_mask) != subj_mask;)
		sp = sp->prev;
	if (!sp)
	    goto num_dec;
	do {
	    if (ap)
		ap = prev_art(ap);
	    else
		ap = last_art(sp);
	    while (!ap) {
		while ((sp = sp->prev) != Nullsubj
		    && (sp->flags & subj_mask) != subj_mask)
		    ;
		if (!sp)
		    break;
		ap = last_art(sp);
	    }
	} while (ap && ((!rereading && (ap->flags & AF_READ))
		     || (sel_flag && !(ap->flags & AF_SEL))));
	if ((artp = ap) != Nullart)
	    art = article_num(ap);
	else
	    art = absfirst-1;
	return;
    }

    /* Otherwise, just decrement through the art numbers */
  num_dec:
    ap = article_ptr(art);
    do {
	if (--art < absfirst) {
	    ap = Nullart;
	    break;
	}
	ap--;
    } while ((!rereading && (ap->flags & AF_READ))
	  || (sel_flag && !(ap->flags & AF_SEL))
	  || (ap->flags & AF_MISSING));
    artp = ap;
}

/* Bump the param to the next article in depth-first order.
*/
ARTICLE *
bump_art(ap)
register ARTICLE *ap;
{
    if (ap->child1)
	return ap->child1;
    while (!ap->sibling) {
	if (!(ap = ap->parent))
	    return Nullart;
    }
    return ap->sibling;
}

/* Bump the param to the next REAL article.  Uses subject order in a
** non-threaded group; honors the breadth_first flag in a threaded one.
*/
ARTICLE *
next_art(ap)
register ARTICLE *ap;
{
try_again:
    if (!ThreadedGroup) {
	ap = ap->subj_next;
	goto done;
    }
    if (breadth_first) {
	if (ap->sibling) {
	    ap = ap->sibling;
	    goto done;
	}
	if (ap->parent)
	    ap = ap->parent->child1;
	else
	    ap = ap->subj->thread;
    }
    do {
	if (ap->child1) {
	    ap = ap->child1;
	    goto done;
	}
	while (!ap->sibling) {
	    if (!(ap = ap->parent))
		return Nullart;
	}
	ap = ap->sibling;
    } while (breadth_first);
done:
    if (ap && (ap->flags & AF_MISSING))
	goto try_again;
    return ap;
}

/* Bump the param to the previous REAL article.  Uses subject order in a
** non-threaded group.
*/
ARTICLE *
prev_art(ap)
register ARTICLE *ap;
{
    register ARTICLE *initial_ap;

try_again:
    initial_ap = ap;
    if (!ThreadedGroup) {
	if ((ap = ap->subj->articles) == initial_ap)
	    ap = Nullart;
	else
	    while (ap->subj_next != initial_ap)
		ap = ap->subj_next;
	goto done;
    }
    ap = (ap->parent ? ap->parent->child1 : ap->subj->thread);
    if (ap == initial_ap) {
	ap = ap->parent;
	goto done;
    }
    while (ap->sibling != initial_ap)
	ap = ap->sibling;
    while (ap->child1) {
	ap = ap->child1;
	while (ap->sibling)
	    ap = ap->sibling;
    }
done:
    if (ap && (ap->flags & AF_MISSING))
	goto try_again;
    return ap;
}

/* Find the next art/artp with the same subject as this one.  Returns
** FALSE if no such article exists.
*/
bool
next_art_with_subj()
{
    register ARTICLE *ap = artp;
    register SUBJECT *sp;
    bool try_this_subj_again = TRUE;

    if (!ap)
	return FALSE;
    sp = ap->subj;

    do {
	ap = ap->subj_next;
	if (!ap) {
	    if (try_this_subj_again) {
		ap = sp->articles;
		try_this_subj_again = FALSE;
	    } else {
		if (!art)
		    art = firstart;
		return FALSE;
	    }
	}
    } while ((ap->flags & (AF_READ|AF_MISSING))
	  || (selected_only && !(ap->flags & AF_SEL)));
    artp = ap;
    art = article_num(ap);
#ifdef ARTSEARCH
    srchahead = -1;
#endif
    return TRUE;
}

/* Find the previous art/artp with the same subject as this one.  Returns
** FALSE if no such article exists.
*/
bool
prev_art_with_subj()
{
    register ARTICLE *ap = artp, *ap2;
    register SUBJECT *sp;
    bool try_this_subj_again = TRUE;

    if (!ap)
	return FALSE;
    sp = ap->subj;

    do {
	ap2 = ap->subj->articles;
	if (ap2 == ap)
	    ap = Nullart;
	else {
	    while (ap2 && ap2->subj_next != ap)
		ap2 = ap2->subj_next;
	    ap = ap2;
	}
	if (!ap) {
	    if (try_this_subj_again) {
		ap = sp->articles;
		while (ap->subj_next)
		    ap = ap->subj_next;
		try_this_subj_again = FALSE;
	    } else {
		if (!art)
		    art = lastart;
		return FALSE;
	    }
	}
    } while ((ap->flags & (AF_READ|AF_MISSING))
	  || (selected_only && !(ap->flags & AF_SEL)));
    artp = ap;
    art = article_num(ap);
    return TRUE;
}

/* Select a single article.
*/
void
select_article(ap, sel_flags)
register ARTICLE *ap;
int sel_flags;
{
    int desired_flags = (sel_rereading? AF_READ : 0);
#ifdef VERBOSE
    bool echo;

    if (sel_flags & AF_ECHO) {
	echo = TRUE;
	sel_flags &= ~AF_ECHO;
    } else
	echo = FALSE;
#else
    sel_flags &= ~AF_ECHO;
#endif
    if (sel_flags & (AF_AUTOSELECT|AF_AUTOSELECTALL))
	save_ids = TRUE;
    if ((ap->flags & (AF_MISSING|AF_READ)) == desired_flags) {
	if (!(ap->flags & sel_mask)) {
	    selected_count++;
#ifdef VERBOSE
	    if (echo) {
		IF(verbose)
		    fputs("\tSelected",stdout);
	    }
#endif
	}
	ap->flags = (ap->flags & ~AF_DEL) | sel_mask | sel_flags;
    } else
	ap->flags |= sel_flags;
    if (!(ap->subj->flags & sel_mask))
	selected_subj_cnt++;
    ap->subj->flags = (ap->subj->flags&~SF_DEL)|sel_mask|sel_flags|SF_VISIT;
    if (sel_mode == SM_THREAD) {
	if ((ap = ap->subj->thread) != NULL)
	    ap->subj->flags |= SF_VISIT;
    }
    selected_only = (selected_only || selected_count != 0);
}

/* Select all the articles in a subject.
*/
void
select_subject(subj, sel_flags)
SUBJECT *subj;
int sel_flags;
{
    register ARTICLE *ap;
    int desired_flags = (sel_rereading? AF_READ : 0);
    int old_count = selected_count;

    if (sel_flags & (AF_AUTOSELECT|AF_AUTOSELECTALL)) {
	save_ids = TRUE;
	if (sel_flags & AF_AUTOSELECTALL)
	    subj->flags |= SF_AUTOSELECT;
    }
    for (ap = subj->articles; ap; ap = ap->subj_next) {
	if ((ap->flags & (AF_MISSING|AF_READ|sel_mask)) == desired_flags) {
	    ap->flags |= sel_mask | sel_flags;
	    selected_count++;
	} else
	    ap->flags |= sel_flags;
    }
    if (selected_count > old_count) {
	if (!(subj->flags & sel_mask))
	    selected_subj_cnt++;
	subj->flags = (subj->flags & ~SF_DEL)
		    | sel_mask | SF_VISIT | SF_WASSELECTED;
	if (sel_mode == SM_THREAD) {
	    if ((ap = subj->thread) != NULL)
		ap->subj->flags |= SF_VISIT;
	}
	selected_only = TRUE;
    } else
	subj->flags |= SF_WASSELECTED;
}

/* Select all the articles in a thread.
*/
void
select_thread(thread, sel_flags)
register ARTICLE *thread;
int sel_flags;
{
    register SUBJECT *sp;

    sp = thread->subj;
    do {
	select_subject(sp, sel_flags);
	sp = sp->thread_link;
    } while (sp != thread->subj);
}

/* Select the subthread attached to this article.
*/
void
select_subthread(ap, sel_flags)
register ARTICLE *ap;
int sel_flags;
{
    register ARTICLE *limit;
    SUBJECT *subj;
    int desired_flags = (sel_rereading? AF_READ : 0);
    int old_count = selected_count;

    if (!ap)
	return;
    subj = ap->subj;
    for (limit = ap; limit; limit = limit->parent) {
	if (limit->sibling) {
	    limit = limit->sibling;
	    break;
	}
    }

    if (sel_flags & (AF_AUTOSELECT|AF_AUTOSELECTALL))
	save_ids = TRUE;
    for (; ap != limit; ap = bump_art(ap)) {
	if ((ap->flags & (AF_MISSING|AF_READ|sel_mask)) == desired_flags) {
	    ap->flags |= sel_mask | sel_flags;
	    selected_count++;
	} else
	    ap->flags |= sel_flags;
    }
    if (selected_count > old_count) {
	if (!(subj->flags & sel_mask))
	    selected_subj_cnt++;
	subj->flags = (subj->flags & ~SF_DEL) | sel_mask | SF_VISIT;
	if (sel_mode == SM_THREAD) {
	    if ((ap = subj->thread) != NULL)
		ap->subj->flags |= SF_VISIT;
	}
	selected_only = TRUE;
    }
}

/* Deselect a single article.
*/
void
deselect_article(ap)
register ARTICLE *ap;
{
    if (ap->flags & sel_mask) {
	ap->flags &= ~sel_mask;
	if (!selected_count--)
	    selected_count = 0;
#ifdef VERBOSE
	if (mode != 't') {
	    IF(verbose)
		fputs("\tDeselected",stdout);
	}
#endif
    }
    if (sel_rereading && sel_mode == SM_ARTICLE)
	ap->flags |= AF_DEL;
}

/* Deselect all the articles in a subject.
*/
void
deselect_subject(subj)
SUBJECT *subj;
{
    register ARTICLE *ap;

    for (ap = subj->articles; ap; ap = ap->subj_next) {
	if (ap->flags & sel_mask) {
	    ap->flags &= ~sel_mask;
	    if (!selected_count--)
		selected_count = 0;
	}
    }
    if (subj->flags & sel_mask) {
	subj->flags &= ~sel_mask;
	selected_subj_cnt--;
    }
    subj->flags &= ~(SF_VISIT | SF_WASSELECTED);
    if (sel_rereading)
	subj->flags |= SF_DEL;
    else
	subj->flags &= ~SF_DEL;
}

/* Deselect all the articles in a thread.
*/
void
deselect_thread(thread)
register ARTICLE *thread;
{
    register SUBJECT *sp;

    sp = thread->subj;
    do {
	deselect_subject(sp);
	sp = sp->thread_link;
    } while (sp != thread->subj);
}

/* Deselect everything.
*/
void
deselect_all()
{
    register SUBJECT *sp;

    for (sp = first_subject; sp; sp = sp->next)
	deselect_subject(sp);
    selected_count = selected_subj_cnt = 0;
    sel_page_sp = 0;
    sel_page_app = 0;
    sel_last_ap = 0;
    sel_last_sp = 0;
    selected_only = FALSE;
}

/* Kill all unread articles attached to the given subject.
*/
void
kill_subject(subj, kill_flags)
SUBJECT *subj;
int kill_flags;
{
    register ARTICLE *ap;
    register int killmask = ((kill_flags&KF_ALL)? AF_READ:(AF_READ|sel_mask));

    if (kill_flags & KF_KILLFILE) {
	save_ids = TRUE;
	kill_flags = AF_AUTOKILLALL;
    } else
	kill_flags = 0;
    for (ap = subj->articles; ap; ap = ap->subj_next) {
	if (!(ap->flags & killmask))
	    set_read(ap);
	ap->flags |= kill_flags;
    }
    subj->flags &= ~(SF_VISIT | SF_WASSELECTED);
}

/* Kill all unread articles attached to the given thread.
*/
void
kill_thread(thread, kill_flags)
register ARTICLE *thread;
int kill_flags;
{
    register SUBJECT *sp;

    sp = thread->subj;
    do {
	kill_subject(sp, kill_flags);
	sp = sp->thread_link;
    } while (sp != thread->subj);
}

/* Kill the subthread attached to this article.
*/
void
kill_subthread(ap, kill_flags)
register ARTICLE *ap;
int kill_flags;
{
    register ARTICLE *limit;

    if (!ap)
	return;
    for (limit = ap; limit; limit = limit->parent) {
	if (limit->sibling) {
	    limit = limit->sibling;
	    break;
	}
    }

    if (kill_flags & KF_KILLFILE) {
	save_ids = TRUE;
	kill_flags = AF_AUTOKILL;
    } else
	kill_flags = 0;
    for (; ap != limit; ap = bump_art(ap)) {
	if (!(ap->flags & (AF_READ|AF_MISSING)))
	    set_read(ap);
	ap->flags |= kill_flags;
    }
}

/* Unkill all the articles attached to the given subject.
*/
void
unkill_subject(subj)
SUBJECT *subj;
{
    register ARTICLE *ap;

    for (ap = subj->articles; ap; ap = ap->subj_next) {
	if (sel_rereading) {
	    if ((ap->flags & (AF_DELSEL|AF_MISSING)) == AF_DELSEL) {
		if (ap->flags & AF_READ)
		    toread[ng]++;
		ap->flags = (ap->flags & ~(AF_DELSEL|AF_READ)) | AF_SEL;
	    } else
		ap->flags &= ~(AF_DEL|AF_DELSEL);
	} else {
	    if ((ap->flags & (AF_READ|AF_MISSING)) == AF_READ)
		onemore(ap);
	    if (selected_only && !(ap->flags & (AF_SEL|AF_READ))) {
		ap->flags = (ap->flags & ~AF_DEL) | AF_SEL;
		selected_count++;
	    }
	}
    }
    if (!sel_rereading && selected_only && !(subj->flags & SF_SEL)) {
	subj->flags |= SF_SEL | SF_VISIT | SF_WASSELECTED;
	if (sel_mode == SM_THREAD) {
	    if ((ap = subj->thread) != NULL)
		ap->subj->flags |= SF_VISIT;
	}
	selected_subj_cnt++;
    }
    subj->flags &= ~(SF_DEL|SF_DELSEL);
}

/* Unkill all the articles attached to the given thread.
*/
void
unkill_thread(thread)
register ARTICLE *thread;
{
    register SUBJECT *sp;

    sp = thread->subj;
    do {
	unkill_subject(sp);
	sp = sp->thread_link;
    } while (sp != thread->subj);
}

/* Unkill the subthread attached to this article.
*/
void
unkill_subthread(ap)
register ARTICLE *ap;
{
    register ARTICLE *limit;
    register SUBJECT *sp;

    if (!ap)
	return;
    for (limit = ap; limit; limit = limit->parent) {
	if (limit->sibling) {
	    limit = limit->sibling;
	    break;
	}
    }

    sp = ap->subj;
    for (; ap != limit; ap = bump_art(ap)) {
	if ((ap->flags & (AF_READ|AF_MISSING)) == AF_READ)
	    onemore(ap);
	if (selected_only && !(ap->flags & AF_SEL)) {
	    ap->flags |= AF_SEL;
	    selected_count++;
	}
    }
    if (!(sp->flags & sel_mask))
	selected_subj_cnt++;
    sp->flags = (sp->flags & ~SF_DEL) | SF_SEL | SF_VISIT;
    if (sel_mode == SM_THREAD) {
	if ((ap = sp->thread) != NULL)
	    ap->subj->flags |= SF_VISIT;
    }
    selected_only = (selected_only || selected_count != 0);
}

/* Kill all unread articles attached to the given subject.
*/
void
clear_subject(subj)
SUBJECT *subj;
{
    register ARTICLE *ap;

    for (ap = subj->articles; ap; ap = ap->subj_next) {
	ap->flags &= ~AF_AUTOFLAGS;
    }
    subj->flags &= ~(SF_WASSELECTED | SF_AUTOSELECT);
}

/* Kill all unread articles attached to the given thread.
*/
void
clear_thread(thread)
register ARTICLE *thread;
{
    register SUBJECT *sp;

    sp = thread->subj;
    do {
	clear_subject(sp);
	sp = sp->thread_link;
    } while (sp != thread->subj);
}

/* Kill the subthread attached to this article.
*/
void
clear_subthread(ap)
register ARTICLE *ap;
{
    register ARTICLE *limit;

    if (!ap)
	return;
    for (limit = ap; limit; limit = limit->parent) {
	if (limit->sibling) {
	    limit = limit->sibling;
	    break;
	}
    }

    for (; ap != limit; ap = bump_art(ap)) {
	ap->flags &= ~AF_AUTOFLAGS;
    }
}

ARTICLE *
subj_art(sp)
SUBJECT *sp;
{
    register ARTICLE *ap = Nullart;
    int art_mask = (selected_only? AF_SEL : 0);
    bool TG_save = ThreadedGroup;

    ThreadedGroup = (sel_mode == SM_THREAD);
    ap = first_art(sp);
    while (ap && (ap->flags & (art_mask|AF_READ)) != art_mask)
	ap = next_art(ap);
    if (!ap) {
	reread = TRUE;
	ap = first_art(sp);
	if (art_mask) {
	    while (ap && !(ap->flags & AF_SEL))
		ap = next_art(ap);
	    if (!ap)
		ap = first_art(sp);
	}
    }
    ThreadedGroup = TG_save;
    return ap;
}

/* Find the next thread (first if art > lastart).  If articles are selected,
** only choose from threads with selected articles.
*/
void
next_subject()
{
    register SUBJECT *sp;
    register ARTICLE *ap;

    sp = ((ap = artp) ? ap->subj->next : first_subject);
    for (; sp; sp = sp->next) {
	if (sp->flags & SF_VISIT) {
	    if ((ap = subj_art(sp)) != Nullart) {
		art = article_num(ap);
		artp = ap;
		return;
	    }
	    reread = FALSE;
	}
    }
    artp = Nullart;
    art = lastart+1;
    forcelast = TRUE;
}

/* Find previous thread (or last if artp == NULL).  If articles are selected,
** only choose from threads with selected articles.
*/
void
prev_subject()
{
    register SUBJECT *sp;
    register ARTICLE *ap;

    sp = ((ap = artp) ? ap->subj->prev : last_subject);
    for (; sp; sp = sp->prev) {
	if (sp->flags & SF_VISIT) {
	    if ((ap = subj_art(sp)) != Nullart) {
		art = article_num(ap);
		artp = ap;
		return;
	    }
	    reread = FALSE;
	}
    }
    artp = Nullart;
    art = lastart+1;
    forcelast = TRUE;
}

/* Find artp's parent or oldest ancestor.  Returns FALSE if no such
** article.  Sets art and artp otherwise.
*/
bool
find_parent(keep_going)
bool_int keep_going;
{
    register ARTICLE *ap = artp;

    if (!ap->parent)
	return FALSE;

    do {
	ap = ap->parent;
    } while (keep_going && ap->parent);

    if (((artp = ap)->flags & AF_TMPMEM) == AF_TMPMEM)
	art = 0;
    else
	art = article_num(ap);
    return TRUE;
}

/* Find artp's first child or youngest decendent.  Returns FALSE if no
** such article.  Sets art and artp otherwise.
*/
bool
find_leaf(keep_going)
bool_int keep_going;
{
    register ARTICLE *ap = artp;

    if (!ap->child1)
	return FALSE;

    do {
	ap = ap->child1;
    } while (keep_going && ap->child1);

    if (((artp = ap)->flags & AF_TMPMEM) == AF_TMPMEM)
	art = 0;
    else
	art = article_num(ap);
    return TRUE;
}

static ARTICLE *first_sib(), *last_sib();

/* Find the next "sibling" of artp, including cousins that are the
** same distance down the thread as we are.  Returns FALSE if no such
** article.  Sets art and artp otherwise.
*/
bool
find_next_sib()
{
    ARTICLE *ta, *tb;
    int ascent;

    ascent = 0;
    ta = artp;
    for (;;) {
	while (ta->sibling) {
	    ta = ta->sibling;
	    if (tb = first_sib(ta, ascent)) {
		if (((artp = tb)->flags & AF_TMPMEM) == AF_TMPMEM)
		    art = 0;
		else
		    art = article_num(tb);
		return TRUE;
	    }
	}
	if (!(ta = ta->parent))
	    break;
	ascent++;
    }
    return FALSE;
}

/* A recursive routine to find the first node at the proper depth.  This
** article is at depth 0.
*/
static ARTICLE *
first_sib(ta, depth)
ARTICLE *ta;
int depth;
{
    ARTICLE *tb;

    if (!depth)
	return ta;

    for (;;) {
	if (ta->child1 && (tb = first_sib(ta->child1, depth-1)))
	    return tb;

	if (!ta->sibling)
	    return Nullart;

	ta = ta->sibling;
    }
}

/* Find the previous "sibling" of artp, including cousins that are
** the same distance down the thread as we are.  Returns FALSE if no
** such article.  Sets art and artp otherwise.
*/
bool
find_prev_sib()
{
    ARTICLE *ta, *tb;
    int ascent;

    ascent = 0;
    ta = artp;
    for (;;) {
	tb = ta;
	if (ta->parent)
	    ta = ta->parent->child1;
	else
	    ta = ta->subj->thread;
	if (tb = last_sib(ta, ascent, tb)) {
	    if (((artp = tb)->flags & AF_TMPMEM) == AF_TMPMEM)
		art = 0;
	    else
		art = article_num(tb);
	    return TRUE;
	}
	if (!(ta = ta->parent))
	    break;
	ascent++;
    }
    return FALSE;
}

/* A recursive routine to find the last node at the proper depth.  This
** article is at depth 0.
*/
static ARTICLE *
last_sib(ta, depth, limit)
ARTICLE *ta;
int depth;
ARTICLE *limit;
{
    ARTICLE *tb, *tc;

    if (ta == limit)
	return Nullart;

    if (ta->sibling) {
	tc = ta->sibling;
	if (tc != limit && (tb = last_sib(tc,depth,limit)))
	    return tb;
    }
    if (!depth)
	return ta;
    if (ta->child1)
	return last_sib(ta->child1, depth-1, limit);
    return Nullart;
}

/* Get each subject's article count; count total articles and selected
** articles (use sel_rereading to determine whether to count read or
** unread articles); deselect any subjects we find that are empty if
** CS_UNSELECT or CS_UNSEL_STORE is specified.  If mode is CS_RESELECT
** is specified, the selections from the last CS_UNSEL_STORE are
** reselected.
*/
void
count_subjects(mode)
int mode;
{
    register int count, sel_count;
    register ARTICLE *ap;
    register SUBJECT *sp;
    int desired_flags = (sel_rereading? AF_READ : 0);
    time_t subjdate;

    article_count = selected_count = selected_subj_cnt = 0;
    if (last_cached >= lastart)
	firstart = lastart+1;

    for (sp = first_subject; sp; sp = sp->next)
	sp->flags &= ~SF_VISIT;
    for (sp = first_subject; sp; sp = sp->next) {
	subjdate = 0;
	count = sel_count = 0;
	for (ap = sp->articles; ap; ap = ap->subj_next) {
	    if ((ap->flags & (AF_MISSING|AF_READ)) == desired_flags) {
		count++;
		if (ap->flags & sel_mask)
		    sel_count++;
		if (!subjdate)
		    subjdate = ap->date;
		if (article_num(ap) < firstart)
		    firstart = article_num(ap);
	    }
	}
	if (mode == CS_UNSEL_STORE) {
	    if (sp->flags & SF_SEL)
		sp->flags |= SF_OLDSEL;
	    else
		sp->flags &= ~SF_OLDSEL;
	} else if (mode == CS_RESELECT) {
	    if (sp->flags & SF_OLDSEL)
		sp->flags |= SF_SEL;
	    else
		sp->flags &= ~SF_SEL;
	}
	sp->misc = count;
	if (subjdate)
	    sp->date = subjdate;
	article_count += count;
	if (sel_count) {
	    sp->flags = (sp->flags & ~(SF_SEL|SF_DEL)) | sel_mask;
	    selected_count += sel_count;
	    selected_subj_cnt++;
	} else if (mode >= CS_UNSELECT)
	    sp->flags &= ~sel_mask;
	else if (sp->flags & sel_mask) {
	    sp->flags &= ~SF_DEL;
	    selected_subj_cnt++;
	}
	if (count && (!selected_only || (sp->flags & sel_mask))) {
	    sp->flags |= SF_VISIT;
	    if (sel_mode == SM_THREAD) {
		if ((ap = sp->thread) != NULL)
		    ap->subj->flags |= SF_VISIT;
	    }
	}
    }
    if (mode && !article_count && !selected_only) {
	for (sp = first_subject; sp; sp = sp->next)
	    sp->flags |= SF_VISIT;
    }
}

int
subjorder_subject(spp1, spp2)
register SUBJECT **spp1;
register SUBJECT **spp2;
{
    return strCASEcmp((*spp1)->str+4, (*spp2)->str+4) * sel_direction;
}

int
subjorder_date(spp1, spp2)
register SUBJECT **spp1;
register SUBJECT **spp2;
{
    return (int)((*spp1)->date - (*spp2)->date) * sel_direction;
}

int
subjorder_count(spp1, spp2)
register SUBJECT **spp1;
register SUBJECT **spp2;
{
    int eq;
    if ((eq = (int)((*spp1)->misc - (*spp2)->misc)) != 0)
	return eq * sel_direction;
    return (int)((*spp1)->date - (*spp2)->date) * sel_direction;
}

int
threadorder_subject(spp1, spp2)
SUBJECT **spp1;
SUBJECT **spp2;
{
    register ARTICLE *t1 = (*spp1)->thread;
    register ARTICLE *t2 = (*spp2)->thread;
    if (t1 != t2 && t1 && t2)
	return strCASEcmp(t1->subj->str+4, t2->subj->str+4) * sel_direction;
    return (int)((*spp1)->date - (*spp2)->date) * sel_direction;
}

int
threadorder_date(spp1, spp2)
SUBJECT **spp1;
SUBJECT **spp2;
{
    register ARTICLE *t1 = (*spp1)->thread;
    register ARTICLE *t2 = (*spp2)->thread;
    if (t1 != t2 && t1 && t2) {
	register SUBJECT *sp1, *sp2;
	int eq;
	if (!(sp1 = t1->subj)->misc)
	    for (sp1=sp1->thread_link; sp1 != t1->subj; sp1=sp1->thread_link)
		if (sp1->misc)
		    break;
	if (!(sp2 = t2->subj)->misc)
	    for (sp2=sp2->thread_link; sp2 != t2->subj; sp2=sp2->thread_link)
		if (sp2->misc)
		    break;
	if ((eq = (int)(sp1->date - sp2->date) * sel_direction) != 0)
	    return eq;
	return strCASEcmp(sp1->str+4, sp2->str+4) * sel_direction;
    }
    return (int)((*spp1)->date - (*spp2)->date) * sel_direction;
}

int
threadorder_count(spp1, spp2)
SUBJECT **spp1;
SUBJECT **spp2;
{
    register int size1 = (*spp1)->misc;
    register int size2 = (*spp2)->misc;
    if ((*spp1)->thread != (*spp2)->thread) {
	register SUBJECT *sp;
	for (sp = (*spp1)->thread_link; sp != *spp1; sp = sp->thread_link)
	    size1 += sp->misc;
	for (sp = (*spp2)->thread_link; sp != *spp2; sp = sp->thread_link)
	    size2 += sp->misc;
    }
    if (size1 != size2)
	return (size1 - size2) * sel_direction;
    return threadorder_date(spp1, spp2);
}

/* Sort the subjects according to the chosen order.
*/
void
sort_subjects()
{
    register SUBJECT *sp;
    register int i;
    SUBJECT **lp, **subj_list;
    int (*sort_procedure)();

    /* If we don't have at least two subjects, we're done! */
    if (!first_subject || !first_subject->next)
	return;

    switch (sel_sort) {
    case SS_DATE:
    case SS_AUTHOR:
    case SS_GROUPS:
	sort_procedure = (sel_mode == SM_THREAD?
			  threadorder_date : subjorder_date);
	break;
    case SS_SUBJECT:
	sort_procedure = (sel_mode == SM_THREAD?
			  threadorder_subject : subjorder_subject);
	break;
    case SS_COUNT:
	sort_procedure = (sel_mode == SM_THREAD?
			  threadorder_count : subjorder_count);
	break;
    }

    subj_list = (SUBJECT**)safemalloc(subject_count * sizeof (SUBJECT*));
    for (lp = subj_list, sp = first_subject; sp; sp = sp->next)
	*lp++ = sp;
    assert(lp - subj_list == subject_count);

    qsort(subj_list, subject_count, sizeof (SUBJECT*), sort_procedure);

    first_subject = sp = subj_list[0];
    sp->prev = Nullsubj;
    for (i = subject_count, lp = subj_list; --i; lp++) {
	lp[0]->next = lp[1];
	lp[1]->prev = lp[0];
	if (sel_mode == SM_THREAD) {
	    if (lp[0]->thread == lp[1]->thread)
		lp[0]->thread_link = lp[1];
	    else {
		lp[0]->thread_link = sp;
		sp = lp[1];
	    }
	}
    }
    last_subject = lp[0];
    last_subject->next = Nullsubj;
    if (sel_mode == SM_THREAD)
	last_subject->thread_link = sp;
    free((char*)subj_list);
}

int
artorder_date(art1, art2)
register ARTICLE **art1;
register ARTICLE **art2;
{
    return (int)((*art1)->date - (*art2)->date) * sel_direction;
}

int
artorder_subject(art1, art2)
register ARTICLE **art1;
register ARTICLE **art2;
{
    if ((*art1)->subj == (*art2)->subj)
	return (int)((*art1)->date - (*art2)->date);
    return strCASEcmp((*art1)->subj->str + 4, (*art2)->subj->str + 4)
	* sel_direction;
}

int
artorder_author(art1, art2)
register ARTICLE **art1;
register ARTICLE **art2;
{
    int eq;
    if ((eq = strCASEcmp((*art1)->from, (*art2)->from)) != 0)
	return eq * sel_direction;
    return (int)((*art1)->date - (*art2)->date);
}

int
artorder_groups(art1, art2)
register ARTICLE **art1;
register ARTICLE **art2;
{
    if ((*art1)->subj == (*art2)->subj)
	return (int)((*art1)->date - (*art2)->date);
    return (int)((*art1)->subj->date - (*art2)->subj->date) * sel_direction;
}

/* Sort the articles according to the chosen order.
*/
void
sort_articles()
{
    int (*sort_procedure)();

    build_artptrs();

    /* If we don't have at least two articles, we're done! */
    if (article_count <2)
	return;

    switch (sel_sort) {
    case SS_DATE:
    case SS_COUNT:
	sort_procedure = artorder_date;
	break;
    case SS_SUBJECT:
	sort_procedure = artorder_subject;
	break;
    case SS_AUTHOR:
	sort_procedure = artorder_author;
	break;
    case SS_GROUPS:
	sort_procedure = artorder_groups;
	break;
    }
    if (sel_page_app)
	sel_last_ap = *sel_page_app;
    sel_page_app = 0;
    qsort(artptr_list, article_count, sizeof (ARTICLE*), sort_procedure);
}

static long artptr_list_size = 0;

static void
build_artptrs()
{
    ARTICLE **app, *ap;
    long count = article_count;
    int desired_flags = (sel_rereading? AF_READ : 0);

    if (!artptr_list || artptr_list_size != count) {
	artptr_list = (ARTICLE**)saferealloc((char*)artptr_list,
		(MEM_SIZE)count * sizeof (ARTICLE*));
	artptr_list_size = count;
    }
    for (app = artptr_list, ap = article_list; count; ap++) {
	if ((ap->flags & (AF_MISSING|AF_READ)) == desired_flags) {
	    *app++ = ap;
	    count--;
	}
    }
}
