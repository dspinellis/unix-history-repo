/* $Id: cache.h,v 3.0 1991/09/09 20:18:23 davison Trn $
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

/* Subjects get their own structure */

typedef struct rt_subj {
    struct rt_subj *next;
    struct rt_subj *prev;
    struct rt_art *articles;
    struct rt_art *thread;
    struct rt_subj *thread_link;
    char *str;
    time_t date;
    short flags;
    short misc;		/* used for temporary totals and subject numbers */
} SUBJECT;

/* subject flags */

#define SF_SEL		0x0001
#define SF_DEL		0x0002
#define SF_DELSEL	0x0004
#define SF_OLDSEL	0x0008
#define SF_INCLUDED	0x0010

#define SF_THREAD	0x0100
#define SF_VISIT	0x0200
#define SF_WASSELECTED  0x0400
#define SF_AUTOSELECT	0x0800
#define SF_SUBJTRUNCED	0x1000

/* This is our article-caching structure */

typedef struct rt_art {
    time_t date;
    SUBJECT *subj;
    char *from;
    char *msgid;
    char *xrefs;
    struct rt_art *parent;	/* parent article */
    struct rt_art *child1;	/* first child of a chain */
    struct rt_art *sibling;	/* our next sibling */
    struct rt_art *subj_next;	/* next article in subject order */
    short flags;		/* user-settable flags */
    short padding;
} ARTICLE;

/* article flags */

#define AF_SEL		0x0001
#define AF_DEL		0x0002
#define AF_DELSEL	0x0004
#define AF_OLDSEL	0x0008
#define AF_INCLUDED	0x0010

#define AF_READ		0x0020
#define AF_CACHED	0x0040
#define AF_THREADED	0x0080
#define AF_MISSING	0x0100
#define AF_AUTOKILL	0x0200
#define AF_AUTOKILLALL	0x0400
#define AF_AUTOSELECT	0x0800
#define AF_AUTOSELECTALL 0x1000
#define AF_AUTOFLAGS    (AF_AUTOKILL|AF_AUTOKILLALL|AF_AUTOSELECT|AF_AUTOSELECTALL)

/* If AF_MISSING is NOT set the last 3 bits have the following meaning: */
#define AF_HAS_RE	0x2000
#define AF_YANKBACK	0x4000
#define AF_FROMTRUNCED	0x8000

/* These flags only have meaning when combined with AF_MISSING */
#define AF_TMPMEM	(0x2000|AF_MISSING)
#define AF_FAKE		(0x4000|AF_MISSING)

/* The following define is only valid as a flag to the select_article call */
#define AF_ECHO 	0x8000

#define Nullart Null(ARTICLE*)
#define Nullsubj Null(SUBJECT*)

#define was_read(a)     (article_ptr(a)->flags & AF_READ)

/* These must never use their args more than once in the definition */
#define article_num(ap)      (((ap)-article_list)+absfirst)
#define article_ptr(artnum)  (article_list+((artnum)-absfirst))

EXT ARTICLE *article_list INIT(Nullart);
EXT ARTICLE **artptr_list INIT(0);
EXT ARTICLE **artptr;

#ifdef ARTSEARCH
EXT ART_NUM srchahead INIT(0); 	/* are we in subject scan mode? */
				/* (if so, contains art # found or -1) */
#endif

EXT ART_NUM first_cached;
EXT ART_NUM last_cached;
EXT bool cached_all_in_range;
EXT ARTICLE *sentinel_artp;

EXT struct rt_subj *first_subject INIT(0);
EXT struct rt_subj *last_subject INIT(0);

EXT bool untrim_cache INIT(FALSE);

#ifdef PENDING
EXT ART_NUM subj_to_get;
EXT ART_NUM xref_to_get;
#endif

void	cache_init _((void));
void	build_cache _((void));
void	grow_cache _((ART_NUM));
void	close_cache _((void));
void	cache_article _((ARTICLE*));
void	check_poster _((ARTICLE*));
void	uncache_article _((ARTICLE*,bool_int));
char	*fetchcache _((ART_NUM,int));	/* return actual cache ptr */
char	*get_cached_line _((ARTICLE*, int, bool_int));
void	set_subj_line _((ARTICLE*, char*, int));
void	set_cached_line _((ARTICLE*, int, char*));
void	look_ahead _((void));
void	cache_until_key _((void));
bool	cache_subjects _((void));
bool	cache_xrefs _((void));
bool	cache_all_arts _((void));
bool	cache_unread_arts _((void));
bool	art_data _((ART_NUM,ART_NUM,bool_int,bool_int));
bool	cache_range _((ART_NUM,ART_NUM));
void	clear_article _((ARTICLE*));
void	free_subject _((SUBJECT*));
