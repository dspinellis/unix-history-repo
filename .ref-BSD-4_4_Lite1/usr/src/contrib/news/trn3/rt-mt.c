/* $Id: rt-mt.c,v 3.0 1992/12/14 00:14:13 davison Trn $
*/

#include "EXTERN.h"
#include "common.h"
#include "intrp.h"
#include "trn.h"
#include "cache.h"
#include "bits.h"
#include "ng.h"
#include "ngdata.h"
#include "rcln.h"
#include "util.h"
#include "hash.h"
#include "nntp.h"
#include "rthread.h"
#include "rt-process.h"

#ifdef USE_MT
#include "INTERN.h"
#include "rt-mt.h"

extern HASHTABLE *msgid_hash;
extern bool try_mt;

#ifndef USE_XTHREAD
static FILE *fp;
#endif
static bool word_same, long_same;
static BMAP my_bmap, mt_bmap;

static char *strings = Nullch;
static WORD *author_cnts = 0;
static WORD *ids = 0;

static ARTICLE **article_array = 0;
static SUBJECT **subject_array = 0;
static char **author_array = 0;

static TOTAL total;
static PACKED_ROOT p_root;
static PACKED_ARTICLE p_article;

/* Initialize our thread code by determining the byte-order of the thread
** files and our own current byte-order.  If they differ, set flags to let
** the read code know what we'll need to translate.
*/
bool
mt_init()
{
    int i;
#ifdef USE_XTHREAD
    long size;
#endif
    bool success = TRUE;			/* I'm an optimist */

    word_same = long_same = TRUE;
#ifdef USE_XTHREAD
    sprintf(ser_line, "XTHREAD DBINIT");
    nntp_command(ser_line);
    size = nntp_readcheck();
    if (size < 0)
	return FALSE;
    size = nntp_read((char*)&mt_bmap, (long)sizeof (BMAP));
    if (size >= sizeof (BMAP) - 1) {
#else /* !USE_XTHREAD */
    if ((fp = fopen(filexp(DBINIT), FOPEN_RB)) != Nullfp
     && fread((char*)&mt_bmap, 1, sizeof (BMAP), fp) >= sizeof (BMAP) - 1) {
#endif
	if (mt_bmap.version != DB_VERSION) {
	    printf("\nMthreads database is the wrong version -- ignoring it.\n")
		FLUSH;
	    return FALSE;
	}
	mybytemap(&my_bmap);
	for (i = 0; i < sizeof (LONG); i++) {
	    if (i < sizeof (WORD)) {
		if (my_bmap.w[i] != mt_bmap.w[i]) {
		    word_same = FALSE;
		}
	    }
	    if (my_bmap.l[i] != mt_bmap.l[i]) {
		long_same = FALSE;
	    }
	}
    } else
	success = FALSE;
#ifdef USE_XTHREAD
    while (nntp_read(ser_line, (long)sizeof ser_line))
	;		/* trash any extraneous bytes */
#else
    if (fp != Nullfp)
	fclose(fp);
#endif
    return success;
}

/* Open and process the data in the group's thread file.  Returns TRUE unless
** we discovered a bogus thread file, destroyed the cache, and re-built it.
*/
bool
mt_data()
{
    bool success = TRUE;
#ifdef USE_XTHREAD		/* use remote thread file? */
    long size;

    sprintf(ser_line, "XTHREAD THREAD");
    nntp_command(ser_line);
    size = nntp_readcheck();
    if (size < 0)
	return TRUE;

    printf("\nGetting thread file."), fflush(stdout);
    if (nntp_read((char*)&total, (long)sizeof (TOTAL)) < sizeof (TOTAL))
	goto exit;

#else /* !USE_XTHREAD */
    if ((fp = fopen(mt_name(ngname), FOPEN_RB)) == Nullfp)
	return TRUE;
    printf("\nReading thread file."), fflush(stdout);

    if (fread((char*)&total, 1, sizeof (TOTAL), fp) < sizeof (TOTAL))
	goto exit;

#endif /* !USE_XTHREAD */

    lp_bmap(&total.first, 4);
    wp_bmap(&total.root, 5);
    if (!total.root) {
	tweak_data();
	goto exit;
    }
    if (total.last > lastart)
	grow_cache(total.last);

    if (read_authors()
     && read_subjects()
     && read_roots()
     && read_articles()
     && read_ids())
    {
	tweak_data();
	first_cached = absfirst;
	last_cached = (total.last < absfirst ? absfirst-1: total.last);
	cached_all_in_range = TRUE;
	goto exit;
    }
    /* Something failed.  Safefree takes care of checking if some items
    ** were already freed.  Any partially-allocated structures were freed
    ** before we got here.  All other structures are cleaned up now.
    */
    close_cache();
    safefree(&strings);
    safefree((char**)&article_array);
    safefree((char**)&subject_array);
    safefree((char**)&author_array);
    safefree((char**)&ids);
    try_mt = FALSE;
    build_cache();
    try_mt = TRUE;
    success = FALSE;

exit:
#ifdef USE_XTHREAD
    while (nntp_read(ser_line, (long)sizeof ser_line))
	;		/* trash any extraneous bytes */
#else
    fclose(fp);
#endif
    return success;
}

#ifndef USE_XTHREAD
/* Change a newsgroup name into the name of the thread data file.  We
** subsitute any '.'s in the group name into '/'s (unless LONG_THREAD_NAMES
** is defined), prepend the path, and append the '/.thread' or '.th' on to
** the end.
*/
static char *
mt_name(group)
char *group;
{
#ifdef LONG_THREAD_NAMES
    sprintf(buf, "%s/%s", threaddir, group);
#else
    register char *cp;

    cp = strcpy(buf, threaddir) + strlen(threaddir);
    *cp++ = '/';
    strcpy(cp, group);
    while ((cp = index(cp, '.')))
	*cp = '/';
#endif
    if (threaddir == spool)
	strcat(buf, "/.thread");
    else
	strcat(buf, ".th");
    return buf;
}
#endif

static char *subject_strings, *string_end;

/* The author information is an array of use-counts, followed by all the
** null-terminated strings crammed together.  The subject strings are read
** in at the same time, since they are appended to the end of the author
** strings.
*/
static int
read_authors()
{
    register int count;
    register char *string_ptr, **author_ptr;

    if (!read_item((char**)&author_cnts, (MEM_SIZE)total.author*sizeof (WORD)))
	return 0;
    safefree((char**)&author_cnts);   /* we don't need these */

    if (!read_item(&strings, (MEM_SIZE)total.string1))
	return 0;

    string_ptr = strings;
    string_end = string_ptr + total.string1;
    if (string_end[-1] != '\0') {
	/*error("first string table is invalid.\n");*/
	return 0;
    }

    /* We'll use this array to point each article at its proper author
    ** (the packed values were saved as indexes).
    */
    author_array = (char**)safemalloc(total.author * sizeof (char*));
    author_ptr = author_array;

    for (count = total.author; count; count--) {
	if (string_ptr >= string_end)
	    break;
	*author_ptr++ = string_ptr;
	string_ptr += strlen(string_ptr) + 1;
    }
    subject_strings = string_ptr;

    if (count) {
	/*error("author unpacking failed.\n");*/
	return 0;
    }
    return 1;
}

/* The subject values consist of the crammed-together null-terminated strings
** (already read in above) and the use-count array.  They were saved in the
** order that the roots require while being unpacked.
*/
static int
read_subjects()
{
    register int count;
    register char *string_ptr;
    register SUBJECT **subj_ptr;
    WORD *subject_cnts;

    if (!read_item((char**)&subject_cnts,
		   (MEM_SIZE)total.subject * sizeof (WORD))) {
	/* (Error already logged.) */
	return 0;
    }
    free((char*)subject_cnts);		/* we don't need these */

    /* Use this array when unpacking the article's subject offset. */
    subject_array = (SUBJECT**)safemalloc(total.subject * sizeof (SUBJECT*));
    subj_ptr = subject_array;

    string_ptr = subject_strings;	/* string_end is already set */

    for (count = total.subject; count; count--) {
	int len;
	ARTICLE arty;
	if (string_ptr >= string_end)
	    break;
	len = strlen(string_ptr);
	arty.subj = 0;
	set_subj_line(&arty, string_ptr, len);
	if (len == 72)
	    arty.subj->flags |= SF_SUBJTRUNCED;
	string_ptr += len + 1;
	*subj_ptr++ = arty.subj;
    }
    if (count || string_ptr != string_end) {
	/*error("subject data is invalid.\n");*/
	return 0;
    }
    return 1;
}

/* Read in the packed root structures to set each subject's thread article
** offset.  This gets turned into a real pointer later.
*/
static int
read_roots()
{
    register int count, i;
    register SUBJECT **subj_ptr;
    int ret;

    subj_ptr = subject_array;

    for (count = total.root; count--; ) {
#ifdef USE_XTHREAD
	ret = nntp_read((char*)&p_root, (long)sizeof (PACKED_ROOT));
#else
	ret = fread((char*)&p_root, 1, sizeof (PACKED_ROOT), fp);
#endif
	if (ret != sizeof (PACKED_ROOT)) {
	    /*error("failed root read -- %d bytes instead of %d.\n",
		ret, sizeof (PACKED_ROOT));*/
	    return 0;
	}
	wp_bmap(&p_root.articles, 3);	/* converts subject_cnt too */
	if (p_root.articles < 0 || p_root.articles >= total.article) {
	    /*error("root has invalid values.\n");*/
	    return 0;
	}
	i = p_root.subject_cnt;
	if (i <= 0 || (subj_ptr - subject_array) + i > total.subject) {
	    /*error("root has invalid values.\n");*/
	    return 0;
	}
	subj_ptr[i-1]->thread_link = subj_ptr[0];
	while (i--) {
	    union { ARTICLE *ap; int num; } uni;
	    if (i)
		subj_ptr[0]->thread_link = subj_ptr[1];
	    subj_ptr[0]->flags &= ~SF_THREAD;
	    uni.num = p_root.articles;
	    (*subj_ptr++)->thread = uni.ap;
	}
    }
    return 1;
}

static bool invalid_data;

/* A simple routine that checks the validity of the article's subject value.
** A -1 means that it is NULL, otherwise it should be an offset into the
** subject array we just unpacked.
*/
static SUBJECT *
the_subject(num)
WORD num;
{
    if (num == -1)
	return Nullsubj;
    if (num < 0 || num >= total.subject) {
	/*printf("Invalid subject in thread file: %d [%ld]\n", num, art_num);*/
	invalid_data = TRUE;
	return Nullsubj;
    }
    return subject_array[num];
}

/* Ditto for author checking. */
static char *
the_author(num)
WORD num;
{
    if (num == -1)
	return Nullch;
    if (num < 0 || num >= total.author) {
	/*error("invalid author in thread file: %d [%ld]\n", num, art_num);*/
	invalid_data = TRUE;
	return Nullch;
    }
    return savestr(author_array[num]);
}

/* Our parent/sibling information is a relative offset in the article array.
** zero for none.  Child values are always found in the very next array
** element if child_cnt is non-zero.
*/
static ARTICLE *
the_article(relative_offset, num)
WORD relative_offset;
int num;
{
    union { ARTICLE *ap; int num; } uni;

    if (!relative_offset)
	return Nullart;
    num += relative_offset;
    if (num < 0 || num >= total.article) {
	/*error("invalid article offset in thread file.\n");*/
	invalid_data = TRUE;
	return Nullart;
    }
    uni.num = num+1;
    return uni.ap;		/* slip them an offset in disguise */
}

/* Read the articles into their trees.  Point everything everywhere. */
static int
read_articles()
{
    register int count;
    register ARTICLE *article, **art_ptr;
    int ret;

    /* Build an array to interpret interlinkages of articles. */
    article_array = (ARTICLE**)safemalloc(total.article * sizeof (ARTICLE*));
    art_ptr = article_array;

    invalid_data = FALSE;
    for (count = 0; count < total.article; count++) {
#ifdef USE_XTHREAD
	ret = nntp_read((char*)&p_article, (long)sizeof (PACKED_ARTICLE));
#else
	ret = fread((char*)&p_article, 1, sizeof (PACKED_ARTICLE), fp);
#endif
	if (ret != sizeof (PACKED_ARTICLE)) {
	    /*error("failed article read -- %d bytes instead of %d.\n",
		ret, sizeof (PACKED_ARTICLE));*/
	    return 0;
	}
	lp_bmap(&p_article.num, 2);
	wp_bmap(&p_article.subject, 8);

	article = *art_ptr++ = allocate_article(p_article.num);
	article->date = p_article.date;
#ifndef DBM_XREFS
	if (olden_days < 2 && !(p_article.flags & HAS_XREFS))
	    article->xrefs = nullstr;
#endif
	article->from = the_author(p_article.author);
	article->parent = the_article(p_article.parent, count);
	article->child1 = the_article(p_article.child_cnt ? 1 : 0, count);
	article->sibling = the_article(p_article.sibling, count);
	article->subj = the_subject(p_article.subject);
	if (invalid_data) {
	    /* (Error already logged.) */
	    return 0;
	}
	/* This is ok because parent articles precede their children */
	if (article->parent) {
	    union { ARTICLE *ap; int num; } uni;
	    uni.ap = article->parent;
	    article->parent = article_array[uni.num-1];
	}
	if (article->subj) {
	    if (!(article->flags & AF_MISSING)) {
		article->flags |= AF_FROMTRUNCED | AF_THREADED
		    | ((p_article.flags & ROOT_ARTICLE)? 0 : AF_HAS_RE);
	    }
	    /* Give this subject to any faked parent articles */
	    while (article->parent && !article->parent->subj) {
		article->parent->subj = article->subj;
		article = article->parent;
	    }
	} else
	    article->flags |= AF_FAKE;
    }

    /* We're done with most of the pointer arrays at this point. */
    safefree((char**)&subject_array);
    safefree((char**)&author_array);
    safefree(&strings);

    return 1;
}

/* Read the message-id strings and attach them to each article.  The data
** format consists of the mushed-together null-terminated strings (a domain
** name followed by all its unique-id prefixes) and then the article offsets
** to which they belong.  The first domain name was omitted, as it is a null
** domain for those truly weird message-id's without '@'s.
*/
static int
read_ids()
{
    register ARTICLE *article;
    register char *string_ptr;
    register int i, count, len, len2;

    if (!read_item(&strings, (MEM_SIZE)total.string2)
     || !read_item((char**)&ids,
		(MEM_SIZE)(total.article+total.domain+1) * sizeof (WORD))) {
	return 0;
    }
    wp_bmap(ids, total.article + total.domain + 1);

    string_ptr = strings;
    string_end = string_ptr + total.string2;

    if (string_end[-1] != '\0') {
	/*error("second string table is invalid.\n");*/
	return 0;
    }

    for (i = 0, count = total.domain + 1; count--; i++) {
	if (i) {
	    if (string_ptr >= string_end) {
		/*error("error unpacking domain strings.\n");*/
		return 0;
	    }
	    sprintf(buf, "@%s", string_ptr);
	    len = strlen(string_ptr) + 1;
	    string_ptr += len;
	} else {
	    *buf = '\0';
	    len = 0;
	}
	if (ids[i] != -1) {
	    if (ids[i] < 0 || ids[i] >= total.article) {
		/*error("error in id array.\n");*/
		return 0;
	    }
	    article = article_array[ids[i]];
	    for (;;) {
		if (string_ptr >= string_end) {
		    /*error("error unpacking domain strings.\n");*/
		    return 0;
		}
		len2 = strlen(string_ptr);
		article->msgid = safemalloc(len2 + len + 2 + 1);
		sprintf(article->msgid, "<%s%s>", string_ptr, buf);
		string_ptr += len2 + 1;
		if (msgid_hash)
		{
		    HASHDATUM data;
		    if ((article->flags & AF_TMPMEM) == AF_TMPMEM) {
			data.dat_ptr = (char*)article;
			data.dat_len = 0;
		    } else {
			data.dat_ptr = Nullch;
			data.dat_len = article_num(article);
		    }
		    hashstore(msgid_hash, article->msgid, len2+len+2, data);
		}
		if (++i >= total.article + total.domain + !count) {
		    /*error("overran id array unpacking domains.\n");*/
		    return 0;
		}
		if (ids[i] != -1) {
		    if (ids[i] < 0 || ids[i] >= total.article)
			return 0;
		    article = article_array[ids[i]];
		} else
		    break;
	    }
	}
    }
    safefree((char**)&ids);
    safefree(&strings);

    return 1;
}

/* And finally, turn all the links into real pointers and mark missing
** articles as read.
*/
static void
tweak_data()
{
    register int count;
    register ARTICLE *ap, **art_ptr;
    register SUBJECT *sp;
    register ART_NUM i;
    union { ARTICLE *ap; int num; } uni;

    for (sp = first_subject; sp; sp = sp->next) {
	uni.ap = sp->thread;
	sp->thread = article_array[uni.num];
	sp->thread->subj->flags |= SF_THREAD;
    }
    art_ptr = article_array;
    for (count = total.article; count--; ) {
	ap = *art_ptr++;
	if (ap->child1) {
	    uni.ap = ap->child1;
	    ap->child1 = article_array[uni.num-1];
	}
	if (ap->sibling) {
	    uni.ap = ap->sibling;
	    ap->sibling = article_array[uni.num-1];
	}
	if (!(ap->flags & AF_MISSING))
	    cache_article(ap);
    }

    /* Mark any missing articles as read */
    for (i = absfirst, ap = article_ptr(i); i <= total.last; i++, ap++) {
	if ((ap->flags & (AF_CACHED|AF_MISSING)) == AF_CACHED)
	    check_poster(ap);
	else
	    onemissing(ap);
    }
    safefree((char**)&article_array);
}

/* A shorthand for reading a chunk of the file into a malloc'ed array.
*/
static int
read_item(dest, len)
char **dest;
MEM_SIZE len;
{
    long ret;

    *dest = safemalloc(len);
#ifdef USE_XTHREAD
    ret = nntp_read(*dest, (long)len);
#else
    ret = fread(*dest, 1, (int)len, fp);
#endif
    if (ret != len) {
	free(*dest);
	*dest = Nullch;
	return 0;
    }
    putchar('.'), fflush(stdout);
    return 1;
}

/* Free some memory if it hasn't already been freed.
*/
static void
safefree(pp)
char **pp;
{
    if (*pp) {
	free(*pp);
	*pp = Nullch;
    }
}

/* Determine this machine's byte map for WORDs and LONGs.  A byte map is an
** array of BYTEs (sizeof (WORD) or sizeof (LONG) of them) with the 0th BYTE
** being the byte number of the high-order byte in my <type>, and so forth.
*/
static void
mybytemap(map)
BMAP *map;
{
    union {
	BYTE b[sizeof (LONG)];
	WORD w;
	LONG l;
    } u;
    register BYTE *mp;
    register int i, j;

    mp = &map->w[sizeof (WORD)];
    u.w = 1;
    for (i = sizeof (WORD); i > 0; i--) {
	for (j = 0; j < sizeof (WORD); j++) {
	    if (u.b[j] != 0)
		break;
	}
	if (j == sizeof (WORD))
	    goto bad_news;
	*--mp = j;
	while (u.b[j] != 0 && u.w)
	    u.w <<= 1;
    }

    mp = &map->l[sizeof (LONG)];
    u.l = 1;
    for (i = sizeof (LONG); i > 0; i--) {
	for (j = 0; j < sizeof (LONG); j++) {
	    if (u.b[j] != 0)
		break;
	}
	if (j == sizeof (LONG)) {
	  bad_news:
	    /* trouble -- set both to *something* consistent */
	    for (j = 0; j < sizeof (WORD); j++)
		map->w[j] = j;
	    for (j = 0; j < sizeof (LONG); j++)
		map->l[j] = j;
	    return;
	}
	*--mp = j;
	while (u.b[j] != 0 && u.l)
	    u.l <<= 1;
    }
}

/* Transform each WORD's byte-ordering in a buffer of the designated length.
*/
static void
wp_bmap(buf, len)
WORD *buf;
int len;
{
    union {
	BYTE b[sizeof (WORD)];
	WORD w;
    } in, out;
    register int i;

    if (word_same)
	return;

    while (len--) {
	in.w = *buf;
	for (i = 0; i < sizeof (WORD); i++)
	    out.b[my_bmap.w[i]] = in.b[mt_bmap.w[i]];
	*buf++ = out.w;
    }
}

/* Transform each LONG's byte-ordering in a buffer of the designated length.
*/
static void
lp_bmap(buf, len)
LONG *buf;
int len;
{
    union {
	BYTE b[sizeof (LONG)];
	LONG l;
    } in, out;
    register int i;

    if (long_same)
	return;

    while (len--) {
	in.l = *buf;
	for (i = 0; i < sizeof (LONG); i++)
	    out.b[my_bmap.l[i]] = in.b[mt_bmap.l[i]];
	*buf++ = out.l;
    }
}

#endif /* USE_MT */
