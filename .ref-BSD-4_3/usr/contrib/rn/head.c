/* $Header: head.c,v 4.3.1.2 85/05/10 13:47:25 lwall Exp $
 *
 * $Log:	head.c,v $
 * Revision 4.3.1.2  85/05/10  13:47:25  lwall
 * Added debugging stuff.
 * 
 * Revision 4.3.1.1  85/05/10  11:32:30  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:38:21  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "artio.h"
#include "bits.h"
#include "util.h"
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
}

#ifdef DEBUGGING
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

#ifdef DEBUGGING
    if (debug & 4)
	dumpheader("start_header\n");
#endif
    for (i=0; i<HEAD_LAST; i++) {
	htype[i].ht_minpos = -1;
	htype[i].ht_maxpos = 0;
    }
    in_header = SOME_LINE;
    first_one = FALSE;
#ifdef ASYNC_PARSE
    parsed_art = artnum;
#endif
}

bool
parseline(art_buf,newhide,oldhide)
char *art_buf;
int newhide, oldhide;
{
    if (*art_buf == ' ' || *art_buf == '\t')
					/* header continuation line? */
	return oldhide;
    else {				/* maybe another header line */
	char *s;

	if (first_one) {		/* did we just pass 1st occurance? */
	    first_one = FALSE;
	    htype[in_header].ht_maxpos = artpos;
					/* remember where line left off */
	}
	s = index(art_buf,':');
	if (s == Nullch || s-art_buf > LONGKEY+2) {
			    /* is it the end of the header? */
	    htype[PAST_HEADER].ht_minpos =
		(*art_buf == '\n') ? ftell(artfp) : artpos;
			    /* remember where body starts */
	    in_header = PAST_HEADER;
	}
	else {	/* it is a new header line */
	    in_header = set_line_type(art_buf,s);
	    first_one = (htype[in_header].ht_minpos < 0);
	    if (first_one)
		htype[in_header].ht_minpos = artpos;
#ifdef DEBUGGING
	    if (debug & 4)
		dumpheader(art_buf);
#endif
	    if (htype[in_header].ht_flags & HT_HIDE)
		return newhide;
	}
    }
    return FALSE;			/* don't hide this line */
}

#ifdef ASYNC_PARSE
int
parse_maybe(artnum)
ART_NUM artnum;
{
    char tmpbuf[LBUFLEN];

    if (parsed_art == artnum)
	return 0;
    /* no maybe about it now */
    if (artopen(artnum) == Nullfp) {
	return -1;
    }
    start_header(artnum);
    while (in_header) {
	artpos = ftell(artfp);
	if (fgets(tmpbuf,LBUFLEN,artfp) == Nullch)
	    break;
	parseline(tmpbuf,FALSE,FALSE);
    }
    in_header = PAST_HEADER;
    return 0;
}
#endif

/* get the subject line for an article */

char *
fetchsubj(artnum,current_subject,copy)
ART_NUM artnum;				/* article to get subject from */
bool current_subject;			/* is it in a parsed header? */
bool copy;				/* do you want it savestr()ed? */
{
    char *s = Nullch, *t;

#ifdef CACHESUBJ
    if (!subj_list) {
	register ART_NUM i;
	

#ifndef lint
	subj_list =
	  (char**)safemalloc((MEM_SIZE)((OFFSET(lastart)+2)*sizeof(char *)));
#endif lint
	for (i=0; i<=OFFSET(lastart); i++)
	    subj_list[i] = Nullch;
    }
    if (!artnum || artnum > lastart)
	s = nullstr;
    else
	s = subj_list[OFFSET(artnum)];
#endif
    if (s == Nullch) {
	if (current_subject) {
	    s = fetchlines(artnum,SUBJ_LINE);
#ifdef CACHESUBJ
	    subj_list[OFFSET(artnum)] = s;
#endif
	}
	else {
	    s = safemalloc((MEM_SIZE)256);
	    *s = '\0';
	    if (artopen(artnum) != Nullfp) {
		do {
		    if (fgets(s,256,artfp) == Nullch)
			strcpy(s, "Title: \n");
		} while (strnNE(s,"Title:",6) && strnNE(s,"Subject:",8));
		s[strlen(s)-1] = '\0';
		t = index(s,':')+1;
		while (*t == ' ') t++;
		strcpy(s, t);
	    }
	    s = saferealloc(s, (MEM_SIZE)strlen(s)+1);
#ifdef CACHESUBJ
	    subj_list[OFFSET(artnum)] = s;
#endif 
	}
    }
#ifdef CACHESUBJ
    if (copy) {
	t = savestr(s);
	return t;
    }
    else
	return s;
#else
    if (copy)
	return s;
    else {
	safecpy(cmd_buf,s,CBUFLEN);	/* hope this is okay--we're */
	free(s);
	return cmd_buf;			/* really scraping for space here */
    }
#endif
}

/* get header lines from an article */

char *
fetchlines(artnum,which_line)
ART_NUM artnum;				/* article to get line from */
int which_line;				/* type of line desired */
{
    char *newbuf, *t, tmp_buf[LBUFLEN];
    register ART_POS curpos;
    int size;
    register ART_POS firstpos;
    register ART_POS lastpos;
    
#ifdef ASYNC_PARSE
    if (parse_maybe(artnum))
	artnum = 0;
#endif
    firstpos = htype[which_line].ht_minpos;
    lastpos = htype[which_line].ht_maxpos;
    if (!artnum || firstpos < 0 || artopen(artnum) == Nullfp) {
	newbuf = safemalloc((unsigned int)1);
	*newbuf = '\0';
	return newbuf;
    }
#ifndef lint
    size = lastpos - firstpos + 1;
#else
    size = Null(int);
#endif lint
#ifdef DEBUGGING
    if (debug && (size < 1 || size > 1000)) {
	printf("Firstpos = %ld, lastpos = %ld\n",(long)firstpos,(long)lastpos);
	gets(tmp_buf);
    }
#endif
    newbuf = safemalloc((unsigned int)size);
    *newbuf = '\0';
    fseek(artfp,firstpos,0);
    for (curpos = firstpos; curpos < lastpos; curpos = ftell(artfp)) {
	if (fgets(tmp_buf,LBUFLEN,artfp) == Nullch)
	    break;
	if (*tmp_buf == ' ' || *tmp_buf == '\t')
	    t = tmp_buf;
	else
	    t = index(tmp_buf,':')+1;
	if (t == Nullch)
	    break;
	else {
	    while (*t == ' ' || *t == '\t') t++;
	    safecat(newbuf,t,size);
	}
    }
    return newbuf;
}

