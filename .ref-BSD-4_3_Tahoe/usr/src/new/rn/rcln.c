/* $Header: rcln.c,v 4.3.1.3 86/11/04 15:57:03 lwall Exp $
 *
 * $Log:	rcln.c,v $
 * Revision 4.3.1.3  86/11/04  15:57:03  lwall
 * realloc attempted on unalloced area.
 * 
 * Revision 4.3.1.2  85/07/23  17:39:08  lwall
 * Oops, was freeing a static buf on -c in checkexpired.
 * 
 * Revision 4.3.1.1  85/05/10  11:37:08  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:45:36  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "rcstuff.h"
#include "ngdata.h"
#include "INTERN.h"
#include "rcln.h"

void
rcln_init()
{
    ;
}

#ifdef CATCHUP
void
catch_up(ngx)
NG_NUM ngx;
{
    char tmpbuf[128];
    
#ifdef VERBOSE
    IF(verbose)
	printf("\nMarking %s as all read.\n",rcline[ngx]) FLUSH;
    ELSE
#endif
#ifdef TERSE
	fputs("\nMarked read\n",stdout) FLUSH;
#endif
    sprintf(tmpbuf,"%s: 1-%ld", rcline[ngx],(long)getngsize(ngx));
    free(rcline[ngx]);
    rcline[ngx] = savestr(tmpbuf);
    *(rcline[ngx] + rcnums[ngx] - 1) = '\0';
    write_rc();
}
#endif

/* add an article number to a newsgroup, if it isn't already read */

int
addartnum(artnum,ngnam)
ART_NUM artnum;
char *ngnam;
{
    register NG_NUM ngnum = find_ng(ngnam);
    register char *s, *t, *maxt = Nullch;
    ART_NUM min = 0, max = -1, lastnum = 0;
    char *mbuf;
    bool morenum;

    if (!artnum)
	return 0;
    if (ngnum == nextrcline || !rcnums[ngnum])
					/* not found in newsrc? */
	return 0;
#ifdef CACHEFIRST
    if (!abs1st[ngnum])
#else
    if (!toread[ngnum])
#endif
					/* now is a good time to trim down */
	set_toread(ngnum);		/* the list due to expires if we */
					/* have not yet. */
#ifdef DEBUGGING
    if (artnum > ngmax[ngnum] + 10	/* allow for incoming articles */
       ) {
	printf("\nCorrupt Xref line!!!  %ld --> %s(1..%ld)\n",
	    artnum,ngnam,
	    ngmax[ngnum]) FLUSH;
	paranoid = TRUE;		/* paranoia reigns supreme */
	return -1;			/* hope this was the first newsgroup */
    }
#endif

    if (toread[ngnum] == TR_BOGUS)
	return 0;
#ifdef DEBUGGING
    if (debug & DEB_XREF_MARKER) {
	printf("%ld->\n%s%c%s\n",(long)artnum,rcline[ngnum],rcchar[ngnum],
	  rcline[ngnum] + rcnums[ngnum]) FLUSH;
    }
#endif
    s = rcline[ngnum] + rcnums[ngnum];
    while (*s == ' ') s++;		/* skip spaces */
    t = s;
    while (isdigit(*s) && artnum >= (min = atol(s))) {
					/* while it might have been read */
	for (t = s; isdigit(*t); t++) ;	/* skip number */
	if (*t == '-') {		/* is it a range? */
	    t++;			/* skip to next number */
	    if (artnum <= (max = atol(t)))
		return 0;		/* it is in range => already read */
	    lastnum = max;		/* remember it */
	    maxt = t;			/* remember position in case we */
					/* want to overwrite the max */
	    while (isdigit(*t)) t++;	/* skip second number */
	}
	else {
	    if (artnum == min)		/* explicitly a read article? */
		return 0;
	    lastnum = min;		/* remember what the number was */
	    maxt = Nullch;		/* last one was not a range */
	}
	while (*t && !isdigit(*t)) t++;	/* skip comma and any spaces */
	s = t;
    }
    
    /* we have not read it, so insert the article number before s */
    
    morenum = isdigit(*s);		/* will it need a comma after? */
    *(rcline[ngnum] + rcnums[ngnum] - 1) = rcchar[ngnum];
    mbuf = safemalloc((MEM_SIZE)(strlen(s) + (s-rcline[ngnum]) + 8));
    strcpy(mbuf,rcline[ngnum]);		/* make new rc line */
    if (maxt && lastnum && artnum == lastnum+1)
    					/* can we just extend last range? */
	t = mbuf + (maxt-rcline[ngnum]);/* then overwrite previous max */
    else {
	t = mbuf + (t-rcline[ngnum]);	/* point t into new line instead */
	if (lastnum) {			/* have we parsed any line? */
	    if (!morenum)		/* are we adding to the tail? */
		*t++ = ',';		/* supply comma before */
	    if (!maxt && artnum == lastnum+1 && *(t-1) == ',')
					/* adjacent singletons? */
		*(t-1) = '-';		/* turn them into a range */
	}
    }
    if (morenum) {			/* is there more to life? */
	if (min == artnum+1) {		/* can we consolidate further? */
	    bool range_before = (*(t-1) == '-');
	    bool range_after;
	    char *nextmax;

	    for (nextmax = s; isdigit(*nextmax); nextmax++) ;
	    range_after = *nextmax++ == '-';
	    
	    if (range_before)
		*t = '\0';		/* artnum is redundant */
	    else
		sprintf(t,"%ld-",(long)artnum);/* artnum will be new min */
	    
	    if (range_after)
		s = nextmax;		/* *s is redundant */
	/*  else
		s = s */		/* *s is new max */
	}
	else
	    sprintf(t,"%ld,",(long)artnum);	/* put the number and comma */
    }
    else
	sprintf(t,"%ld",(long)artnum);	/* put the number there (wherever) */
    strcat(t,s);			/* copy remainder of line */
#ifdef DEBUGGING
    if (debug & DEB_XREF_MARKER) {
	printf("%s\n",mbuf) FLUSH;
    }
#endif
    free(rcline[ngnum]);
    rcline[ngnum] = mbuf;		/* pull the switcheroo */
    *(rcline[ngnum] + rcnums[ngnum] - 1) = '\0';
					/* wipe out : or ! */
    if (toread[ngnum] > TR_NONE)	/* lest we turn unsub into bogus */
	--toread[ngnum];
    return 0;
}

#ifdef MCHASE
/* delete an article number from a newsgroup, if it is there */

void
subartnum(artnum,ngnam)
register ART_NUM artnum;
char *ngnam;
{
    register NG_NUM ngnum = find_ng(ngnam);
    register char *s, *t;
    register ART_NUM min, max;
    char *mbuf;
    int curlen;

    if (!artnum)
	return;
    if (ngnum == nextrcline || !rcnums[ngnum])
	return;				/* not found in newsrc? */
#ifdef DEBUGGING
    if (debug & DEB_XREF_MARKER) {
	printf("%ld<-\n%s%c%s\n",(long)artnum,rcline[ngnum],rcchar[ngnum],
	  rcline[ngnum] + rcnums[ngnum]) FLUSH;
    }
#endif
    s = rcline[ngnum] + rcnums[ngnum];
    while (*s == ' ') s++;		/* skip spaces */
    
    /* a little optimization, since it is almost always the last number */
    
    for (t=s; *t; t++) ;		/* find end of string */
    curlen = t-rcline[ngnum];
    for (t--; isdigit(*t); t--) ;	/* find previous delim */
    if (*t == ',' && atol(t+1) == artnum) {
	*t = '\0';
	if (toread[ngnum] >= TR_NONE)
	    ++toread[ngnum];
#ifdef DEBUGGING
	if (debug & DEB_XREF_MARKER)
	    printf("%s%c %s\n",rcline[ngnum],rcchar[ngnum],s) FLUSH;
#endif
	return;
    }

    /* not the last number, oh well, we may need the length anyway */

    while (isdigit(*s) && artnum >= (min = atol(s))) {
					/* while it might have been read */
	for (t = s; isdigit(*t); t++) ;	/* skip number */
	if (*t == '-') {		/* is it a range? */
	    t++;			/* skip to next number */
	    max = atol(t);
	    while (isdigit(*t)) t++;	/* skip second number */
	    if (artnum <= max) {
					/* it is in range => already read */
		if (artnum == min) {
		    min++;
		    artnum = 0;
		}
		else if (artnum == max) {
		    max--;
		    artnum = 0;
		}
		*(rcline[ngnum] + rcnums[ngnum] - 1) = rcchar[ngnum];
		mbuf = safemalloc((MEM_SIZE)(curlen + (artnum?15:2)));
		*s = '\0';
		strcpy(mbuf,rcline[ngnum]);	/* make new rc line */
		s = mbuf + (s-rcline[ngnum]);
					/* point s into mbuf now */
		if (artnum) {		/* split into two ranges? */
		    prange(s,min,artnum-1);
		    s += strlen(s);
		    *s++ = ',';
		    prange(s,artnum+1,max);
		}
		else			/* only one range */
		    prange(s,min,max);
		s += strlen(s);
		strcpy(s,t);		/* copy remainder over */
#ifdef DEBUGGING
		if (debug & DEB_XREF_MARKER) {
		    printf("%s\n",mbuf) FLUSH;
		}
#endif
		free(rcline[ngnum]);
		rcline[ngnum] = mbuf;	/* pull the switcheroo */
		*(rcline[ngnum] + rcnums[ngnum] - 1) = '\0';
					/* wipe out : or ! */
		if (toread[ngnum] >= TR_NONE)
		    ++toread[ngnum];
		return;
	    }
	}
	else {
	    if (artnum == min) {	/* explicitly a read article? */
		if (*t == ',')		/* pick a comma, any comma */
		    t++;
		else if (s[-1] == ',')
		    s--;
		else if (s[-2] == ',')	/* (in case of space) */
		    s -= 2;
		strcpy(s,t);		/* no need to realloc */
		if (toread[ngnum] >= TR_NONE)
		    ++toread[ngnum];
#ifdef DEBUGGING
		if (debug & DEB_XREF_MARKER) {
		    printf("%s%c%s\n",rcline[ngnum],rcchar[ngnum],
		      rcline[ngnum] + rcnums[ngnum]) FLUSH;
		}
#endif
		return;
	    }
	}
	while (*t && !isdigit(*t)) t++;	/* skip comma and any spaces */
	s = t;
    }
}

void
prange(where,min,max)
char *where;
ART_NUM min,max;
{
    if (min == max)
	sprintf(where,"%ld",(long)min);
    else
	sprintf(where,"%ld-%ld",(long)min,(long)max);
}
#endif

/* calculate the number of unread articles for a newsgroup */

void
set_toread(ngnum)
register NG_NUM ngnum;
{
    register char *s, *c, *h;
    char tmpbuf[64], *mybuf = tmpbuf;
    char *nums;
    int length;
#ifdef CACHEFIRST
    bool virgin_ng = (!abs1st[ngnum]);
#endif
    ART_NUM ngsize = getngsize(ngnum);
    ART_NUM unread = ngsize;
    ART_NUM newmax;

#ifdef DEBUGGING
    ngmax[ngnum] = ngsize;		/* for checking out-of-range Xrefs */
#endif
    if (ngsize == TR_BOGUS) {
	printf("Warning!  Bogus newsgroup: %s\n",rcline[ngnum]) FLUSH;
	paranoid = TRUE;
	toread[ngnum] = TR_BOGUS;
	return;
    }
#ifdef CACHEFIRST
    if (virgin_ng)
#else
    if (!toread[ngnum])
#endif
    {
	sprintf(tmpbuf," 1-%ld",(long)ngsize);
	if (strNE(tmpbuf,rcline[ngnum]+rcnums[ngnum]))
	    checkexpired(ngnum,ngsize);	/* this might realloc rcline */
    }
    nums = rcline[ngnum]+rcnums[ngnum];
    length = strlen(nums);
    if (length >= 60)
	mybuf = safemalloc((MEM_SIZE)(length+5));
    strcpy(mybuf,nums);
    mybuf[length++] = ',';
    mybuf[length] = '\0';
    for (s = mybuf; isspace(*s); s++)
	    ;
    for ( ; (c = index(s,',')) != Nullch ; s = ++c) {
				    /* for each range */
	*c = '\0';			/* keep index from running off */
	if ((h = index(s,'-')) != Nullch)	/* find - in range, if any */
	    unread -= (newmax = atol(h+1)) - atol(s) + 1;
	else if (newmax = atol(s))
	    unread--;		/* recalculate length */
	if (newmax > ngsize) {	/* paranoia check */
	    unread = -1;
	    break;
	}
    }
    if (unread >= 0)		/* reasonable number? */
	toread[ngnum] = (ART_UNREAD)unread;
					/* remember how many are left */
    else {				/* SOMEONE RESET THE NEWSGROUP!!! */
	toread[ngnum] = (ART_UNREAD)ngsize;
					/* assume nothing carried over */
	printf("Warning!  Somebody reset %s--assuming nothing read.\n",
	    rcline[ngnum]) FLUSH;
	*(rcline[ngnum] + rcnums[ngnum]) = '\0';
	paranoid = TRUE;		/* enough to make a guy paranoid */
    }
    if (mybuf != tmpbuf)
	free(mybuf);
    if (rcchar[ngnum] == NEGCHAR)
	toread[ngnum] = TR_UNSUB;
}

/* make sure expired articles are marked as read */

void
checkexpired(ngnum,ngsize)
register NG_NUM ngnum;
ART_NUM ngsize;
{
    register ART_NUM a1st = getabsfirst(ngnum,ngsize);
    register char *s, *t;
    register ART_NUM num, lastnum = 0;
    char *mbuf, *newnum;

    if (a1st<=1)
	return;
#ifdef DEBUGGING
    if (debug & DEB_XREF_MARKER) {
	printf("1-%ld->\n%s%c%s\n",(long)(a1st-1),rcline[ngnum],rcchar[ngnum],
	  rcline[ngnum] + rcnums[ngnum]) FLUSH;
    }
#endif
    for (s = rcline[ngnum] + rcnums[ngnum]; isspace(*s); s++);
    while (*s && (num = atol(s)) <= a1st) {
	while (isdigit(*s)) s++;
	while (*s && !isdigit(*s)) s++;
	lastnum = num;
    }
    if (*s) {
	if (s[-1] == '-') {			/* landed in a range? */
	    if (lastnum != 1)
		sprintf(rcline[ngnum]+rcnums[ngnum]," 1-%s",s);
	    goto ret;
	}
    }
    /* s now points to what should follow first range */
    if (s - rcline[ngnum] > rcnums[ngnum] + 10) 
	mbuf = rcline[ngnum];
    else {
	mbuf = safemalloc((MEM_SIZE)(rcnums[ngnum] + strlen(s) + 10));
	strcpy(mbuf,rcline[ngnum]);
    }
    newnum = t = mbuf+rcnums[ngnum];
    sprintf(t," 1-%ld",(long)(a1st - (lastnum != a1st)));
    if (*s) {
	t += strlen(t);
	*t++ = ',';
	strcpy(t,s);
    }
    if (!checkflag && mbuf == rcline[ngnum]) {
	rcline[ngnum] = saferealloc(rcline[ngnum],
	    (MEM_SIZE)(rcnums[ngnum] + strlen(newnum) + 1));
    }
    else {
	if (!checkflag)
	    free(rcline[ngnum]);
	rcline[ngnum] = mbuf;
    }

ret:;		/* semicolon in case DEBUGGING undefined */
#ifdef DEBUGGING
    if (debug & DEB_XREF_MARKER) {
	printf("%s%c%s\n",rcline[ngnum],rcchar[ngnum],
	  rcline[ngnum] + rcnums[ngnum]) FLUSH;
    }
#endif
}

