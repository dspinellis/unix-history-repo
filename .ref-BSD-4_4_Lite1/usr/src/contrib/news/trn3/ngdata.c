/* $Id: ngdata.c,v 3.0 1992/02/01 03:09:32 davison Trn $
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
#include "rcstuff.h"
#include "trn.h"
#include "cache.h"
#include "bits.h"
#include "head.h"
#include "rthread.h"
#include "rt-select.h"
#include "ng.h"
#include "intrp.h"
#include "kfile.h"
#include "final.h"
#include "rcln.h"
#include "term.h"
#include "util.h"
#include "nntp.h"
#include "ndir.h"
#include "INTERN.h"
#include "ngdata.h"

/* open the active file */

void
ngdata_init()
{
#ifdef USE_NNTP
    char *cp;

    nntp_command("LIST");	/* tell server we want the active file */
    if (nntp_check(TRUE) != NNTP_CLASS_OK) { /* and then see if that's ok */
	printf("Can't get active file from server: \n%s\n", ser_line);
	finalize(1);
    }
    time(&lastactfetch);

    cp = filexp("%P/rrnact.%$");	/* make a temporary name */
    strcpy(active_name, cp);
    actfp = fopen(active_name, "w+");	/* and get ready */
    if (actfp == Nullfp) {
	printf(cantopen,active_name) FLUSH;
	finalize(1);
    }

    activeitems = 0;
    while (1) {
	nntp_gets(ser_line, sizeof ser_line);
	if (ser_line[0] == '.')		/* while there's another line */
	    break;			/* get it and write it to */
	activeitems++;
	fputs(ser_line, actfp);
	putc('\n', actfp);
    }

    if (ferror(actfp)) {
	printf("Error writing to active file %s.\n", active_name) FLUSH;
	finalize(1);
    }
#else /* !USE_NNTP */
    ngdatansrv_init();
#endif
    if (fseek(actfp,0L,0) == -1) {	/* just get to the beginning */
	printf("Error seeking in active file.\n") FLUSH;
	finalize(1);
    }
    return;
}

bool
access_ng()
{
#ifdef USE_NNTP
    long unread, first, last;

    if (!nntp_group(ngname)) {
	toread[ng] = TR_BOGUS;
	return FALSE;
    }
    if ((lastart = getngsize(ng)) < 0)	/* this cannot happen (laugh here) */
	return FALSE;
    (void) sscanf(ser_line,"%*d%ld%ld%ld",&unread,&first,&last);

    /* NNTP mangles the high/low values when no articles are present. */
    if (!unread)
	absfirst = lastart+1;
    else {
	absfirst = (ART_NUM)first;
	if (last > lastart)
	    lastart = (ART_NUM)last;
    }
    ngmax[ng] = lastart;	/* ensure getngsize() knows the new maximum */
    first = abs1st[ng];
    abs1st[ng] = absfirst;
    if (absfirst > first)
	checkexpired(ng);
#else /* !USE_NNTP */

    if (eaccess(ngdir,5)) {		/* directory read protected? */
	if (eaccess(ngdir,0)) {
# ifdef VERBOSE
	    IF(verbose)
		printf("\nNewsgroup %s does not have a spool directory!\n",
		    ngname) FLUSH;
	    ELSE
# endif
# ifdef TERSE
		printf("\nNo spool for %s!\n",ngname) FLUSH;
# endif
# ifdef CATCHUP
	    catch_up(ng);
# endif
	} else {
# ifdef VERBOSE
	    IF(verbose)
		printf("\nNewsgroup %s is not currently accessible.\n",
		    ngname) FLUSH;
	    ELSE
# endif
# ifdef TERSE
		printf("\n%s not readable.\n",ngname) FLUSH;
# endif
	}
	toread[ng] = TR_NONE; /* make this newsgroup temporarily invisible */
	return FALSE;
    }

    /* chdir to newsgroup subdirectory */

    if (chdir(ngdir)) {
	printf(nocd,ngdir) FLUSH;
	return FALSE;
    }
    if ((lastart = getngsize(ng)) < 0)	/* this cannot happen (laugh here) */
	return FALSE;
    absfirst = abs1st[ng];
    setfoundbits();			/* might reset absfirst */
#endif /* !USE_NNTP */

    dmcount = 0;

    build_cache();
    return TRUE;
}

void
grow_ng(newlast)
ART_NUM newlast;
{
    ART_NUM tmpfirst;

    forcegrow = FALSE;
    if (newlast > lastart) {
	ART_NUM tmpart = art;
	toread[ng] += (ART_UNREAD)(newlast-lastart);
	grow_cache(newlast);
	tmpfirst = lastart+1;
	lastart = newlast;
	thread_grow();
#ifdef KILLFILES
#ifdef VERBOSE
	IF(verbose)
	    sprintf(buf,
		"%ld more article%s arrived -- processing memorized commands...\n\n",
		(long)(lastart - tmpfirst + 1),
		(lastart > tmpfirst ? "s have" : " has" ) );
	ELSE			/* my, my, how clever we are */
#endif
#ifdef TERSE
	    strcpy(buf, "More news -- auto-processing...\n\n");
#endif
	if (has_normal_kills)
	    kill_unwanted(tmpfirst,buf,TRUE);
#endif
	art = tmpart;
    }
}

void
ng_skip()
{
#ifndef USE_NNTP			/* never read it & cannot find it? */
    if (errno != ENOENT) {	/* has it not been deleted? */
	clear();
# ifdef VERBOSE
	IF(verbose)
	    printf("\n(Article %ld exists but is unreadable.)\n",(long)art)
		FLUSH;
	ELSE
# endif
# ifdef TERSE
	    printf("\n(%ld unreadable.)\n",(long)art) FLUSH;
# endif
	if (novice_delays) {
	    pad(just_a_sec);
	    sleep(2);
	}
    }
    inc_art(selected_only,FALSE);	/* try next article */

#else /* USE_NNTP */
    ART_NUM artnum;

    clear();
# ifdef VERBOSE
    IF(verbose)
	fputs("Skipping unavailable article\n",stdout);
    ELSE
# endif /* VERBOSE */
# ifdef TERSE
	fputs("Skipping\n",stdout);
# endif /* TERSE */
    if (novice_delays) {
	pad(just_a_sec/3);
	sleep(1);
    }
    art++;
    artp++;
    do {
	/* tries to grab PREFETCH_SIZE XHDRS, flagging missing articles */
	(void) fetchsubj(art, FALSE);
	artnum = art+PREFETCH_SIZE-1;
	if (artnum > lastart)
	    artnum = lastart;
	while (art <= artnum) {
	    if (!(artp->flags & AF_MISSING))
		return;
	    art++;
	    artp++;
	}
    } while (art <= lastart);
#endif /* USE_NNTP */
}

/* find the maximum article number of a newsgroup */

ART_NUM
getngsize(num)
register NG_NUM num;
{
    register int len;
    register char *nam;
    char tmpbuf[LBUFLEN];
    ART_POS oldsoft;
    long last, first;
    char ch;

    nam = rcline[num];
    len = rcnums[num] - 1;
    softtries++;
#ifdef DEBUG
    if (debug & DEB_SOFT_POINTERS)
	printf("Softptr = %ld\n",(long)softptr[num]) FLUSH;
#endif
    oldsoft = softptr[num];
#ifndef USE_NNTP
    fseek(actfp,100000L,1);    /* hopefully this forces a reread */
#endif
    if ((softptr[num] = findact(tmpbuf, nam, len, (long)oldsoft)) >= 0) {
	if (softptr[num] != oldsoft) {
	    softmisses++;
	    writesoft = TRUE;
	}
    }
    else {
	softptr[num] = 0;
	if (RCCHAR(rcchar[num]) == ':')
	    rcchar[num] = NEGCHAR;
	return TR_BOGUS;		/* well, not so quietly, actually */
    }
	
#ifdef DEBUG
    if (debug & DEB_SOFT_POINTERS) {
	printf("Should be %ld\n",(long)softptr[num]) FLUSH;
    }
#endif
#ifndef ANCIENT_NEWS
    sscanf(tmpbuf+len+1, "%ld %ld %c", &last, &first, &ch);
#else
    sscanf(tmpbuf+len+1, "%ld %c", &last, &ch);
    first = 1;
#endif
    if (!abs1st[num])
	abs1st[num] = (ART_NUM)first;
    if (!in_ng) {
	switch (ch) {
	case 'n': moderated = getval("NOPOSTRING"," (no posting)"); break;
	case 'm': moderated = getval("MODSTRING", " (moderated)"); break;
	/* This shouldn't even occur.  What are we doing in a non-existent
	   group?  Disallow it. */
	case 'x': return TR_BOGUS;
	/* what should be done about refiled groups?  rn shouldn't even
	   be in them (ie, if sci.aquaria is refiled to rec.aquaria, then
	   get the news there) */
	case '=': return TR_BOGUS;
	default: moderated = nullstr;
	}
    }
    if (last < ngmax[num])
	return ngmax[num];
    return ngmax[num] = (ART_NUM)last;
}
