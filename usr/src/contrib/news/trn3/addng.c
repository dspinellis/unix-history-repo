/* $Id: addng.c,v 3.0 1992/02/01 03:09:32 davison Trn $
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
#include "trn.h"
#include "ngdata.h"
#include "last.h"
#include "util.h"
#include "intrp.h"
#include "only.h"
#include "rcstuff.h"
#include "nntp.h"
#include "final.h"
#include "INTERN.h"
#include "addng.h"

void
addng_init()
{
    ;
}

#ifdef FINDNEWNG
/* generate a list of new newsgroups from active file */

bool
newlist(munged,checkinlist)
bool_int munged;			/* are we scanning the whole file? */
bool_int checkinlist;
{
    char *tmpname;
    register char *s, *status;
    register NG_NUM ngnum;
#ifndef ACTIVE_TIMES
    long birthof();
#endif

    tmpname = filexp(RNEWNAME);
    tmpfp = fopen(tmpname,"w+");
    if (tmpfp == Nullfp) {
	printf(cantcreate,tmpname) FLUSH;
	return FALSE;
    }
    UNLINK(tmpname);			/* be nice to the world */

    while (fgets(buf,LBUFLEN,actfp) != Nullch) {
	/* Check if they want to break out of the new newsgroups search */
	if (int_count) {
	    int_count = 0;
	    fclose(tmpfp);
	    return FALSE;
	}
	if (s = index(buf,' ')) {
	    status=s;
	    while (isdigit(*status) || isspace(*status)) status++;
	    *s++ = '\0';
	    if (strnEQ(buf,"to.",3) || *status == 'x' || *status == '=')
	        /* since = groups are refiling to another group, just
		   ignore their existence */
		continue;
#ifdef ACTIVE_TIMES
	    if (inlist(buf) && ((ngnum = find_ng(buf)) == nextrcline
				|| toread[ngnum] == TR_UNSUB)
#else
	    if (checkinlist ?
		(inlist(buf) && ((ngnum = find_ng(buf)) == nextrcline
				 || toread[ngnum] == TR_UNSUB))
	      : (find_ng(buf) == nextrcline
		 && birthof(buf,(ART_NUM)atol(s)) > lasttime)
#endif
	    ) {
					/* if not in .newsrc and younger */
					/* than the last time we checked */
		fprintf(tmpfp,"%s\n",buf);
					/* then remember said newsgroup */
	    }
#ifdef FASTNEW
	    else {			/* not really a new group */
		if (!munged) {		/* did we assume not munged? */
		    fclose(tmpfp);	/* then go back, knowing that */
		    return TRUE;	/* active file was indeed munged */
		}
	    }
#endif
	}
#ifdef DEBUG
	else
	    printf("Bad active record: %s\n",buf) FLUSH;
#endif
    }

    /* we have successfully generated the list */

    fseek(tmpfp,0L,0);			/* rewind back to the beginning */
    while (fgets(buf,LBUFLEN,tmpfp) != Nullch) {
	buf[strlen(buf)-1] = '\0';
	get_ng(buf,GNG_RELOC);		/* add newsgroup, maybe */
    }
    fclose(tmpfp);			/* be nice to ourselves */
    return FALSE;			/* do not call us again */
}

#ifdef ACTIVE_TIMES
#ifdef USE_NNTP

bool
find_new_groups()
{
    char *tmpname;
    register char *s;
    time_t server_time;
    NG_NUM oldnext = nextrcline;	/* remember # lines in newsrc */

    tmpname = filexp(RNEWNAME);
    tmpfp = fopen(tmpname,"w+");
    if (tmpfp == Nullfp) {
	printf(cantcreate,tmpname) FLUSH;
	return FALSE;
    }
    UNLINK(tmpname);			/* be nice to the world */

    server_time = nntp_time();
    if (!nntp_newgroups(lastnewtime)) {
	fclose(tmpfp);
	printf("Can't get new groups from server:\n%s\n", ser_line);
	return FALSE;
    }

    while (1) {
	nntp_gets(ser_line, sizeof ser_line);
#ifdef DEBUG
	if (debug & DEB_NNTP)
	    printf("<%s\n", ser_line) FLUSH;
#endif
	if (ser_line[0] == '.')
	    break;
	if ((s = index(ser_line, ' ')) != Nullch)
	    *s = '\0';
	fprintf(tmpfp,"%s\n",ser_line);
    }

    /* we have successfully generated the list */

    if (ftell(tmpfp)) {
	fputs("\nFinding new newsgroups:\n",stdout) FLUSH;

	fseek(tmpfp,0L,0);		/* rewind back to the beginning */
	while (fgets(buf,LBUFLEN,tmpfp) != Nullch) {
	    buf[strlen(buf)-1] = '\0';
	    get_ng(buf,0);		/* add newsgroup, maybe */
	}
	lastnewtime = server_time;	/* remember when we found new groups */
    }					/* (ends up back in .rnlast) */
    fclose(tmpfp);			/* be nice to ourselves */

    return oldnext != nextrcline;
}
#else /* !USE_NNTP */

bool
find_new_groups()
{
    register char *s;
    long lastone;
    NG_NUM oldnext = nextrcline;	/* remember # lines in newsrc */

    fstat(fileno(actfp),&filestat);	/* find active file size */
    lastactsiz = filestat.st_size;	/* just to save it in .rnlast */

    stat(ACTIVE_TIMES,&filestat);	/* did active.times file grow? */
    if (filestat.st_size == lastnewsize)
	return FALSE;
    lastnewsize = filestat.st_size;

    fputs("\nChecking for new newsgroups...\n",stdout) FLUSH;

    s = filexp(ACTIVE_TIMES);
    tmpfp = fopen(s,"r");
    if (tmpfp == Nullfp) {
	printf(cantopen,s) FLUSH;
	return FALSE;
    }
    lastone = time(Null(time_t*)) - 24L * 60 * 60 - 1;
    while (fgets(buf,LBUFLEN,tmpfp) != Nullch) {
	if ((s = index(buf, ' ')) != Nullch)
	    if ((lastone = atol(s+1)) >= lastnewtime) {
		char tmpbuf[LBUFLEN];
		*s = '\0';
		if (findact(tmpbuf, buf, s - buf, 0L) >= 0)
		    get_ng(buf,0);	/* add newsgroup, maybe */
	    }
    }
    fclose(tmpfp);
    lastnewtime = lastone+1;		/* remember time of last new group */
					/* (ends up back in .rnlast) */
    return oldnext != nextrcline;
}
#endif /* !USE_NNTP */
#else /* not ACTIVE_TIMES */

bool
find_new_groups()
{
    long oldactsiz = lastactsiz;
    NG_NUM oldnext = nextrcline;	/* remember # lines in newsrc */

    fstat(fileno(actfp),&filestat);	/* did active file grow? */

    if (filestat.st_size == lastactsiz)
	return FALSE;
    lastactsiz = filestat.st_size;	/* remember new size */

#ifdef VERBOSE
    IF(verbose)
	fputs("\nChecking active file for new newsgroups...\n",stdout) FLUSH;
    ELSE
#endif
#ifdef TERSE
	fputs("\nNew newsgroups:\n",stdout) FLUSH;
#endif

#ifdef FASTNEW				/* bad soft ptrs -> edited active */
    if (!writesoft && oldactsiz) {	/* maybe just do tail of file? */
	fseek(actfp,oldactsiz-NL_SIZE,0);
	fgets(buf,LBUFLEN,actfp);
	if (*buf == '\n' && !newlist(FALSE,FALSE))
	    goto bugout;
    }
#endif
    fseek(actfp,0L,0);		/* rewind active file */
    newlist(TRUE,FALSE);		/* sure hope they use hashing... */
bugout:
    return oldnext != nextrcline;
}

/* return creation time of newsgroup */

long
birthof(ngnam,ngsize)
char *ngnam;
ART_NUM ngsize;
{
#ifdef USE_NNTP		/* ngsize not used */
    long tot;

    if (!nntp_group(ngnam))
	return 0;	/* not a real group */
    (void) sscanf(ser_line,"%*d%ld",&tot);
    if (tot > 0)
	return time(Null(long *));
    return 0;

#else /* !USE_NNTP */
    char tst[128];

    sprintf(tst, ngsize ? "%s/%s/1" : "%s/%s" ,spool,getngdir(ngnam));
    if (stat(tst,&filestat) < 0)
	return (ngsize ? 0L : time(Null(long *)));
    /* not there, assume something good */
    return filestat.st_mtime;

#endif /* !USE_NNTP */
}
#endif /* ACTIVE_TIMES */

bool
scanactive()
{
    NG_NUM oldnext = nextrcline;	/* remember # lines in newsrc */

    fseek(actfp,0L,0);
    newlist(TRUE,TRUE);
    if (nextrcline != oldnext) {	/* did we add any new groups? */
	return TRUE;
    }
    return FALSE;
}

#endif

