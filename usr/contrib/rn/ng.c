/* $Header: ng.c,v 4.3.1.6 85/09/10 11:03:42 lwall Exp $
 *
 * $Log:	ng.c,v $
 * Revision 4.3.1.6  85/09/10  11:03:42  lwall
 * Improved %m in in_char().
 * 
 * Revision 4.3.1.5  85/09/05  12:34:37  lwall
 * Catchup command could make unread article count too big.
 * 
 * Revision 4.3.1.4  85/07/23  18:19:46  lwall
 * Added MAILCALL environment variable.
 * 
 * Revision 4.3.1.3  85/05/16  16:48:09  lwall
 * Fixed unsubsubscribe.
 * 
 * Revision 4.3.1.2  85/05/13  09:29:28  lwall
 * Added CUSTOMLINES option.
 * 
 * Revision 4.3.1.1  85/05/10  11:36:00  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:43:43  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "rn.h"
#include "term.h"
#include "final.h"
#include "util.h"
#include "artsrch.h"
#include "cheat.h"
#include "help.h"
#include "kfile.h"
#include "rcstuff.h"
#include "head.h"
#include "artstate.h"
#include "bits.h"
#include "art.h"
#include "artio.h"
#include "ngstuff.h"
#include "intrp.h"
#include "respond.h"
#include "ngdata.h"
#include "backpage.h"
#include "rcln.h"
#include "last.h"
#include "search.h"
#include "INTERN.h"
#include "ng.h"
#include "artstate.h"			/* somebody has to do it */

/* art_switch() return values */

#define AS_NORM 0
#define AS_INP 1
#define AS_ASK 2
#define AS_CLEAN 3

ART_NUM recent_art = 0;		/* previous article # for '-' command */
ART_NUM curr_art = 0;                /* current article # */
int exit_code = NG_NORM;

void
ng_init()
{

#ifdef KILLFILES
    open_kfile(KF_GLOBAL);
#endif
#ifdef CUSTOMLINES
    init_compex(&hide_compex);
    init_compex(&page_compex);
#endif
}

/* do newsgroup on line ng with name ngname */

/* assumes that we are chdir'ed to SPOOL, and assures that that is
 * still true upon return, but chdirs to SPOOL/ngname in between
 *
 * If you can understand this routine, you understand most of the program.
 * The basic structure is:
 *	for each desired article
 *		for each desired page
 *			for each line on page
 *				if we need another line from file
 *					get it
 *					if it's a header line
 *						do special things
 *				for each column on page
 *					put out a character
 *				end loop
 *			end loop
 *		end loop
 *	end loop
 *
 *	(Actually, the pager is in another routine.)
 *
 * The chief problem is deciding what is meant by "desired".  Most of
 * the messiness of this routine is due to the fact that people want
 * to do unstructured things all the time.  I have used a few judicious
 * goto's where I thought it improved readability.  The rest of the messiness
 * arises from trying to be both space and time efficient.  Have fun.
 */

int
do_newsgroup(start_command)
char *start_command;			/* command to fake up first */
{
    char oldmode = mode;
    register long i;			/* scratch */
    int skipstate;			/* how many unavailable articles */
					/*   have we skipped already? */
    
    char *whatnext = "%sWhat next? [%s]";

#ifdef ARTSEARCH
    srchahead = (scanon && ((ART_NUM)toread[ng]) >= scanon ? -1 : 0);
					/* did they say -S? */
#endif
    
    mode = 'a';
    recent_art = curr_art = 0;
    exit_code = NG_NORM;
    if (eaccess(ngdir,5)) {		/* directory read protected? */
	if (eaccess(ngdir,0)) {
#ifdef VERBOSE
	    IF(verbose)
		printf("\nNewsgroup %s does not have a spool directory!\n",
		    ngname) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		printf("\nNo spool for %s!\n",ngname) FLUSH;
#endif
#ifdef CATCHUP
	    catch_up(ng);
#endif
	    toread[ng] = TR_NONE;
	}
	else {
#ifdef VERBOSE
	    IF(verbose)
		printf("\nNewsgroup %s is not currently accessible.\n",
		    ngname) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		printf("\n%s not readable.\n",ngname) FLUSH;
#endif
	    toread[ng] = TR_NONE;	/* make this newsgroup invisible */
					/* (temporarily) */
	}
	mode = oldmode;
	return -1;
    }

    /* chdir to newsgroup subdirectory */

    if (chdir(ngdir)) {
	printf(nocd,ngdir) FLUSH;
	mode = oldmode;
	return -1;
    }

#ifdef CACHESUBJ
    subj_list = Null(char **);		/* no subject list till needed */
#endif
    
    /* initialize control bitmap */

    if (initctl()) {
	mode = oldmode;
	return -1;
    }

    /* FROM HERE ON, RETURN THRU CLEANUP OR WE ARE SCREWED */
    
    in_ng = TRUE;			/* tell the world we are here */
    forcelast = TRUE;			/* if 0 unread, do not bomb out */
    art=firstart;
    
    /* remember what newsgroup we were in for sake of posterity */

    writelast();

    /* see if there are any special searches to do */

#ifdef KILLFILES
    open_kfile(KF_LOCAL);
#ifdef VERBOSE
    IF(verbose)
	kill_unwanted(firstart,"Looking for articles to kill...\n\n",TRUE);
    ELSE
#endif
#ifdef TERSE
	kill_unwanted(firstart,"Killing...\n\n",TRUE);
#endif
#endif
    
    /* do they want a special top line? */

    firstline = getval("FIRSTLINE",Nullch);

    /* custom line suppression, custom page ending */

#ifdef CUSTOMLINES
    if (hideline = getval("HIDELINE",Nullch))
	compile(&hide_compex,hideline,TRUE,TRUE);
    if (pagestop = getval("PAGESTOP",Nullch))
	compile(&page_compex,pagestop,TRUE,TRUE);
#endif

    /* now read each unread article */

    rc_changed = doing_ng = TRUE;	/* enter the twilight zone */
    skipstate = 0;			/* we have not skipped anything (yet) */
    checkcount = 0;			/* do not checkpoint for a while */
    do_fseek = FALSE;			/* start 1st article at top */
    if (art > lastart)
	art=firstart;			/* init the for loop below */
    for (; art<=lastart+1; ) {		/* for each article */

	/* do we need to "grow" the newsgroup? */

	if (art > lastart || forcegrow)
	    grow_ctl();
	check_first(art);		/* make sure firstart is still 1st */
	if (start_command) {		/* fake up an initial command? */
	    prompt = whatnext;
	    strcpy(buf,start_command);
	    free(start_command);
	    start_command = Nullch;
	    art = lastart+1;
	    goto article_level;
	}
	if (art>lastart) {		/* are we off the end still? */
	    ART_NUM ucount = 0;		/* count of unread articles left */

	    for (i=firstart; i<=lastart; i++)
		if (!(ctl_read(i)))
		    ucount++;		/* count the unread articles */
#ifdef DEBUGGING
	    /*NOSTRICT*/
	    if (debug && ((ART_NUM)toread[ng]) != ucount)
		printf("(toread=%ld sb %ld)",(long)toread[ng],(long)ucount)
		  FLUSH;
#endif
	    /*NOSTRICT*/
	    toread[ng] = (ART_UNREAD)ucount;	/* this is perhaps pointless */
	    art = lastart + 1;		/* keep bitmap references sane */
	    if (art != curr_art) {
		recent_art = curr_art;
					/* remember last article # (for '-') */
		curr_art = art;      /* remember this article # */
	    }
	    if (erase_screen)
		clear();			/* clear the screen */
	    else
		fputs("\n\n",stdout) FLUSH;
#ifdef VERBOSE
	    IF(verbose)
		printf("End of newsgroup %s.",ngname);
					/* print pseudo-article */
	    ELSE
#endif
#ifdef TERSE
		printf("End of %s",ngname);
#endif
	    if (ucount) {
		printf("  (%ld article%s still unread)",
		    (long)ucount,ucount==1?nullstr:"s");
	    }
	    else {
		if (!forcelast)
		    goto cleanup;	/* actually exit newsgroup */
	    }
	    prompt = whatnext;
#ifdef ARTSEARCH
	    srchahead = 0;		/* no more subject search mode */
#endif
	    fputs("\n\n",stdout) FLUSH;
	    skipstate = 0;		/* back to none skipped */
	}
	else if (!reread && was_read(art)) {
					/* has this article been read? */
	    art++;			/* then skip it */
	    continue;
	}
	else if
	  (!reread && !was_read(art)
	    && artopen(art) == Nullfp) {	/* never read it, & cannot find it? */
	    if (errno != ENOENT) {	/* has it not been deleted? */
#ifdef VERBOSE
		IF(verbose)
		    printf("\n(Article %ld exists but is unreadable.)\n",
			(long)art) FLUSH;
		ELSE
#endif
#ifdef TERSE
		    printf("\n(%ld unreadable.)\n",(long)art) FLUSH;
#endif
		skipstate = 0;
		sleep(2);
	    }
	    switch(skipstate++) {
	    case 0:
		clear();
#ifdef VERBOSE
		IF(verbose)
		    fputs("Skipping unavailable article",stdout);
		ELSE
#endif
#ifdef TERSE
		    fputs("Skipping",stdout);
#endif
		for (i = just_a_sec/3; i; --i)
		    putchar(PC);
		fflush(stdout);
		sleep(1);
		break;
	    case 1:
		fputs("..",stdout);
		fflush(stdout);
		break;
	    default:
		putchar('.');
		fflush(stdout);
#define READDIR
#ifdef READDIR
		{			/* fast skip patch */
		    ART_NUM newart;
		    
		    if (! (newart=getngmin(".",art)))
			newart = lastart+1;
		    for (i=art; i<newart; i++)
			oneless(i);
		    art = newart - 1;
		}
#endif
		break;
	    }
	    oneless(art);		/* mark deleted as read */
	    art++;			/* try next article */
	    continue;
	}
	else {				/* we have a real live article */
	    skipstate = 0;		/* back to none skipped */
	    if (art != curr_art) {
		recent_art = curr_art;
					/* remember last article # (for '-') */
		curr_art = art;      /* remember this article # */
	    }
	    if (!do_fseek) {		/* starting at top of article? */
		artline = 0;		/* start at the beginning */
		topline = -1;		/* and remember top line of screen */
					/*  (line # within article file) */
	    }
	    clear();			/* clear screen */
	    artopen(art);		/* make sure article file is open */
	    if (artfp == Nullfp) {	/* could not find article? */
		printf("Article %ld of %s is not available.\n\n",
		    (long)art,ngname) FLUSH;
		prompt = whatnext;
#ifdef ARTSEARCH
		srchahead = 0;
#endif
	    }
	    else {			/* found it, so print it */
		switch (do_article()) {
		case DA_CLEAN:		/* quit newsgroup */
		    goto cleanup;
		case DA_TOEND:		/* do not mark as read */
		    goto reask_article; 
		case DA_RAISE:		/* reparse command at end of art */
		    goto article_level;
		case DA_NORM:		/* normal end of article */
		    break;
		}
	    }
	    mark_as_read(art);		 /* mark current article as read */
	    reread = FALSE;
	    do_hiding = TRUE;
#ifdef ROTATION
	    rotate = FALSE;
#endif
	}

/* if these gotos bother you, think of this as a little state machine */

reask_article:
#ifdef MAILCALL
	setmail();
#endif
	setdfltcmd();
#ifdef CLEAREOL
	if (erase_screen && can_home_clear)	/* PWP was here */
	    clear_rest();
#endif CLEAREOL
	unflush_output();		/* disable any ^O in effect */
	standout();			/* enter standout mode */
	printf(prompt,mailcall,dfltcmd);/* print prompt, whatever it is */
	un_standout();			/* leave standout mode */
	putchar(' ');
	fflush(stdout);
reinp_article:
	eat_typeahead();
#ifdef PENDING
	look_ahead();			/* see what we can do in advance */
	if (!input_pending())
	    collect_subjects();		/* loads subject cache until */
					/* input is pending */
#endif
	getcmd(buf);
	if (errno || *buf == '\f') {
	    if (LINES < 100 && !int_count)
		*buf = '\f';		/* on CONT fake up refresh */
	    else {
		putchar('\n') FLUSH;		/* but only on a crt */
		goto reask_article;
	    }
	}
article_level:

	/* parse and process article level command */

	switch (art_switch()) {
	case AS_INP:			/* multichar command rubbed out */
	    goto reinp_article;
	case AS_ASK:			/* reprompt "End of article..." */
	    goto reask_article;
	case AS_CLEAN:			/* exit newsgroup */
	    goto cleanup;
	case AS_NORM:			/* display article art */
	    break;
	}
    }					/* end of article selection loop */
    
/* shut down newsgroup */

cleanup:
#ifdef KILLFILES
    kill_unwanted(firstart,"\nCleaning up...\n\n",FALSE);
					/* do cleanup from KILL file, if any */
#endif
    in_ng = FALSE;			/* leave newsgroup state */
    if (artfp != Nullfp) {		/* article still open? */
	fclose(artfp);			/* close it */
	artfp = Nullfp;			/* and tell the world */
	openart = 0;
    }
    putchar('\n') FLUSH;
    yankback();				/* do a Y command */
    restore_ng();			/* reconstitute .newsrc line */
    doing_ng = FALSE;			/* tell sig_catcher to cool it */
    free(ctlarea);			/* return the control area */
#ifdef CACHESUBJ
    if (subj_list) {
	for (i=OFFSET(lastart); i>=0; --i)
	    if (subj_list[i])
		free(subj_list[i]);
#ifndef lint
	free((char*)subj_list);
#endif lint
    }
#endif
    write_rc();				/* and update .newsrc */
    rc_changed = FALSE;			/* tell sig_catcher it is ok */
    if (chdir(spool)) {
	printf(nocd,spool) FLUSH;
	sig_catcher(0);
    }
#ifdef KILLFILES
    if (localkfp) {
	fclose(localkfp);
	localkfp = Nullfp;
    }
#endif
    mode = oldmode;
    return exit_code;
}					/* Whew! */

/* decide what to do at the end of an article */

int
art_switch()
{
    register ART_NUM i;
      
    setdef(buf,dfltcmd);
#ifdef VERIFY
    printcmd();
#endif
    switch (*buf) {
    case 'p':			/* find previous unread article */
	do {
	    if (art <= firstart)
		break;
	    art--;
	} while (was_read(art) || artopen(art) == Nullfp);
#ifdef ARTSEARCH
	srchahead = 0;
#endif
	return AS_NORM;
    case 'P':			/* goto previous article */
	if (art > absfirst)
	    art--;
	else {
#ifdef VERBOSE
	    IF(verbose)
		fputs("\n\
There are no articles prior to this one.\n\
",stdout) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("\nNo previous articles\n",stdout) FLUSH;
#endif
	    return AS_ASK;
	}
	reread = TRUE;
#ifdef ARTSEARCH
	srchahead = 0;
#endif
	return AS_NORM;
    case '-':
	if (recent_art) {
	    art = recent_art;
	    reread = TRUE;
#ifdef ARTSEARCH
	    srchahead = -(srchahead != 0);
#endif
	    return AS_NORM;
	}
	else {
	    exit_code = NG_MINUS;
	    return AS_CLEAN;
	}
    case 'n':		/* find next unread article? */
	if (art > lastart) {
	    if (toread[ng])
		art = firstart;
	    else
		return AS_CLEAN;
	}
#ifdef ARTSEARCH
	else if (scanon && srchahead) {
	    *buf = Ctl('n');
	    goto normal_search;
	}
#endif
	else
	    art++;
#ifdef ARTSEARCH
	srchahead = 0;
#endif
	return AS_NORM;
    case 'N':			/* goto next article */
	if (art > lastart)
	    art = absfirst;
	else
	    art++;
	if (art <= lastart)
	    reread = TRUE;
#ifdef ARTSEARCH
	srchahead = 0;
#endif
	return AS_NORM;
    case '$':
	art = lastart+1;
	forcelast = TRUE;
#ifdef ARTSEARCH
	srchahead = 0;
#endif
	return AS_NORM;
    case '1': case '2': case '3':	/* goto specified article */
    case '4': case '5': case '6':	/* or do something with a range */
    case '7': case '8': case '9': case '.':
	forcelast = TRUE;
	switch (numnum()) {
	case NN_INP:
	    return AS_INP;
	case NN_ASK:
	    return AS_ASK;
	case NN_REREAD:
	    reread = TRUE;
#ifdef ARTSEARCH
	    if (srchahead)
		srchahead = -1;
#endif
	    break;
	case NN_NORM:
	    if (was_read(art)) {
		art = firstart;
		pad(just_a_sec/3);
	    }
	    else
		return AS_ASK;
	    break;
	}
	return AS_NORM;
    case Ctl('k'):
	edit_kfile();
	return AS_ASK;
    case 'K':
    case 'k':
    case Ctl('n'): case Ctl('p'):
    case '/': case '?':
#ifdef ARTSEARCH
normal_search:
    {		/* search for article by pattern */
	char cmd = *buf;
	
	reread = TRUE;		/* assume this */
	switch (art_search(buf, (sizeof buf), TRUE)) {
	case SRCH_ERROR:
	    return AS_ASK;
	case SRCH_ABORT:
	    return AS_INP;
	case SRCH_INTR:
#ifdef VERBOSE
	    IF(verbose)
		printf("\n(Interrupted at article %ld)\n",(long)art) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		printf("\n(Intr at %ld)\n",(long)art) FLUSH;
#endif
	    art = curr_art;
			    /* restore to current article */
	    return AS_ASK;
	case SRCH_DONE:
	    fputs("done\n",stdout) FLUSH;
	    pad(just_a_sec/3);	/* 1/3 second */
	    if (srchahead)
		art = firstart;
	    else
		art = curr_art;
	    reread = FALSE;
	    return AS_NORM;
	case SRCH_SUBJDONE:
#ifdef UNDEF
	    fputs("\n\n\n\nSubject not found.\n",stdout) FLUSH;
	    pad(just_a_sec/3);	/* 1/3 second */
#endif
	    art = firstart;
	    reread = FALSE;
	    return AS_NORM;
	case SRCH_NOTFOUND:
	    fputs("\n\n\n\nNot found.\n",stdout) FLUSH;
	    art = curr_art;  /* restore to current article */
	    return AS_ASK;
	case SRCH_FOUND:
	    if (cmd == Ctl('n') || cmd == Ctl('p'))
		oldsubject = TRUE;
	    break;
	}
	return AS_NORM;
    }
#else
    buf[1] = '\0';
    notincl(buf);
    return AS_ASK;
#endif
    case 'u':			/* unsubscribe from this newsgroup? */
	rcchar[ng] = NEGCHAR;
	return AS_CLEAN;
    case 'M':
#ifdef DELAYMARK
	if (art <= lastart) {
	    delay_unmark(art);
	    printf("\nArticle %ld will return.\n",(long)art) FLUSH;
	}
#else
	notincl("M");
#endif
	return AS_ASK;
    case 'm':
	if (art <= lastart) {
	    unmark_as_read(art);
	    printf("\nArticle %ld marked as still unread.\n",(long)art) FLUSH;
	}
	return AS_ASK;
    case 'c':			/* catch up */
      reask_catchup:
#ifdef VERBOSE
	IF(verbose)
	    in_char("\nDo you really want to mark everything as read? [yn] ",
		'C');
	ELSE
#endif
#ifdef TERSE
	    in_char("\nReally? [ynh] ", 'C');
#endif
	putchar('\n') FLUSH;
	setdef(buf,"y");
#ifdef VERIFY
	printcmd();
#endif
	if (*buf == 'h') {
#ifdef VERBOSE
	    IF(verbose)
		fputs("\
Type y or SP to mark all articles as read.\n\
Type n to leave articles marked as they are.\n\
Type u to mark everything read and unsubscribe.\n\
",stdout) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("\
y or SP to mark all read.\n\
n to forget it.\n\
u to mark all and unsubscribe.\n\
",stdout) FLUSH;
#endif
	    goto reask_catchup;
	}
	else if (*buf == 'n' || *buf == 'q') {
	    return AS_ASK;
	}
	else if (*buf != 'y' && *buf != 'u') {
	    fputs(hforhelp,stdout) FLUSH;
	    settle_down();
	    goto reask_catchup;
	}
	for (i = firstart; i <= lastart; i++) {
	    oneless(i);		/* mark as read */
	}
#ifdef DELAYMARK
	if (dmfp)
	    yankback();
#endif
	if (*buf == 'u') {
	    rcchar[ng] = NEGCHAR;
	    return AS_CLEAN;
	}
	art = lastart+1;
	forcelast = FALSE;
	return AS_NORM;
    case 'Q':
	exit_code = NG_ASK;
	/* FALL THROUGH */
    case 'q':			/* go back up to newsgroup level? */
	return AS_CLEAN;
    case 'j':
	putchar('\n') FLUSH;
	if (art <= lastart)
	    mark_as_read(art);
	return AS_ASK;
    case 'h': {			/* help? */
	int cmd;

	if ((cmd = help_art()) > 0)
	    pushchar(cmd);
	return AS_ASK;
    }
    case '&':
	if (switcheroo()) /* get rest of command */
	    return AS_INP;	/* if rubbed out, try something else */
	return AS_ASK;
    case '#':
#ifdef VERBOSE
	IF(verbose)
	    printf("\nThe last article is %ld.\n",(long)lastart) FLUSH;
	ELSE
#endif
#ifdef TERSE
	    printf("\n%ld\n",(long)lastart) FLUSH;
#endif
	return AS_ASK;
    case '=': {
	char tmpbuf[256];
	ART_NUM oldart = art;
	int cmd;
	char *subjline = getval("SUBJLINE",Nullch);
#ifndef CACHESUBJ
	char *s;
#endif
	
	page_init();
#ifdef CACHESUBJ
	if (!subj_list)
	    fetchsubj(art,TRUE,FALSE);
#endif
	for (i=firstart; i<=lastart && !int_count; i++) {
#ifdef CACHESUBJ
	    if (!was_read(i) &&
	      (subj_list[OFFSET(i)] != Nullch || fetchsubj(i,FALSE,FALSE)) &&
	      *subj_list[OFFSET(i)] ) {
		sprintf(tmpbuf,"%5ld ", i);
		if (subjline) {
		    art = i;
		    interp(tmpbuf + 6, (sizeof tmpbuf) - 6, subjline);
		}
		else
		    safecpy(tmpbuf + 6, subj_list[OFFSET(i)],
			(sizeof tmpbuf) - 6);
		if (cmd = print_lines(tmpbuf,NOMARKING)) {
		    if (cmd > 0)
			pushchar(cmd);
		    break;
		}
	    }
#else
	    if (!was_read(i) && (s = fetchsubj(i,FALSE,FALSE)) && *s) {
		sprintf(tmpbuf,"%5ld ", i);
		if (subjline) {	/* probably fetches it again! */
		    art = i;
		    interp(tmpbuf + 6, (sizeof tmpbuf) - 6, subjline);
		}
		else
		    safecpy(tmpbuf + 6, s, (sizeof tmpbuf) - 6);
		if (cmd = print_lines(tmpbuf,NOMARKING)) {
		    if (cmd > 0)
			pushchar(cmd);
		    break;
		}
	    }
#endif
	}
	int_count = 0;
	art = oldart;
	return AS_ASK;
    }
    case '^':
	art = firstart;
#ifdef ARTSEARCH
	srchahead = 0;
#endif
	return AS_NORM;
#if defined(CACHESUBJ) && defined(DEBUGGING)
    case 'D':
	printf("\nFirst article: %ld\n",(long)firstart) FLUSH;
	if (!subj_list)
	    fetchsubj(art,TRUE,FALSE);
	if (subj_list != Null(char **)) {
	    for (i=1; i<=lastart && !int_count; i++) {
		if (subj_list[OFFSET(i)])
		    printf("%5ld %c %s\n",
			i, (was_read(i)?'y':'n'), subj_list[OFFSET(i)]) FLUSH;
	    }
	}
	int_count = 0;
	return AS_ASK;
#endif
    case 'v':
	if (art <= lastart) {
	    reread = TRUE;
	    do_hiding = FALSE;
	}
	return AS_NORM;
#ifdef ROTATION
    case Ctl('x'):
#endif
    case Ctl('r'):
#ifdef ROTATION
	rotate = (*buf==Ctl('x'));
#endif
	if (art <= lastart)
	    reread = TRUE;
	return AS_NORM;
#ifdef ROTATION
    case 'X':
	rotate = !rotate;
	/* FALL THROUGH */
#else
    case Ctl('x'):
    case 'x':
    case 'X':
	notincl("x");
	return AS_ASK;
#endif
    case 'l': case Ctl('l'):		/* refresh screen */
	if (art <= lastart) {
	    reread = TRUE;
	    clear();
	    do_fseek = TRUE;
	    artline = topline;
	    if (artline < 0)
		artline = 0;
	}
	return AS_NORM;
    case 'b': case Ctl('b'):		/* back up a page */
	if (art <= lastart) {
	    ART_LINE target;

	    reread = TRUE;
	    clear();
	    do_fseek = TRUE;
	    target = topline - (LINES - 2);
	    artline = topline;
	    do {
		artline--;
	    } while (artline >= 0 && artline > target &&
		vrdary(artline-1) >= 0);
	    topline = artline;
	    if (artline < 0)
		artline = 0;
	}
	return AS_NORM;
    case '!':			/* shell escape */
	if (escapade())
	    return AS_INP;
	return AS_ASK;
    case 'C': {
	cancel_article();
	return AS_ASK;
    }
    case 'R':
    case 'r': {			/* reply? */
	reply();
	return AS_ASK;
    }
    case 'F':
    case 'f': {			/* followup command */
	followup();
	forcegrow = TRUE;		/* recalculate lastart */
	return AS_ASK;
    }
    case '|':
    case 'w': case 'W':
    case 's': case 'S':		/* save command */
	if (save_article() == SAVE_ABORT)
	    return AS_INP;
	return AS_ASK;
#ifdef DELAYMARK
    case 'Y':				/* yank back M articles */
	yankback();
	art = firstart;			/* from the beginning */
	return AS_NORM;			/* pretend nothing happened */
#endif
#ifdef STRICTCR
    case '\n':
	fputs(badcr,stdout) FLUSH;
	return AS_ASK;
#endif
    default:
	printf("\n%s",hforhelp) FLUSH;
	settle_down();
	return AS_ASK;
    }
}

#ifdef MAILCALL
/* see if there is any mail */

void
setmail()
{
    if (! (mailcount++)) {
	char *mailfile = filexp(getval("MAILFILE",MAILFILE));
	
	if (stat(mailfile,&filestat) < 0 || !filestat.st_size
	    || filestat.st_atime > filestat.st_mtime)
	    mailcall = nullstr;
	else
	    mailcall = getval("MAILCALL","(Mail) ");
    }
    mailcount %= 10;			/* check every 10 articles */
}
#endif

void
setdfltcmd()
{
    if (toread[ng]) {
#ifdef ARTSEARCH
	if (srchahead)
	    dfltcmd = "^Nnpq";
	else
#endif
	    dfltcmd = "npq";
    }
    else {
	if (art > lastart)
	    dfltcmd = "qnp";
	else
	    dfltcmd = "npq";
    }
}

