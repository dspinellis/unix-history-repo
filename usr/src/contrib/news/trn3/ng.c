/* $Id: ng.c,v 3.0 1992/02/01 03:09:32 davison Trn $
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
#include "term.h"
#include "final.h"
#include "util.h"
#include "cache.h"
#include "bits.h"
#include "artsrch.h"
#include "help.h"
#include "kfile.h"
#include "rcstuff.h"
#include "head.h"
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
#include "nntp.h"
#include "rthread.h"
#include "rt-select.h"
#include "rt-wumpus.h"
#include "decode.h"
#include "INTERN.h"
#include "ng.h"
#include "artstate.h"			/* somebody has to do it */

/* art_switch() return values */

#define AS_NORM 0
#define AS_INP 1
#define AS_ASK 2
#define AS_CLEAN 3

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

/* assumes that we are chdir'ed to NEWSSPOOL, and assures that that is
 * still true upon return, but chdirs to NEWSSPOOL/ngname in between
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
    char *whatnext = "%sWhat next? [%s]";

#ifdef ARTSEARCH
    srchahead = (scanon && !ThreadedGroup	/* did they say -S? */
	      && ((ART_NUM)toread[ng]) >= scanon ? -1 : 0);
#endif
    
    exit_code = NG_NORM;
    save_ids = FALSE;
    killfirst = 0;

    if (extractdest) {
	free(extractdest);
	extractdest = Nullch;
    }
    if (extractprog) {
	free(extractprog);
	extractprog = Nullch;
    }

    /* initialize the newsgroup data structures */

    if (!access_ng())
	return -1;

    /* FROM HERE ON, RETURN THRU CLEANUP OR WE ARE SCREWED */

    in_ng = TRUE;			/* tell the world we are here */
    forcelast = TRUE;			/* if 0 unread, do not bomb out */
    recent_artp = curr_artp = Nullart;
    recent_art = curr_art = lastart+1;
    prompt = whatnext;

    /* remember what newsgroup we were in for sake of posterity */

    writelast();

    /* see if there are any special searches to do */

    has_normal_kills = FALSE;
#ifdef KILLFILES
    open_kfile(KF_LOCAL);
# ifdef VERBOSE
    IF(verbose)
	kill_unwanted(firstart,"Processing memorized commands...\n\n",TRUE);
    ELSE
# endif
# ifdef TERSE
	kill_unwanted(firstart,"Auto-processing...\n\n",TRUE);
# endif
#endif
    if (!selected_count)
	selected_only = FALSE;
    top_article();

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
    checkcount = 0;			/* do not checkpoint for a while */
    do_fseek = FALSE;			/* start 1st article at top */
    for (; art<=lastart+1; ) {		/* for each article */
	mode = 'a';

	/* do we need to "grow" the newsgroup? */

	if (art > lastart || forcegrow) {
	    ART_NUM oldlast = lastart;
#ifdef USE_NNTP
	    ART_NUM newlast = lastart;
	    while (nntp_stat(newlast+1))
		newlast++;
	    if (newlast > oldlast) {
		ngmax[ng] = newlast;
		grow_ng(newlast);
	    }
#else
	    grow_ng(getngsize(ng));
#endif
	    if (forcelast && art > oldlast)
		art = lastart+1;
	}
	find_article(art);		/* sets artp */
	if (start_command) {		/* do we have an initial command? */
	    pushstring(start_command, 0);
	    free(start_command);
	    start_command = Nullch;
	    art = curr_art = lastart+1;
	    artp = curr_artp = Nullart;
	    if (input_pending())
		goto reinp_article;
	}
	if (art>lastart) {		/* are we off the end still? */
	    ARTICLE *ap;
	    ART_NUM i;
	    art = lastart + 1;		/* keep pointer references sane */
	    if (!forcelast && toread[ng] && selected_only && !selected_count) {
		art = curr_art;
		artp = curr_artp;
		strcpy(buf, "+");
		goto article_level;
	    }
	    count_subjects(CS_NORM);
	    for (i=last_cached+1, ap=article_ptr(i); i<=lastart; i++, ap++)
		if (!(ap->flags & AF_READ))
		    article_count++;
	    toread[ng] = (ART_UNREAD)article_count;
	    if (artp != curr_artp) {
		recent_art = curr_art;	/* remember last article # (for '-') */
		curr_art = art;		/* set current article # */
		recent_artp = curr_artp;
		curr_artp = artp;
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
	    if (article_count) {
		if (selected_only)
		    printf("  (%ld + %ld articles still unread)",
			(long)selected_count,
			(long)article_count-selected_count);
		else
		    printf("  (%ld article%s still unread)",
			(long)article_count,article_count==1?nullstr:"s");
	    }
	    else if (!forcelast)
		goto cleanup;		/* actually exit newsgroup */
	    mode = 'e';
	    prompt = whatnext;
#ifdef ARTSEARCH
	    srchahead = 0;		/* no more subject search mode */
#endif
	    fputs("\n\n",stdout) FLUSH;
	}
	else if (!reread && (was_read(art) || (artp->flags & AF_MISSING)
		|| (selected_only && !(artp->flags & AF_SEL)))) {
					/* has this article been read? */
	    inc_art(selected_only,FALSE);/* then skip it */
	    continue;
	}
	else if (!reread && !parseheader(art)) {
	    oneless(artp);		/* mark deleted as read */
	    ng_skip();
	}
	else {				/* we have a real live article */
	    if (artp != curr_artp) {
		recent_art = curr_art;	/* remember last article # (for '-') */
		curr_art = art;		/* set current article # */
		recent_artp = curr_artp;
		curr_artp = artp;
	    }
	    if (!do_fseek) {		/* starting at top of article? */
		artline = 0;		/* start at the beginning */
		topline = -1;		/* and remember top line of screen */
					/*  (line # within article file) */
	    }
	    clear();			/* clear screen */
	    if (!artopen(art)) {	/* make sure article is found & open */
		char tmpbuf[256];
		/* see if we have tree data for this article anyway */
		init_tree();
		sprintf(tmpbuf,"%s: article is not available.",ngname);
		if (artp && !(artp->flags & AF_CACHED)) {
		    if (absfirst < first_cached || last_cached < lastart
		     || !cached_all_in_range)
			sprintf(tmpbuf,"%s: article may show up in a moment.",
				ngname);
		}
		tree_puts(tmpbuf,0,0);
		vwtary((ART_LINE)0,(ART_POS)0);
		finish_tree(1);
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
	    if (art >= absfirst)	/* don't mark non-existant articles */
		mark_as_read();		/* mark current article as read */
	    do_hiding = TRUE;
#ifdef ROTATION
	    rotate = FALSE;
#endif
	}

/* if these gotos bother you, think of this as a little state machine */

reask_article:
#ifdef MAILCALL
	setmail(FALSE);
#endif
	setdfltcmd();
#ifdef CLEAREOL
	if (erase_screen && can_home_clear)
	    clear_rest();
#endif /* CLEAREOL */
	unflush_output();		/* disable any ^O in effect */
	standout();			/* enter standout mode */
	printf(prompt,mailcall,dfltcmd);/* print prompt, whatever it is */
	un_standout();			/* leave standout mode */
	putchar(' ');
	fflush(stdout);
reinp_article:
	reread = FALSE;
	forcelast = FALSE;
	eat_typeahead();
#ifdef PENDING
	look_ahead();			/* see what we can do in advance */
	cache_until_key();
#endif
	art = curr_art;
	artp = curr_artp;
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
	output_chase_phrase = TRUE;

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
    decode_end();
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
    deselect_all();
    yankback();				/* do a Y command */
    bits_to_rc();			/* reconstitute .newsrc line */
    doing_ng = FALSE;			/* tell sig_catcher to cool it */
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
    case '<':			/* goto previous subject/thread */
	prev_subject();
	return AS_NORM;
    case '>':			/* goto next subject/thread */
	next_subject();
	return AS_NORM;
    case 'U': {			/* unread some articles */
	char *u_prompt, *u_help_thread;

	dfltcmd = "+";
	if (!artp) {
	    u_help_thread = nullstr;
#ifdef VERBOSE
	    IF(verbose)
		u_prompt = "\nSet unread: +select or all? [+an] ";
	    ELSE
#endif
#ifdef TERSE
		u_prompt = "\nSet unread? [+an] ";
#endif
	}
	else {
#ifdef VERBOSE
	    IF(verbose) {
		u_prompt = "\n\
Set unread: +select, thread, subthread, or all? [+tsan] ";
		u_help_thread = "\
Type t or SP to mark this thread's articles as unread.\n\
Type s to mark the current article and its descendants as unread.\n";
	    }
	    ELSE
#endif
#ifdef TERSE
	    {
		u_prompt = "\nSet unread? [+tsan] ";
		u_help_thread = "\
t or SP to mark thread unread.\n\
s to mark subthread unread.\n";
	    }
#endif
	}
      reask_unread:
	in_char(u_prompt,'u');
	setdef(buf,dfltcmd);
#ifdef VERIFY
	printcmd();
#endif
	putchar('\n') FLUSH;
	if (*buf == 'h') {
#ifdef VERBOSE
	    IF(verbose)
	    {
		fputs("\
Type + to enter select thread mode using all the unread articles.\n\
(The selected threads will be marked as unread and displayed as usual.)\n\
",stdout) FLUSH;
		fputs(u_help_thread,stdout);
		fputs("\
Type a to mark all articles in this group as unread.\n\
Type n to change nothing.\n\
",stdout) FLUSH;
	    }
	    ELSE
#endif
#ifdef TERSE
	    {
		fputs("\
+ to select threads from the unread.\n\
",stdout) FLUSH;
		fputs(u_help_thread,stdout);
		fputs("\
a to mark all articles unread.\n\
n to change nothing.\n\
",stdout) FLUSH;
	    }
#endif
	    goto reask_unread;
	}
	else if (*buf == 'n' || *buf == 'q')
	    return AS_ASK;
	else if (*buf == 't' && u_help_thread != nullstr) {
	    unkill_thread(artp->subj->thread);
	    if ((artp = first_art(artp->subj)) != Nullart)
		art = article_num(artp);
	} else if (*buf == 's' && u_help_thread != nullstr)
	    unkill_subthread(artp);
	else if (*buf == 'a') {
	    register ARTICLE *ap;
	    check_first(absfirst);
	    ap = article_ptr(absfirst);
	    for (i = absfirst; i <= lastart; i++, ap++)
		if ((ap->flags & (AF_READ|AF_MISSING)) == AF_READ) {
		    ap->flags &= ~AF_READ;		/* mark as unread */
		    toread[ng]++;
		}
	    count_subjects(CS_NORM);
	}
	else if (*buf == '+') {
	    *buf = 'U';
	    goto run_the_selector;
	}
	else {
	    fputs(hforhelp,stdout) FLUSH;
	    settle_down();
	    goto reask_unread;
	}
	return AS_NORM;
    }
    case '[':			/* goto parent article */
    case '{':			/* goto thread's root article */
	if (artp) {
	    if (!find_parent(*buf == '{')) {
		register char *cp = (*buf=='['?"parent":"root");
#ifdef VERBOSE
		IF(verbose)
		    printf("\nThere is no %s article prior to this one.\n",
			cp) FLUSH;
		ELSE
#endif
#ifdef TERSE
		    printf("\nNo prior %s.\n",cp) FLUSH;
#endif
		return AS_ASK;
	    }
	    reread = TRUE;
	    return AS_NORM;
	}
not_threaded:
	if (ThreadedGroup) {
#ifdef VERBOSE
	    IF(verbose)
		fputs("\nThis article is not threaded.\n",stdout) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("\nUnthreaded article.\n",stdout) FLUSH;
#endif
	    return AS_ASK;
	}
#ifdef VERBOSE
	IF(verbose)
	    fputs("\nThis group is not threaded.\n",stdout) FLUSH;
	ELSE
#endif
#ifdef TERSE
	    fputs("\nUnthreaded group.\n",stdout) FLUSH;
#endif
	return AS_ASK;
    case ']':			/* goto child article */
    case '}':			/* goto thread's leaf article */
	if (artp) {
	    if (!find_leaf(*buf == '}')) {
#ifdef VERBOSE
		IF(verbose)
		    fputs("\n\
This is the last leaf in this tree.\n",stdout) FLUSH;
		ELSE
#endif
#ifdef TERSE
		    fputs("\nLast leaf.\n",stdout) FLUSH;
#endif
		return AS_ASK;
	    }
	    reread = TRUE;
	    return AS_NORM;
	}
	goto not_threaded;
    case '(':			/* goto previous sibling */
    case ')':			/* goto next sibling */
	if (artp) {
	    if (!(*buf == '(' ? find_prev_sib() : find_next_sib())) {
		register char *cp = (*buf == '(' ? "previous" : "next");
#ifdef VERBOSE
		IF(verbose)
		    printf("\nThis article has no %s sibling.\n",cp) FLUSH;
		ELSE
#endif
#ifdef TERSE
		    printf("\nNo %s sibling.\n",cp) FLUSH;
#endif
		return AS_ASK;
	    }
	    reread = TRUE;
	    return AS_NORM;
	}
	goto not_threaded;
    case 'T':
	if (!ThreadedGroup)
	    goto not_threaded;
	/* FALL THROUGH */
    case 'A':
	if (!artp) {
	    printf("You're not at an article.\n");
	    return AS_ASK;
	}
	switch (ask_memorize(*buf)) {
	case ',':  case 'j':
	    return AS_NORM;
	}
	return AS_ASK;
    case 'K':
	if (!artp) {
	    printf("You're not at an article.\n");
	    return AS_ASK;
	}
	/* first, write kill-subject command */
	(void)art_search(buf, (sizeof buf), TRUE);
	art = curr_art;
	artp = curr_artp;
	kill_subject(artp->subj,KF_ALL);/* take care of any prior subjects */
	return AS_NORM;
    case ',':		/* kill this node and all descendants */
	if (ThreadedGroup)
	    kill_subthread(artp,KF_ALL);
	else if (art >= absfirst && art <= lastart)
	    mark_as_read();
	return AS_NORM;
    case 'J':		/* Junk all nodes in this thread */
	if (ThreadedGroup) {
	    kill_thread(artp->subj->thread,KF_ALL);
	    return AS_NORM;
	}
	/* FALL THROUGH */
    case 'k':		/* kill current subject */
	kill_subject(artp->subj,KF_ALL);
	if (last_cached < lastart) {
	    *buf = 'k';
	    goto normal_search;
	}
	return AS_NORM;
    case 't':
	carriage_return();
#ifndef CLEAREOL
	erase_eol();		/* erase the prompt */
#else
	if (erase_screen && can_home_clear)
	    clear_rest();
	else
	    erase_eol();	/* erase the prompt */
#endif /* CLEAREOL */
	fflush(stdout);
	page_line = 1;
	entire_tree(curr_artp);
	return AS_ASK;
    case ':':			/* execute command on selected articles */
	page_line = 1;
	if (!use_selected())
	    return AS_INP;
	putchar('\n');
	art = curr_art;
	artp = curr_artp;
	return AS_ASK;
    case 'p':			/* find previous unread article */
	do {
	    dec_art(selected_only,FALSE);
	} while (art >= firstart && (was_read(art) || !parseheader(art)));
#ifdef ARTSEARCH
	srchahead = 0;
#endif
	if (art >= firstart)
	    return AS_NORM;
	art = absfirst;	
	/* FALL THROUGH */
    case 'P':		/* goto previous article */
	dec_art(FALSE,TRUE);
      check_dec_art:
	if (art < absfirst) {
#ifdef VERBOSE
	    IF(verbose)
		printf("\nThere are no%s%s articles prior to this one.\n",
			*buf=='P'?nullstr:" unread",
			selected_only?" selected":nullstr) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		printf("\nNo previous%s%s articles\n",
			*buf=='P'?nullstr:" unread",
			selected_only?" selected":nullstr) FLUSH;
#endif
	    art = curr_art;
	    artp = curr_artp;
	    return AS_ASK;
	}
	reread = TRUE;
#ifdef ARTSEARCH
	srchahead = 0;
#endif
	return AS_NORM;
    case '-':
	if (recent_art >= 0) {
	    art = recent_art;
	    artp = recent_artp;
	    reread = TRUE;
	    forcelast = TRUE;
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
	    if (!toread[ng])
		return AS_CLEAN;
	    top_article();
	}
#ifdef ARTSEARCH
	else if (scanon && !ThreadedGroup && srchahead) {
	    *buf = Ctl('n');
	    if (!next_art_with_subj())
		goto normal_search;
	    return AS_NORM;
	}
#endif
	else
	    inc_art(selected_only,FALSE);

#ifdef ARTSEARCH
	srchahead = 0;
#endif
	return AS_NORM;
    case 'N':			/* goto next article */
	if (art > lastart)
	    if (!first_subject) {
		art = absfirst;
		artp = article_ptr(art);
	    } else {
		artp = first_subject->articles;
		if (artp->flags & AF_MISSING)
		    inc_art(FALSE,TRUE);
		else
		    art = article_num(artp);
	    }
	else
	    inc_art(FALSE,TRUE);
	if (art <= lastart)
	    reread = TRUE;
#ifdef ARTSEARCH
	srchahead = 0;
#endif
	return AS_NORM;
    case '$':
	art = lastart+1;
	artp = Nullart;
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
		top_article();
		pad(just_a_sec/3);
	    }
	    else {
		putchar('\n');
		return AS_ASK;
	    }
	    break;
	}
	return AS_NORM;
    case Ctl('k'):
	edit_kfile();
	return AS_ASK;
    case Ctl('n'):	/* search for next article with same subject */
    case Ctl('p'):	/* search for previous article with same subject */
	if (*buf == Ctl('n')? next_art_with_subj() : prev_art_with_subj())
	    return AS_NORM;
    case '/': case '?':
normal_search:
#ifdef ARTSEARCH
    {		/* search for article by pattern */
	char cmd = *buf;
	
	reread = TRUE;		/* assume this */
	page_line = 1;
	switch (art_search(buf, (sizeof buf), TRUE)) {
	case SRCH_ERROR:
	    art = curr_art;
	    return AS_ASK;
	case SRCH_ABORT:
	    art = curr_art;
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
	    art = curr_art;	    /* restore to current article */
	    return AS_ASK;
	case SRCH_DONE:
	    fputs("done\n",stdout) FLUSH;
	    pad(just_a_sec/3);	/* 1/3 second */
	    if (!srchahead) {
		art = curr_art;
		return AS_ASK;
	    }
	    top_article();
	    reread = FALSE;
	    return AS_NORM;
	case SRCH_SUBJDONE:
#ifdef UNDEF
	    fputs("\n\n\n\nSubject not found.\n",stdout) FLUSH;
	    pad(just_a_sec/3);	/* 1/3 second */
#endif
	    top_article();
	    reread = FALSE;
	    return AS_NORM;
	case SRCH_NOTFOUND:
	    fputs("\n\n\n\nNot found.\n",stdout) FLUSH;
	    art = curr_art;  /* restore to current article */
	    return AS_ASK;
	case SRCH_FOUND:
	    if (cmd == Ctl('n') || cmd == Ctl('p')) {
		oldsubject = TRUE;
		reread = FALSE;
	    }
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
	if (art <= lastart) {
	    delay_unmark(artp);
	    oneless(artp);
	    printf("\nArticle %ld will return.\n",(long)art) FLUSH;
	}
	return AS_ASK;
    case 'm':
	if (art >= absfirst && art <= lastart) {
	    unmark_as_read();
	    printf("\nArticle %ld marked as still unread.\n",(long)art) FLUSH;
	}
	return AS_ASK;
    case 'c':			/* catch up */
	switch (ask_catchup()) {
	case 'n':
	    return AS_ASK;
	case 'u':
	    return AS_CLEAN;
	}
	art = lastart+1;
	artp = Nullart;
	forcelast = FALSE;
	return AS_NORM;
    case 'Q':
	exit_code = NG_ASK;
	/* FALL THROUGH */
    case 'q':			/* go back up to newsgroup level? */
	return AS_CLEAN;
    case 'j':
	putchar('\n') FLUSH;
	if (art >= absfirst && art <= lastart)
	    mark_as_read();
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
    case '+':			/* enter selection mode */
run_the_selector:
	*buf = do_selector(*buf);
	switch (*buf) {
	case '+':
	    putchar('\n') FLUSH;
	    return AS_ASK;
	case 'Q':
	    exit_code = NG_ASK;
	    /* FALL THROUGH */
	case 'q':
	    break;
	case 'N':
	    exit_code = NG_SELNEXT;
	    break;
	case 'P':
	    exit_code = NG_SELPRIOR;
	    break;
	default:
	    if (toread[ng])
		return AS_NORM;
	    break;
	}
	return AS_CLEAN;
    case '=': {			/* list subjects */
	char tmpbuf[256];
	ART_NUM oldart = art;
	int cmd;
	char *s;
	char *subjline = getval("SUBJLINE",Nullch);
	ARTICLE *ap = article_ptr(firstart);

	page_init();
	for (i=firstart; i<=lastart && !int_count; i++, ap++) {
	    if (!(ap->flags & AF_READ) && (s = fetchsubj(i,FALSE)) != Nullch) {
		sprintf(tmpbuf,"%5ld ", i);
		if (subjline) {
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
	}
	int_count = 0;
	art = oldart;
	return AS_ASK;
    }
    case '^':
	top_article();
#ifdef ARTSEARCH
	srchahead = 0;
#endif
	return AS_NORM;
#ifdef DEBUG
    case 'D':
	printf("\nFirst article: %ld\n",(long)firstart) FLUSH;
	{
	    ARTICLE *ap = article_ptr(firstart);
	    for (i = firstart; i <= lastart && !int_count; i++, ap++) {
		if (ap->subj)
		    printf("%5ld %c %s\n",i,(was_read(i)?'y':'n'),
			   ap->subj->str) FLUSH;
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
	else
	    forcelast = TRUE;
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
    case Ctl('f'):
	carriage_return();
	erase_eol();		/* erase the prompt */
#ifdef MAILCALL
	setmail(TRUE);		/* force a mail check */
#endif
	return AS_ASK;
    case 'b': case Ctl('b'):		/* back up a page */
	if (art <= lastart) {
	    ART_LINE target;

	    reread = TRUE;
	    clear();
	    do_fseek = TRUE;
	    target = topline - (LINES - 2);
	    artline = topline;
	    if (artline >= 0) do {
		artline--;
	    } while(artline >= 0 && artline > target && vrdary(artline-1) >= 0);
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
    case 'Z':
    case 'z': {
	supersede_article();	/* supersedes */
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
    case 'e':			/* extract command */
	if (save_article() == SAVE_ABORT)
	    return AS_INP;
	int_count = 0;
	return AS_ASK;
    case 'E':
	if (decode_fp)
	    decode_end();
	else
	    putchar('\n') FLUSH;
	return AS_ASK;
    case 'Y':				/* yank back M articles */
	yankback();
	top_article();			/* from the beginning */
	return AS_NORM;			/* pretend nothing happened */
#ifdef STRICTCR
    case '\n':
	fputs(badcr,stdout) FLUSH;
	return AS_ASK;
#endif
    case '_':
	if (!finish_dblchar())
	    return AS_INP;
	switch (buf[1] & 0177) {
	case 'P':
	    art--;
	    goto check_dec_art;
	case 'N':
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
	case '+':
	    if (ThreadedGroup) {
		select_thread(artp->subj->thread, 0);
		printf("\nSelected all articles in this thread.\n");
	    } else {
		select_subject(artp->subj, 0);
		printf("\nSelected all articles in this subject.\n");
	    }
	    return AS_ASK;
	case '-':
	    if (sel_mode == SM_THREAD) {
		deselect_thread(artp->subj->thread);
		printf("\nDeselected all articles in this thread.\n");
	    } else {
		deselect_subject(artp->subj);
		printf("\nDeselected all articles in this subject.\n");
	    }
	    return AS_ASK;
	case 'a':  case 's':  case 't':  case 'T':
	    *buf = buf[1];
	    goto run_the_selector;
	}
	/* FALL THROUGH */
    default:
	printf("\n%s",hforhelp) FLUSH;
	settle_down();
	break;
    }
    return AS_ASK;
}

#ifdef MAILCALL
/* see if there is any mail */

void
setmail(force)
bool_int force;
{
    if (force)
	mailcount = 0;
    if (!(mailcount++)) {
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
    if (!toread[ng]) {
	if (art > lastart)
	    dfltcmd = "qnp";
	else
	    dfltcmd = "npq";
    }
    else {
#ifdef ARTSEARCH
	if (srchahead)
	    dfltcmd = "^Nnpq";
	else
#endif
	    dfltcmd = "npq";
    }
}

/* Ask the user about catching-up the current group.  Returns 'y' if yes,
** 'n' or 'N' if no ('N' means we used one line when in the selector),
** or 'u' for yes with unsubscribe.  Actually performs the catchup and
** unsubscription as needed.
*/
char
ask_catchup()
{
    char ch;
    bool use_one_line = (mode == 't');

    if (!use_one_line)
	putchar('\n') FLUSH;
reask_catchup:
#ifdef VERBOSE
    IF(verbose)
	in_char("Do you really want to mark everything as read? [yn] ", 'C');
    ELSE
#endif
#ifdef TERSE
	in_char("Really? [ynh] ", 'C');
#endif
    setdef(buf,"y");
#ifdef VERIFY
    printcmd();
#endif
    if ((ch = *buf) == 'h') {
	use_one_line = FALSE;
#ifdef VERBOSE
	IF(verbose)
	    fputs("\n\
Type y or SP to mark all articles as read.\n\
Type n to leave articles marked as they are.\n\
Type u to mark everything read and unsubscribe.\n\n\
",stdout) FLUSH;
	ELSE
#endif
#ifdef TERSE
	    fputs("\n\
y or SP to mark all read.\n\
n to forget it.\n\
u to mark all and unsubscribe.\n\n\
",stdout) FLUSH;
#endif
	goto reask_catchup;
    }
    if (ch == 'n' || ch == 'q') {
	if (use_one_line)
	    return 'N';
	putchar('\n') FLUSH;
	return 'n';
    }
    if (ch != 'y' && ch != 'u') {
	use_one_line = FALSE;
	printf("\n%s\n", hforhelp) FLUSH;
	settle_down();
	goto reask_catchup;
    }
    if (mode == 'n') {
	putchar('\n') FLUSH;
	catch_up(ng);
    }
    else {
	int i;
	ARTICLE *ap;
	for (i = firstart, ap = article_ptr(i); i <= lastart; i++, ap++)
	    ap->flags = ((ap->flags & ~sel_mask) | AF_READ);
	selected_count = selected_subj_cnt = selected_only = 0;
	toread[ng] = 0;
	if (dmcount)
	    yankback();
	putchar('\n') FLUSH;
    }
    if (ch == 'u')
	rcchar[ng] = NEGCHAR;
    return ch;
}

char
ask_memorize(ch)
char_int ch;
{
    bool thread_cmd = (ch == 'T');
    bool use_one_line = (mode == 't');
    char *mode_string = (thread_cmd? "thread" : "subject");
    char *mode_phrase = (thread_cmd? "replies to this article" :
				     "this subject and all replies");
    ART_NUM art_hold = art;
    ARTICLE *artp_hold = artp;

    if (!use_one_line)
	putchar('\n') FLUSH;
    sprintf(cmd_buf,"Memorize %s command: [+.j,cC]", mode_string);
reask_memorize:
    in_char(cmd_buf, 'm');
    setdef(buf,"+");
#ifdef VERIFY
    printcmd();
#endif
    if ((ch = *buf) == 'h') {
	use_one_line = FALSE;
#ifdef VERBOSE
	IF(verbose)
	    printf("\n\
Type + or SP to auto-select this %s (i.e. includes future articles).\n\
Type . to auto-select %s.\n\
Type j to auto-kill (junk) this %s.\n\
Type , to auto-kill %s.\n\
Type c to clear all selection/killing on this %s.\n\
Type C to clear all selection/killing on %s.\n\
Type q to abort the operation.\n\n\
",mode_string,mode_phrase,mode_string,mode_phrase,mode_string,mode_phrase) FLUSH;
	ELSE
#endif
#ifdef TERSE
	    printf("\n\
+ or SP auto-selects this %s.\n\
. auto-selects %s.\n\
j auto-kills this %s.\n\
, auto-kills %s.\n\
c clears auto-commands for this %s.\n\
C clears auto-commands for %s.\n\
q aborts.\n\n\
",mode_string,mode_phrase,mode_string,mode_phrase,mode_string,mode_phrase) FLUSH;
#endif
	goto reask_memorize;
    }
    if (ch == 'q') {
	if (use_one_line)
	    return 'Q';
	putchar('\n');
	return 'q';
    }
    if (ch == '+') {
	if (!thread_cmd) {
	    (void)art_search(buf, (sizeof buf), TRUE);
	    art = art_hold;
	    artp = artp_hold;
	    ch = '.';
	} else
	    ch = (use_one_line? '+' : '.');
	if (thread_cmd)
	    select_thread(artp->subj->thread, AF_AUTOSELECTALL);
	else
	    select_subject(artp->subj, 0);
	if (mode != 't')
	    printf("\nSelection memorized.\n");
    } else if (ch == '.') {
	if (!thread_cmd) {
	    (void)art_search(buf, (sizeof buf), TRUE);
	    art = art_hold;
	    artp = artp_hold;
	} else
	    ch = (use_one_line? '+' : '.');
	select_subthread(artp,thread_cmd? AF_AUTOSELECT : 0);
	if (mode != 't')
	    printf("\nSelection memorized.\n");
    } else if (ch == 'j') {
	if (!thread_cmd) {
	    *buf = 'K';
	    (void)art_search(buf, (sizeof buf), TRUE);
	    art = art_hold;
	    artp = artp_hold;
	}
	if (thread_cmd)
	    kill_thread(artp->subj->thread,KF_ALL|KF_KILLFILE);
	else
	    kill_subject(artp->subj,KF_ALL);
	if (mode != 't')
	    printf("\nKill memorized.\n");
    } else if (ch == ',') {
	if (!thread_cmd) {
	    (void)art_search(buf, (sizeof buf), TRUE);
	    art = art_hold;
	    artp = artp_hold;
	}
	kill_subthread(artp,KF_ALL|(thread_cmd?KF_KILLFILE:0));
	if (mode != 't')
	    printf("\nKill memorized.\n");
    } else if (ch == 'c') {
	if (thread_cmd)
	    clear_thread(artp->subj->thread);
	else
	    clear_subject(artp->subj);
    } else if (ch == 'C') {
	clear_subthread(artp);
    } else {
	use_one_line = FALSE;
	printf("\n%s\n", hforhelp) FLUSH;
	settle_down();
	goto reask_memorize;
    }
    if (!use_one_line)
	putchar('\n') FLUSH;
    return ch;
}
