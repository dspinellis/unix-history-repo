/* $Id: ngstuff.c,v 3.0 1991/09/09 20:23:31 davison Trn $
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
#include "term.h"
#include "util.h"
#include "cache.h"
#include "bits.h"
#include "ngdata.h"
#include "ng.h"
#include "intrp.h"
#include "head.h"
#include "final.h"
#include "sw.h"
#include "rthread.h"
#include "rt-select.h"
#include "rt-wumpus.h"
#include "trn.h"
#include "rcstuff.h"
#include "respond.h"
#include "kfile.h"
#include "decode.h"
#include "INTERN.h"
#include "ngstuff.h"

void
ngstuff_init()
{
    ;
}

/* do a shell escape */

int
escapade()
{
    register char *s;
    bool interactive = (buf[1] == FINISHCMD);
    bool docd;
    char whereiam[512];

    if (!finish_command(interactive))	/* get remainder of command */
	return -1;
    s = buf+1;
    docd = *s != '!';
    if (!docd) {
	s++;
    }
    else {
	getwd(whereiam);
	if (chdir(cwd)) {
	    printf(nocd,cwd) FLUSH;
	    sig_catcher(0);
	}
    }
    while (*s == ' ') s++;
					/* skip leading spaces */
    interp(cmd_buf, (sizeof cmd_buf), s);/* interpret any % escapes */
    resetty();				/* make sure tty is friendly */
    doshell(Nullch,cmd_buf);	/* invoke the shell */
    noecho();				/* and make terminal */
    crmode();				/*   unfriendly again */
    if (docd) {
	if (chdir(whereiam)) {
	    printf(nocd,whereiam) FLUSH;
	    sig_catcher(0);
	}
    }
#ifdef MAILCALL
    mailcount = 0;			/* force recheck */
#endif
    return 0;
}

/* process & command */

int
switcheroo()
{
    if (!finish_command(TRUE)) /* get rest of command */
	return -1;	/* if rubbed out, try something else */
    if (!buf[1])
	pr_switches();
    else if (buf[1] == '&') {
	if (!buf[2]) {
	    page_init();
	    show_macros();
	}
	else {
	    char tmpbuf[LBUFLEN];
	    register char *s;

	    for (s=buf+2; isspace(*s); s++);
	    mac_line(s,tmpbuf,(sizeof tmpbuf));
	}
    }
    else {
	bool docd = (instr(buf,"-d", TRUE) != Nullch);
 	char whereami[512];
 
	if (docd)
	    getwd(whereami);
	sw_list(buf+1);
	if (docd) {
	    cwd_check();
	    if (chdir(whereami)) {		/* -d does chdirs */
		printf(nocd,whereami) FLUSH;
		sig_catcher(0);
	    }
	}
    }
    return 0;
}

/* process range commands */

int
numnum()
{
    ART_NUM min, max;
    char *cmdlst = Nullch;
    register char *s, *c;
    ART_NUM oldart = art;
    char tmpbuf[LBUFLEN];
    bool justone = TRUE;		/* assume only one article */

    perform_cnt = 0;
    if (!finish_command(TRUE))	/* get rest of command */
	return NN_INP;
	if (lastart < 1) {
	    fputs("\nNo articles\n",stdout) FLUSH;
	    return NN_ASK;
	}
#ifdef ARTSRCH
    if (srchahead)
	srchahead = -1;
#endif
    for (s=buf; *s && (isdigit(*s) || index(" ,-.$",*s)); s++)
	if (!isdigit(*s))
	    justone = FALSE;
    if (*s) {
	cmdlst = savestr(s);
	justone = FALSE;
    }
    else if (!justone)
	cmdlst = savestr("m");
    *s++ = ',';
    *s = '\0';
    safecpy(tmpbuf,buf,LBUFLEN);
    for (s = tmpbuf; c = index(s,','); s = ++c) {
	*c = '\0';
	if (*s == '.')
	    min = oldart;
	else
	    min = atol(s);
	if (min < absfirst) {
	    min = absfirst;
	    printf("(First article is %ld)\n",(long)absfirst) FLUSH;
	    pad(just_a_sec/3);
	}
	if ((s=index(s,'-')) != Nullch) {
	    s++;
	    if (*s == '$')
		max = lastart;
	    else if (*s == '.')
		max = oldart;
	    else
		max = atol(s);
	}
	else
	    max = min;
	if (max>lastart) {
	    max = lastart;
	    if (min > max)
		min = max;
	    printf("(Last article is %ld)\n",(long)lastart) FLUSH;
	    pad(just_a_sec/3);
	}
	if (max < min) {
	    fputs("\nBad range\n",stdout) FLUSH;
	    if (cmdlst)
		free(cmdlst);
	    return NN_ASK;
	}
	if (justone) {
	    art = min;
	    return NN_REREAD;
	}
	for (art=min, artp=article_ptr(min); art<=max; art++, artp++) {
	    if (perform(cmdlst,TRUE)) {
#ifdef VERBOSE
		IF(verbose)
		    printf("\n(Interrupted at article %ld)\n",(long)art)
		      FLUSH;
		ELSE
#endif
#ifdef TERSE
		    printf("\n(Intr at %ld)\n",(long)art) FLUSH;
#endif
		if (cmdlst)
		    free(cmdlst);
		return NN_ASK;
	    }
	}
    }
    art = oldart;
    if (cmdlst)
	free(cmdlst);
    return NN_NORM;
}

int
use_selected()
{
    register SUBJECT *sp;
    register ARTICLE *ap;
    bool want_read;
    char *cmdstr;
    int len, ret = 1;
    int subj_mask = (sel_mode == SM_THREAD? (SF_THREAD|SF_VISIT) : SF_VISIT);

    if (!finish_command(TRUE))	/* get rest of command */
	return 0;
    if (!buf[1])
	return -1;
    cmdstr = savestr(buf+1);
    want_read = (sel_rereading || *cmdstr =='m');

    perform_cnt = 0;
    page_line = 1;
    len = strlen(cmdstr);

    /* A few commands can just loop through the subjects. */
    if ((len == 1 && (*cmdstr == 't' || *cmdstr == 'J'))
     || (len == 2
      && (((*cmdstr == '+' || *cmdstr == '-') && cmdstr[0] == cmdstr[1])
       || *cmdstr == 'T'))) {
	for (sp = first_subject; sp; sp = sp->next) {
	    if ((sp->flags & subj_mask) == subj_mask) {
		artp = first_art(sp);
		if (artp) {
		    art = article_num(artp);
		    if (perform(cmdstr, FALSE)) {
			fputs("\nInterrupted\n", stdout) FLUSH;
			goto break_out;
		    }
		}
#ifdef VERBOSE
		IF(verbose)
		    if (mode != 't' && *cmdstr != 't' && *cmdstr != 'T')
			putchar('\n') FLUSH;
#endif
	    }
	}
    } else if (strEQ(cmdstr, "E")) {
	/* The 'E'nd-decode command doesn't do any looping at all. */
	if (decode_fp)
	    decode_end();
	else
	    ret = 2;
    } else {
	/* The rest loop through all (selected) articles. */
	/* Use the explicit article-order if it exists */
	if (artptr_list) {
	    ARTICLE **app, **limit = artptr_list + article_count;
	    for (app = artptr_list; app < limit; app++)
		if ((!((ap = *app)->flags & AF_READ) ^ want_read)
		 && (ap->flags & sel_mask)) {
		    art = article_num(ap);
		    artp = ap;
		    if (perform(cmdstr, TRUE)) {
			fputs("\nInterrupted\n", stdout) FLUSH;
			goto break_out;
		    }
		}
	} else {
	    for (sp = first_subject; sp; sp = sp->next)
		if ((sp->flags & subj_mask) == subj_mask)
		    for (ap = first_art(sp); ap; ap = next_art(ap))
			if ((!(ap->flags & AF_READ) ^ want_read)
			 && (ap->flags & sel_mask)) {
			    art = article_num(ap);
			    artp = ap;
			    if (perform(cmdstr, TRUE)) {
				fputs("\nInterrupted\n", stdout) FLUSH;
				goto break_out;
			    }
			}
	}
    }
  break_out:
    free(cmdstr);
    return ret;
}

int
perform(cmdlst,toplevel)
register char *cmdlst;
int toplevel;
{
    register int ch;
    bool saveit = FALSE;
    
    if (toplevel) {
	printf("%-6ld ",art);
	fflush(stdout);
    }
    perform_cnt++;
    for (; ch = *cmdlst; cmdlst++) {
	if (isspace(ch) || ch == ':')
	    continue;
	if (ch == 'j' && !saveit) {
	    if (!was_read(art)) {
		mark_as_read();
#ifdef VERBOSE
		IF(verbose)
		    fputs("\tJunked",stdout);
#endif
	    }
	    if (sel_rereading)
		deselect_article(artp);
	} else if (ch == '+') {
	    if (cmdlst[1] == '+') {
		if (sel_mode == SM_THREAD)
		    select_thread(artp->subj->thread,
				  saveit? AF_AUTOSELECTALL : 0);
		else
		    select_subject(artp->subj, saveit? AF_AUTOSELECTALL : 0);
		cmdlst++;
	    } else
		select_article(artp, (saveit? AF_AUTOSELECT : 0) | AF_ECHO);
	} else if (ch == '.') {
	    select_subthread(artp, saveit? AF_AUTOSELECT : 0);
	} else if (ch == '-') {
	    if (cmdlst[1] == '-') {
		if (sel_mode == SM_THREAD)
		    deselect_thread(artp->subj->thread);
		else
		    deselect_subject(artp->subj);
		cmdlst++;
	    } else
		deselect_article(artp);
	} else if (ch == ',') {
	    kill_subthread(artp, saveit? (KF_ALL|KF_KILLFILE) : KF_ALL);
	} else if (ch == 'J' || ch == 'j') {
	    kill_thread(artp->subj->thread,
			saveit? (KF_ALL|KF_KILLFILE) : KF_ALL);
	} else if (ch == 't') {
	    entire_tree(artp);
	} else if (ch == 'T') {
	    saveit = TRUE;
	} else if (ch == 'm') {
	    if (was_read(art)) {
		unmark_as_read();
#ifdef VERBOSE
		IF(verbose)
		    fputs("\tMarked unread",stdout);
#endif
	    }
	}
	else if (ch == 'M') {
	    delay_unmark(artp);
	    oneless(artp);
#ifdef VERBOSE
	    IF(verbose)
		fputs("\tWill return",stdout);
#endif
	}
	else if (ch == '=') {
	    printf("\t%s",fetchsubj(art,FALSE));
#ifdef VERBOSE
	    IF(verbose)
		;
	    ELSE
#endif
		putchar('\n') FLUSH;		/* ghad! */
	}
	else if (ch == 'C') {
#ifdef ASYNC_PARSE
	    printf("\t%sancelled",(cancel_article() ? "Not c" : "C"));
#else
	    notincl("C");
	    return -1;
#endif
	}
	else if (ch == '%') {
#ifdef ASYNC_PARSE
	    char tmpbuf[512];

	    if (one_command)
		interp(tmpbuf, (sizeof tmpbuf), cmdlst);
	    else
		cmdlst = dointerp(tmpbuf, (sizeof tmpbuf), cmdlst, ":") - 1;
	    perform_cnt--;
	    if (perform(tmpbuf,FALSE))
		return -1;
#else
	    notincl("%");
	    return -1;
#endif
	}
	else if (index("!&sSwWe|",ch)) {
	    if (one_command)
		strcpy(buf,cmdlst);
	    else
		cmdlst = cpytill(buf,cmdlst,':') - 1;
	    /* we now have the command in buf */
	    if (ch == '!') {
		escapade();
#ifdef VERBOSE
		IF(verbose)
		    fputs("\tShell escaped",stdout);
#endif
	    }
	    else if (ch == '&') {
		switcheroo();
#ifdef VERBOSE
		IF(verbose)
		    if (buf[1] && buf[1] != '&')
			fputs("\tSwitched",stdout);
#endif
	    }
	    else {
		putchar('\t');
		save_article();
#ifdef VERBOSE
		IF(verbose)
		    ;
		ELSE
#endif
		    putchar('\n') FLUSH;
	    }
	}
	else {
	    printf("\t???%s\n",cmdlst);
	    return -1;
	}
#ifdef VERBOSE
	fflush(stdout);
#endif
	if (one_command)
	    break;
    }
    if (toplevel) {
#ifdef VERBOSE
	IF(verbose)
	    putchar('\n') FLUSH;
#endif
    }
    if (int_count) {
	int_count = 0;
	return -1;
    }
    return 0;
}
