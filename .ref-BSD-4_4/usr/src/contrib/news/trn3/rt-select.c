/* $Id: rt-select.c,v 3.0 1992/12/14 00:14:12 davison Trn $
*/

#include "EXTERN.h"
#include "common.h"
#include "trn.h"
#include "term.h"
#include "final.h"
#include "util.h"
#include "help.h"
#include "cache.h"
#include "bits.h"
#include "artsrch.h"
#include "ng.h"
#include "ngdata.h"
#include "ngstuff.h"
#include "kfile.h"
#include "rthread.h"
#include "rt-page.h"
#include "rt-util.h"
#include "INTERN.h"
#include "rt-select.h"

/* When display mode is 'l', each author gets a separate line; when 's', no
** authors are displayed.
*/
char *display_mode = select_order;
char sel_disp_char[] = { ' ', '+', '-', '*' };

static char sel_ret;
static bool empty_ok;
static bool disp_status_line;
static bool clean_screen;

/* Display a menu of threads/subjects/articles for the user to choose from.
** If "cmd" is '+' we display all the unread items and allow the user to mark
** them as selected and perform various commands upon them.  If "cmd" is 'U'
** the list consists of previously-read items for the user to mark as unread.
*/
char
do_selector(cmd)
char_int cmd;
{
    register int j;
    int got_dash;
    int ch, action;
    char page_char, end_char;
    char promptbuf[80];
    char oldmode = mode;
    char *in_select;

    mode = 't';
    sel_rereading = (cmd == 'U');
    clear_on_stop = TRUE;
    empty_ok = FALSE;

    set_sel_mode(cmd);

    if (!cache_range(sel_rereading? absfirst : firstart, lastart)) {
	sel_ret = '+';
	goto sel_exit;
    }

  start_selector:
    /* Setup for selecting articles to read or set unread */
    if (sel_rereading) {
	page_char = '>';
	end_char = 'Z';
	sel_page_app = Null(ARTICLE**);
	sel_page_sp = Nullsubj;
	sel_mask = AF_DELSEL;
    } else {
	page_char = page_select;
	end_char = end_select;
	if (curr_artp) {
	    sel_last_ap = curr_artp;
	    sel_last_sp = curr_artp->subj;
	}
	sel_mask = AF_SEL;
    }
    selected_only = TRUE;
    count_subjects(cmd ? CS_UNSEL_STORE : CS_NORM);

    /* If nothing to display, we're done. */
    if (!article_count && !empty_ok) {
	empty_complaint();
	sel_ret = '+';
	goto sel_exit;
    }
    init_pages();
    sel_item_index = 0;
    *promptbuf = '\0';
    disp_status_line = FALSE;
    if (added_articles > 0) {
	sprintf(promptbuf, "** %ld new article%s arrived **  ",
		(long)added_articles, added_articles == 1? nullstr : "s");
	disp_status_line = TRUE;
    }
    added_articles = 0;
    if (cmd && selected_count) {
	sprintf(promptbuf+strlen(promptbuf), "%ld article%s selected.",
		(long)selected_count, selected_count == 1? " is" : "s are");
	disp_status_line = TRUE;
    }
    cmd = 0;
display_selector:
    /* Present a page of items to the user */
    display_page();

    /* Check if there is really anything left to display. */
    if (!sel_item_cnt && !empty_ok) { /*TODO: this may not be needed anymore */
	empty_complaint();
	sel_ret = '+';
	goto sel_exit;
    }
    empty_ok = FALSE;

    if (sel_item_index >= sel_item_cnt)
	sel_item_index = 0;
    if (disp_status_line) {
	printf("\n%s", promptbuf);
	if (can_home) {
	    carriage_return();
	    goto_line(sel_last_line+1, sel_last_line);
	} else
	    putchar('\n');
    }
reask_selector:
    /* Prompt the user */
#ifdef MAILCALL
    setmail(FALSE);
#endif
    if (sel_at_end)
	sprintf(cmd_buf, "%s [%c%c] --",
		(!sel_prior_arts? "All" : "Bot"), end_char, page_char);
    else
	sprintf(cmd_buf, "%s%ld%% [%c%c] --",
		(!sel_prior_arts? "Top " : nullstr),
		(long)((sel_prior_arts+sel_page_arts)*100 / sel_total_arts),
		page_char, end_char);
    sprintf(promptbuf, "%s-- %s %s (%s%s order) -- %s", mailcall,
	    sel_exclusive? "SELECTED" : "Select", sel_mode_string,
	    sel_direction<0? "reverse " : nullstr, sel_sort_string, cmd_buf);
#ifdef CLEAREOL
    if (erase_screen && can_home_clear)
	clear_rest();
#endif
    standout();
    fputs(promptbuf, stdout);
    un_standout();
    if (can_home)
	carriage_return();
    sel_line = sel_last_line;
position_selector:
    got_dash = 0;
    if (can_home)
	goto_line(sel_line, sel_items[sel_item_index].line);
    sel_line = sel_items[sel_item_index].line;
reinp_selector:
    /* Grab some commands from the user */
    fflush(stdout);
    eat_typeahead();
    spin_char = sel_chars[sel_item_index];
    cache_until_key();
    spin_char = ' ';
#ifdef CONDSUB
    getcmd(buf);
    ch = *buf;
#else
    getcmd(cmd_buf);	/* If no conditionals, don't allow macros */ 
    buf[0] = ch = *cmd_buf;
    buf[1] = FINISHCMD;
#endif
    if (errno)
	ch = Ctl('l');
    if (disp_status_line) {
	if (can_home) {
	    goto_line(sel_line, sel_last_line+1);
	    erase_eol();
	    sel_line = sel_last_line+1;
	}
	disp_status_line = FALSE;
    }
    if (ch == '-') {
	got_dash = 1;
	if (!can_home)
	    putchar('-'), fflush(stdout);
	goto reinp_selector;
    }
    if (ch == ' ') {
	if (sel_at_end)
	    ch = end_char;
	else
	    ch = page_char;
    }
    in_select = index(sel_chars, ch);
    if (in_select) {
	j = in_select - sel_chars;
	if (j >= sel_item_cnt) {
	    dingaling();
	    goto position_selector;
	} else if (got_dash)
	    ;
	else if (sel_items[j].sel == 1)
	    action = (sel_rereading ? 'k' : '-');
	else
	    action = '+';
    } else if (ch == '*' && sel_mode == SM_ARTICLE) {
	register ARTICLE *ap = (ARTICLE*)sel_items[sel_item_index].ptr;
	if (sel_items[sel_item_index].sel)
	    deselect_subject(ap->subj);
	else
	    select_subject(ap->subj, 0);
	update_page();
	goto position_selector;
    } else if (ch == 'y' || ch == '.' || ch == '*') {
	j = sel_item_index;
	if (sel_items[j].sel == 1)
	    action = (sel_rereading ? 'k' : '-');
	else
	    action = '+';
    } else if (ch == 'k' || ch == 'j' || ch == ',') {
	j = sel_item_index;
	action = 'k';
    } else if (ch == 'm' || ch == '\\') {
	j = sel_item_index;
	action = '-';
    } else if (ch == 'M') {
	j = sel_item_index;
	action = 'M';
    } else if (ch == '@') {
	sel_item_index = 0;
	j = sel_item_cnt-1;
	got_dash = 1;
	action = '@';
    } else if (ch == '[' || ch == 'p') {
	if (--sel_item_index < 0)
	    sel_item_index = sel_item_cnt ? sel_item_cnt-1 : 0;
	goto position_selector;
    } else if (ch == ']' || ch == 'n') {
	if (++sel_item_index >= sel_item_cnt)
	    sel_item_index = 0;
	goto position_selector;
    } else {
	sel_ret = ch;
	switch (sel_command(ch)) {
	case DS_POS:
	    if (!clean_screen)
		goto display_selector;
	    goto position_selector;
	case DS_ASK:
	    if (!clean_screen)
		goto display_selector;
	    goto reask_selector;
	case DS_DISPLAY:
	ds_display:
	    if (disp_status_line)
		strcpy(promptbuf, buf);
	    goto display_selector;
	case DS_UPDATE:
	    if (!clean_screen)
		goto ds_display;
	    if (disp_status_line) {
		printf("\n%s",buf);
		if (can_home) {
		    carriage_return();
		    up_line();
		    erase_eol();
		}
	    }
	    update_page();
	    if (can_home) {
		goto_line(sel_line, sel_last_line);
		sel_line = sel_last_line;
	    }
	    goto reask_selector;
	case DS_RESTART:
	    goto start_selector;
	case DS_QUIT:
	    sel_cleanup();
	    if (!output_chase_phrase)
		putchar('\n') FLUSH;
	    goto sel_exit;
	case DS_STATUS:
	    disp_status_line = TRUE;
	    if (!clean_screen) {
		strcpy(promptbuf, buf);
		goto display_selector;
	    }
	    if (can_home) {
		goto_line(sel_line, sel_last_line+1);
		sel_line = sel_last_line+1;
	    } else
		putchar('\n');

	    fputs(buf, stdout);

	    if (can_home)
		carriage_return();
	    else
		putchar('\n');
	    goto position_selector;
	}
    }

    /* The user manipulated one of the letters -- handle it. */
    if (!got_dash)
	sel_item_index = j;
    else {
	if (j < sel_item_index) {
	    ch = sel_item_index-1;
	    sel_item_index = j;
	    j = ch;
	}
    }
    if (++j == sel_item_cnt)
	j = 0;
    do {
	register int sel = sel_items[sel_item_index].sel;
	register SUBJECT *sp = (SUBJECT*)sel_items[sel_item_index].ptr;
	if (can_home) {
	    goto_line(sel_line, sel_items[sel_item_index].line);
	    sel_line = sel_items[sel_item_index].line;
	}
	if (action == '@') {
	    if (sel == 2)
		ch = (sel_rereading ? '+' : ' ');
	    else if (sel_rereading)
		ch = 'k';
	    else if (sel == 1)
		ch = '-';
	    else
		ch = '+';
	} else
	    ch = action;
	switch (ch) {
	case '+':
	    if (sel_mode == SM_THREAD)
		select_thread(sp->thread, 0);
	    else if (sel_mode == SM_SUBJECT)
		select_subject(sp, 0);
	    else
		select_article((ARTICLE*)sp, 0);
	    output_sel(1);
	    break;
	case '-':  case 'k':  case 'M':
	   {
	    bool sel_reread_save = sel_rereading;
	    if (ch == 'M') {
		if (sel_mode == SM_ARTICLE)
		    delay_unmark((ARTICLE*)sp);
		else {
		    register ARTICLE *ap;
		    if (sel_mode == SM_THREAD) {
			for (ap = first_art(sp); ap; ap = next_art(ap))
			    if (!(ap->flags & AF_READ) ^ sel_rereading)
				delay_unmark(ap);
		    } else {
			for (ap = sp->articles; ap; ap = ap->subj_next)
			    if (!(ap->flags & AF_READ) ^ sel_rereading)
				delay_unmark(ap);
		    }
		}
	    }
	    if (ch == '-')
		sel_rereading = FALSE;
	    else
		sel_rereading = TRUE;
	    if (sel_mode == SM_THREAD)
		deselect_thread(sp->thread);
	    else if (sel_mode == SM_SUBJECT)
		deselect_subject(sp);
	    else
		deselect_article((ARTICLE*)sp);
	    sel_rereading = sel_reread_save;
	    output_sel(ch == '-'? 0 : 2);
	    break;
	   }
	}
	fflush(stdout);
	if (++sel_item_index == sel_item_cnt)
	    sel_item_index = 0;
	if (can_home)
	    carriage_return();
    } while (sel_item_index != j);
    goto position_selector;

sel_exit:
    if (sel_rereading) {
	sel_rereading = 0;
	sel_mask = AF_SEL;
    }
    if (sel_mode != SM_ARTICLE || sel_sort == SS_GROUPS
     || sel_sort == SS_SUBJECT) {
	if (artptr_list) {
	    free((char*)artptr_list);
	    artptr_list = sel_page_app = Null(ARTICLE**);
	    sort_subjects();
	}
	artptr = Null(ARTICLE**);
#ifdef ARTSEARCH
	if (!ThreadedGroup)
	    srchahead = -1;
#endif
    }
#ifdef ARTSEARCH
    else
	srchahead = 0;
#endif
    selected_only = (selected_count || !article_count);
    if (sel_ret != '#')
	count_subjects(sel_ret == '+'? CS_RESELECT : CS_UNSELECT);
    clear_on_stop = FALSE;
    mode = oldmode;
    if (sel_ret == '+') {
	art = curr_art;
	artp = curr_artp;
    } else
	top_article();
    return sel_ret;
}

static void
sel_cleanup()
{
    if (sel_rereading) {
	/* Turn selections into unread selected articles.  Let
	** count_subjects() fix the counts after we're through.
	*/
	register SUBJECT *sp;
	sel_last_ap = Nullart;
	sel_last_sp = Nullsubj;
	for (sp = first_subject; sp; sp = sp->next)
	    unkill_subject(sp);
    } else {
	if (sel_mode == SM_ARTICLE) {
	    register ARTICLE *ap;
	    register ART_NUM an;
	    for (an=absfirst, ap=article_ptr(an); an<=lastart; an++, ap++) {
		if (ap->flags & AF_DEL) {
		    ap->flags &= ~AF_DEL;
		    set_read(ap);
		}
	    }
	} else {
	    register SUBJECT *sp;
	    for (sp = first_subject; sp; sp = sp->next) {
		if (sp->flags & SF_DEL) {
		    sp->flags &= ~SF_DEL;
		    if (sel_mode == SM_THREAD)
			kill_thread(sp->thread, KF_UNSELECTED);
		    else
			kill_subject(sp, KF_UNSELECTED);
		}
	    }
	}
    }
}

static int
sel_command(ch)
char_int ch;
{
    if (can_home)
	goto_line(sel_line, sel_last_line);
    sel_line = sel_last_line;
    clean_screen = TRUE;
  do_command:
    output_chase_phrase = TRUE;
    switch (ch) {
    case '>':
	sel_item_index = 0;
	if (next_page())
	    return DS_DISPLAY;
	return DS_POS;
    case '<':
	sel_item_index = 0;
	if (prev_page())
	    return DS_DISPLAY;
	return DS_POS;
    case '^':  case Ctl('r'):
	sel_item_index = 0;
	if (first_page())
	    return DS_DISPLAY;
	return DS_POS;
    case '$':
	sel_item_index = 0;
	if (last_page())
	    return DS_DISPLAY;
	return DS_POS;
    case Ctl('l'):
	return DS_DISPLAY;
    case Ctl('f'):
	erase_eol();		/* erase the prompt */
#ifdef MAILCALL
	setmail(TRUE);		/* force a mail check */
#endif
	return DS_ASK;
    case '#':
       {
	register SUBJECT *sp;
	for (sp = first_subject; sp; sp = sp->next)
	    sp->flags &= ~SF_VISIT;
	selected_count = 0;
	sp = (SUBJECT*)sel_items[sel_item_index].ptr;
	switch (sel_mode) {
	case SM_THREAD:
	    deselect_thread(sp->thread);
	    select_thread(sp->thread, 0);
	    break;
	case SM_SUBJECT:
	    deselect_subject(sp);
	    select_subject(sp, 0);
	    break;
	case SM_ARTICLE:
	    deselect_article((ARTICLE*)sp);
	    select_article((ARTICLE*)sp, 0);
	    break;
	}
	return DS_QUIT;
       }
    case '\r':  case '\n':
	if (!selected_count) {
	    if (sel_rereading || sel_items[sel_item_index].sel != 2) {
		register SUBJECT *sp = (SUBJECT*)sel_items[sel_item_index].ptr;
		switch (sel_mode) {
		case SM_THREAD:
		    select_thread(sp->thread, 0);
		    break;
		case SM_SUBJECT:
		    select_subject(sp, 0);
		    break;
		case SM_ARTICLE:
		    select_article((ARTICLE*)sp, 0);
		    break;
		}
	    }
	}
	return DS_QUIT;
    case 'Z':  case '\t':
	return DS_QUIT;
    case 'q':  case 'Q':
	return DS_QUIT;
    case Ctl('Q'):  case '\033':  case '+':
	sel_ret = '+';
	return DS_QUIT;
    case 'N':  case 'P':
	return DS_QUIT;
    case 'L':
	if (!*++display_mode)
	    display_mode = select_order;
	return DS_DISPLAY;
    case 'Y':
	if (!dmcount) {
	    sprintf(buf,"No marked articles to yank back.");
	    return DS_STATUS;
	}
	yankback();
	sel_line++;
	if (!sel_rereading)
	    sel_cleanup();
	disp_status_line = TRUE;
	count_subjects(CS_NORM);
	sel_page_sp = Nullsubj;
	sel_page_app = Null(ARTICLE**);
	init_pages();
	return DS_DISPLAY;
    case 'U':
	sel_cleanup();
	sel_rereading = !sel_rereading;
	sel_page_sp = Nullsubj;
	sel_page_app = Null(ARTICLE**);
	if (!cache_range(sel_rereading? absfirst : firstart, lastart))
	    sel_rereading = !sel_rereading;
	empty_ok = TRUE;
	return DS_RESTART;
    case '=':
	if (!sel_rereading)
	    sel_cleanup();
	if (sel_mode == SM_ARTICLE) {
	    set_selector(sel_threadmode, sel_threadsort);
	    sel_page_sp = sel_page_app[0]->subj;
	} else {
	    set_selector(SM_ARTICLE, sel_artsort);
	    sel_page_app = 0;
	}
	count_subjects(CS_NORM);
	sel_item_index = 0;
	init_pages();
	return DS_DISPLAY;
    case 'S':
	if (!sel_rereading)
	    sel_cleanup();
	erase_eol();		/* erase the prompt */
    reask_output:
	in_char("Selector mode:  Threads, Subjects, Articles? [tsa] ", 'o');
	setdef(buf,"t");
#ifdef VERIFY
	printcmd();
#endif
	if (*buf == 'h') {
#ifdef VERBOSE
	    IF(verbose)
		fputs("\n\
Type t or SP to display/select thread groups (threads the group, if needed).\n\
Type s to display/select subject groups.\n\
Type a to display/select individual articles.\n\
Type q to leave things as they are.\n\n\
",stdout) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("\n\
t or SP selects thread groups (threads the group too).\n\
s selects subject groups.\n\
a selects individual articles.\n\
q does nothing.\n\n\
",stdout) FLUSH;
#endif
	    clean_screen = FALSE;
	    goto reask_output;
	} else if (*buf == 'q') {
	    if (can_home) {
		carriage_return();
		erase_eol();
	    }
	    return DS_ASK;
	}
	set_sel_mode(*buf);
	count_subjects(CS_NORM);
	init_pages();
	return DS_DISPLAY;
    case 'O':
	if (!sel_rereading)
	    sel_cleanup();
	erase_eol();		/* erase the prompt */
    reask_sort:
	if (sel_mode == SM_ARTICLE)
	    in_char("Order by Date, Subject, Author, subject-date Groups? [dsagDSAG] ",
		    'q');
	else
	    in_char("Order by Date, Subject, or Count? [dscDSC] ", 'q');
	setdef(buf,"d");
#ifdef VERIFY
	printcmd();
#endif
	if (*buf == 'h') {
#ifdef VERBOSE
	    IF(verbose) {
		fputs("\n\
Type d or SP to order the displayed items by date.\n\
Type s to order the items by subject.\n\
",stdout) FLUSH;
		if (sel_mode == SM_ARTICLE)
		    fputs("\
Type a to order the items by author.\n\
Type g to order the items in subject-groups by date.\n\
",stdout) FLUSH;
		else
		    fputs("\
Type c to order the items by article-count.\n\
",stdout) FLUSH;
		fputs("\
Typing your selection in upper case it will reverse the selected order.\n\
Type q to leave things as they are.\n\n\
",stdout) FLUSH;
	    }
	    ELSE
#endif
#ifdef TERSE
	    {
		fputs("\n\
d or SP sorts by date.\n\
s sorts by subject.\n\
",stdout) FLUSH;
		if (sel_mode == SM_ARTICLE)
		    fputs("\
a sorts by author.\n\
g sorts in subject-groups by date.\n\
",stdout) FLUSH;
		else
		    fputs("\
c sorts by article-count.\n\
",stdout) FLUSH;
		fputs("\
Upper case reverses the sort.\n\
q does nothing.\n\n\
",stdout) FLUSH;
	    }
#endif
	    clean_screen = FALSE;
	    goto reask_sort;
	} else if (*buf == 'q') {
	    if (can_home) {
		carriage_return();
		erase_eol();
	    }
	    return DS_ASK;
	}
	set_sel_sort(*buf);
	count_subjects(CS_NORM);
#if 0
	sel_last_sp = (SUBJECT*)sel_items[sel_item_index].ptr;
	sel_last_ap = sel_page_app[sel_item_index];
#else
	sel_page_sp = Nullsubj;
	sel_page_app = Null(ARTICLE**);
#endif
	init_pages();
	return DS_DISPLAY;
    case 'R':
	if (!sel_rereading)
	    sel_cleanup();
	sel_direction *= -1;
	count_subjects(CS_NORM);
#if 0
	sel_last_sp = (SUBJECT*)sel_items[sel_item_index].ptr;
	sel_last_ap = sel_page_app[sel_item_index];
#else
	sel_page_sp = Nullsubj;
	sel_page_app = Null(ARTICLE**);
#endif
	init_pages();
	return DS_DISPLAY;
    case 'E':
	if (!sel_rereading)
	    sel_cleanup();
	sel_exclusive = !sel_exclusive;
	count_subjects(CS_NORM);
	sel_page_sp = Nullsubj;
	sel_page_app = Null(ARTICLE**);
	init_pages();
	empty_ok = TRUE;
	return DS_DISPLAY;
    case 'X':  case 'D':  case 'J':
	if (!sel_rereading) {
	    if (sel_mode == SM_ARTICLE) {
		register ARTICLE *ap, **app, **limit;
		limit = artptr_list + article_count;
		if (ch == 'D')
		    app = sel_page_app;
		else
		    app = artptr_list;
		for (;;) {
		    ap = *app;
		    if ((!(ap->flags & AF_SEL) ^ (ch == 'J'))
		     || (ap->flags & AF_DEL))
			if (!sel_exclusive || (ap->flags & AF_INCLUDED))
			    set_read(ap);
		    app++;
		    if (app >= limit || (ch == 'D' && app == sel_next_app))
			break;
		}
	    } else {
		register SUBJECT *sp;
		if (ch == 'D')
		    sp = sel_page_sp;
		else
		    sp = first_subject;
		for (;;) {
		    if (((!(sp->flags & SF_SEL) ^ (ch == 'J')) && sp->misc)
		     || (sp->flags & SF_DEL)) {
			if (!sel_exclusive || (sp->flags & SF_INCLUDED))
			    kill_subject(sp, ch=='J'? KF_ALL : KF_UNSELECTED);
		    }
		    sp = sp->next;
		    if (!sp || (ch == 'D' && sp == sel_next_sp))
			break;
		}
	    }
	    count_subjects(CS_UNSELECT);
	    if (article_count
	     && (ch == 'J' || (ch == 'D' && !selected_count))) {
		if (ch == 'D') {
		    sel_page_sp = sel_next_sp;
		    sel_page_app = sel_next_app;
		}
		init_pages();
		sel_item_index = 0;
		return DS_DISPLAY;
	    }
	    if (artptr_list && article_count)
		sort_articles();
	    return DS_QUIT;
	} else if (ch == 'J') {
	    register SUBJECT *sp;
	    for (sp = first_subject; sp; sp = sp->next)
		deselect_subject(sp);
	    selected_subj_cnt = selected_count = 0;
	    return DS_DISPLAY;
	}
	sprintf(buf,"That command does not work in the set-unread selector.");
	return DS_STATUS;
    case 'T':
	if (!ThreadedGroup) {
	    sprintf(buf,"Group is not threaded.");
	    return DS_STATUS;
	}
	/* FALL THROUGH */
    case 'A':
	erase_eol();		/* erase the prompt */
	if (sel_mode == SM_ARTICLE)
	    artp = (ARTICLE*)sel_items[sel_item_index].ptr;
	else
	    artp = ((SUBJECT*)sel_items[sel_item_index].ptr)->articles;
	art = article_num(artp);
	/* This call executes the action too */
	switch (ask_memorize(ch)) {
	case 'j':  case ',':
	    count_subjects(sel_rereading ? CS_NORM : CS_UNSELECT);
	    init_pages();
	    sprintf(buf,"Kill memorized.");
	    disp_status_line = TRUE;
	    return DS_DISPLAY;
	case '.':
	    sprintf(buf,"Selection memorized.");
	    disp_status_line = TRUE;
	    return DS_DISPLAY;
	case '+':
	    sprintf(buf,"Selection memorized.");
	    disp_status_line = TRUE;
	    return DS_UPDATE;
	case 'c':  case 'C':
	    sprintf(buf,"Auto-commands cleared.");
	    disp_status_line = TRUE;
	    return DS_DISPLAY;
	case 'q':
	    return DS_DISPLAY;
	case 'Q':
	    break;
	}
	if (can_home) {
	    carriage_return();
	    erase_eol();
	}
	return DS_ASK;
    case Ctl('k'):
	edit_kfile();
	return DS_DISPLAY;
    case ':':  case '/':  case '&':  case '!':
	erase_eol();		/* erase the prompt */
	if (!finish_command(TRUE)) {	/* get rest of command */
	    if (clean_screen)
		return DS_ASK;
	    goto extend_done;
	}
	if (ch == '&' || ch == '!') {
	    one_command = TRUE;
	    perform(buf, FALSE);
	    one_command = FALSE;
	    putchar('\n') FLUSH;
	    clean_screen = FALSE;
	} else {
	    int sel_art_save = selected_count;

	    if (ch == ':') {
		clean_screen = (use_selected() == 2) && clean_screen;
		if (!sel_rereading) {
		    register SUBJECT *sp;
		    for (sp = first_subject; sp; sp = sp->next) {
			if (sp->flags & SF_DEL) {
			    sp->flags = 0;
			    if (sel_mode == SM_THREAD)
				kill_thread(sp->thread, KF_UNSELECTED);
			    else
				kill_subject(sp, KF_UNSELECTED);
			}
		    }
		}
	    } else {
		/* Force the search to begin at absfirst or firstart,
		** depending upon whether they specified the 'r' option.
		*/
		art = lastart+1;
		page_line = 1;
		switch (art_search(buf, sizeof buf, FALSE)) {
		case SRCH_ERROR:
		case SRCH_ABORT:
		case SRCH_INTR:
		    fputs("\nInterrupted\n", stdout) FLUSH;
		    break;
		case SRCH_DONE:
		case SRCH_SUBJDONE:
		    fputs("Done\n", stdout) FLUSH;
		    break;
		case SRCH_NOTFOUND:
		    fputs("\nNot found.\n", stdout) FLUSH;
		    break;
		case SRCH_FOUND:
		    break;
		}
		clean_screen = FALSE;
	    }
	    /* Recount, in case something has changed. */
	    count_subjects(sel_rereading ? CS_NORM : CS_UNSELECT);
	    init_pages();
	    sel_item_index = 0;

	    sel_art_save -= selected_count;
	    if (sel_art_save) {
		putchar('\n');
		if (sel_art_save < 0) {
		    fputs("S", stdout);
		    sel_art_save *= -1;
		} else
		    fputs("Des", stdout);
		printf("elected %d article%s.",
			sel_art_save, sel_art_save == 1 ? nullstr : "s");
		clean_screen = FALSE;
	    }
	    if (!clean_screen)
		putchar('\n') FLUSH;
	}/* if !& else :/ */

	if (clean_screen) {
	    carriage_return();
	    up_line();
	    erase_eol();
	    return DS_ASK;
	}
      extend_done:
	if ((ch = pause_getcmd())) {
	  got_cmd:
	    if (ch > 0) {
		/* try to optimize the screen update for some commands. */
		if (!index(sel_chars, ch)
		 && (index("<+>^$!?&:/hDEJLNOPqQRSUXYZ\n\r\t\033", ch)
		  || ch == Ctl('k'))) {
		    buf[0] = sel_ret = ch;
		    buf[1] = FINISHCMD;
		    goto do_command;
		}
		pushchar(ch | 0200);
	    }
	}
	return DS_DISPLAY;
    case 'c':
	erase_eol();		/* erase the prompt */
	if ((ch = ask_catchup()) == 'y' || ch == 'u') {
	    count_subjects(CS_UNSELECT);
	    if (ch != 'u' && article_count) {
		sel_page_sp = Nullsubj;
		sel_page_app = Null(ARTICLE**);
		init_pages();
		return DS_DISPLAY;
	    }
	    sel_ret = 'Z';
	    return DS_QUIT;
	}
	if (ch != 'N')
	    return DS_DISPLAY;
	if (can_home) {
	    carriage_return();
	    erase_eol();
	}
	return DS_ASK;
    case 'h':  case '?':
	putchar('\n');
	if ((ch = help_select()) || (ch = pause_getcmd()))
	    goto got_cmd;
        return DS_DISPLAY;
    default:
	sprintf(buf,"Type ? for help.");
	settle_down();
	if (clean_screen)
	    return DS_STATUS;
	printf("\n%s\n",buf);
	goto extend_done;
    }
}

static void
empty_complaint()
{
    clear_on_stop = FALSE;
    putchar('\n');
    if (sel_rereading) {
#ifdef VERBOSE
	IF (verbose)
	    fputs("\nNo articles to set unread.\n", stdout);
	ELSE
#endif
#ifdef TERSE
	    fputs("\nNo articles.\n", stdout) FLUSH;
#endif
	sel_rereading = 0;
	sel_mask = AF_SEL;
    } else {
#ifdef VERBOSE
	IF (verbose)
	    fputs("\nNo unread articles to select.", stdout);
	ELSE
#endif
#ifdef TERSE
	    fputs("\nNo unread articles.", stdout);
#endif
	putchar('\n');	/* let "them" FLUSH */
    }
    selected_only = FALSE;
    art = curr_art;
    artp = curr_artp;
}
