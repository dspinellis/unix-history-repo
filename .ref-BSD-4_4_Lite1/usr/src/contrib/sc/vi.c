/*	SC	A Spreadsheet Calculator
 *
 *	One line vi emulation
 *	$Revision: 6.8 $
 */


#include <signal.h>
#include <curses.h>

#ifdef BSD42
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif

#if !defined(strchr) && !defined(UPORT)
#define strchr index
#endif
extern	char	*strchr();

#include <stdio.h>
#include <ctype.h>
#include "sc.h"

#define istext(a) (isalnum(a) || ((a) == '_'))

extern int showrange;
extern char mode_ind;		/* Mode indicator */

/* values for mode below */

#define INSERT_MODE	0	/* Insert mode */
#define EDIT_MODE       1	/* Edit mode */
#define REP_MODE        2	/* Replace mode */
#define SEARCH_MODE	3	/* Get arguments for '/' command */
 
static int mode = INSERT_MODE;
static char *history[HISTLEN];
static int histp = -1;
static char *last_search;
static char *undo_line;
static int undo_lim;
static char dotb[100];
static int doti = 0;
static int do_dot = 0;

void
write_line(c)
int c;
{
    if (mode == EDIT_MODE) {
	switch(c) {
	case (ctl('h')):	linelim = back_line();		break;
	case (ctl('m')):  cr_line();			break;
	case ESC:	stop_edit();			break;
	case '+':	for_hist();			break;
	case '-':	back_hist();			break;
	case '$':	last_col();			break;
	case '.':	dotcmd();			break;
	case '/':	search_mode();			break;
	case '0':	col_0();			break;
	case 'D':	u_save(c);del_to_end();		break;
	case 'I':	u_save(c);col_0();insert_mode();break;
	case 'R':	replace_mode();			break;
	case 'X':	u_save(c); back_space();	break;
	case 'a':	u_save(c); append_line();	break;
	case 'b':	linelim = back_word();		break;
	case 'c':	u_save(c); change_cmd();	break;
	case 'd':	u_save(c); delete_cmd();	break;
	case 'f':	linelim = find_char();		break;
	case 'h':	linelim = back_line();		break;
	case 'i':	u_save(c); insert_mode();	break;
	case 'j':	for_hist();			break;
	case 'k':	back_hist();			break;
	case 'l':	linelim = for_line(0);		break;
	case 'n':	search_again();			break;
	case 'q':	stop_edit();			break;
	case 'r':	u_save(c); rep_char();		break;
	case 't':	linelim = to_char();		break;
	case 'u':	restore_it();			break;
	case 'w':	linelim = for_word(0);		break;
	case 'x':	u_save(c); del_in_line();	break;
	default:	break;
	}
    } else if (mode == INSERT_MODE) { 
	savedot(c);
	switch(c) {
	case (ctl('h')):	back_space();			break;
	case (ctl('m')):  cr_line();			break;
	case ESC:	edit_mode();			break;
	default:	ins_in_line(c);			break;
	}
    } else if (mode == SEARCH_MODE) {
	switch(c) {
	case (ctl('h')):	back_space();			break;
	case (ctl('m')):  search_hist();			break;
	case ESC:	edit_mode();			break;
	default:	ins_in_line(c);			break;
	}
   } else if (mode == REP_MODE) {
	savedot(c);
	switch(c) {
	case (ctl('h')):	back_space();			break;
	case (ctl('m')):  cr_line();			break;
	case ESC:	edit_mode();			break;
	default:	replace_in_line(c);		break;
	}
    }
}

edit_mode()
{
    mode = EDIT_MODE;
    mode_ind = 'e';
    histp = -1;
    if (line[linelim] == '\0')
	linelim = back_line();
}

void
insert_mode()
{
    mode_ind = 'i';
    mode = INSERT_MODE;
}

search_mode()
{
    line[0] = '/';
    line[1] = 0;
    linelim = 1;
    histp = -1;
    mode_ind = '/';
    mode = SEARCH_MODE;
}

replace_mode()
{
    mode_ind = 'R';
    mode = REP_MODE;
}

/* dot command functions.  Saves info so we can redo on a '.' command */

savedot(c)
int c;
{
    if (do_dot)
	return;

    dotb[doti++] = c;
    dotb[doti] = 0;
}

dotcmd()
{
    int c;

    do_dot = 1;
    doti = 0;
    while(dotb[doti] != 0) {
	c = dotb[doti++];
	write_line(c);
    }
    do_dot = 0;
    doti = 0;
}

vigetch()
{
    int c;

    if(do_dot) {
	if (dotb[doti] != 0) {
	    return(dotb[doti++]);
	} else {
	    do_dot = 0;
	    doti = 0;
	    return(nmgetch());
	}
    }
    c = nmgetch();
    savedot(c);
    return(c);
}

/* saves the current line for possible use by an undo cmd */

u_save(c)
int c;
{
    if (undo_line) {
	xfree(undo_line);
	undo_line = 0;
    }
    undo_line = strcpy(xmalloc((unsigned)(strlen(line)+1)), line);
    undo_lim = linelim;

    /* reset dot command if not processing it. */

    if (!do_dot) {
        doti = 0;
	savedot(c);
    }
}

/* Restores the current line saved by u_save() */

restore_it()
{
    register char *tempc;
    register int tempi;

    if (!undo_line)
	return;
    tempc = strcpy(xmalloc((unsigned)(strlen(line)+1)), line);
    tempi = linelim;
    strcpy(line, undo_line);
    linelim = undo_lim;
    xfree(undo_line);
    undo_line = tempc;
    undo_lim = tempi;
}

/* This command stops the editing process. */

stop_edit()
{
    showrange = 0;
    linelim = -1;
    (void) move(1, 0);
    (void) clrtoeol();
}

/*
 * Motion commands.  Forward motion commands take an argument
 * which, when set, cause the forward motion to continue onto
 * the null at the end of the line instead of stopping at the
 * the last character of the line.
 */

for_line(stop_null)
int stop_null;
{
    if (linelim >= 0 && line[linelim] != 0 && 
    		        (line[linelim+1] != 0 || stop_null))
	return(linelim+1);
    else
	return(linelim);
}

for_word(stop_null)
int stop_null;
{
    register int c;
    register int cpos;

    cpos = linelim;

    if (line[cpos] == ' ') {
	while (line[cpos] == ' ')
	    cpos++;
	if (cpos > 0 && line[cpos] == 0)
	    --cpos;
	return(cpos);
    }

    if (istext(line[cpos])) {
    	while ((c = line[cpos]) && istext(c)) 
		cpos++;
    } else {
	while ((c = line[cpos]) && !istext(c) && c != ' ')
		cpos++;
    }

    while (line[cpos] == ' ')
        cpos++;

    if (cpos > 0 && line[cpos] == 0 && !stop_null) 
        --cpos;

    return(cpos);
}

back_line()
{
    if (linelim)
        return(linelim-1);
    else
	return(0);
}

back_word()
{
    register int c;
    register int cpos;

    cpos = linelim;

    if (line[cpos] == ' ') {
	/* Skip white space */
        while (cpos > 0 && line[cpos] == ' ')
	    --cpos;
    } else if (cpos > 0 && (line[cpos-1] == ' ' 
		     ||  istext(line[cpos]) && !istext(line[cpos-1])
		     || !istext(line[cpos]) &&  istext(line[cpos-1]))) {
	/* Started on the first char of a word - back up to prev. word */
	--cpos;
        while (cpos > 0 && line[cpos] == ' ')
	    --cpos;
    }

    /* Skip across the word - goes 1 too far */
    if (istext(line[cpos])) {
    	while (cpos > 0 && (c = line[cpos]) && istext(c)) 
		--cpos;
    } else {
	while (cpos > 0 && (c = line[cpos]) && !istext(c) && c != ' ')
		--cpos;
    }

    /* We are done - fix up the one too far */
    if (cpos > 0 && line[cpos] && line[cpos+1]) 
	cpos++;

    return(cpos);
}

/* Text manipulation commands */

del_in_line()
{
    register int len, i;

    if (linelim >= 0) {
	len = strlen(line);
	if (linelim == len && linelim > 0)
	    linelim--;
	for (i = linelim; i < len; i++)
	    line[i] = line[i+1];
    }
    if (linelim > 0 && line[linelim] == 0)
	--linelim;
}

ins_in_line(c)
int c;
{
    register int i, len;

    len = strlen(line);
    for (i = len; i >= linelim; --i)
	line[i+1] = line[i];
    line[linelim++] = c;
    line[len+1] = 0;
}

void
ins_string(s)
char *s;
{
    while (*s)
	ins_in_line(*s++);
}

append_line()
{
    register int i;

    i = linelim;
    if (i >= 0 && line[i])
	linelim++;
    insert_mode();
}

rep_char()
{
    int c;

    c = vigetch();
    if (line[linelim] != 0) {
    	line[linelim] = c;
    } else {
	line[linelim] = c;
	line[linelim+1] = 0;
    }
}

replace_in_line(c)
{
    register int len;

    len = strlen(line);
    line[linelim++] = c;
    if (linelim > len)
	line[linelim] = 0;
}
    
back_space()
{
    if (linelim == 0)
	return;

    if (line[linelim] == 0) {
	linelim = back_line();
	del_in_line();
	linelim = strlen(line);
    } else {
	linelim = back_line();
	del_in_line();
    }
}

get_motion()
{
    int c;

    c = vigetch();
    switch (c) {
    case 'b':	return(back_word());
    case 'f':	return(find_char()+1);
    case 'h':	return(back_line());
    case 'l':	return(for_line(1));
    case 't':	return(to_char()+1);
    case 'w':	return(for_word(1));
    default:	return(linelim);
    }
}

delete_cmd()
{
    int cpos;

    cpos = get_motion();
    del_chars(cpos, linelim);
}

change_cmd()
{
    delete_cmd();
    insert_mode();
}

del_chars(first, last)
register int first, last;
{
    int temp;

    if (first == last)
	return;

    if (last < first) {
	temp = last; last = first; first = temp;
    }

    linelim = first;
    while(first < last) {
	del_in_line();
	--last;
    }
}

del_to_end()
{
    if (linelim < 0)
	return;
    line[linelim] = 0;
    linelim = back_line();
}

cr_line()
{
    showrange = 0;
    insert_mode();
    save_hist();
    linelim = 0;
    (void) yyparse ();
    linelim = -1;
}

/* History functions */

save_hist()
{
    register int i;

    /* free the oldest one */
    if (history[HISTLEN-1]) {
	xfree(history[HISTLEN-1]);
	history[HISTLEN-1] = 0;
    }

    /* Move the others back */
    for (i = HISTLEN-1; i > 0; --i)
	history[i] = history[i-1];

    history[0] = xmalloc((unsigned) strlen(line)+1);
    strcpy(history[0], line);
}

back_hist()
{
    if (histp == -1 || histp < HISTLEN-1 && history[histp + 1])
	histp++;

    if (history[histp]) {
    	strcpy(line, history[histp]);
	linelim = 0;
    } else
	line[linelim = 0] = 0;

}

search_hist()
{
    if (last_search) {
	xfree(last_search);
	last_search = 0;
    }

    if(linelim < 1) {
	linelim = 0;
	edit_mode();
	return;
    }

    last_search = strcpy(xmalloc((unsigned)(strlen(line+1)+1)), line+1);
    search_again();
    mode = EDIT_MODE;
}

search_again()
{
    int found_it;
    int do_next;
    int prev_histp;
    char *look_here;

    prev_histp = histp;
    if (!last_search)
	return;

    do {
	back_hist();
	if (prev_histp == histp)
	    break;
	prev_histp = histp;
	look_here = line;
	found_it = do_next = 0;
	while ((look_here = strchr(look_here, last_search[0])) &&
						!found_it && !do_next) {

	    if (strncmp(look_here, last_search, strlen(last_search)) == 0)
		found_it++;
	    else if (look_here < line + strlen(line) - 1)
	        look_here++;
	    else
		do_next++;
	}
    } while (!found_it);
}

for_hist()
{
    if (histp > 0)
        histp--;

    if (histp >= 0 && history[histp]) {
    	strcpy(line, history[histp]);
	linelim = 0;
    } else
	line[linelim = 0] = 0;
}

col_0()
{
    linelim = 0;
}

last_col()
{
    linelim = strlen(line);
    if (linelim > 0)
	--linelim;
}

find_char()
{
    register int c;
    register int i;


    c = vigetch();
    i = linelim;
    while(line[i] && line[i] != c)
	i++;
    if (!line[i])
	i = linelim;
    return(i);
}

to_char()
{
    register int i;

    i = find_char();
    if (i > 0 && i != linelim)
	--i;

    return(i);
}
