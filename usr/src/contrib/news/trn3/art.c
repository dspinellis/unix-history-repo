/* $Id: art.c,v 3.0 1992/02/01 03:09:32 davison Trn $
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
#include "ngstuff.h"
#include "ngdata.h"
#include "cache.h"
#include "bits.h"
#include "head.h"
#include "help.h"
#include "search.h"
#include "artio.h"
#include "ng.h"
#include "final.h"
#include "artstate.h"
#include "rcstuff.h"
#include "term.h"
#include "sw.h"
#include "util.h"
#include "backpage.h"
#include "intrp.h"
#include "rthread.h"
#include "rt-select.h"
#include "rt-util.h"
#include "rt-wumpus.h"
#include "INTERN.h"
#include "art.h"

/* page_switch() return values */

#define PS_NORM 0
#define PS_ASK 1
#define PS_RAISE 2
#define PS_TOEND 3

bool special = FALSE;		/* is next page special length? */
int slines = 0;			/* how long to make page when special */
ART_LINE highlight = -1;	/* next line to be highlighted */
char *restart = Nullch;		/* if nonzero, the place where last */
				/* line left off on line split */
char *blinebeg;			/* where in buffer current line began */
ART_POS alinebeg;		/* where in file current line began */

#ifdef INNERSEARCH
ART_POS innersearch = 0;	/* artpos of end of line we found */
				/* for 'g' command */
ART_LINE isrchline = 0;			/* last line to display */
bool hide_everything = FALSE;
				/* if set, do not write page now, */
				/* but refresh when done with page */
COMPEX gcompex;				/* in article search pattern */
#endif

bool firstpage;			/* is this the 1st page of article? */

char art_buf[LBUFLEN];		/* place for article lines */

void
art_init()
{
    ;
}

#ifdef METAMAIL
#define VERY_LONG_STRING        200
int
display_mime()
{
    if (!getenv("NOMETAMAIL")) {
	int code;

	interp(cmd_buf,(sizeof cmd_buf),getval("MIMESHOW",MIMESHOW));
	termlib_reset();
	resetty();
	code = doshell(SH,cmd_buf);
	noecho();
	crmode();
	termlib_init();
	return code;
    }
    return 1;
}
#endif


int
do_article()
{
    register char *s;
    ART_POS artsize;			/* size in bytes of article */
    bool hide_this_line = FALSE;	/* hidden header line? */
    ART_LINE linenum;	/* line # on page, 1 origin */
#ifdef ULSMARTS
    bool under_lining = FALSE;		/* are we underlining a word? */
#endif
    register char *bufptr = art_buf;	/* pointer to input buffer */
    register int outpos;		/* column position of output */
    static char prompt_buf[64];		/* place to hold prompt */
    bool notesfiles = FALSE;		/* might there be notesfiles junk? */
    char oldmode = mode;
    char *ctime();
#ifdef METAMAIL
    bool tried_display_mime = FALSE;
#endif
#ifdef INNERSEARCH
    register int outputok;
#endif

#ifdef METAMAIL
    mime_article = FALSE;
#endif

    if (fstat(fileno(artfp),&filestat)) /* get article file stats */
	return DA_CLEAN;
    if ((filestat.st_mode & S_IFMT) != S_IFREG)
	return DA_NORM;
    artsize = filestat.st_size;		/* from that get article size */
    sprintf(prompt_buf,
	"%%sEnd of article %ld (of %ld) -- what next? [%%s]",
	(long)art,(long)lastart);	/* format prompt string */
    prompt = prompt_buf;
    int_count = 0;		/* interrupt count is 0 */
    if ((firstpage = (topline < 0)) != 0) {
	parseheader(art);
	fseek(artfp,htype[PAST_HEADER].ht_minpos,0);
    }
    for (;;) {			/* for each page */
	if (ThreadedGroup && max_tree_lines)
	    init_tree();	/* init tree display */
	assert(art == openart);
	if (do_fseek) {
	    parseheader(art);		/* make sure header is ours */
	    artpos = vrdary(artline);
	    if (artpos < 0)
		artpos = -artpos;	/* labs(), anyone? */
	    if (firstpage)
		artpos = (ART_POS)0;
	    if (artpos < htype[PAST_HEADER].ht_minpos) {
		in_header = SOME_LINE;
		fseek(artfp,htype[PAST_HEADER].ht_minpos,0);
	    } else
		fseek(artfp,artpos,0);
	    do_fseek = FALSE;
	    restart = Nullch;
	}
	linenum = 1;
	if (firstpage) {
	    if (firstline) {
		interp(art_buf, (sizeof art_buf), firstline);
		linenum += tree_puts(art_buf,linenum+topline,0);
		artopen(art);		/* rewind article in case interp */
					/* forced a header parse */
	    } else 
            {
		ART_NUM i;
		int selected, unseen;

		selected = (curr_artp->flags & AF_SEL);
		unseen = !was_read(art);
		sprintf(art_buf,"%s%s #%ld",ngname,moderated,(long)art);
		if (selected_only) {
		    i = selected_count - (unseen && selected);
		    sprintf(art_buf+strlen(art_buf)," (%ld + %ld more)",
			    (long)i,(long)toread[ng] - selected_count
					- (!selected && unseen));
		}
		else if ((i = (ART_NUM)(toread[ng] - unseen)) != 0
		       || (!ThreadedGroup && dmcount))
		    sprintf(art_buf+strlen(art_buf)," (%ld more)",(long)i);
		if (!ThreadedGroup && dmcount)
		    sprintf(art_buf+strlen(art_buf)-1," + %ld Marked to return)",
			    (long)dmcount);
		linenum += tree_puts(art_buf,linenum+topline,0);
	    }
	    start_header(art);
	    forcelast = FALSE;		/* we will have our day in court */
	    restart = Nullch;
	    artline = 0;		/* start counting lines */
	    artpos = 0;
	    vwtary(artline,artpos);	/* remember pos in file */
	}
	for (;				/* linenum already set */
	  in_header || (
#ifdef INNERSEARCH
	  innersearch ? innermore() :
#endif
	  linenum<(firstpage?initlines:(special?slines:LINES)) );
	  linenum++) {		/* for each line on page */
	    if (int_count) {	/* exit via interrupt? */
		putchar('\n') FLUSH;	/* get to left margin */
		int_count = 0;	/* reset interrupt count */
		mode = oldmode;
		special = FALSE;
		return DA_NORM;	/* skip out of loops */
	    }
	    if (restart) {		/* did not finish last line? */
		bufptr = restart;	/* then start again here */
		restart = Nullch;	/* and reset the flag */
	    }
	    else if (in_header && headbuf[artpos] != '\0') {
		bufptr = index(headbuf+artpos,'\n') + 1;
		bcopy(headbuf+artpos,art_buf,bufptr-headbuf-artpos);
		art_buf[bufptr-headbuf-artpos] = '\0';
		bufptr = art_buf;
	    } else {
		if (fgets(art_buf,LBUFLEN,artfp)==Nullch) {
					/* if all done */
		    mode = oldmode;
		    special = FALSE;
		    return DA_NORM;	/* skip out of loops */
		}
		bufptr = art_buf;	/* so start at beginning */
		art_buf[LBUFLEN-1] = '\0';
					/* make sure string ends */
	    }
	    blinebeg = bufptr;	/* remember where we began */
	    alinebeg = artpos;	/* both in buffer and file */
	    if (in_header && bufptr == art_buf) {
		hide_this_line =
		    parseline(art_buf,do_hiding,hide_this_line);
		if (!in_header) {
		    linenum += finish_tree(linenum+topline);
		    end_header();
		}
	    } else if (notesfiles && do_hiding &&
	      bufptr == art_buf && *art_buf == '#' &&
	      isupper(art_buf[1]) && art_buf[2] == ':' ) {
		fgets(art_buf,sizeof(art_buf),artfp);
		if (index(art_buf,'!') != Nullch)
		    fgets(art_buf,sizeof(art_buf),artfp);
		htype[PAST_HEADER].ht_minpos = ftell(artfp);
					/* exclude notesfiles droppings */
		hide_this_line = TRUE;	/* and do not print either */
		notesfiles = FALSE;
	    }
#ifdef CUSTOMLINES
	    if (hideline && bufptr == art_buf &&
		  execute(&hide_compex,art_buf) )
		hide_this_line = TRUE;
#endif
	    if (in_header && htype[in_header].ht_flags & HT_MAGIC) {
		if (in_header == NGS_LINE) {
		    if ((s = index(art_buf,'\n')) != Nullch)
			*s = '\0';
		    hide_this_line = (index(art_buf,',') == Nullch)
			&& strEQ(art_buf+12, ngname);
		    if (s != Nullch)
			*s = '\n';
		}
		else if (in_header == EXPIR_LINE) {
		    if (!(htype[EXPIR_LINE].ht_flags & HT_HIDE))
			hide_this_line = (strlen(art_buf) < 10);
		}
		else if (in_header == FROM_LINE) {
		    if (do_hiding && (s = extract_name(art_buf+6)) != Nullch)
			strcpy(art_buf+6,s);
		}
		else if (in_header == DATE_LINE) {
		    if (do_hiding && curr_artp->date != -1)
			strftime(art_buf+6, sizeof(art_buf)-6,
				 getval("LOCALTIMEFMT", LOCALTIMEFMT),
				 localtime(&curr_artp->date));
		}
#ifdef METAMAIL
		else if (in_header == CONTENT_LINE) {
		    mime_article = nontext(art_buf+14);
		}
#endif
	    }
	    if (in_header == SUBJ_LINE &&
		htype[SUBJ_LINE].ht_flags & HT_MAGIC) {
			    /* is this the subject? */
		int length;

		length = strlen(art_buf)-1;
		artline++;
		art_buf[length] = '\0';		/* wipe out newline */
#ifdef NOFIREWORKS
		no_ulfire();
#endif
		notesfiles =
		    (instr(&art_buf[length-10]," - (nf", TRUE) != Nullch);
		/* tree_puts(, ,1) underlines subject */
		linenum += tree_puts(art_buf,linenum+topline,1)-1;
	    }
	    else if (hide_this_line && do_hiding) {
					/* do not print line? */
		linenum--;		/* compensate for linenum++ */
		if (!in_header)
		    hide_this_line = FALSE;
	    }
	    else if (in_header) {
		artline++;
		linenum += tree_puts(art_buf,linenum+topline,0)-1;
	    }
	    else {			/* just a normal line */
#ifdef METAMAIL
		if (mime_article && !tried_display_mime) {
		    if (display_mime() == 0)
			return DA_NORM;
		    else
			tried_display_mime = TRUE;
		}
#endif
		if (highlight==artline) {	/* this line to be highlit? */
		    if (marking == STANDOUT) {
#ifdef NOFIREWORKS
			if (erase_screen)
			    no_sofire();
#endif
			standout();
		    }
		    else {
#ifdef NOFIREWORKS
			if (erase_screen)
			    no_ulfire();
#endif
			underline();
		    }
		    if (*bufptr == '\n')
			putchar(' ');
		}
#ifdef INNERSEARCH
		outputok = !hide_everything;
					/* get it into register, hopefully */
#endif
#ifdef CLEAREOL
#ifdef INNERSEARCH
		if (outputok)
#endif
		maybe_eol();	
#endif /* CLEAREOL */
#ifdef CUSTOMLINES
		if (pagestop && bufptr == art_buf && 
		  execute(&page_compex,art_buf) )
		    linenum = 32700;
#endif
		for (outpos = 0; outpos < COLS; ) {
				    /* while line has room */
		    if (*(unsigned char *)bufptr >= ' ') { /* normal char? */
#ifdef ULSMARTS
			if (*bufptr == '_') {
			    if (bufptr[1] == '\b') {
				if (!under_lining && highlight!=artline
#ifdef INNERSEARCH
				    && outputok
#endif
				    ) {
				    under_lining++;
				    if (UG) {
					if (bufptr != buf &&
					  bufptr[-1] == ' ') {
					    outpos--;
					    backspace();
					}
				    }
				    underline();
				}
				bufptr += 2;
			    }
			}
			else {
			    if (under_lining) {
				under_lining = 0;
				un_underline();
				if (UG) {
				    if (*bufptr == ' ')
					goto skip_put;
				    outpos++;
				}
			    }
			}
#endif
#ifdef INNERSEARCH
			if (outputok)
#endif
			{
#ifdef ROTATION
			    if (rotate && !in_header
			      && isalpha(*bufptr)) {
				if ((*bufptr & 31) <= 13)
				    putchar(*bufptr+13);
				else
				    putchar(*bufptr-13);
			    }
			    else
#endif
				putchar(*bufptr);
			}
			if (*UC && ((highlight==artline && marking == 1)
#ifdef ULSMARTS
			    || under_lining
#endif
			    )) {
			    backspace();
			    underchar();
			}
		    skip_put:
			bufptr++;
			outpos++;
		    }
		    else if (*bufptr == '\n' || !*bufptr) {
						    /* newline? */
#ifdef ULSMARTS
			if (under_lining) {
			    under_lining = 0;
			    un_underline();
			}
#endif
#ifdef DEBUG
			if (debug & DEB_INNERSRCH && outpos < COLS - 6) {
			    standout();
			    printf("%4d",artline); 
			    un_standout();
			}
#endif
#ifdef INNERSEARCH
			if (outputok)
#endif
			    putchar('\n') FLUSH;
			restart = 0;
			outpos = 1000;	/* signal normal \n */
		    }
		    else if (*bufptr == '\t') {	/* tab? */
			int incpos =  8 - outpos % 8;
#ifdef INNERSEARCH
			if (outputok)
#endif
			    if (GT)
				putchar(*bufptr);
			    else
				while (incpos--) putchar(' ');
			bufptr++;
			outpos += 8 - outpos % 8;
		    }
		    else if (*bufptr == '\f') {	/* form feed? */
#ifdef INNERSEARCH
			if (outputok)
#endif
			    fputs("^L",stdout);
			if (bufptr == blinebeg && highlight != artline)
			    linenum = 32700;
			    /* how is that for a magic number? */
			bufptr++;
			outpos += 2;
		    }
		    else {		/* other control char */
#ifdef INNERSEARCH
			if (outputok)
#endif
			{
			    if (dont_filter_control)
				putchar(*bufptr);
			    else {
				putchar('^');
				if (highlight == artline && *UC && marking == 1) {
				    backspace();
				    underchar();
				    putchar(*bufptr+64);
				    backspace();
				    underchar();
				}
				else
				    putchar(*bufptr+64);
			    }
			}
			bufptr++;
			outpos += 2;
		    }
		    
		} /* end of column loop */

		if (outpos < 1000) {/* did line overflow? */
		    restart = bufptr;
				    /* restart here next time */
		    if (!AM || XN) {/* no automatic margins on tty? */
#ifdef INNERSEARCH		    /* then move it down ourselves */
			if (outputok)
#endif
			    putchar('\n') FLUSH;
		    }
		    if (*bufptr == '\n')    /* skip the newline */
			restart = 0;
		}

		/* handle normal end of output line formalities */

		if (highlight == artline) {
					/* were we highlighting line? */
		    if (marking == STANDOUT)
			un_standout();
		    else
			un_underline();
		    highlight = -1;	/* no more we are */
		}
		artline++;	/* count the line just printed */
		if (artline - LINES + 1 > topline)
			    /* did we just scroll top line off? */
		    topline = artline - LINES + 1;
			    /* then recompute top line # */
	    }

	    /* determine actual position in file */

	    if (restart)	/* stranded somewhere in the buffer? */
		artpos += restart - blinebeg;
			    /* just calculate position */
	    else if (in_header)
		artpos = index(headbuf+artpos, '\n') - headbuf + 1;
	    else		/* no, ftell will do */
		artpos = ftell(artfp);
	    vwtary(artline,artpos);	/* remember pos in file */
	} /* end of line loop */

#ifdef INNERSEARCH
	innersearch = 0;
	if (hide_everything) {
	    hide_everything = FALSE;
	    *buf = Ctl('l');
	    goto fake_command;
	}
#endif
	if (linenum >= 32700)/* did last line have formfeed? */
	    vwtary(artline-1,-vrdary(artline-1));
			    /* remember by negating pos in file */

	special = FALSE;	/* end of page, so reset page length */
	firstpage = FALSE;	/* and say it is not 1st time thru */

	/* extra loop bombout */

	if (artpos == artsize) {/* did we just now reach EOF? */
	    mode = oldmode;
	    return DA_NORM;	/* avoid --MORE--(100%) */
	}

/* not done with this article, so pretend we are a pager */

reask_pager:		    
	unflush_output();	/* disable any ^O in effect */
	standout();		/* enter standout mode */
	printf("--MORE--(%ld%%)",(long)(artpos*100/artsize));
	un_standout();	/* leave standout mode */
#ifdef CLEAREOL
 	maybe_eol();
#endif
	fflush(stdout);
/* reinp_pager:     			/* unused, commented for lint */
	eat_typeahead();
#ifdef DEBUG
	if (debug & DEB_CHECKPOINTING) {
	    printf("(%d %d %d)",checkcount,linenum,artline);
	    fflush(stdout);
	}
#endif
	if (checkcount >= docheckwhen &&
	  linenum == LINES &&
	  (artline > 40 || checkcount >= docheckwhen+10) ) {
			    /* while he is reading a whole page */
			    /* in an article he is interested in */
	    checkcount = 0;
	    checkpoint_rc();	/* update .newsrc */
	}
	cache_until_key();
	mode = 'p';
	getcmd(buf);
	if (errno) {
	    if (LINES < 100 && !int_count)
		*buf = '\f';/* on CONT fake up refresh */
	    else {
		*buf = 'q';	/* on INTR or paper just quit */
	    }
	}
	carriage_return();
#ifndef CLEAREOL
	erase_eol();	/* and erase the prompt */
#else
	if (erase_screen && can_home_clear)	
	    clear_rest();
	else
	    erase_eol();	/* and erase the prompt */
#endif /* CLEAREOL */
	carriage_return();	/* Resets kernel's tab column counter to 0 */
	fflush(stdout);

    fake_command:		/* used by innersearch */
	output_chase_phrase = TRUE;

	/* parse and process pager command */

	switch (page_switch()) {
	case PS_ASK:	/* reprompt "--MORE--..." */
	    goto reask_pager;
	case PS_RAISE:	/* reparse on article level */
	    mode = oldmode;
	    return DA_RAISE;
	case PS_TOEND:	/* fast pager loop exit */
	    mode = oldmode;
	    return DA_TOEND;
	case PS_NORM:	/* display more article */
	    break;
	}
    } /* end of page loop */
}

/* process pager commands */

int
page_switch()
{
    register char *s;
    
    switch (*buf) {
    case 'd':
    case Ctl('d'):	/* half page */
	special = TRUE;
	slines = LINES / 2 + 1;
	if (marking && *blinebeg != '\f'
#ifdef CUSTOMLINES
	  && (!pagestop || blinebeg != art_buf ||
	      !execute(&page_compex,blinebeg))
#endif
	  ) {
	    up_line();
	    highlight = --artline;
	    restart = blinebeg;
	    artpos = alinebeg;
	}
	return PS_NORM;
    case '!':			/* shell escape */
	escapade();
	return PS_ASK;
#ifdef INNERSEARCH
    case Ctl('i'):
	gline = 3;
	sprintf(cmd_buf,"^[^%c]",*blinebeg);
	compile(&gcompex,cmd_buf,TRUE,TRUE);
	goto caseG;
    case Ctl('g'):
	gline = 3;
	compile(&gcompex,"^Subject:",TRUE,TRUE);
	goto caseG;
    case 'g':		/* in-article search */
	if (!finish_command(FALSE))/* get rest of command */
	    return PS_ASK;
	s = buf+1;
	if (isspace(*s))
	    s++;
	if ((s = compile(&gcompex,s,TRUE,TRUE)) != Nullch) {
			    /* compile regular expression */
	    printf("\n%s\n",s) FLUSH;
	    return PS_ASK;
	}
	carriage_return();
	erase_eol();	/* erase the prompt */
	carriage_return();	/* Resets kernel's tab column counter to 0 */
	/* FALL THROUGH */
    caseG:
    case 'G': {
	/* ART_LINE lines_to_skip = 0; */
	ART_POS start_where;

	if (gline < 0 || gline > LINES-2)
	    gline = LINES-2;
#ifdef DEBUG
	if (debug & DEB_INNERSRCH)
	    printf("Start here? %d  >=? %d\n",topline + gline + 1,artline)
	      FLUSH;
#endif
	if (*buf == Ctl('i') || topline+gline+1 >= artline)
	    start_where = artpos;
			/* in case we had a line wrap */
	else {
	    start_where = vrdary(topline+gline+1);
	    if (start_where < 0)
		start_where = -start_where;
	}
	if (start_where < htype[PAST_HEADER].ht_minpos)
	    start_where = htype[PAST_HEADER].ht_minpos;
	fseek(artfp,(long)start_where,0);
	innersearch = 0; /* assume not found */
	while (fgets(buf, sizeof buf, artfp) != Nullch) {
	    /* lines_to_skip++; 		NOT USED NOW */
#ifdef DEBUG
	    if (debug & DEB_INNERSRCH)
		printf("Test %s",buf) FLUSH;
#endif
	    if (execute(&gcompex,buf) != Nullch) {
		innersearch = ftell(artfp);
		break;
	    }
	}
	if (!innersearch) {
	    fseek(artfp,artpos,0);
	    fputs("(Not found)",stdout) FLUSH;
	    return PS_ASK;
	}
#ifdef DEBUG
	if (debug & DEB_INNERSRCH)
	    printf("On page? %ld <=? %ld\n",(long)innersearch,(long)artpos)
	      FLUSH;
#endif
	if (innersearch <= artpos) {	/* already on page? */
	    if (innersearch < artpos) {
		artline = topline+1;
		while (vrdary(artline) < innersearch)
		    artline++;
	    }
	    highlight = artline - 1;
#ifdef DEBUG
	    if (debug & DEB_INNERSRCH)
		printf("@ %d\n",highlight) FLUSH;
#endif
	    topline = highlight - gline;
	    if (topline < -1)
		topline = -1;
	    *buf = '\f';		/* fake up a refresh */
	    innersearch = 0;
	    return page_switch();
	}
	else {				/* who knows how many lines it is? */
	    do_fseek = TRUE;
	    hide_everything = TRUE;
	}
	return PS_NORM;
    }
#else
    case 'g': case 'G': case Ctl('g'):
	notincl("g");
	return PS_ASK;
#endif
    case '\n':		/* one line */
	special = TRUE;
	slines = 2;
	return PS_NORM;
#ifdef ROTATION
    case 'X':
	rotate = !rotate;
	/* FALL THROUGH */
#endif
    case 'l':
    case '\f':		/* refresh screen */
#ifdef DEBUG
	if (debug & DEB_INNERSRCH) {
	    printf("Topline = %d",topline) FLUSH;
	    gets(buf);
	}
#endif
	clear();
	carriage_return();	/* Resets kernel's tab column counter to 0 */
	do_fseek = TRUE;
	artline = topline;
	if (artline < 0)
	    artline = 0;
	firstpage = (topline < 0);
	return PS_NORM;
    case 'b':
    case '\b':
    case Ctl('b'): {	/* back up a page */
	ART_LINE target;

#ifndef CLEAREOL
	clear();
#else
	if (can_home_clear)	/* if we can home do it */
	    home_cursor();
	else
	    clear();

#endif /* CLEAREOL */
	carriage_return();	/* Resets kernel's tab column counter to 0 */
	do_fseek = TRUE;	/* reposition article file */
	target = topline - (LINES - 2);
	artline = topline;
	if (artline >= 0) do {
	    artline--;
	} while(artline >= 0 && artline > target && vrdary(artline-1) >= 0);
	topline = artline;
			/* remember top line of screen */
			/*  (line # within article file) */
	if (artline < 0)
	    artline = 0;
	firstpage = (topline < 0);
	return PS_NORM;
    }
    case 'h': {		/* help */
	int cmd;

	if ((cmd = help_page()) > 0)
	    pushchar(cmd);
	return PS_ASK;
    }
    case 't':		/* output thread data */
	page_line = 1;
	entire_tree(curr_artp);
	return PS_ASK;
    case '_':
	if (!finish_dblchar())
	    return PS_ASK;
	switch (buf[1] & 0177) {
	default:
	    goto leave_pager;
	}
	break;
    case '\177':
    case '\0':		/* treat del,break as 'n' */
	*buf = 'n';
	/* FALL THROUGH */
    case 'k':	case 'K':
    case 'T':	case 'J':
    case 'n':	case 'N':	case Ctl('n'):
    case 's':	case 'S':
    case 'e':
    case 'u':
    case 'w':	case 'W':
    case '|':
	mark_as_read();		/* mark article as read */
	/* FALL THROUGH */
    case 'U':	case ',':
    case '<':	case '>':
    case '[':	case ']':
    case '{':	case '}':
    case '(':   case ')':
    case '+':   case ':':
    case '#':
    case '$':
    case '&':
    case '-':
    case '.':
    case '/':
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
    case '=':
    case '?':
    case 'c':	case 'C':	
#ifdef DEBUG
    case 'D':
#endif
    case 'E':
    case 'f':	case 'F':	case Ctl('f'):
    case 'j':
				case Ctl('k'):
    case 'm':	case 'M':	
    case 'p':	case 'P':	case Ctl('p'):	
		case 'Q':
    case 'r':	case 'R':	case Ctl('r'):
    case 'v':
		case 'Y':
#ifndef ROTATION
    case 'x':	case 'X':
#endif
    case Ctl('x'):
    case 'z':	case 'Z':
    case '^':
leave_pager:
#ifdef ROTATION
	rotate = FALSE;
#endif
	reread = FALSE;
	do_hiding = TRUE;
	if (index("nNpP\016\020",*buf) == Nullch &&
	  index("wWsSe:!&|/?123456789.",*buf) != Nullch) {
	    setdfltcmd();
	    standout();		/* enter standout mode */
	    printf(prompt,mailcall,dfltcmd);
			    /* print prompt, whatever it is */
	    un_standout();	/* leave standout mode */
	    putchar(' ');
	    fflush(stdout);
	}
	return PS_RAISE;	/* and pretend we were at end */
#ifdef ROTATION
    case 'x':
	rotate = TRUE;
	/* FALL THROUGH */
#endif
    case 'y':
    case Ctl('v'):
					/* Leaving it undocumented in case */
					/* I want to steal the key--LAW */
    case ' ':	/* continue current article */
	if (erase_screen) {	/* -e? */
#ifndef CLEAREOL
	    clear();		/* clear screen */
#else
	    if (can_home_clear)	/* if we can home do it */
		home_cursor();
	    else
		clear();	/* else clear screen */

#endif /* CLEAREOL */
	    carriage_return();	/* Resets kernel's tab column counter to 0 */
	    fflush(stdout);

	    if (*blinebeg != '\f'
#ifdef CUSTOMLINES
	      && (!pagestop || blinebeg != art_buf ||
	          !execute(&page_compex,blinebeg))
#endif
	      ) {
		restart = blinebeg;
		artline--;	 /* restart this line */
		artpos = alinebeg;
		if (marking)	/* and mark repeated line */
		    highlight = artline;
	    }
	    topline = artline;
			/* and remember top line of screen */
			/*  (line # within article file) */
	}
	else if (marking && *blinebeg != '\f'
#ifdef CUSTOMLINES
	  && (!pagestop || blinebeg != art_buf ||
	      !execute(&page_compex,blinebeg))
#endif
	  ) {
				/* are we marking repeats? */
	    up_line();		/* go up one line */
	    highlight = --artline;/* and get ready to highlight */
	    restart = blinebeg;	/*   the old line */
	    artpos = alinebeg;
	}
	return PS_NORM;
    case 'q':	/* quit this article? */
	do_hiding = TRUE;
	return PS_TOEND;
    default:
	fputs(hforhelp,stdout) FLUSH;
	settle_down();
	return PS_ASK;
    }
}

#ifdef INNERSEARCH
bool
innermore()
{
    if (artpos < innersearch) {		/* not even on page yet? */
#ifdef DEBUG
	if (debug & DEB_INNERSRCH)
	    printf("Not on page %ld < %ld\n",(long)artpos,(long)innersearch)
	      FLUSH;
#endif
	return TRUE;
    }
    if (artpos == innersearch) {	/* just got onto page? */
	isrchline = artline;		/* remember first line after */
	highlight = artline - 1;
#ifdef DEBUG
	if (debug & DEB_INNERSRCH)
	    printf("There it is %ld = %ld, %d @ %d\n",(long)artpos,
		(long)innersearch,hide_everything,highlight) FLUSH;
#endif
	if (hide_everything) {		/* forced refresh? */
	    topline = highlight - gline;
	    if (topline < -1)
		topline = -1;
	    return FALSE;		/* let refresh do it all */
	}
    }
#ifdef DEBUG
    if (debug & DEB_INNERSRCH)
	printf("Not far enough? %d <? %d + %d\n",artline,isrchline,gline)
	  FLUSH;
#endif
    if (artline < isrchline + gline) {
	return TRUE;
    }
    return FALSE;
}
#endif

#ifdef METAMAIL
int
nontext(content_type)
char *content_type;
{
    char *t;

    if (content_type[0] == '\n')
	return 0;
    while (content_type && isspace(*content_type))
	content_type++;
    t = index(content_type, ';');
    if (!t)
	t = index(content_type, '\n');
    if (t)
	*t-- = '\0';
    while (t && *t && t > content_type && isspace(*t))
	*t-- = '\0';
    if (notplain(content_type))
	return 1;
    return 0;
}

int
notplain(s)
char *s;
{
    char *t;
    if (!s)
	return 1;
    while (*s && isspace(*s))
	s++;
    for (t=s; *t; ++t) {
	if (isupper(*t))
	    *t = tolower(*t);
    }
    while (t > s && isspace(*--t)) ;
    if (((t-s) == 3) && !strncmp(s, "text", 4))
	return 0;
    if (strncmp(s, "text/plain", 10))
	return 1;
    t = index(s, ';');
    while (t) {
	t++;
	while (*t && isspace(*t)) t++;
	if (!strncmp(t, "charset", 7)) {
	    s = index(t, '=');
	    if (s) {
		s++;
		while (*s && isspace(*s)) s++;
		if (!strncmp(s, "us-ascii", 8))
		    return 0;
	    }
	    return(1);
	}
	t = index(t, ';');
    }
    return 0;	/* no charset, was text/plain */
}
#endif
