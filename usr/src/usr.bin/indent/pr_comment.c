/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pr_comment.c	5.1 (Berkeley) %G%";
#endif not lint

/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


NAME:
	pr_comment

FUNCTION:
	This routine takes care of scanning and printing comments.

ALGORITHM:
	1) Decide where the comment should be aligned, and if lines should
	   be broken.
	2) If lines should not be broken and filled, just copy up to end of
	   comment.
	3) If lines should be filled, then scan thru input_buffer copying
	   characters to com_buf.  Remember where the last blank, tab, or
	   newline was.  When line is filled, print up to last blank and 
	   continue copying.

PARAMETERS:
	None

RETURNS:
	Nothing

GLOBALS:
	combuf =
	s_com
	e_com =

	buf_ptr =
	buf_end

	bl_line
	col_1
	com_col =
	com_ind
	decl_com_ind
	decl_on_line
	had_eof
	ind_level
	ind_size
	line_no =
	max_col
	out_com =	Count number of comments
	unindent_displace
	use_ff =

CALLS:
	count_spaces
	dump_line
	fill_buffer
	printf		(lib)

CALLED BY:
	main

HISTORY:
	November 1976	D A Willcox of CAC	Initial coding
	12/6/76		D A Willcox of CAC	Modification to handle 
						UNIX-style comments

*/

/* this routine processes comments.  It makes an attempt to keep comments from
   going over the max line length.  If a line is too long, it moves everything
   from the last blank to the next comment line.  Blanks and tabs from the
   beginning of the input line are removed */

#include "indent_globs.h";


pr_comment () {
    int     now_col;
 /* column we are in now */
    int     box_com;
 /* set to true when we are in a "boxed" comment. In that case, the first
    non-blank char should be lined up with the / in /* */
    int     col_1_com;
 /* this comment should not be touched */
    char   *last_bl;
 /* points to the last blank in the output buffer */
    char    achar;
    char   *t_ptr; /* used for movinf string */
    int     unix_comment;
 /* tri-state variable used to decide if it is a unix-style comment. 0 means
    only blanks since /*, 1 means regular style comment, 2 means unix style
    comment */


    last_bl = 0;	       /* no blanks found so far */
    box_com = col_1_com = false;
 /* at first, assume that we are not in a boxed comment or some other comment
    that should not be touched */
    ++out_coms;		       /* keep track of number of comments */
    unix_comment = 0;	       /* set flag to let us figure out if there is a
			          unix-style comment */

/*----------------------------------------------------------*\ 
|   Figure where to align and how to treat the comment
\*----------------------------------------------------------*/

    if (col_1) {	       /* if comment starts in column 1 it should not
			          be touched */
	col_1_com = box_com = true;
	com_col = 1;
    }
    else {
	if (*buf_ptr == '-')
	    box_com = true;    /* a comment with a '-' immediately after the /*
			          is assumed to be a boxed comment */
	if ( /* bl_line && */ (s_lab == e_lab) && (s_code == e_code)) {
	/* klg: check only if this line is blank */
	/* 
	 * If this (*and previous lines are*) blank,
	 * don't put comment way out at left
	 */
	    com_col = (ind_level - unindent_displace) * ind_size + 1;
	    if (com_col <= 1)
		com_col = 2;
	}
	else {
	    com_col = (decl_on_line || ind_level == 0 ? decl_com_ind : com_ind);
	}
    }

    *e_com++ = '/';	       /* put '/*' into buffer */
    *e_com++ = '*';
    if (*buf_ptr != ' ' && !box_com)
	*e_com++ = ' ';

    *e_com = '\0';
    now_col = count_spaces (com_col, s_com);
 /* figure where what column we would be in if we printed the comment now */


/*----------------------------------------------------------*\ 
|    Start to copy the comment
\*----------------------------------------------------------*/

    while (1) {		       /* this loop will go until the comment is copied 
			       */
	switch (*buf_ptr) {    /* this checks for various spcl cases */
	    case 014: 	       /* check for a form feed */
		if (!box_com) {/* in a text comment, break the line here */
		    use_ff = true;
		/* fix so dump_line uses a form feed */
		    dump_line ();
		    last_bl = 0;
		    *e_com++ = ' ';
		    *e_com++ = ' ';
		    *e_com++ = ' ';
		    do {       /* get rid of leading blanks */
			if (++buf_ptr >= buf_end)
			    fill_buffer ();
		    } while (*buf_ptr == ' ' || *buf_ptr == '\t');
		}
		else {
		    if (++buf_ptr >= buf_end)
			fill_buffer ();
		    *e_com++ = 014;
		}

		break;

	    case '\n': 
		if (had_eof) { /* check for unexpected eof */
		    printf ("Unterminated comment\n");
		    *e_com = '\0';
		    dump_line ();
		    return;
		}

		if (box_com) { /* if this is a boxed comment, we don't ignore
			          the newline */
		    *e_com = '\0';
		    dump_line ();
		    ++line_no;
		    now_col = com_col;

		    if (!col_1_com) {
		    /* if merely a boxed comment, we should line up first
		       non-blank character */
			do {   /* flush leading non-blanks */
			    if (++buf_ptr >= buf_end)
				fill_buffer ();
			} while (*buf_ptr == ' ' || *buf_ptr == '\t');
		    }
		    else {     /* make sure we at least flush the blank */
			if (++buf_ptr >= buf_end)
			    fill_buffer ();
		    }

		    break;
		}

		if (unix_comment != 1) {
		/* we are in unix_style comment */
		    if (unix_comment == 0 && s_code == e_code) {
		    /* if it is a UNIX-style comment, ignore the requirement
		       that pervious line be blank for unindention */
			com_col = (ind_level - unindent_displace) * ind_size + 1;
			if (com_col <= 1)
			    com_col = 2;
		    }

		    unix_comment = 2;
		/* permanently remember that we are in this type of comment */
		    dump_line ();
		    ++line_no;
		    now_col = com_col;
		    *e_com++ = ' ';
		/* fix so that the star at the start of the line will line up 
		*/
		    do	       /* flush leading white space */
			if (++buf_ptr >= buf_end)
			    fill_buffer ();
		    while (*buf_ptr == ' ' || *buf_ptr == '\t');
		    break;
		}

		if (*(e_com - 1) == ' ' || *(e_com - 1) == '\t')
		    last_bl = e_com - 1;
	    /* if there was a space at the end of the last line, remember where
	       it was */
		else {	       /* otherwise, insert one */
		    last_bl = e_com;
		    *e_com++ = ' ';
		    ++now_col;
		}

		++line_no;     /* keep track of input line number */
		do {	       /* copy any blanks and/or tabs at start of next
			          line */
		    if (++buf_ptr >= buf_end)
			fill_buffer ();
		} while (*buf_ptr == ' ' || *buf_ptr == '\t');

		break;	       /* end of case for newline */

	    case '*': 	       /* must check for possibility of being at end of
			          comment */
		if (++buf_ptr >= buf_end)
			       /* get to next char after * */
		    fill_buffer ();

		if (unix_comment == 0)
			       /* set flag to show we are not in unix-style
			          comment */
		    unix_comment = 1;

		if (*buf_ptr == '/') {
		/* it is the end!!! */
		    if (++buf_ptr >= buf_end)
			fill_buffer ();

		    if (*(e_com - 1) != ' ' && !box_com) {
		    /* insure blank before end */
			*e_com++ = ' ';
			++now_col;
		    }

		    if (now_col > max_col - 2 && !box_com) {
		    /* check if star-slash will go over line */
			*e_com = '\0';
		    /* it will */
			dump_line ();
			now_col = com_col;
		    }

		    *e_com++ = '*';
		/* move end of comment */
		    *e_com++ = '/';
		    *e_com = '\0';
		    return;    /* we is done */
		}	       /* end of end of comment */


		else {	       /* handle isolated '*' */
		    *e_com++ = '*';
		    ++now_col;
		    break;
		}
	    /* end of processing of * */

	    default: 	       /* we have a random char */
		if (unix_comment == 0 && *buf_ptr != ' ' && *buf_ptr != '\t')
		    unix_comment = 1;
	    /* we are not in unix-style comment */

		*e_com = *buf_ptr++;
		if (buf_ptr >= buf_end)
		    fill_buffer ();

		if (*e_com == '\t')
			       /* keep track of column */
		    now_col = ((now_col - 1) & tabmask) + tabsize + 1;
		else
		    if (*e_com == '')
			       /* this is a backspace */
			--now_col;
		    else
			++now_col;

		if (*e_com == ' ' || *e_com == '\t')
		    last_bl = e_com;
	    /* remember we saw a blank */

		++e_com;
		if (now_col > max_col && !box_com && unix_comment == 1) {
		/* the comment is too long, it must be broken up */
		    if (last_bl == 0) {
		    /* we have seen no blanks */
			printf ("%d: Comment too long\n", line_no);
			last_bl = e_com;
		    /* fake it */
			*e_com++ = ' ';
		    }

		    *e_com = '\0';
		/* print what we have */
		    *last_bl = '\0';
		    e_com = last_bl;
		    dump_line ();

		    *e_com++ = ' ';
		/* add blanks for continuation */
		    *e_com++ = ' ';
		    *e_com++ = ' ';

		    t_ptr = last_bl + 1;
		    last_bl = 0;
		    while (*t_ptr != '\0') {
		    /* move unprinted pare of comment down in buffer */
			if (*t_ptr == ' ' || *t_ptr == '\t')
			    last_bl = e_com;
			*e_com++ = *t_ptr++;
		    }

		    *e_com = '\0';
		    now_col = count_spaces (com_col, s_com);
		/* recompute current position */
		}	       /* end of code for splitting a comment */
		break;	       /* end of default case */


	}		       /* end of switch */

    }			       /* end of while (1) */
}		   /* end of pr_comment */
