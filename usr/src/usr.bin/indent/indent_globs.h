/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)indent_globs.h	5.1 (Berkeley) %G%
 */

/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved

FILE NAME:
	indent_globs.h

PURPOSE:
	This include file contains the declarations for all global variables
	used in indent.

GLOBALS:
	The names of all of the variables will not be repeated here.  The 
	declarations start on the next page.

FUNCTIONS:
	None
*/
#define bufsize 600/* size of internal buffers */
#define inp_bufs 512
		   /* size of input buffer */
#define sc_size 5000
		   /* size of save_com buffer */
#define label_offset 2
		   /* number of levels a label is placed to left of code 
		   */

#define d_ljust 0  /* default for ljust_decl */
#define d_max_col 75
		   /* default for max_col */
#define d_com_ind 33
		   /* default com_ind */
#define d_ind_size 4
		   /* default ind_size */
#define d_verbose 0/* default verbose */
#define d_unindent 1
		   /* default value for unindent_displace */
#define d_leave_comma 0
		   /* default value for leave_comma */
#define d_btype_2 1/* default value for btype_2 */

#define tabsize 8  /* the size of a tab */
#define tabmask 0177770
		   /* mask used when figuring length of lines with tabs */


#define false 0
#define true  1


int     input;     /* the fid for the input file */
int     output;    /* the fid for the output file */

char    labbuf[bufsize];
		   /* buffer for label */
char   *s_lab;     /* start ... */
char   *e_lab;     /* .. and end of stored label */

char    codebuf[bufsize];
		   /* buffer for code section */
char   *s_code;    /* start ... */
char   *e_code;    /* .. and end of stored code */

char    combuf[bufsize];
		   /* buffer for comments */
char   *s_com;     /* start ... */
char   *e_com;     /* ... and end of stored comments */

char    in_buffer[inp_bufs];
		   /* input buffer */
char   *buf_ptr;   /* ptr to next character to be taken from in_buffer */
char   *buf_end;   /* ptr to first after last char in in_buffer */

char    save_com[sc_size];
		   /* input text is saved here when looking for the brace
		      after an if, while, etc */
char   *sc_end;    /* pointer into save_com buffer */

char   *bp_save;   /* saved value of buf_ptr when taking input from
		      save_com */
char   *be_save;   /* similarly saved value of buf_end */

char    token[bufsize];
		   /* the last token scanned */




int     bl_line;   /* set to 1 by dump_line if the line is blank */
int     break_comma;
		   /* when true and not in parens, break after a comma */
int     btype_2;   /* when true, brace should be on same line as if,
		      while, etc */
int     case_ind;  /* indentation level to be used for a "case n:" */
int     code_lines;/* count of lines with code */
int     col_1;     /* set to true if the last token started in column 1 */
int     com_col;   /* this is the column in which the current coment
		      should start */
int     com_ind;   /* the column in which comments to the right of code
		      should start */
int     com_lines; /* the number of lines with comments, set by dump_line 
		   */
int     dec_nest;  /* current nesting level for structure or init */
int     decl_com_ind;
		   /* the column in which comments after declarations
		      should be put */
int     decl_on_line;
		   /* set to true if this line of code has part of a
		      declaration on it */
int     had_eof;   /* set to true when input is exhausted */
int     i_l_follow;/* the level to which ind_level should be set after the
		      current line is printed */
int     in_decl;   /* set to true when we are in a declaration stmt.  The
		      processing of braces is then slightly different */
int     in_stmt;   /* set to 1 while in a stmt */
int     ind_level; /* the current indentation level */
int     ind_size;  /* the size of one indentation level */
int     ind_stmt;  /* set to 1 if next line should have an extra
		      indentation level because we are in the middle of a
		      stmt */
int     last_u_d;  /* set to true after scanning a token which forces a
		      following operator to be unary */
int     leave_comma;
		   /* if true, never break declarations after commas */
int     line_no;   /* the current line number. */
int     ljust_decl;/* true if declarations should be left justified */
int     max_col;   /* the maximum allowable line length */
int     out_coms;  /* the number of comments processed, set by pr_comment 
		   */
int     out_lines; /* the number of lines written, set by dump_line */
int     p_l_follow;/* used to remember how to indent following statement 
		   */
int     paren_level;
		   /* parenthesization level. used to indent within stmts 
		   */
int     pcase;     /* set to 1 if the current line label is a case.  It is
		      printed differently from  a regular label */
int     search_brace;
		   /* set to true by parse when it is necessary to buffer
		      up all info up to the start of a stmt after an if,
		      while, etc */
int     unindent_displace;
		   /* comments not to the right of code will be placed
		      this many indentation levels to the left of code */
int     use_ff;    /* set to one if the current line should be terminated
		      with a form feed */
int     verbose;   /* when true, non-essential error messages are printed 
		   */
