/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)indent.c	5.1 (Berkeley) %G%";
#endif not lint

/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


NAME:
	indent main program

FUNCTION:
	This is the main program of the indent program.  Indent will take a C
	program source and reformat it into a semi-reasonable form.

ALGORITHM:
	The routine lexi scans tokens and passes them back one at a time to the
	main routine.  The subroutine parse takes care of much of the work of
	figuring indentation level.  

	1) Call lexi
	2) Enter a monster switch statement on the code returned by lexi.  If 
	   the indentation level for the line yet to be printed should be 
	   changed, set the variable ind_level.  If the indentation level for
	   the following line should be changed, set the variable i_l_follow.

PARAMETERS:
	None

RETURNS:
	Nothing

GLOBALS:
	be_save =
	break_comma
	bp_save =
	btype_2 =
	code_lines
	com_ind =
	com_lines
	dec_nest =
	decl_com_ind =
	decl_on_line =
	i_l_follow =
	in_decl =
	ind_level =
	ind_size =
	ind_stmt =
	last_u_d =
	leave_comma =
	line_no =
	ljust_decl =
	max_col =
	out_coms
	out_lines
	p_l_follow =
	paren_level =
	pcase =
	sc_end =
	unindent_displace =
	use_ff =
	verbose =

CALLS:
	atoi (lib)
	cmp
	creat (lib)
	dump_line
	eqin
	fill_buffer
	lexi
	open (lib)
	parse
	pr_comment
	printf (lib)
	seek (lib)
	time (lib)

CALLED BY:
	No one (main routine)

HISTORY:
	November 1976	D A Willcox of CAC	Initial coding
	12/9/76		D A Willcox of CAC	Fixed defaults for decl_com_ind
						to be 8 less than com_ind if 
						left justifying declarations
	12/9/76		D A Willcox of CAC	Fixed processing of nested
						<c>?<s>:<s> constructs
	1/7/77		D A Willcox of CAC	Added check for overwrite of
						input file
						Added code to handle -br and -bl
						parameters
*/
#include "indent_globs.h";
#include "indent_codes.h";

/* #define dolog 1	/* if this define is removed, then the code to
			   produce a log file will be removed */

struct templ {		       /* this is a template for the list of
			          command line args */
    char   *str;	       /* pointer to string which is a valid
			          command line arg */
    int     code;	       /* code to be used in switch for processing
			          this arg */
};


struct templ    options[] =
{			       /* warning - because of the way that this
			          table is scanned, if one entry is an
			          initial substring of another, then the
			          longer entry should occur first */
    "-cd", 4,
    "-c", 2,
    "-l", 1,
    "-i", 3,
    "-v", 5,
    "-nv", 6,
    "-dj", 7,
    "-d", 13,		       /* unindented comment placement */
    "-ndj", 8,
    "-bc", 10,		       /* break after command in decl */
    "-nbc", 9,		       /* don't break after comma */
    "-br", 14,		       /* put brace on right of stmt */
    "-bl", 15,		       /* put brace on left by itself */
    "-st", 16,		       /* use the standard input and output
				  files */
    0, 0
};


char   *in_name = "Standard Input";
			       /* will always point to name of input file 
			       */
char   *out_name = "Standard Output";
			       /* will always point to name of output file
			          */
char    bakfile[32] = "";

main (argc, argv)
int     argc;
char  **argv;
{

    int     dec_ind;	       /* current indentation for declarations */
    int     di_stack[20];      /* a stack of structure indentation levels 
			       */
    int     flushed_nl;	       /* used when buffering up comments to
			          remember that a newline was passed over 
			       */
    int     force_nl;	       /* when true, code must be broken */
    int     hd_type;	       /* used to store type of stmt for if (...),
			          for (...), etc */
    register int    i;	       /* local loop counter */
    int     in_or_st;	       /* Will be true iff there has been a
			          declarator (e.g. int or char) and no
			          left paren since the last semicolon.
			          When true, a { is starting a structure
			          definition or an initialization list */
    register int    j;	       /* local loop counter */
    int     scase;	       /* set to true when we see a case, so we
			          will know what to do with the following
			          colon */
    int     sp_sw;	       /* when true, we are in the expressin of
			          if(...), while(...), etc. */
    int     squest;	       /* when this is positive, we have seen a ?
			          without the matching : in a <c>?<s>:<s>
			          construct */
    register char  *t_ptr;     /* used for copying tokens */
    int     type_code;	       /* the type of token, returned by lexi */
    int     want_blank;	       /* set to true when the following token
			          should be prefixed by a blank. (Said
			          prefixing is ignored in some cases.) */

#ifdef dolog		       /* include declarations needed for log */
    int     log_fid;	       /* fid of log file */

    struct logtmpl {	       /* structure of a log entry */
	int     tvec[2];       /* time of execution */
	char    inp;	       /* input fid */
	char    outp;	       /* output fid */
	int     nout;	       /* # output lines */
	int     ncom;	       /* # comments */
	int     wcom;	       /* # lines w/ comments */
	int     wcode;	       /* # lines w/code */
	char    mc;	       /* max line size */
	char    ci;	       /* comment indentation */
	char    inds;	       /* indent size */
	char    dci;	       /* decl comment indentation */
	char    verb;	       /* verbose */
	char    ljus;	       /* left just */
	char    lvcom;	       /* leave commas */
	char    unin;	       /* unindented comment indentation */
	char    uid;	       /* the user id */
	char    bropt;	       /* btype_2 */
	int     reserved[2];
    };

    struct logtmpl  logent;
#endif

/*-----------------------------------------------*\
|    INITIALIZATION
\*-----------------------------------------------*/


    combuf[0] = codebuf[0] = labbuf[0] = ' ';
 /* set up code, label, and comment buffers */
    combuf[1] = codebuf[1] = labbuf[1] = '\0';
    s_lab = e_lab = labbuf + 1;
    s_code = e_code = codebuf + 1;
    s_com = e_com = combuf + 1;

    buf_ptr = buf_end = in_buffer;
    line_no = 1;
    had_eof = in_decl = decl_on_line = break_comma = false;
    sp_sw = force_nl = false;
    in_or_st = false;
    bl_line = true;
    dec_ind = 0;
    di_stack[dec_nest = 0] = 0;
    want_blank = in_stmt = ind_stmt = false;


    scase = pcase = false;
    squest = 0;
    sc_end = 0;
    bp_save = 0;
    be_save = 0;

    input = -1;
    output = -1;
    ljust_decl = d_ljust;



/*--------------------------------------------------*\
|   COMMAND LINE SCAN
\*--------------------------------------------------*/

    max_col = d_max_col;       /* set up some default values */
    com_ind = d_com_ind;
    ind_size = d_ind_size;
    verbose = d_verbose;
    decl_com_ind = 0;	       /* if this is not set to some positive
			          value by an arg, we will set this equal
			          to com_ind */
    btype_2 = d_btype_2;
    unindent_displace = d_unindent;
    leave_comma = d_leave_comma;

    set_profile ();

    for (i = 1; i < argc; ++i) {
    /* look thru args (if any) for changes to defaults */
	if (argv[i][0] != '-') {/* no flag on parameter */
	    if (input < 0) {   /* we must have the input file */
		in_name = argv[i];	/* remember name of input
					   file */
		input = open (in_name, 0);
		if (input < 0) {	/* check for open error */
		    printf ("Can't open %s\n", argv[i]);
		    exit ();
		}
		continue;
	    }
	    else
		if (output < 0) {	/* we have the output file */
		    out_name = argv[i];	/* remember name of output file */
		    if (cmp (in_name, out_name) == 0) {	 /* attempt to
					   overwright the file */
			printf ("Input and output files must be different\n");
			exit ();
		    }
		    output = creat (out_name, 0644);
		    if (output < 0) {   /* check for create error */
			printf ("Can't create %s\n", argv[i]);
			exit ();
		    }
		    continue;
		}

	    printf ("Unknown parameter: %s\n", argv[i]);
	    exit ();
	}
	else
	    set_option (argv[i]);

    }			       /* end of for */

    if (input < 0) {
	printf ("Usage: indent file [ outfile ] [ options ]\n");
	exit ();
    }
    if (output < 0) {
	out_name = in_name;
	bakcopy ();
    }

    if (com_ind <= 1)
	com_ind = 2;	       /* don't put normal comments before column
			          2 */

    if (decl_com_ind <= 0)     /* if not specified by user, set this */
	decl_com_ind = ljust_decl ? (com_ind <= 10 ? 2 : com_ind - 8) : com_ind;

    fill_buffer ();	       /* get first batch of stuff into input
			          buffer */

    parse (semicolon);
/*-----------------------------------------------------
|   START OF MAIN LOOP
\*----------------------------------------------------*/

    while (1) {		       /* this is the main loop.  it will go until
			          we reach eof */
	type_code = lexi ();   /* lexi reads one token.  The actual
			          characters read are stored in "token".
			          lexi returns a code indicating the type
			          of token */

    /* 
     * The following code moves everything following an if (), while (),
     * else, etc. up to the start of the following stmt to a buffer.  This
     * allows proper handling of both kinds of brace placement.
     */

	flushed_nl = false;
	while (search_brace) { /* if we scanned an if(), while(), etc., we
			          might need to copy stuff into a buffer 
	*//* we must loop, copying stuff into save_com, until we find the
	   start of the stmt which follows the if, or whatever */
	    switch (type_code) {
		case newline: 
		    ++line_no;
		    flushed_nl = true;
		case form_feed: 
		    break;     /* form feeds and newlines found here will
			          be ignored */

		case lbrace:   /* this is a brace that starts the compound
			          stmt */
		    if (sc_end == 0) {
		    /* ignore buffering if a comment wasn't stored up */
			search_brace = false;
			goto check_type;
		    }

		    if (btype_2) {
			save_com[0] = '{';
		    /* we either want to put the brace right after the if 
		    */
			goto sw_buffer;
		    /* go to common code to get out of this loop */
		    }

		default:       /* it is the start of a normal statment */
		    if (flushed_nl)
			       /* if we flushed a newline, make sure it is
			          put back */
			force_nl = true;

		    if (sc_end == 0) {
		    /* ignore buffering if comment wasn't saved up */
			search_brace = false;
			goto check_type;
		    }

		    if (force_nl) {
		    /* if we should insert a nl here, put it into the
		       buffer */
			force_nl = false;
			--line_no;
		    /* this will be re-increased when the nl is read from
		       the buffer */
			*sc_end++ = '\n';
			*sc_end++ = ' ';
			if (verbose && !flushed_nl)
			       /* print error msg if the line was not
			          already broken */
			    printf ("%d: Line broken\n", line_no);
			flushed_nl = false;
		    }

		    for (t_ptr = token; *t_ptr; ++t_ptr)
			*sc_end++ = *t_ptr;
		/* copy token into temp buffer */

	    sw_buffer: 
		    search_brace = false;
		/* stop looking for start of stmt */
		    bp_save = buf_ptr;
		/* save current input buffer */
		    be_save = buf_end;
		    buf_ptr = save_com;
		/* fix so that subsequent calls to lexi will take tokens
		   out of save_com */
		    *sc_end++ = ' ';
		/* add trailing blank, just in case */
		    buf_end = sc_end;
		    sc_end = 0;
		    break;

		case comment:  /* we have a comment, so we must copy it
			          into the buffer */
		    if (sc_end == 0) {
		    /* if this is the first comment, we must set up the
		       buffer */
			save_com[0] = save_com[1] = ' ';
			sc_end = &(save_com[2]);
		    }
		    else {
			*sc_end++ = '\n';
		    /* add newline between comments */
			*sc_end++ = ' ';
			--line_no;
		    }

		    *sc_end++ = '/';
		/* copy in start of comment */
		    *sc_end++ = '*';

		    for (;;) { /* loop until we get to the end of the
			          comment */
			*sc_end = *buf_ptr++;
			if (buf_ptr >= buf_end)
			    fill_buffer ();

			if (*sc_end++ == '*' && *buf_ptr == '/')
			    break;
		    /* we are at end of comment */

			if (sc_end >= &(save_com[sc_size])) {
			/* check for temp buffer overflow */
			    printf ("%d: Internal buffer overflow.\n",
				    line_no);
			    printf ("Move big comment from right after if,\
 while, or whatever.\n");
			    exit ();
			}
		    }

		    *sc_end++ = '/';
		/* add ending slash */
		    if (++buf_ptr >= buf_end)/* get past / in buffer */
			fill_buffer ();
		    break;
	    }		       /* end of switch */

	    if (type_code != 0)/* we must make this check, just in case
			          there was an unexpected EOF */
		type_code = lexi ();
	/* read another token */
	}		       /* end of while (serach_brace) */
check_type: 

	if (type_code == 0) {  /* we got eof */
	    if (s_lab != e_lab || s_code != e_code
		    || s_com != e_com)/* must dump end of line */
		dump_line ();
	    if (i_l_follow != 0)/* check for balanced braces */
		printf ("%d too few }'s\n", i_l_follow);

#ifdef dolog		       /* only include this stuff if we want to
			          keep a log */
	    log_fid = open ("/mnt/net/willcox/indent/indent_log", 1);
	/* open the log file */
	    if (log_fid >= 0) {
		seek (log_fid, 0, 2);
	    /* point to end of log */
		time (logent.tvec);
	    /* get current time */
		logent.inp = input;
	    /* set up the log entry */
		logent.outp = output;
		logent.nout = out_lines;
		logent.ncom = out_coms;
		logent.wcom = com_lines;
		logent.wcode = code_lines;
		logent.mc = max_col;
		logent.ci = com_ind;
		logent.inds = ind_size;
		logent.dci = decl_com_ind;
		logent.verb = verbose;
		logent.ljus = ljust_decl;
		logent.lvcom = leave_comma;
		logent.unin = unindent_displace;
		logent.uid = getuid ();
		logent.bropt = btype_2;
		write (log_fid, &logent, sizeof logent);
	    }
#endif
	    if (verbose) {
		printf ("There were %d output lines and %d comments\n",
			out_lines, out_coms);
		printf ("(Lines with comments)/(Lines with code): %6.3f\n",
			(1.0 * com_lines) / code_lines);
	    }

	    exit ();
	}

	if (
		(type_code != comment) &&
		(type_code != newline) &&
		(type_code != preesc) &&
		(type_code != form_feed)) {
	    if (
		    force_nl
		    &&
		    (type_code != semicolon) &&
		    (
			type_code != lbrace
			||
			!btype_2
		    )) {       /* we should force a broken line here */
		if (verbose && !flushed_nl)
		    printf ("%d: Line broken\n", line_no);
		flushed_nl = false;
		dump_line ();
		want_blank = false;
	    /* don't insert blank at line start */
		force_nl = false;
	    }

	    in_stmt = true;    /* turn on flag which causes an extra level
			          of indentation. this is turned off by a
			          ; or } */
	    if (s_com != e_com) {
	    /* the turkey has embedded a comment in a line. fix it */
		*e_code++ = ' ';
		for (t_ptr = s_com; *t_ptr; ++t_ptr)
		    *e_code++ = *t_ptr;
		*e_code++ = ' ';
		*e_code = '\0';/* null terminate code sect */
		want_blank = false;
		e_com = s_com;
	    }
	}
	else
	    if (type_code != comment)
			       /* preserve force_nl thru a comment */
		force_nl = false;
    /* cancel forced newline after newline, form feed, etc */



    /*----------------------------------------------------*\
    |   do switch on type of token scanned
    \*----------------------------------------------------*/
	switch (type_code) {   /* now, decide what to do with the token */

	    case form_feed:    /* found a form feed in line */
		use_ff = true; /* a form feed is treated much like a
			          newline */
		dump_line ();
		want_blank = false;
		break;

	    case newline: 
		dump_line ();
		++line_no;     /* keep track of input line number */
		want_blank = false;
		break;

	    case lparen:       /* got a ( or [ */
		++p_l_follow;  /* count parens to make Healy happy */
		if (want_blank && *token != '[')
			       /* don't put space in front of square
			          bracket */
		    *e_code++ = ' ';

		if (in_decl)
		    while ((e_code - s_code) < dec_ind)
			*e_code++ = ' ';

		*e_code++ = token[0];
		want_blank = false;
		if (in_or_st && *token == '(') {
		/* this is a kluge to make sure that declarations will be
		   aaigned right if proc decl has an explicit type on it,
		   i.e. "int a(x) {..." */
		    parse (semicolon);
		/* I said this was a kluge... */
		    in_or_st = false;
		/* turn off flag for structure decl or initialization */
		}

		break;

	    case rparen:       /* got a ) or ] */
		if (--p_l_follow < 0) {
		    p_l_follow = 0;
		    printf ("%d: Extra %c\n", line_no, *token);
		}

		if (e_code == s_code)/* if the paren starts the line */
		    paren_level = p_l_follow;
	    /*    then indent it */

		*e_code++ = token[0];
		want_blank = true;

		if (sp_sw && (p_l_follow == 0)) {
		/* check for end of if (...), or some such */
		    sp_sw = false;
		    force_nl = true;
		/* must force newline after if */
		    last_u_d = true;
		/* inform lexi that a following operator is unary */
		    in_stmt = false;
		/* don't use stmt continuation indentation */

		    parse (hd_type);
		/* let parser worry about if, or whatever */
		}

		search_brace = btype_2;
	    /* this should insure that constructs such as main(){... and
	       int[]{... have their braces put in the right place */
		break;

	    case unary_op:     /* this could be any unary operation */
		if (want_blank)
		    *e_code++ = ' ';

		if (in_decl) { /* if this is a unary op in a *//*
			          declaration, we should indent this token
			          */
		    for (i = 0; token[i]; ++i);
		/* find length of token */
		    while ((e_code - s_code) < (dec_ind - i))
			*e_code++ = ' ';
		/* pad it */
		}

		for (t_ptr = token; *t_ptr; ++t_ptr)
		    *e_code++ = *t_ptr;
	    /* move the token to buffer */
		want_blank = false;
		break;

	    case binary_op:    /* any binary operation */
	do_binary: 
		if (want_blank)
		    *e_code++ = ' ';
		for (t_ptr = token; *t_ptr; ++t_ptr)
		    *e_code++ = *t_ptr;
	    /* move the operator */
		want_blank = true;
		break;

	    case postop:       /* got a trailing ++ or -- */
		*e_code++ = token[0];
		*e_code++ = token[1];
		want_blank = true;
		break;

	    case question:     /* got a ? */
		squest++;      /* this will be used when a later colon
			          appears so we can distinguish the
			          <c>?<n>:<n> construct */
		if (want_blank)
		    *e_code++ = ' ';
		*e_code++ = '?';
		want_blank = true;
		break;

	    case casestmt:     /* got word 'case' or 'default' */
		scase = true;  /* so we can process the later colon
			          properly */
		if (want_blank)
		    *e_code++ = ' ';
		for (t_ptr = token; *t_ptr; ++t_ptr)
		    *e_code++ = *t_ptr;
		want_blank = true;
		break;

	    case colon:        /* got a ':' */
		if (squest > 0) {
		/* it is part of the <c>?<n>: <n> construct */
		    --squest;
		    if (want_blank)
			*e_code++ = ' ';
		    *e_code++ = ':';
		    want_blank = true;
		    break;
		}

		in_stmt = false;
	    /* seeing a label does not imply we are in a stmt */
		for (t_ptr = s_code; *t_ptr; ++t_ptr)
		    *e_lab++ = *t_ptr;
	    /* turn everything so far into a label */
		e_code = s_code;
		*e_lab++ = ':';
		*e_lab++ = ' ';
		*e_lab = '\0';

		force_nl = pcase = scase;
	    /* pcase will be used by dump_line to decide how to indent the
	       label. force_nl will force a case n: to be on a line by
	       itself */
		scase = false;
		want_blank = false;
		break;

	    case semicolon:    /* got a ';' */
		in_or_st = false;
	    /* we are not in an initialization or structure declaration */
		scase = false; /* these will only need resetting in a
			          error */
		squest = 0;

		if (in_decl && s_code == e_code)
			       /* align this in a declaration */
		    while ((e_code - s_code) < (dec_ind - 1))
			*e_code++ = ' ';

		in_decl = (dec_nest > 0);
	    /* if we were in a first level structure declaration, we
	       aren't any more */

		if ((!sp_sw || hd_type != forstmt) && p_l_follow > 0) {
		/* This should be true iff there were unbalanced parens in
		   the stmt.  It is a bit complicated, because the
		   semicolon might be in a for stmt */
		    printf ("%d: Unbalanced parens\n", line_no);
		    p_l_follow = 0;
		    if (sp_sw) {
		    /* this is a check for a if, while, etc. with
		       unbalanced parens */
			sp_sw = false;
			parse (hd_type);
		    /* don't lose the if, or whatever */
		    }
		}

		*e_code++ = ';';
		want_blank = true;
		in_stmt = (p_l_follow > 0);
	    /* we are no longer in the middle of a stmt */

		if (!sp_sw) {  /* if not if for (;;) */
		    parse (semicolon);
		/* let parser know about end of stmt */
		    force_nl = true;
		/* force newline after a end of stmt */
		}

		break;

	    case lbrace:       /* got a { */
		in_stmt = false;
	    /* don't indent the { */
		force_nl = true;
	    /* force other stuff on same line as { onto new line */

		if (s_code != e_code && !btype_2) {
		/* bracket is not alone on line */
		    if (verbose)
			printf ("%d: Line broken\n", line_no);
		    dump_line ();
		    want_blank = false;
		}

		if (p_l_follow > 0) {
		/* check for preceeding unbalanced parens */
		    printf ("%d: Unbalanced parens\n", line_no);
		    p_l_follow = 0;
		    if (sp_sw) {
		    /* check for unclosed if, for, etc. */
			sp_sw = false;
			parse (hd_type);
			ind_level = i_l_follow;
		    }
		}

		if (s_code == e_code)
		    ind_stmt = false;
	    /* don't put extra indentation on line with '{' */
		if (in_decl && in_or_st) {
		/* this is either a structure declaration or an init */
		    di_stack[dec_nest++] = dec_ind;
		    dec_ind = 0;
		}
		else
		    decl_on_line = false;
	    /* we can't be in the middle of a declaration, so don't do
	       special indentation of comments */

		parse (lbrace);/* let parser know about this */
		if (want_blank)/* put a blank before { if { is not at
			          start of line */
		    *e_code++ = ' ';
		want_blank = false;
		*e_code++ = '{';
		break;

	    case rbrace:       /* got a } */
		if (p_l_follow) {
		/* check for unclosed if, for, else. */
		    printf ("%d: Unbalanced parens\n", line_no);
		    p_l_follow = 0;
		    sp_sw = false;
		}

		if (s_code != e_code) {
		/* } must be first on line */
		    if (verbose)
			printf ("%d: Line broken\n", line_no);
		    dump_line ();
		}

		*e_code++ = '}';
		want_blank = true;
		in_stmt = ind_stmt = false;

		if (dec_nest > 0) {
		/* we are in multi-level structure declaration */
		    dec_ind = di_stack[--dec_nest];
		    in_decl = true;
		}

		parse (rbrace);/*   let parser know about this */
		break;

	    case swstmt:       /* got keyword "switch" */
		sp_sw = true;
		hd_type = swstmt;
	    /* keep this for when we have seen the expression */
		goto copy_id;  /* go move the token into buffer */

	    case sp_paren:     /* token is if, while, for */
		sp_sw = true;  /* the interesting stuff is done after the
			          expression is scanned */
		hd_type = (*token == 'i' ? ifstmt :
			(*token == 'w' ? whilestmt : forstmt));
	    /* remember the type of header for later use by parser */
		goto copy_id;  /* copy the token into line */

	    case sp_nparen:    /* got else, do */
		in_stmt = false;
		if (e_code != s_code) {
		/* make sure this starts a line */
		    if (verbose)
			printf ("%d: Line broken\n", line_no);
		    dump_line ();
		    want_blank = false;
		}

		force_nl = true;
	    /* also, following stuff must go onto new line */
		parse (*token == 'e' ? elselit : dolit);
	    /* pass token on to parser */
		goto copy_id;  /* move the token into line */

	    case decl: 	       /* we have a declaration type (int,
			          register, etc.) */
		parse (decl);  /* let parser worry about indentation */
		in_or_st = true;
	    /* this might be a structure or initialization declaration */
		in_decl = decl_on_line = true;
		for (i = 0; token[i++];);
	    /* get length of token */

		if (i <= 3)
		    i = 4;

		dec_ind = ((e_code - s_code + i) / ind_size + 1) * ind_size;
	    /* this will tell us how far to indent subsequent identifiers 
	    */
		goto copy_id;

	    case ident:        /* got an identifier or constant */
		if (in_decl) { /* if we are in a declaration, we must
			          indent identifier */
		    if (want_blank)
			*e_code++ = ' ';
		    want_blank = false;

		    while ((e_code - s_code) < dec_ind)
			*e_code++ = ' ';
		}
		else
		    if (sp_sw && p_l_follow == 0) {
		    /* check for if expr w/o parens *//* this will make
		       JRM's obsurd "for ever" statements work */
			sp_sw = false;
			force_nl = true;
			last_u_d = true;
			in_stmt = false;
			parse (hd_type);
		    }

	copy_id: 
		if (want_blank)
		    *e_code++ = ' ';
		for (t_ptr = token; *t_ptr; ++t_ptr)
		    *e_code++ = *t_ptr;
		want_blank = true;
		break;

	    case period:       /* treat a period kind of like a binary
			          operation */
		*e_code++ = '.';
	    /* move the period into line */
		want_blank = false;
	    /* don't put a blank after a period */
		break;

	    case comma: 
		want_blank = (s_code != e_code);
	    /* only put blank after comma if comma does not start the line
	       */
		if (in_decl)   /* align these in a declaration */
		    while ((e_code - s_code) < (dec_ind - 1))
			*e_code++ = ' ';

		*e_code++ = ',';

		if (break_comma && p_l_follow == 0 && !leave_comma)
		    force_nl = true;

		break;

	    case preesc:       /* got the character '#' */
		if (
			(s_com != e_com) ||
			(s_lab != e_lab) ||
			(s_code != e_code)) {
		/* true iff the '#' was not at start of the line */
		    printf ("%d: What is this # doing here?\n", line_no);
		    goto do_binary;
		/* treat it as a binary operator */
		}

		*e_lab++ = '#';/* move whole line to 'label' buffer */
		while (*buf_ptr != '\n') {
		    *e_lab = *buf_ptr++;
		    if (buf_ptr >= buf_end)
			fill_buffer ();

		    if (*e_lab++ == '/' && *buf_ptr == '*') {
		    /* check for comment on preprocessor line */
			e_lab - = 2;
		    /* skip back over slash */
			while (*e_lab == '\t' || *e_lab == ' ')
			    --e_lab;
		    /* strip off trailing blanks and tabs */
			*(++e_lab) = '\0';
		    /* null terminate the line */
			if (++buf_ptr >= buf_end)
			       /* space past start of comment */
			    fill_buffer ();
			col_1 = false;
		    /* don't let pr_comment think that this comment starts
		       in column 1 */
			decl_on_line = true;
		    /* treat this as a declaration for comment placement
		       purposes */
			goto proc_comment;
		    /* go process the comment */
		    }
		}

		*e_lab = '\0'; /* null terminate line */
		pcase = false;
		break;	       /* subsequent processing of the newline
			          character will cause the line to be
			          printed */

	    case comment:      /* we have gotten a /*  this is a biggie */
	proc_comment: 
		pr_comment ();
		break;
	}		       /* end of big switch stmt */

	*e_code = '\0';	       /* make sure code section is null
			          terminated */

    }			       /* end of main while (1) loop */
};

/*
 * copy input file to backup file
 * if in_name is /blah/blah/blah/file, then backup file
 * will be ".Bfile"
 * then make the backup file the input and original
 * input file the output
 */
bakcopy () {
    int     n,
            bakchn;
    char    buff[512];
    register char  *p;

 /* construct file name .Bfile */
    for (p = in_name; *p; p++);/* skip to end of string */
    while (p > in_name && *p != '/')/* find last '/' */
	p--;
    if (*p == '/')
	p++;
    sprintf (bakfile, ".B%s", p);

 /* copy in_name to backup file */
    bakchn = creat (bakfile, 0600);
    if (bakchn < 0) {
	printf ("can't create backup file \"%s\"\n", bakfile);
	exit ();
    }
    while (n = read (input, buff, 512))
	write (bakchn, buff, n);
    close (bakchn);
    close (input);

 /* re-open backup file as the input file */
    input = open (bakfile, 0);
    if (input < 0) {
	printf ("can't re-open backup file\n");
	exit ();
    }

 /* now the original input file will be the output */
    output = creat (in_name, 0644);
    if (output < 0) {
	printf ("can't create %s\n", in_name);
	unlink (bakfile);
	exit ();
    }
}


set_option (arg)
char   *arg;
{
    register    j;
    for (j = 0; options[j].str != 0; ++j) {
			       /* look thru list of possible options */
	if (eqin (options[j].str, arg)) {
	    set_var (j, arg);
	    break;	       /* get out of for loop */
	}
    }

    if (options[j].str == 0) { /* illegal arg given */
	printf ("Unknown parameter: %s\n", arg);
	exit ();
    }
}


set_var (j, arg)
char   *arg;
{
    switch (options[j].code) {
	case 1: 	       /* have -lnnn */
	    max_col = atoi (&arg[2]);
	    break;
	case 2: 	       /* have -cnnn */
	    com_ind = atoi (&arg[2]);
	    break;
	case 3: 	       /* have -innn */
	    ind_size = atoi (&arg[2]);
	    break;
	case 4: 	       /* have -cdnnn */
	    decl_com_ind = atoi (&arg[3]);
	    break;
	case 5: 	       /* have -v */
	    verbose = true;
	    break;
	case 6: 	       /* have -nv */
	    verbose = false;
	    break;
	case 7: 	       /* have -dj */
	    ljust_decl = true;
	    break;
	case 8: 	       /* have -ndj */
	    ljust_decl = false;
	    break;
	case 9: 	       /* -nbc */
	    leave_comma = true;
	    break;
	case 10: 	       /* -bc */
	    leave_comma = false;
	    break;
	case 13: 	       /* -dnnn */
	    unindent_displace = atoi (&arg[2]);
	    break;
	case 14: 	       /* -br */
	    btype_2 = true;
	    break;
	case 15: 	       /* -bl */
	    btype_2 = false;
	    break;
	case 16:
	    if(input<0) input = 0;
	    if(output<0) output = 1;
	    break;
    }
}


/*
 * GETPRO - get profile file
 * profile file is max 127 characters
 */
getpro (name, buf)
char   *name,		       /* profile file name, as in '.indent.pro' 
			       */
       *buf;		       /* will receive contents of .pro file */
{
    register    chn,
                n;
    char    file[32];

    file[0] = 0;
    strcat (file, getenv ("HOME"));
    strcat (file, "/");
    strcat (file, name);
    chn = open (file, 0);
    if (chn < 0)
	return (-1);
    n = read (chn, buf, 127);
    if (n < 0)
	return (-1);
    buf[n--] = 0;	       /* null terminate line */
    if (buf[n] == '\n')
	buf[n] = 0;
    close (chn);
    return (0);
}


/*
 * strip off arguments in a string:
 * p is address of a character pointer
 * nextchr returns pointer to front of first arg
 * arg is null terminated.
 * p is reset to after arg for subsequent calls
 */
char   *nxtarg (p)
char  **p;
{
    register char  *f,
                   *b;
    f = b = *p;
    while (*f && (*f == ' ' || *f == '\t'))
	f++;
    while (*b && (*b != ' ' && *b != '\t'))
	b++;
    if (*b != 0)
	*b++ = 0;
    *p = b;
    return (f);
}


set_profile () {
    char    line[128],
           *b;
    register char  *f;
    extern char *nxtarg ();

    if (getpro (".indent.pro", line) < 0)
	return;
    b = line;
    if(verbose) printf ("profile: %s\n", b);
    while (*(f = nxtarg (&b)))
	set_option (f);
}
