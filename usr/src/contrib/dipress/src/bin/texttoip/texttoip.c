/*
 *  Interpress utility
 *
 *  Written for Xerox Corporation by William LeFebvre
 *
 *  6-June-1984
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * HISTORY
 * 13-Jan-86  lee at Xerox, WRC
 *	Changed on strcmpn to strncmp.
 *
 * 01-Dec-85  lee at Xerox, WRC
 *	Linted.
 *
 *
 * 8-apr-85  ed flint	add conditional compilation for vax11-c (vms)
 *				add error condition for sbrk failure
 *
 * 30-aug-85  ed flint	eliminate compiler warning on check of sbreak return
 */

/*
 *  texttoip - convert a textual representation of an interpress file (such as
 *  	       produced by iptotext) to an encoded interpress format.
 */

#ifdef vax11c
# include stdio
# include "iptokens.h"
# include "texttoken.h"
# include "literal.h"
#else
# include <stdio.h>
# include <sys/file.h>
# include <sys/types.h>
# include "iptokens.h"
# include "texttoken.h"
# include "literal.h"
#endif

long yylval_long;
char *yylval_charP;

int line_number;
int ipress_file;
int radix = 10;
char *filename;
char *outputname;
extern FILE *yyin;

main(argc, argv)

int argc;
char *argv[];

{
    int token;
    long longVal0;
    int arg;
    int pushed_token = 0;
    char first_file = 1;

    /* look for an output option */
    if (argc > 1 && strncmp(argv[1], "-o", 2) == 0)
    {
	/* it's there -- get the file name */
	if (strlen(argv[1]) > 2)
	{
	    outputname = &(argv[1][2]);
	    argc--;
	    argv++;
	}
	else if (argc > 2)
	{
	    outputname = argv[2];
	    argc -= 2;
	    argv += 2;
	}
	else
	{
	    /* "-o" with no file name -- strangeness, so give up */
	    exit(1);
	}
    }
    else
    {
	/* default output -- use stdout if it is NOT a terminal */
	if (isatty(1))
	{
	    /* use default filename */
	    outputname = "intertext.ip";
	}
	else
	{
	    /* use standard output */
	    outputname = NULL;
	    ipress_file = 1;
	}
    }

    /* open the output file */
#ifdef vax11c
    if (outputname != NULL &&
	(ipress_file = creat(outputname,0,"rfm=udf")) == -1)
#else
    if (outputname != NULL &&
	(ipress_file = open(outputname, O_WRONLY | O_CREAT | O_TRUNC, 0666)) == -1)
#endif
    {
	perror(outputname);
	exit(1);
    }

    /* main loop */
    for (arg = 1; arg < argc; arg++)
    {
	/* open next file for reading */
	if ((yyin = fopen(argv[arg], "r")) == NULL)
	{
	    perror(argv[arg]);
	    continue;
	}

	/* set file dependent stuff */
	filename = argv[arg];
	line_number = 1;

	/* get header */
	if (first_file)
	{
	    if ((token = yylex()) == T_header)
	    {
		if (yylex() != T_string)
		{
		    terror("Header was not a string, using standard header");
		    ip_select(ipress_file);
		}
		else
		{
		    ip_raw_select(ipress_file);
		    append_bytes(strlen(yylval_charP),
				(unsigned char *) yylval_charP);
		}
	    }
	    else
	    {
		terror("No header for file, using standard header");
		ip_select(ipress_file);
		pushed_token = token;
	    }
	    first_file = 0;

	    /* gobble the newline */
	    if (yylex() != T_newline)
	    {
		terror("newline expected");
	    }
	}
	else
	{
	    if ((token = yylex()) == T_header)
	    {
		/* gobble stuff */
		(void) yylex();  (void) yylex();
	    }
	    else
	    {
		pushed_token = token;
	    }
	}

	while (pushed_token == 0 ?
		(token = yylex()) != 0 :
		(token = pushed_token, pushed_token = 0, 1))
	{
	    switch(token)
	    {
		case T_header:
		    terror("Stray header");
		    break;
    
		case T_seq_comment:
		    if (yylex() != T_string)
		    {
			terror("Comment was not a string");
		    }
		    else
		    {
			AppendComment(yylval_charP);
		    }
		    break;
    
		case T_seq_identifier:
		    if (yylex() != T_identifier)
		    {
			terror("Identifier is invalid");
		    }
		    else
		    {
			AppendIdentifier(yylval_charP);
		    }
		    break;
    
		case T_seq_insert_file:
		    if (yylex() != T_string)
		    {
			terror("Insert file is not a string");
		    }
		    else
		    {
			AppendInsertFile(yylval_charP);
		    }
		    break;
    
		case T_seq_integer:
		    if (yylex() != T_number)
		    {
			terror("Integer is invalid");
		    }
		    else
		    {
			append_integer_sequence(yylval_long);
		    }
		    break;
    
		case T_seq_rational:
		    if (yylex() != T_number)
		    {
			terror("Rational numerator is invalid");
			break;
		    }
		    longVal0 = yylval_long;
		    if (yylex() != T_character || (char) yylval_long != '/')
		    {
			terror("Rational separator invalid");
			break;
		    }
		    if (yylex() != T_number)
		    {
			terror("Rational denominator is invalid");
			break;
		    }

		    AppendRational(longVal0, (long) yylval_long);
		    break;
    
		case T_seq_string:
		    if (yylex() != T_string)
		    {
			terror("String is invalid");
		    }
		    else
		    {
			AppendString1(yylval_charP);
		    }
		    break;

		/* Pixel vector sequences */
		case T_seq_apv:
		    get_pixel_vector(sequenceAdaptivePixelVector);
		    break;

		case T_seq_cpv:
		    get_pixel_vector(sequenceCompressedPixelVector);
		    break;

		case T_seq_ppv:
		    get_pixel_vector(sequencePackedPixelVector);
		    break;

		case T_number:
		    AppendInteger(yylval_long);
		    break;
    
		case T_operator:
		    AppendOp((int) yylval_long);
		    break;
    
		case T_newline:
		    line_number++;
		    break;
    
		case T_string:
		    terror("Stray string");
		    break;
    
		case T_identifier:
		    terror("Stray string");
		    break;
    
		case T_character:
		    terror("Stray character");
		    break;
	    }
	}
    }
    ip_close();
}

char *sbrk();

get_pixel_vector(type)

int type;

{
    register int len = 0;
    int token;
    unsigned char *buffer = NULL;
    register unsigned char *ptr;

    if (yylex() != T_character || yylval_long != '[')
    {
	terror("Bad pixel vector, expected '['");
	return;
    }
    radix = 16;

    /* allocate space to hold the data */
    if (buffer == NULL)
    {
	
	buffer = (unsigned char *) sbrk(300000); /*  too large for PDP-11's */

	if ( buffer == (unsigned char *)-1 )
	{
	    fprintf(stderr,"\ncannot allocate room for pixel array\n");
	    exit(1);
	}
    }

    ptr = buffer;
    while ((token = yylex()) != T_character)
    {
	if (token != T_newline)
	{
	    *ptr++ = (unsigned char) (yylval_long >> 8);
	    *ptr++ = (unsigned char) yylval_long;
	    len += 2;
	}
    }
    (void) yylex();	/* gobble trailing newline */

    append_Sequence(type, len, buffer);
    radix = 10;
}

terror(string)

char *string;

{
    fprintf(stderr, "\"%s\", line %d: %s\n", filename, line_number, string);
}
#ifdef vax11c
/*
 *  this is needed for the parsing software produced by lex
 */

yywrap()
{
    return(1);
}
#endif
