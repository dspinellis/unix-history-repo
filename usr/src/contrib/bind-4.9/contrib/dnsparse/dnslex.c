#ifndef lint
char rcsid[] = "$Id: dnslex.c,v 2.0 90/09/11 11:07:31 hakanson Rel $";
#endif /* lint */

/*
 * A lexical analyzer for DNS master files.
 *   Marion Hakanson (hakanson@cse.ogi.edu)
 *   Oregon Graduate Institute of Science and Technology
 *
 * Copyright (c) 1990, Marion Hakanson.
 *
 * You may distribute under the terms of the GNU General Public License
 * as specified in the README file that comes with the dnsparse kit.
 *
 * This program accepts as input DNS master files, as described
 * in RFC-1035.  It breaks up the input in such a way that a single
 * resource record (RR) is output on a single line, with a delimiter
 * character between each token (or field) of the RR.
 *
 * The output format was designed for consumption by programs such
 * as awk or perl, so the delimiter character can be used to quickly
 * split the RR into its components.
 *
 * It is likely that one could change the add_*char, end_word, and
 * end_line macros to put chars into a separate buffer or perform
 * some other function, and thus use do_dblex() inside a full parser
 * program (such as a DNS server).
 *
 * One other thing.  This program could probably have been written
 * in lex instead of C, but lex has a number of builtin limits to
 * the length of expressions (where RFC-1035 does not, in all cases).
 * I probably should have used flex instead, but the simple state
 * machine below was not that tough to implement, and it's also
 * pretty quick and pretty small.
 */

#include <stdio.h>

#define strchr index	/* system dependent */
#define strrchr index
extern char *strchr(), *strrchr();

extern getopt(), optind, opterr;
extern char *optarg;

#define FALSE 0
#define TRUE  1

/* Globals */
char *prog;


usage_and_die()
{
    fprintf(stderr, "usage: %s [-d <char>]\n", prog);
    exit(1);
}



/* Special (to DNS) chars we output in printed (non-decimal-escaped) form */
#define SPECS " \t\n;()\\.@"	/* for consumption by strchr() */
#define SPECP " \\t\\n;()\\.@"	/* for consumption by fprintf() */


main(argc, argv)
    int  argc;
    char **argv;
{
int opt;

/* Defaults */
register FILE	*ifile = stdin,
		*ofile = stdout;
char delim = ':';

if ( prog = strrchr(argv[0], '/') )
    prog++;
else
    prog = argv[0];

/* Parse the arguments */
opterr = 0;
while ( (opt = getopt(argc, argv, "d:")) != EOF )
    switch ((char)opt)
    {
	case '?':
	    usage_and_die();
	    break;
	case 'd':
	    delim = *optarg;
	    break;
	default:
	    /* Not supposed to happen. */
	    fprintf(stderr, "%s: Hey getopt(3)!  Wake up!\n", prog);
	    usage_and_die();
	    break;
    }
	    

/* This saves checking every char output against delim */
if ( strchr(SPECS, delim) != NULL ) {
    fprintf(stderr, "%s: delimiter '%c' cannot be one of '%s'.\n",
			prog, delim, SPECP);
    exit(1);
}

(void) do_dblex(ifile, ofile, delim);

exit(0);
}



int
do_dblex (ifile, ofile, delim)
    register FILE *ifile, *ofile;
    char delim;
{
register int c;
register int newword = FALSE;
register int wordlen = 0;
register int linelen = 0;
int inbrackets = FALSE;
int inquotes = FALSE;

/* no delim after last word on a line */
#define add_char(c) \
( \
    (newword ? (newword = FALSE, putc(delim,ofile)) : 0), \
    putc((c),ofile), \
    wordlen++ \
)

/* don't count the backslash, but do check for delim */
#define add_esc_char(c) ( add_char('\\'), putc((c),ofile) )
#define add_dec_char(c) ( add_char('\\'), fprintf(ofile,"%3.3d",(char)(c)) )

/* ignore empty words except at beginning of a line */
#define end_word() \
( \
    (wordlen > 0 || linelen == 0) ? ( \
	newword = TRUE, \
	wordlen = 0, \
	linelen++ \
    ) : (0) \
)

/* no delim at beginning of line; ignore empty lines */
#define end_line() \
( \
    (wordlen > 0) ? linelen++ : (0), \
    newword = FALSE, \
    wordlen = 0, \
    (linelen > 0) ? ( \
	putc('\n', ofile), \
	linelen = 0 \
    ) : (0) \
)



while ( ! feof(ifile) ) {

    c = getc(ifile);
    switch (c) {
	case EOF:
	    break;
	case ' ':
	case '\t':
	    if ( inquotes )
		add_esc_char(c);
	    else {
		end_word();
		while ( (c = getc(ifile)) != EOF && (c == ' ' || c == '\t') );
		ungetc(c, ifile);
	    }
	    break;
	case '\n':
	    if ( inquotes )
		; /* do nothing */
	    else if ( inbrackets )
		end_word();
	    else
		end_line();
	    break;
	case ';':
	    if ( inquotes )
		add_esc_char(c);
	    else {
		if ( ! inbrackets )
		    end_line();
		while ( (c = getc(ifile)) != EOF && c != '\n' ); /* skip */
	    }
	    break;
	case '"':
	    if ( inquotes )
		inquotes = FALSE;
	    else
		inquotes = TRUE;
	    break;
	case '(':
	    if ( inbrackets || inquotes )
		add_esc_char(c);
	    else {
		inbrackets = TRUE;
		end_word();
	    }
	    break;
	case ')':
	    if ( inbrackets && (! inquotes) ) {
		inbrackets = FALSE;
		end_word();
	    } else
		add_esc_char(c);
	    break;
	case '\\':
	    if ( (c = getc(ifile)) == EOF ) {
		add_esc_char('\\');
		end_line();
	    } else if ( c == '\n' ) {
		if ( ! inquotes )
		    end_word();
	    } else if ( c == delim )
		add_dec_char(c);	/* no delims inside fields */
	    else
		add_esc_char(c);
	    break;
	case '.':
	case '@':
	    if ( inquotes )
		add_esc_char(c);
	    else
		add_char(c);
	    break;
	default:
	    if ( c == delim )
		add_dec_char(c);	/* no delims inside fields */
	    else
		add_char(c);
    }
}

}

