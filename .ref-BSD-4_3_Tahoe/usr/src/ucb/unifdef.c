#ifndef lint
static char sccsid[] = "@(#)unifdef.c	4.4 (Berkeley) 8/11/83";
#endif

/*
 * unifdef - remove ifdef'ed lines
 */

#include <stdio.h>
#include <ctype.h>
#define BSS
FILE *input;
#ifndef YES
#define YES 1
#define NO  0
#endif

char *progname BSS;
char *filename BSS;
char text BSS;          /* -t option in effect: this is a text file */
char lnblank BSS;       /* -l option in effect: blank deleted lines */
char complement BSS;    /* -c option in effect: complement the operation */
#define MAXSYMS 100
char true[MAXSYMS] BSS;
char ignore[MAXSYMS] BSS;
char *sym[MAXSYMS] BSS;
char insym[MAXSYMS] BSS;
char nsyms BSS;
char incomment BSS;
#define QUOTE1 0
#define QUOTE2 1
char inquote[2] BSS;
int exitstat BSS;
char *skipcomment ();
char *skipquote ();

main (argc, argv)
int argc;
char **argv;
{
    char **curarg;
    register char *cp;
    register char *cp1;
    char ignorethis;

    progname = argv[0][0] ? argv[0] : "unifdef";

    for (curarg = &argv[1]; --argc > 0; curarg++) {
	if (*(cp1 = cp = *curarg) != '-')
	    break;
	if (*++cp1 == 'i') {
	    ignorethis = YES;
	    cp1++;
	}
	else
	    ignorethis = NO;
	if (   (   *cp1 == 'D'
		|| *cp1 == 'U'
	       )
	    && cp1[1] != '\0'
	   ) {
	    if (nsyms >= MAXSYMS) {
		prname ();
		fprintf (stderr, "too many symbols.\n");
		exit (2);
	    }
	    ignore[nsyms] = ignorethis;
	    true[nsyms] = *cp1 == 'D' ? YES : NO;
	    sym[nsyms++] = &cp1[1];
	}
	else if (ignorethis)
	    goto unrec;
	else if (strcmp (&cp[1], "t") == 0)
	    text = YES;
	else if (strcmp (&cp[1], "l") == 0)
	    lnblank = YES;
	else if (strcmp (&cp[1], "c") == 0)
	    complement = YES;
	else {
 unrec:
	    prname ();
	    fprintf (stderr, "unrecognized option: %s\n", cp);
	    goto usage;
	}
    }
    if (nsyms == 0) {
 usage:
	fprintf (stderr, "\
Usage: %s [-l] [-t] [-c] [[-Dsym] [-Usym] [-idsym] [-iusym]]... [file]\n\
    At least one arg from [-D -U -id -iu] is required\n", progname);
	exit (2);
    }

    if (argc > 1) {
	prname ();
	fprintf (stderr, "can only do one file.\n");
    }
    else if (argc == 1) {
	filename = *curarg;
	if ((input = fopen (filename, "r")) != NULL) {
	    pfile();
	    fclose (input);
	}
	else {
	    prname ();
	    perror(*curarg);
	}
    }
    else {
	filename = "[stdin]";
	input = stdin;
	pfile();
    }

    fflush (stdout);
    exit (exitstat);
}

/* types of input lines: */
#define PLAIN       0   /* ordinary line */
#define TRUE        1   /* a true  #ifdef of a symbol known to us */
#define FALSE       2   /* a false #ifdef of a symbol known to us */
#define OTHER       3   /* an #ifdef of a symbol not known to us */
#define ELSE        4   /* #else */
#define ENDIF       5   /* #endif */
#define LEOF        6   /* end of file */

char reject BSS;    /* 0 or 1: pass thru; 1 or 2: ignore comments */
int linenum BSS;    /* current line number */
int stqcline BSS;   /* start of current coment or quote */
char *errs[] = {
#define NO_ERR      0
			"",
#define END_ERR     1
			"",
#define ELSE_ERR    2
			"Inappropriate else",
#define ENDIF_ERR   3
			"Inappropriate endif",
#define IEOF_ERR    4
			"Premature EOF in ifdef",
#define CEOF_ERR    5
			"Premature EOF in comment",
#define Q1EOF_ERR   6
			"Premature EOF in quoted character",
#define Q2EOF_ERR   7
			"Premature EOF in quoted string"
};

pfile ()
{
    reject = 0;
    doif (-1, NO, reject, 0);
    return;
}

doif (thissym, inif, prevreject, depth)
register int thissym;   /* index of the symbol who was last ifdef'ed */
int inif;               /* YES or NO we are inside an ifdef */
int prevreject;         /* previous value of reject */
int depth;              /* depth of ifdef's */
{
    register int lineval;
    register int thisreject;
    int doret;          /* tmp return valud]e of doif */
    int cursym;         /* index of the symbol returned by checkline */
    int stline;         /* line number when called this time */

    stline = linenum;
    for (;;) {
	switch (lineval = checkline (&cursym)) {
	case PLAIN:
	    flushline (YES);
	    break;

	case TRUE:
	case FALSE:
	    thisreject = reject;
	    if (lineval == TRUE)
		insym[cursym] = 1;
	    else {
		if (reject < 2)
		    reject = ignore[cursym] ? 1 : 2;
		insym[cursym] = -1;
	    }
	    if (ignore[cursym])
		flushline (YES);
	    else {
		exitstat = 1;
		flushline (NO);
	    }
	    if ((doret = doif (cursym, YES, thisreject, depth + 1)) != NO_ERR)
		return error (doret, stline, depth);
    	    break;

	case OTHER:
	    flushline (YES);
	    if ((doret = doif (-1, YES, reject, depth + 1)) != NO_ERR)
		return error (doret, stline, depth);
	    break;

	case ELSE:
	    if (inif != 1)
		return error (ELSE_ERR, linenum, depth);
	    inif = 2;
	    if (thissym >= 0) {
		if ((insym[thissym] = -insym[thissym]) < 0)
		    reject = ignore[thissym] ? 1 : 2;
		else
		    reject = prevreject;
		if (!ignore[thissym]) {
		    flushline (NO);
		    break;
		}
	    }
	    flushline (YES);
	    break;

	case ENDIF:
	    if (inif == 0)
		return error (ENDIF_ERR, linenum, depth);
	    if (thissym >= 0) {
		insym[thissym] = 0;
		reject = prevreject;
		if (!ignore[thissym]) {
		    flushline (NO);
		    return NO_ERR;
		}
	    }
	    flushline (YES);
	    return NO_ERR;

	case LEOF: {
	    int err;
	    err =   incomment
		  ? CEOF_ERR
		  : inquote[QUOTE1]
		  ? Q1EOF_ERR
		  : inquote[QUOTE2]
		  ? Q2EOF_ERR
		  : NO_ERR;
	    if (inif) {
		if (err != NO_ERR)
		    error (err, stqcline, depth);
		return error (IEOF_ERR, stline, depth);
	    }
	    else if (err != NO_ERR)
		return error (err, stqcline, depth);
	    else
		return NO_ERR;
	    }
	}
    }
}

#define endsym(c) (!isalpha (c) && !isdigit (c) && c != '_')

#define MAXLINE 256
char tline[MAXLINE] BSS;

checkline (cursym)
int *cursym;
{
    register char *cp;
    register char *symp;
    register char chr;
    char *scp;
    int retval;
    int symind;
#   define KWSIZE 8
    char keyword[KWSIZE];

    linenum++;
    if (getlin (tline, sizeof tline, input, NO) == EOF)
        return LEOF;

    retval = PLAIN;
    if (   *(cp = tline) != '#'
	|| incomment
	|| inquote[QUOTE1]
	|| inquote[QUOTE2]
       )
	goto eol;

    cp = skipcomment (++cp);
    symp = keyword;
    while (!endsym (*cp)) {
	*symp = *cp++;
	if (++symp >= &keyword[KWSIZE])
	    goto eol;
    }
    *symp = '\0';

    if (strcmp (keyword, "ifdef") == 0) {
	retval = YES;
	goto ifdef;
    }
    else if (strcmp (keyword, "ifndef") == 0) {
	retval = NO;
 ifdef:
	scp = cp = skipcomment (++cp);
	if (incomment) {
	    retval = PLAIN;
	    goto eol;
	}
	for (symind = 0; ; ) {
	    if (insym[symind] == 0) {
		for ( symp = sym[symind], cp = scp
		    ; *symp && *cp == *symp
		    ; cp++, symp++
		    )
		    {}
		chr = *cp;
		if (*symp == '\0' && endsym (chr)) {
		    *cursym = symind;
		    retval = (retval ^ true[symind]) ? FALSE : TRUE;
		    break;
		}
	    }
	    if (++symind >= nsyms) {
		retval = OTHER;
		break;
	    }
	}
    }
    else if (strcmp (keyword, "if") == 0)
	retval = OTHER;
    else if (strcmp (keyword, "else") == 0)
	retval = ELSE;
    else if (strcmp (keyword, "endif") == 0)
	retval = ENDIF;

 eol:
    if (!text && !reject)
	for (; *cp; ) {
	    if (incomment)
		cp = skipcomment (cp);
	    else if (inquote[QUOTE1])
		cp = skipquote (cp, QUOTE1);
	    else if (inquote[QUOTE2])
		cp = skipquote (cp, QUOTE2);
	    else if (*cp == '/' && cp[1] == '*')
		cp = skipcomment (cp);
	    else if (*cp == '\'')
		cp = skipquote (cp, QUOTE1);
	    else if (*cp == '"')
		cp = skipquote (cp, QUOTE2);
	    else
		cp++;
	}
    return retval;
}

/*  Skip over comments and stop at the next charaacter
/*  position that is not whitespace.
/**/
char *
skipcomment (cp)
register char *cp;
{
    if (incomment)
	goto inside;
    for (;; cp++) {
        while (*cp == ' ' || *cp == '\t')
            cp++;
	if (text)
            return cp;
	if (   cp[0] != '/'
	    || cp[1] != '*'
	   )
            return cp;
	cp += 2;
	if (!incomment) {
	    incomment = YES;
	    stqcline = linenum;
	}
 inside:
	for (;;) {
	    for (; *cp != '*'; cp++)
		if (*cp == '\0')
		    return cp;
	    if (*++cp == '/')
		break;
	}
	incomment = NO;
    }
}

/*  Skip over a quoted string or character and stop at the next charaacter
/*  position that is not whitespace.
/**/
char *
skipquote (cp, type)
register char *cp;
register int type;
{
    register char qchar;

    qchar = type == QUOTE1 ? '\'' : '"';

    if (inquote[type])
	goto inside;
    for (;; cp++) {
	if (*cp != qchar)
	    return cp;
	cp++;
	if (!inquote[type]) {
	    inquote[type] = YES;
	    stqcline = linenum;
	}
 inside:
	for (; ; cp++) {
	    if (*cp == qchar)
		break;
	    if (   *cp == '\0'
		|| *cp == '\\'
		&& *++cp == '\0'
	       )
		return cp;
	}
	inquote[type] = NO;
    }
}

/*
/*   special getlin - treats form-feed as an end-of-line
/*                    and expands tabs if asked for
/*
/**/
getlin (line, maxline, inp, expandtabs)
register char *line;
int maxline;
FILE *inp;
int expandtabs;
{
    int tmp;
    register int num;
    register int chr;
#ifdef FFSPECIAL
    static char havechar = NO;  /* have leftover char from last time */
    static char svchar BSS;
#endif

    num = 0;
#ifdef FFSPECIAL
    if (havechar) {
	havechar = NO;
	chr = svchar;
	goto ent;
    }
#endif
    while (num + 8 < maxline) {   /* leave room for tab */
        chr = getc (inp);
	if (isprint (chr)) {
#ifdef FFSPECIAL
 ent:
#endif
	    *line++ = chr;
	    num++;
	}
	else
	    switch (chr) {
	    case EOF:
		return EOF;

	    case '\t':
		if (expandtabs) {
		    num += tmp = 8 - (num & 7);
		    do
			*line++ = ' ';
		    while (--tmp);
		    break;
		} 
            default:
                *line++ = chr;
                num++;
		break;

	    case '\n':
                *line = '\n';
                num++;
                goto end;
    
#ifdef FFSPECIAL
	    case '\f':
		if (++num == 1)
		    *line = '\f';
		else {
		    *line = '\n';
		    havechar = YES;
                    svchar = chr;
                }
                goto end;
#endif
	    }
    }
 end:
    *++line = '\0';
    return num;
}

flushline (keep)
{
    if ((keep && reject < 2) ^ complement)
	putlin (tline, stdout);
    else if (lnblank)
	putlin ("\n", stdout);
    return;
}

/*
/*  putlin - for tools
/*
/**/
putlin (line, fio)
register char *line;
register FILE *fio;
{
    register char chr;

    while (chr = *line++)
	putc (chr, fio);
    return;
}

prname ()
{
    fprintf (stderr, "%s: ", progname);
    return;
}


error (err, line, depth)
{
    if (err == END_ERR)
	return err;

    prname ();

#ifndef TESTING
    fprintf (stderr, "Error in %s line %d: %s.\n",
	     filename, line, errs[err]);
#endif

#ifdef TESTING
    fprintf (stderr, "Error in %s line %d: %s. ",
	     filename, line, errs[err]);
    fprintf (stderr, "ifdef depth: %d\n", depth);
#endif

    exitstat = 2;
    return depth > 1 ? IEOF_ERR : END_ERR;
}
