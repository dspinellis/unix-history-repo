    A DEFINITIONS ::=
%{

#define	DEBUG

#include <stdio.h>

/*    DATA */

#define	ps_advise(ps, f) \
	advise (NULLCP, "%s: %s", (f), ps_error ((ps) -> ps_errno))


static char *myname = "integertest";

static enum { ps2test, pl2test } mode = ps2test;


void	adios ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    register int    status = 0;
    register char  *cp;
    register    FILE * fp;

    myname = *argv;
    for (argc--, argv++; cp = *argv; argc--, argv++)
	if (*cp == '-') {
	    if (strcmp (cp + 1, "ps") == 0) {
		mode = ps2test;
		continue;
	    }
	    if (strcmp (cp + 1, "pl") == 0) {
		mode = pl2test;
		continue;
	    }
	    adios (NULLCP, "usage: %s [ -ps | -pl ] [ files... ]",
		    myname);
	}
	else
	    break;

    if (argc == 0)
	status = process ("(stdin)", stdin);
    else
	while (cp = *argv++) {
	    if ((fp = fopen (cp, "r")) == NULL) {
		advise (cp, "unable to read");
		status++;
		continue;
	    }
	    status += process (cp, fp);
	    (void) fclose (fp);
	}

    exit (status);
}

/*  */

static int  process (file, fp)
register char *file;
register FILE *fp;
{
    register PE	    pe;
    register PS	    ps;

    if ((ps = ps_alloc (std_open)) == NULLPS) {
	ps_advise (ps, "ps_alloc");
	return 1;
    }
    if (std_setup (ps, fp) == NOTOK) {
	advise (NULLCP, "%s: std_setup loses", file);
	return 1;
    }

    for (;;) {
	switch (mode) {
	    case ps2test: 
		if ((pe = ps2pe (ps)) == NULLPE)
		    if (ps -> ps_errno) {
			ps_advise (ps, "ps2pe");
		you_lose: ;
			ps_free (ps);
			return 1;
		    }
		    else {
		done: 	;
			ps_free (ps);
			return 0;
		    }
		break;

	    case pl2test: 
		if ((pe = pl2pe (ps)) == NULLPE)
		    if (ps -> ps_errno) {
			ps_advise (ps, "pl2pe");
			goto you_lose;
		    }
		    else
			goto done;
		break;
	}

	(void) do_A_A (pe, 1);

	pe_free (pe);
    }
}

/*  */

%}

    BEGIN

    A ::= CHOICE { B, C }

    B ::= INTEGER
    	%{ fprintf(stderr, "INTEGER = %d (0x%x)\n", $$, $$); %}

    C ::= NumericString
	  %{
	      int   len;
	      char *s = prim2str ($$, &len);
	      fprintf (stderr, "NumericString = %s (len %d)\n", s, len);
	      free (s);
	  %}

    END



%{
/*    DEBUG */

#ifdef	DEBUG
char   *getenv ();

testdebug (pe, s)
register PE pe;
register char *s;
{
    static int  debug = OK;
    char   *cp;
    register PS	    ps;

    switch (debug) {
	case NOTOK: 
	    break;

	case OK: 
	    if ((debug = (cp = getenv ("PEPYDEBUG")) && *cp ? atoi (cp)
			: NOTOK) == NOTOK)
		break;
	    fprintf (stderr, "%s made with %s\n", myname, pepyid);

	default: 
	    fprintf (stderr, "%s\n", s);

	    if ((ps = ps_alloc (std_open)) == NULLPS)
		return;
	    if (std_setup (ps, stderr) != NOTOK)
		(void) pe2pl (ps, pe);
	    fprintf (stderr, "--------\n");
	    ps_free (ps);
	    break;
    }
}
#endif

/*    ERRORS */

/* VARARGS2 */

void	adios (what, fmt, a, b, c, d, e, f, g, h, i, j)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f,
       *g,
       *h,
       *i,
       *j;
{
    advise (what, fmt, a, b, c, d, e, f, g, h, i, j);
    _exit (1);
}

/*  */

/* VARARGS2 */

void	advise (what, fmt, a, b, c, d, e, f, g, h, i, j)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f,
       *g,
       *h,
       *i,
       *j;
{
    (void) fflush (stdout);

    fprintf (stderr, "%s: ", myname);
    fprintf (stderr, fmt, a, b, c, d, e, f, g, h, i, j);
    if (what)
	(void) fputc (' ', stderr), perror (what);
    else
	(void) fputc ('\n', stderr);
    (void) fflush (stderr);
}

%}
