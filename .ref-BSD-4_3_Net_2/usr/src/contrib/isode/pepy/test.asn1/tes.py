Fred DEFINITIONS IMPLICIT TAGS ::=
%{
int	br = 10;
int	frd = 9;
%}
BEGIN

SECTIONS build parse print

Barney ::= SEQUENCE {
	COMPONENTS OF Fred,
	wilma INTEGER [[i frd]],
	fred INTEGER [[i frd]],
	pets SEQUENCE SIZE (1..MAX) OF INTEGER }

Fred ::= SEQUENCE {
	barney-rubble INTEGER [[i br]],
	betty-rubble INTEGER [[i br]],
	the-dog REAL }

END
%{
#include <stdio.h>

char	*myname;

main (argc, argv)
int	argc;
char	**argv;
{
	PE	pe;
	PE	pe2;

	myname = argv[0];

	build_Fred_Barney(&pe, 1, 0, NULLCP, NullParm);
	build_Fred_Fred(&pe2, 1, 0, NULLCP, NullParm);
	parse_Fred_Barney(pe, 1, NULLIP, NULLVP, NullParm);
	parse_Fred_Fred(pe2, 1, NULLIP, NULLVP, NullParm);
	print_Fred_Barney(pe, 1, NULLIP, NULLVP, NullParm);
	print_Fred_Fred(pe2, 1, NULLIP, NULLVP, NullParm);
	exit (0);
}


/* VARARGS2 */

adios (what, fmt, a, b, c, d, e, f, g, h, i, j)
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

void advise (what, fmt, a, b, c, d, e, f, g, h, i, j)
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
