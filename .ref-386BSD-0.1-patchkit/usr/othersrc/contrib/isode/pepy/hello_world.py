HelloWorldDefs DEFINITIONS ::=

%{
#include <stdio.h>


static char *text;
static char *myname = "hello_world";


void adios ();

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int     argc;
char  **argv,
      **envp;
{
    PE	    pe;

    myname = argv[0];

    if (build_HelloWorldDefs_MyStruct (&pe, 1, NULL, NULLCP, NULLCP) == NOTOK)
	adios (NULLCP, "encoder fails");
    if (unbuild_HelloWorldDefs_MyStruct (pe, 1, NULLIP, NULLVP, NULLCP)
	    == NOTOK)
	adios (NULLCP, "decoder fails");

    exit (0);			/* NOTREACHED */
}
%}

BEGIN

ENCODER	build

MyStruct ::=
	PrintableString [[s "Hello World" ]]


DECODER	unbuild

MyStruct ::=
	PrintableString [[s text]]
	%{ printf("%s\n", text); %}

END

%{

/*    ERRORS */

#include <varargs.h>


#ifndef	lint
void	_advise ();


static void  adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

static void  adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
static void  advise (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);
}


static void  _advise (ap)
va_list	ap;
{
    char    buffer[BUFSIZ];

    asprintf (buffer, ap);

    (void) fflush (stdout);

    fprintf (stderr, "%s: ", myname);
    (void) fputs (buffer, stderr);
    (void) fputc ('\n', stderr);

    (void) fflush (stderr);
}
#else
/* VARARGS */

static void  advise (what, fmt)
char   *what,
       *fmt;
{
    advise (what, fmt);
}
#endif

%}
