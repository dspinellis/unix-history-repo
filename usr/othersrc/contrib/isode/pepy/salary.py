SalaryDefs DEFINITIONS ::=

%{
#include <stdio.h>

#define PEPYPARM struct salary_record *


static char *myname = "salary";

static struct salary_record {
    char   *name;
    int     salary;
}                           salary;


void	adios ();

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int     argc;
char  **argv,
      **envp;
{
    PE	    pe;

    myname = argv[0];

    if (argc != 3)
	adios (NULLCP, "usage: %s name salary", myname);
    salary.name = argv[1];
    salary.salary = atoi (argv[2]);

    if (build_SalaryDefs_Salary (&pe, 1, NULL, NULLCP, &salary) == NOTOK)
	adios (NULLCP, "encoder fails");

    salary.name = NULL;
    salary.salary = 0;

    if (unbuild_SalaryDefs_Salary (pe, 1, NULLIP, NULLVP, &salary) == NOTOK)
	adios (NULLCP, "decoder fails");

    exit (0);			/* NOTREACHED */
}
%}

BEGIN

SECTIONS build unbuild none

Salary ::=
	SEQUENCE {
	    name
		PrintableString [[s parm -> name]]
		%{ printf("name %s ", parm -> name); %},

	    salary
		TheSalary [[i parm -> salary ]]
		%{ printf("salary %d\n", parm -> salary); %}
	}

TheSalary ::=
	INTEGER

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
