/* psaptest.c - test out -lpsap */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/psaptest.c,v 7.2 91/02/22 09:36:40 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/psaptest.c,v 7.2 91/02/22 09:36:40 mrose Interim $
 *
 *
 * $Log:	psaptest.c,v $
 * Revision 7.2  91/02/22  09:36:40  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/08/29  15:06:21  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:13:26  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <stdio.h>
#include "psap.h"
#include "tailor.h"
#include <sys/stat.h>

/*  */

static enum {
    ps2pl, ps2ps, pl2pl, pl2ps
} mode;

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    char   *cp;
    register PE pe;
    register PS ps;

    argc--, argv++;
    if (argc != 1) {
usage: 	;
	fprintf (stderr, "usage: psaptest [ps2pl | ps2ps | pl2pl | pl2ps]\n");
	exit (1);
    }

    if (strcmp (*argv, "version") == 0) {
	printf ("%s\n", psapversion);
	exit (0);
    }

    if ((cp = getenv ("PSAPTEST")) && *cp) {
	psap_log -> ll_events = atoi (cp);
	psap_log -> ll_stat |= LLOGTTY;
    }

    if (strcmp (*argv, "string") == 0) {
	int	onceonly = 1;
	register PE *pep;

	if ((pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, PE_PRIM_OCTS))
	        == NULLPE) {
no_pe: ;
	    fprintf (stderr, "pe_alloc: you lose\n");
	    exit (1);
	}
	pep = &pe -> pe_cons;
	for (;;) {
	    int	    i;
	    register PE p;

	    if (onceonly) {
		struct stat st;

		if (fstat (fileno (stdin), &st) == NOTOK
		        || (st.st_mode & S_IFMT) != S_IFREG
		        || (i = st.st_size) <= 0)
		    i = BUFSIZ;

		onceonly = 0;
	    }
	    else
		i = BUFSIZ;
	    
	    if ((p = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_OCTS))
			== NULL
		    || (p -> pe_prim = PEDalloc (p -> pe_len = i)) == NULLPED)
		goto no_pe;

	    switch (i = fread ((char *) p -> pe_prim, sizeof *p -> pe_prim,
			       (int) p -> pe_len, stdin)) {
		case NOTOK:
		    perror ("fread");
		    exit (1);

	        case OK:
		    pe_free (p);
		    mode = ps2ps;
		    goto doit;

		default:
		    p -> pe_len = i;
		    *pep = p, pep = &p -> pe_next;
		    break;
	    }
	}
    }

    if (strcmp (*argv, "binary") == 0) {
	int     i;
	char    buffer[BUFSIZ],
	        packet[BUFSIZ];

	while (fgets (buffer, sizeof buffer, stdin)) {
	    if (*buffer == ' ')
		(void) strncpy (packet, buffer + 1, i = strlen (buffer) - 2);
	    else
		i = implode ((u_char *) packet, buffer, strlen (buffer) - 1);
	    (void) fwrite (packet, sizeof *packet, i, stdout);
	}

	exit (0);
    }

    if (strcmp (*argv, "ps2pl") == 0)
	mode = ps2pl;
    else
	if (strcmp (*argv, "ps2ps") == 0)
	    mode = ps2ps;
	else
	    if (strcmp (*argv, "pl2pl") == 0)
		mode = pl2pl;
	    else
		if (strcmp (*argv, "pl2ps") == 0)
		    mode = pl2ps;
		else
		    goto usage;

    for (;;) {
	if ((ps = ps_alloc (std_open)) == NULLPS) {
	    fprintf (stderr, "ps_alloc(stdin): you lose\n");
	    exit (1);
	}
	if (std_setup (ps, stdin) == NOTOK)
	    ps_die (ps, "std_setup(stdin)");

	switch (mode) {
	    case ps2pl: 
	    case ps2ps: 
		if ((pe = ps2pe (ps)) == NULLPE)
		    if (ps -> ps_errno)
			ps_die (ps, "ps2pe");
		    else
			exit (0);
		break;

	    case pl2ps: 
	    case pl2pl: 
		if ((pe = pl2pe (ps)) == NULLPE)
		    if (ps -> ps_errno)
			ps_die (ps, "pl2pe");
		    else
			exit (0);
		break;
	}

	ps_free (ps);

doit: ;
	if ((ps = ps_alloc (std_open)) == NULLPS) {
	    fprintf (stderr, "ps_alloc(stdout): you lose\n");
	    exit (1);
	}
	if (std_setup (ps, stdout) == NOTOK)
	    ps_die (ps, "std_setup(stdout)");

	switch (mode) {
	    case ps2ps: 
	    case pl2ps: 
		if (pe2ps (ps, pe) == NOTOK)
		    ps_die (ps, "pe2ps");
		break;

	    case pl2pl: 
	    case ps2pl: 
		if (pe2pl (ps, pe) == NOTOK)
		    ps_die (ps, "pe2pl");
		break;
	}

	pe_free (pe);
	ps_free (ps);
    }
}

/*    ERRORS */

static ps_die (ps, s)
register PS	 ps;
register char   *s;
{
    fprintf (stderr, "%s: %s\n", s, ps_error (ps -> ps_errno));
    exit (1);
}
