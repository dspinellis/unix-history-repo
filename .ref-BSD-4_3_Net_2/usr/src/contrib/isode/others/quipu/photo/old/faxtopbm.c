/* faxtopbm.c - FAX to pbm filter */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/faxtopbm.c,v 7.0 89/11/23 22:01:41 mrose Rel $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/faxtopbm.c,v 7.0 89/11/23 22:01:41 mrose Rel $
 *
 *
 * $Log:	faxtopbm.c,v $
 * Revision 7.0  89/11/23  22:01:41  mrose
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
#include "pbm/pbm.h"

/*  */

static	int	passno;

static	bit	black, white;

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    char   *cp,
	   *file;
    FILE   *fp;
    PE	    pe;
    PS	    ps;

    black = PBM_BLACK, white = PBM_WHITE;

    file = NULLCP, fp = stdin;
    for (argv++; cp = *argv; argv++)
	if (*cp == '-') {
	    if (cp[1] == NULL)
		goto usage;

	    if (strncmp (cp, "-reversebits", strlen (cp)) == 0) {
		black = PBM_WHITE, white = PBM_BLACK;
		continue;
	    }
	    goto usage;
	}
	else
	    if (file) {
usage: ;
		fprintf (stderr, "usage: faxtopbm [file]\n"), exit (1);
	    }
	    else
		if ((fp = fopen (file = cp, "r")) == NULL)
		    perror (file), exit (1);

    if ((ps = ps_alloc (std_open)) == NULLPS)
	fprintf (stderr, "ps_alloc: you lose\n"), exit (1);
    if (std_setup (ps, fp) == NOTOK)
	ps_die (ps, "std_setup");
    if ((pe = ps2pe (ps)) == NULLPE)
	ps_die (ps, "ps2pe");
    if (pe_pullup (pe) == NOTOK)
	pe_die (pe, "pe_pullup");
    if (prim2bit (pe) == NULLPE)
	pe_die (pe, "prim2bit");

    for (passno = 1; passno < 3; passno++)
	if (decode_t4 (pe -> pe_prim, file, ps_get_abs (pe)) == NOTOK)
	    fprintf (stderr, "\n"), exit (1);

    pe_free (pe);
    ps_free (ps);

    exit (0);
}

/*    ERRORS */

static ps_die (ps, s)
register PS	 ps;
register char   *s;
{
    fprintf (stderr, "%s: %s\n", s, ps_error (ps -> ps_errno));
    exit (1);
}


static pe_die (pe, s)
register PE	 pe;
register char   *s;
{
    fprintf (stderr, "%s: %s\n", s, pe_error (pe -> pe_errno));
    exit (1);
}

/*    PHOTO */

static	int	x, y, maxx;

static	bit    *bitrow, *bP;


/* ARGSUSED */

photo_start(name)
char   *name;
{
    if (passno == 1)
	maxx = 0;
    x = y = 0;

    return OK;
}


/* ARGSUSED */

photo_end (name)
char   *name;
{
    if (passno == 1) {
	register int	i;

	x = maxx, y--;

	pbm_writepbminit (stdout, maxx, y);
	bitrow = pbm_allocrow (maxx);

	for (i = maxx, bP = bitrow; i-- > 0; )
	    *bP++ = white;
	bP = bitrow;
    }
    else
	pbm_freerow (bitrow);

    return OK;
}


photo_black (length)
int	length;
{
    if (passno == 2) {
	register int	i;

	for (i = length; i > 0; i--)
	    *bP++ = black;
    }

    x += length;

    return OK;
}


photo_white (length)
int	length;
{
    if (passno == 2)
	bP += length;

    x += length;

    return OK;
}


/* ARGSUSED */

photo_line_end (line)
caddr_t line;
{
    if (passno == 1) {
	if (x > maxx)
	    maxx = x;
    }
    else {
	register int	i;

	pbm_writepbmrow (stdout, bitrow, maxx);

	for (i = maxx, bP = bitrow; i-- > 0; )
	    *bP++ = white;
	bP = bitrow;
    }

    x = 0, y++;

    return OK;
}
