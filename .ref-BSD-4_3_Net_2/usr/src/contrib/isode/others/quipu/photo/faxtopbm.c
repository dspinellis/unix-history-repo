/* faxtopbm.c - FAX to pbm filter */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/faxtopbm.c,v 7.2 91/02/22 09:29:17 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/faxtopbm.c,v 7.2 91/02/22 09:29:17 mrose Interim $
 *
 *
 * $Log:	faxtopbm.c,v $
 * Revision 7.2  91/02/22  09:29:17  mrose
 * Interim 6.8
 * 
 * Revision 1.3  91/01/07  23:50:28  kej
 * Support fax images encoded as a SEQUENCE which contains a SET followed by
 * a SEQUENCE of BIT STRING.
 * 
 * Revision 1.2  91/01/05  00:31:36  kej
 * ISODE claimed to be creating fax images as ASN.1-encoded BIT STRING's.
 * However, the encoding was incorrect.  This revision corrects the
 * problem, implements 1-d and 2-d encoding of fax images, and it provides
 * a backward compatible mechanism for reading the old, broken images.
 * 
 * Revision 1.1  91/01/02  21:35:35  kej
 * Initial revision
 * 
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
#include <sys/file.h>
#include "psap.h"
#include "pbm.h"

/*  */

#define ALLOCATION_SIZE 16384L

static	int	passno;

static	bit	black, white;

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    char   *cp;
    char   *data;
    int     fd;
    char   *file;
    int     len;
    long    limit;
    PE	    member;
    char   *newData;
    PE	    pe;
    PS	    ps;
    PE      seq;
    PE      set;
    long    size;
    int     twoDimensional = 0;

    /* process command options and parameters */

    black = PBM_BLACK;
    white = PBM_WHITE;

    file = NULLCP;
    fd = fileno (stdin);

    for (argv++; cp = *argv; argv++) {
        if (*cp == '-') {
	    if (cp[1] == NULL) {
		continue;
	    }
	    else if (strcmp (cp, "-reversebits") == 0) {
		black = PBM_WHITE;
		white = PBM_BLACK;
		continue;
	    }
	    else if (strcmp (cp, "-2d") == 0) {
		twoDimensional = 1;
		continue;
	    }
	    goto usage;
        }
	else if (file) {
usage: ;
	    fprintf (stderr, "usage: faxtopbm [-2d] [file]\n");
	    exit (1);
	}
	else {
	    file = cp;
	}
    }

    if ( file == NULLCP ) {
	file = "<stdin>";
    }
    else {
	fd = open (file, O_RDONLY);
	if ( fd == -1 ) {
	    perror (file);
	    exit (1);
	}
    }

    /* read the entire source file into memory */

    data = (char *)malloc (ALLOCATION_SIZE);
    if ( !data ) {
	fputs ("faxtopbm: out of memory\n", stderr);
	exit (1);
    }
    limit = ALLOCATION_SIZE;
    size = 0L;

    for (;;) {
	if (size + ALLOCATION_SIZE > limit) {
	    newData = (char *)realloc (data, limit + ALLOCATION_SIZE);
	    if ( !newData ) {
		fputs ("faxtopbm: out of memory\n", stderr);
		exit (1);
	    }
	    data = newData;
	    limit += ALLOCATION_SIZE;
	}
	len = read (fd, &data[size], ALLOCATION_SIZE);
	if (len == -1) {
	    perror (file);
	    exit (1);
	}
	else if (len == 0)
	    break;
	size += len;
    }

    if (size < 1) {
	fprintf (stderr, "%s: is not a fax image\n", file);
	exit (1);
    }

    /*
     *  If the first byte does not indicate that the data is an ASN.1-encoded
     *  SET or BIT STRING, attempt to convert the data as a non-ASN.1 image.
     */

    if ((unsigned char)*data != 0xa3 && *data != 0x03) {
	for (passno = 1; passno < 3; passno++) {
	    if (decode_t4_aux (data, file, (int)size, twoDimensional) == NOTOK)
		exit (1);
        }
	free (data);
	exit (0);
    }

    /* attempt to decode the source */

    if ((ps = ps_alloc (str_open)) == NULLPS) {
	fprintf (stderr, "ps_alloc: unable to allocate presentation stream\n");
	exit (1);
    }
    if (str_setup (ps, data, (int)size, 0) == NOTOK)
	ps_die (ps, "str_setup");
    if ((pe = ps2pe (ps)) == NULLPE) { /* maybe it's a non-ASN.1 image */
	ps_free (ps);
	for (passno = 1; passno < 3; passno++) {
	    if (decode_t4_aux (data, file, (int)size, twoDimensional) == NOTOK)
		exit (1);
        }
	free (data);
	exit (0);
    }
    if (pe->pe_class == PE_CLASS_UNIV && pe->pe_form == PE_FORM_PRIM &&
	pe->pe_id == PE_PRIM_BITS) { /* old  BIT STRING-like form */
	if (pe_pullup (pe) == NOTOK)
	    pe_die (pe, "pe_pullup");
	for (passno = 1; passno < 3; passno++) {
	    if (decode_t4_aux (pe -> pe_prim, file, ps_get_abs (pe), 1) == NOTOK)
		exit (1);
        }
    }
    else if (pe->pe_class == PE_CLASS_CONT && pe->pe_form == PE_FORM_CONS &&
	     pe->pe_id == 3) {

	/* first member must be SET which describes G3-Fax options */

	set = first_member (pe);
	if (!set || set->pe_class != PE_CLASS_UNIV ||
	    set->pe_form != PE_FORM_CONS || set->pe_id != PE_CONS_SET) {
	    fprintf (stderr, "decode_fax: %s is not a fax image\n", file);
	    exit (1);
	}

	for (member = first_member (set); member; member = next_member (set, member)) {
	    if (member->pe_class == PE_CLASS_CONT &&
		member->pe_form == PE_FORM_PRIM && member->pe_id == 1) {
	        twoDimensional = bit_test (prim2bit (member), 8);
		if (twoDimensional == -1)
		    pe_die (member, "bit_test");
	    }
	}

	/* SEQUENCE of BIT STRING should follow SET */

	seq = next_member (pe, set);
	if (!seq || seq->pe_class != PE_CLASS_UNIV ||
	    seq->pe_form != PE_FORM_CONS || seq->pe_id != PE_CONS_SEQ) {
	    fprintf (stderr, "%s: is not a fax image\n", file);
	    exit (1);
	}

	for (member = first_member (seq); member; member = next_member (seq, member)) {
	    if (member->pe_class != PE_CLASS_UNIV ||
		member->pe_form != PE_FORM_PRIM ||
		member->pe_id != PE_PRIM_BITS) {
		fprintf (stderr, "%s: is not a fax image\n", file);
		exit (1);
	    }
	    for (passno = 1; passno < 3; passno++) {
		if (decode_t4_aux ((member->pe_prim) + 1, file,
			       ps_get_abs (member) - 1,
			       twoDimensional) == NOTOK)
		    exit (1);
            }
	}
    }
    else { /* maybe its a non-ASN.1 image */
	pe_free (pe);
	ps_free (ps);
	for (passno = 1; passno < 3; passno++) {
	    if (decode_t4_aux (data, file, (int)size, twoDimensional) == NOTOK)
	      exit (1);
        }
	free (data);
	exit (0);
    }

    pe_free (pe);
    ps_free (ps);
    free (data);

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
