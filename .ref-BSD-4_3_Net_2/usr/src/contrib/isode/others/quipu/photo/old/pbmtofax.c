/* pbmtofax.c - pbm to FAX filter */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/pbmtofax.c,v 7.2 90/09/24 15:36:49 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/pbmtofax.c,v 7.2 90/09/24 15:36:49 mrose Exp $
 *
 *
 * $Log:	pbmtofax.c,v $
 * Revision 7.2  90/09/24  15:36:49  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:40:29  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:01:46  mrose
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
#include "quipu/photo.h"
#include "pbm/pbm.h"

/*    DATA */

int	PIC_LINESIZE, STOP, NUMLINES;

extern	int	optlen;


char   *encode_t4 ();


char   *malloc ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    cols,
	    format,
	    rows,
	    skip;
    char   *cp,
	   *data,
	   *file,
	   *optbuf;
    bit	    black;
    FILE   *fp;
    
    black = PBM_BLACK;

    file = NULL, fp = stdin;
    for (argv++; cp = *argv; argv++)
	if (*cp == '-') {
	    if (cp[1] == NULL)
		goto usage;

	    if (strncmp (cp, "-reversebits", strlen (cp)) == 0) {
		black = PBM_WHITE;
		continue;
	    }
	    goto usage;
	}
	else
	    if (file) {
usage: ;
		fprintf (stderr, "usage: pbmtofax [file]\n");
		exit (1);
	    }
	    else
		if ((fp = pm_openr (file = cp)) == NULL)
		    perror (file), exit (1);

    {
	int	bitcount;
	register int    i,
			j;
	unsigned char *byteP;
	bit    *bitrow;
	register bit *bP;

	pbm_readpbminit (fp, &cols, &rows, &format);
	bitrow = pbm_allocrow (cols);

	if ((data = malloc ((unsigned) (cols * rows))) == NULL)
	    fprintf (stderr, "out of memory\n"), exit (1);
	byteP = (unsigned char *) data;

	for (i = rows; i-- > 0; ) {
	    pbm_readpbmrow (fp, bP = bitrow, cols, format);
	    *byteP = NULL, bitcount = 7;

	    for (j = cols; j-- > 0; ) {
		unsigned char mask = 1 << bitcount;

		if (*bP++ == black)
		    *byteP |= mask;
		else
		    *byteP &= ~mask;
		if (--bitcount < 0)
		    *++byteP = NULL, bitcount = 7;
	    }
	    if (bitcount != 7)
		byteP++;
	}

	pm_close (fp);
    }
    
    STOP = (PIC_LINESIZE = cols) + 1;
    NUMLINES = rows;
    if ((skip = 8 - (PIC_LINESIZE % 8)) == 8)
	skip = 0;

    optbuf = encode_t4 (4, data, skip);
    *(optbuf + optlen) = NULL;

    (void) fwrite (optbuf, optlen + 1, sizeof *optbuf, stdout);

    exit (0);
}
