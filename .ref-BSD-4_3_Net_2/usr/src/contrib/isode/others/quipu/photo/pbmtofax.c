/* pbmtofax.c - pbm to FAX filter */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/pbmtofax.c,v 7.4 91/02/22 09:29:23 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/pbmtofax.c,v 7.4 91/02/22 09:29:23 mrose Interim $
 *
 *
 * $Log:	pbmtofax.c,v $
 * Revision 7.4  91/02/22  09:29:23  mrose
 * Interim 6.8
 * 
 * Revision 1.4  91/01/07  23:50:30  kej
 * Support fax images encoded as a SEQUENCE which contains a SET followed by
 * a SEQUENCE of BIT STRING.
 * 
 * Revision 1.3  91/01/05  23:32:00  kej
 * Implement support for specification of all G3-Fax nonbasic parameters.
 * 
 * Revision 1.2  91/01/05  00:31:39  kej
 * ISODE claimed to be creating fax images as ASN.1-encoded BIT STRING's.
 * However, the encoding was incorrect.  This revision corrects the
 * problem, implements 1-d and 2-d encoding of fax images, and it provides
 * a backward compatible mechanism for reading the old, broken images.
 * 
 * Revision 1.1  91/01/02  21:36:31  kej
 * Initial revision
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
#include "pbm.h"

/*    DATA */

int	PIC_LINESIZE, STOP, NUMLINES;

extern	int	optlen;

char   *encode_t4 ();

char   *malloc ();

/*
 *   G3-Fax nonbasic parameters.
 */

extern int twoDimensional;
extern int fineResolution;
extern int unlimitedLength;
extern int b4Length;
extern int a3Width;
extern int b4Width;
extern int uncompressed;
extern int nopreamble;
extern int oldformat;
extern int standardwidth;

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int     bitcount;
    bit    *bitrow;
    register bit *bP;
    unsigned char *byteP;
    register int i;
    register int j;
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

    file = NULL;
    fp = stdin;

    for (argv++; cp = *argv; argv++) {
	if (*cp == '-') {
	    if (cp[1] == NULL)
		continue;

	    else if (strcmp (cp, "-reversebits") == 0)
		black = PBM_WHITE;

	    else if (strcmp (cp, "-2d") == 0)
		twoDimensional = 1;

	    else if (strcmp (cp, "-cr") == 0)
		fineResolution = 0;

	    else if (strcmp (cp, "-ul") == 0)
		unlimitedLength = 1;

	    else if (strcmp (cp, "-b4l") == 0)
		b4Length = 1;

	    else if (strcmp (cp, "-a3w") == 0)
		a3Width = 1;

	    else if (strcmp (cp, "-b4w") == 0)
		b4Width = 1;

	    else if (strcmp (cp, "-uc") == 0)
		uncompressed = 1;

	    else if (strcmp (cp, "-sw") == 0)
		standardwidth = 1;

	    else if (strcmp (cp, "-old") == 0) {
		oldformat = 1;
		twoDimensional = 1;
	    }

	    else if (strcmp (cp, "-nopreamble") == 0)
		nopreamble = 1;

	    else
	        goto usage;
	}
	else if (file) {
usage: ;
		fputs ("usage: pbmtofax [-2d] [-cr] [-ul] [-b4l] [-a3w] [-b4w] [-uc]\n",
		       stderr);
		fputs ("                [-old] [-nopreamble] [file]\n", stderr);
		fputc ('\n', stderr);
		fputs ("  -2d  select two dimensional encoding mode\n", stderr);
		fputs ("  -cr  set coarse resolution indication\n", stderr);
		fputs ("  -ul  set unlimited image length indication\n", stderr);
		fputs ("  -b4l set B4 length indication\n", stderr);
		fputs ("  -a3w set A3 width indication\n", stderr);
		fputs ("  -b4w set B4 width indication\n", stderr);
		fputs ("  -sw  force standard width (1728 pels)\n", stderr);
		fputs ("  -uc  set uncompressed mode indication\n", stderr);
		fputs ("  -old encode fax using old photo format\n", stderr);
		fputs ("  -nopreamble   encode fax without any preamble info\n", stderr);
		fputs ("  -reversebits  create an inverse image\n", stderr);
		exit (1);
        }
	else
	    file = cp;
    }

    if (file) {
	fp = pm_openr (file);
	if (fp == NULL) {
	    perror (file);
	    exit (1);
	}
    }
    else
        file = "<stdin>";

    pbm_readpbminit (fp, &cols, &rows, &format);
    bitrow = pbm_allocrow (cols);

    data = malloc ((unsigned) (cols * rows));
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
    
    STOP = (PIC_LINESIZE = cols) + 1;
    NUMLINES = rows;
    if ((skip = 8 - (PIC_LINESIZE % 8)) == 8)
	skip = 0;

    optbuf = encode_t4 (twoDimensional ? 4 : 1, data, skip);

    (void) fwrite (optbuf, optlen, sizeof *optbuf, stdout);

    exit (0);
}
