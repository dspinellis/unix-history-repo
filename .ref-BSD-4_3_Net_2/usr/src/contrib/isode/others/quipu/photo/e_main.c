/* e_main.c - make the encoding routines into a stand alone program */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/e_main.c,v 7.2 91/02/22 09:29:13 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/e_main.c,v 7.2 91/02/22 09:29:13 mrose Interim $
 *
 *
 * $Log:	e_main.c,v $
 * Revision 7.2  91/02/22  09:29:13  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/01/15  13:42:47  mrose
 * KEJ
 * 
 * Revision 1.2  91/01/05  00:31:32  kej
 * ISODE claimed to be creating fax images as ASN.1-encoded BIT STRING's.
 * However, the encoding was incorrect.  This revision corrects the
 * problem, implements 1-d and 2-d encoding of fax images, and it provides
 * a backward compatible mechanism for reading the old, broken images.
 * 
 * Revision 1.1  91/01/02  21:35:21  kej
 * Initial revision
 * 
 * Revision 7.0  89/11/23  22:01:38  mrose
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
#include <pixrect/pixrect_hs.h>

struct pixrect *pr_load();

extern int PIC_LINESIZE;
extern int STOP;
extern int NUMLINES;
extern int optlen;

/* ROUTINE: main
/*
/* DESCRIPTION:  Interprets the command line parameters then calls the
/* encoding routine. The format of the command line is as follows:-
/*
/*     encode [-K] infile outfile
/*
/*     -K is the K parameter default 1
/*     if infile or outfile omitted then stdin or stdout is used.
*/
char * encode_t4 ();

main (argc,argv)
int     argc;
char ** argv;

{
int     k_param = 1;
int length;
char *  inbuf;
char * outbuf;
FILE *  fptr;
struct pixrect *pix;
struct mpr_data *src_mpr;
int skip;

   argv++;

   if ( (argc > 1) && (**argv == '-')) {
	switch (*++*argv) {
	case '1':       k_param = 1;break;
	case '2':       k_param = 2;break;
	case '4':       k_param = 4;break;
	case 'n':       k_param = 1;break;
	case 'l':       k_param = 2;break;
	case 'h':       k_param = 4;break;
	case 'v':       k_param = 32767;break;
	default :       (void) fprintf (stderr,"Usage: %s -[124nlhv] \n",argv[0]); 
			exit (-1);
	}
	argv++;
	argc--;
	}


   if ((pix = pr_load (stdin, NULL)) == (struct pixrect *)NULL)
	(void) fprintf (stderr,"Not a pixrect.\n");


   PIC_LINESIZE = pix->pr_size.x;
   STOP  = PIC_LINESIZE + 1;
   NUMLINES = pix->pr_size.y;

   src_mpr = (struct mpr_data *)(pix->pr_data);
   inbuf = (char *) src_mpr->md_image;

   skip = 16 - (PIC_LINESIZE % 16);
   if  (skip == 16) skip = 0;

   outbuf = encode_t4 (k_param,inbuf,skip);

   fwrite (outbuf, optlen, 1, stdout);

}

