/* d_main.c - make the decode routine into a stand alone program */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/d_main.c,v 7.2 90/09/24 15:36:39 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/d_main.c,v 7.2 90/09/24 15:36:39 mrose Exp $
 *
 *
 * $Log:	d_main.c,v $
 * Revision 7.2  90/09/24  15:36:39  mrose
 * update
 * 
 * Revision 7.1  90/03/15  11:18:08  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  22:01:36  mrose
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



#include <sys/file.h>
#include <stdio.h>
#include "quipu/photo.h"


int	two_passes = 0;

char   *getenv ();

main (argc,argv)
int     argc;
char ** argv;
{
char    *inbuf;
char	*cp;
char * name;	
int	n, count, len;
char * malloc();

   if (argc > 1) 
	name = *++argv;
   else
       if ((name = getenv ("RDN")) == 0)
	   name = "unknown";

   if ((len = find_size()) < 0) {
        fprintf(stderr, "\n");
        exit(-1);
   }
   if ((inbuf = malloc(len)) == NULL) {
	fprintf(stderr, "PHOTO:  Couldn't malloc() %d bytes\n", n);
	exit(-1);
   }
   for (cp = inbuf, n = len; n > 0; ) {
	if ((count = read (0, cp, n)) <= 0)
	    break;
	cp += count;
	n -= count;
   }
	
   if (decode_t4 (inbuf, name, len) == -1) {
	(void) fprintf (stderr,"\n");
	exit (-1);
   }
   if (two_passes && decode_t4 (inbuf, name, len) == -1) {
	(void) fprintf (stderr,"\n");
	exit (-1);
   }
}

static	find_size()
{
	char c, i;
	long size = 0;

	/* read the ASN.1 "magic number" for bitstring */
	read(0, &c, 1);

	/* how many of the next N bytes store the size? */
	read(0, &c, 1);
	for (c &= 0x7f; c > 0; c--) {
		read(0, &i, 1);
		size = (size << 8) | (i & 0xff);
	}

	return(size);
}
