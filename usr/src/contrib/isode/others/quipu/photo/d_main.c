/* d_main.c - make the decode routine into a stand alone program */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/d_main.c,v 7.4 91/02/22 09:29:06 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/d_main.c,v 7.4 91/02/22 09:29:06 mrose Interim $
 *
 *
 * $Log:	d_main.c,v $
 * Revision 7.4  91/02/22  09:29:06  mrose
 * Interim 6.8
 * 
 * Revision 1.3  91/01/07  23:49:49  kej
 * Support fax images encoded as a SEQUENCE which contains a SET followed by
 * a SEQUENCE of BIT STRING.
 * 
 * Revision 1.2  91/01/05  00:29:07  kej
 * ISODE claimed to be creating fax images as ASN.1-encoded BIT STRING's.
 * However, the encoding was incorrect.  This revision corrects the
 * problem, implements 1-d and 2-d encoding of fax images, and it provides
 * a backward compatible mechanism for reading the old, broken images.
 * 
 * Revision 1.1  91/01/02  21:35:05  kej
 * Initial revision
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



#include <stdio.h>
#include <sys/file.h>
#include "psap.h"
#include "quipu/photo.h"


#define ALLOCATION_SIZE 16384L

int	two_passes = 0;

char   *getenv ();

main (argc, argv)
int     argc;
char ** argv;
{
    int   count;
    char	*cp;
    char *data;
    int   fd;
    char *file = "<stdin>";
    char *inbuf;
    int   len;
    long  limit;
    int	  n;
    char *name;
    char *newData;
    long  size;

    if ((name = getenv ("RDN")) == NULL)
        name = "unknown";

    /* process commmand line options and parameters */

    if (argc > 1)
	    name = *(++argv);

    fd = fileno (stdin);

    /* read the entire source file into memory */

    data = (char *)malloc (ALLOCATION_SIZE);
    if ( !data ) {
	fputs ("decode_fax: out of memory\n", stderr);
	exit (1);
    }
    limit = ALLOCATION_SIZE;
    size = 0L;

    for (;;) {
	if (size + ALLOCATION_SIZE > limit) {
	    newData = (char *)realloc (data, limit + ALLOCATION_SIZE);
	    if ( !newData ) {
		fputs ("decode_fax: out of memory\n", stderr);
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
   if (decode_t4 (data, name, (int)size) == -1) {
	(void) fprintf (stderr,"\n");
	exit (-1);
   }
   if (two_passes && decode_t4 (data, name, (int)size) == -1) {
	(void) fprintf (stderr,"\n");
	exit (-1);
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


static pe_die (pe, s)
register PE	 pe;
register char   *s;
{
    fprintf (stderr, "%s: %s\n", s, pe_error (pe -> pe_errno));
    exit (1);
}
