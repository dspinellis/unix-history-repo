/* test.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/test.c,v 7.2 91/02/22 09:20:31 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/test.c,v 7.2 91/02/22 09:20:31 mrose Interim $
 *
 *
 * $Log:	test.c,v $
 * Revision 7.2  91/02/22  09:20:31  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:43:04  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:44:38  mrose
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


#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/syntaxes.h"

main ()
{
	char * buffer;
	char * getline();
	Attr_Sequence as;
	Attr_Sequence as2;
	Attr_Sequence as_combine();
	extern PS opt;
	extern int parse_line;
	extern int parse_status;
	extern char * TidyString ();
	extern int oidformat;
	PE pe = NULLPE;

	isodetailor ("test",1);

	quipu_syntaxes ();
	load_oid_table ("../oidtable");

	parse_line = 0;
	parse_error ("Attribute parser %s","test");

	for (;;) {
		(void) fprintf (stderr,"-> ");

		if ((buffer = getline(stdin)) == NULLCP)
			exit (0);
		
		parse_status = 0;

		as2 = as_combine (NULLATTR,TidyString(buffer),FALSE);

		as = as_cpy (as2);

		if (as == NULLATTR) {
			(void) fprintf (stderr,"NULL value\n");
			continue;
		}
		if (parse_status != 0) {
			(void) fprintf (stderr,"parse error - non null as\n");
                        continue;
		}

		if (encode_IF_Attribute (&pe, 0, 0, NULLCP, as) == NOTOK) {
			fprintf (stderr,"encode problem\n");
			continue;
		}

		pe2pl (opt,pe);

		as_free (as);	

		if (decode_IF_Attribute (pe, 1, NULLIP, NULLVP, &as) == NOTOK) {
			fprintf (stderr,"decode problem\n");
			continue;
		}

		ps_print (opt,"READOUT:\n");
		as_print (opt,as,READOUT);

		if (as_cmp (as,as2) != 0)
			fprintf (stderr,"*** Compare/Copy problem ***\n");

		ps_print (opt,"EDBOUT:\n");
		as_print (opt,as2,EDBOUT);

		as_free (as2);
		pe = NULLPE;

	}

}
