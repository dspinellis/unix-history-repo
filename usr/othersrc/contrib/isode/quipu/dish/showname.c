/* showname.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/showname.c,v 7.1 91/02/22 09:40:55 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/showname.c,v 7.1 91/02/22 09:40:55 mrose Interim $
 *
 *
 * $Log:	showname.c,v $
 * Revision 7.1  91/02/22  09:40:55  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:20:24  mrose
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
#include "quipu/name.h"

extern DN       current_dn;
extern DN       dn;

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

extern char 	print_format;


call_showname (argc, argv)
int             argc;
char          **argv;
{
	DN              dnptr;
	int             compact = FALSE;
	int             ufn     = TRUE;
	int             x;
	extern DN	rel_dn;

	if ((argc = read_cache (argc, argv)) < 0)
		return;

	for (x = 1; x < argc; x++) {
		if (test_arg (argv[x], "-compact",2))  {	/* compact */
			compact = TRUE;
			argc--;
		} else if (test_arg (argv[x], "-nocompact",3))  {
			compact = FALSE;
			argc--;
		} else if (test_arg (argv[x], "-ufn",3))  { 
			ufn = TRUE;
			argc--;
		} else if (test_arg (argv[x], "-noufn",5))  {
			ufn = FALSE;
			argc--;
		} else {
			ps_printf (OPT,"Unknown option %s\n",argv[x]);
			Usage (argv[0]);
			return;
		}
	}

	if (compact) {
		if (rel_dn != NULLDN) {
			DN a,b;

			a = rel_dn;
			b = current_dn;
			for (; a != NULLDN && b != NULLDN ; a = a->dn_parent, b = b->dn_parent)
				if ( dn_comp_cmp (a,b) == NOTOK) 
					break;
			if (a == NULLDN)
				dn_print (RPS,b,RDNOUT);
			else {
				ps_print (RPS, "@");
				dn_print (RPS,current_dn,RDNOUT);
			}
			ps_print (RPS, "\n");
		} else {
			dn_print (RPS, current_dn, RDNOUT);
			ps_print (RPS, "\n");
		}
	} else {
		if (current_dn == NULLDN) {
			ps_print (RPS, "NULL Name\n");
			return;
		}
		if (ufn) {	
			ufn_dn_print_aux (RPS,current_dn,NULLDN,0);
			ps_print (RPS, "\n");
			return;
		}
		for (dnptr = current_dn; dnptr != NULLDN; dnptr = dnptr->dn_parent) {
			rdn_print (RPS,dnptr->dn_rdn,print_format);
			ps_print (RPS, "\n");
		}
	}
}
