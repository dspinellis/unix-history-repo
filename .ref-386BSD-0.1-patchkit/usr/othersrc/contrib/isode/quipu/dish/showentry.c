/* showentry.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/showentry.c,v 7.5 91/03/09 11:56:31 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/showentry.c,v 7.5 91/03/09 11:56:31 mrose Exp $
 *
 *
 * $Log:	showentry.c,v $
 * Revision 7.5  91/03/09  11:56:31  mrose
 * update
 * 
 * Revision 7.4  91/02/22  09:40:54  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/17  11:55:41  mrose
 * sync
 * 
 * Revision 7.2  90/03/15  11:18:34  mrose
 * quipu-sync
 * 
 * Revision 7.1  90/01/11  18:37:46  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:20:22  mrose
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
#include "quipu/dua.h"
#include "quipu/entry.h"
#include "quipu/read.h"

extern Entry    current_entry;
extern DN       current_dn;

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

extern DN       dn;
char 		move_flag;

extern char	dad_flag;

extern char	fred_flag;
extern char	fred_expand;
extern char	fred_list;
extern char	fred_long;
extern char	fred_phone;
extern char	fred_photo;
extern char	fred_sequence;
extern char	fred_subdisplay;

extern LLog    *log_dua;

call_showentry (argc, argv)
int             argc;
char          **argv;
{
	Attr_Sequence   eptr;
	char           *vect[2];
	int             x;
	extern char	name_flag;
	extern char	all_flag;
	extern Attr_Sequence as_flag;
	extern char	flag_show;
	char            full_edb_flag;
	char            *temp_file_name;
	extern int      mod_template();

	vect[0] = "showentry";

	move_flag = FALSE;
 	fred_flag = FALSE;
 	fred_expand = FALSE;
	fred_list = FALSE;
 	fred_long = 2;
	fred_phone = FALSE;
	fred_photo = FALSE;
 	fred_sequence = TRUE;
 	fred_subdisplay = FALSE;
	name_flag = FALSE;
	full_edb_flag = FALSE;

	for (x=1; x<argc; x++) {
		if (test_arg (argv[x], "-move",2))
			move_flag = TRUE;
		else if (test_arg (argv[x], "-nomove",3))
			move_flag = FALSE;
		else if (test_arg (argv[x], "-fred",4))
			fred_flag = TRUE;
		else if (test_arg (argv[x], "-expand",4))
			fred_expand = TRUE;
		else if (test_arg (argv[x], "-fredlist",8))
			fred_list = TRUE;
		else if (test_arg (argv[x], "-full",4))
			fred_long = TRUE;
		else if (test_arg (argv[x], "-summary",7))
			fred_long = FALSE;
		else if (test_arg (argv[x], "-phone",5))
			fred_phone = TRUE;
		else if (test_arg (argv[x], "-fredphoto",9))
		    	fred_photo = dad_flag;
		else if (test_arg (argv[x], "-nofredseq",9))
			fred_sequence = FALSE;
		else if (test_arg (argv[x], "-subdisplay",10))
			fred_subdisplay = TRUE;
		else if (test_arg (argv[x], "-fedb",4)) {
			if (x + 1 == argc) {	
				ps_printf (opt, "We need a filename for the full edb entry.\n");
				return;
			} else {
				shuffle_up (argc, argv, x);
				argc--;
				if (*(temp_file_name = argv[x]) != '/') {
					ps_printf (opt, "We need a real file spec for the file name.\n");
					return;
				}
			}
			full_edb_flag = TRUE;
		      }
		else
			continue;
		shuffle_up (argc--,argv,x--);

	}
	if ((argc = read_cache (argc, argv)) < 0)
		return;

	
	if (argc != 1) {
		ps_printf (OPT,"Unknown option %s\n",argv[1]);
		Usage (argv[0]);
		return;
	}

	if (current_entry == NULLENTRY) {
		ps_print (OPT,"Specify an entry \n");
		/* this CAN happen - when the entry is not cache, but -noread */
		return;
	}
	if (full_edb_flag) {
	  (void) mod_template(temp_file_name,0);
	  return;
	}

	if (fred_flag) {
	    if (fred_long == 2)
		if ((fred_subdisplay && fred_expand)
			|| (!fred_subdisplay && !fred_expand))
		    fred_long = TRUE;
		else
		    fred_long = fred_expand;
	    if (fred_expand)
		fred_long = fred_subdisplay = TRUE;

	    if (fred_list
		    && frompipe
		    && rps -> ps_byteno == 0
		    && opt -> ps_byteno == 0) {
		DN	new_dn = dn_cpy (current_dn);
		
		showfredDNs (new_dn, fred_long);
		dn_free (new_dn);
	    }

	    (void) showfred (current_dn, fred_long, fred_subdisplay);
	}
	else {
		if (name_flag) {
			dn_print (RPS,dn,EDBOUT);
			ps_print (RPS,"\n");
		}

		if (all_flag) 
			eptr = current_entry->e_attributes;
		else
			eptr = as_flag;

			if (flag_show)
			for (; eptr != NULLATTR; eptr = eptr->attr_link)
				showattribute (eptr->attr_type);
	}

	consolidate_move();
}
