#ifdef PP
#include <isode/manifest.h>
#include <isode/quipu/util.h>
#else 
#include "manifest.h"
#include "quipu/util.h"
#endif

/*
	OPTIONS...

	PP:	Build a version of dish with the PP enhancements.
		Needs libpp.a and libdl.a from PP (PP 4.2 and above).
		Made automatically in PP/Tools/dlist.

	MANAGE: Build a management version of Dish.
		Needs libmanage.a from ISODE/others/quipu/uips/manage
		Made automatically in ISODE/others/quipu/uips/manage.
		

 */

#ifdef MANAGE
IFP call_add_alias ();
IFP call_del_alias ();
IFP call_alias_chk ();
#endif

#ifdef PP
IFP call_dlist ();
#endif

main (argc, argv)
int             argc;
char          **argv;
{

	quipu_syntaxes ();

	revoke_syntax();	

#ifdef PP
	pp_quipu_init (argv[0]);
#endif

	dish_init (argc,argv);

#ifdef PP
	pp_quipu_run ();

        add_dish_command ("lmnpq", call_dlist, 2);
        add_dish_help    ("lmnpq", "[-dncheck] [-orcheck] [-orupdate] [-check] [-update]", 
			   FALSE, FALSE,
                          "List Manager Now using PP and Quipu,");
#endif

#ifdef MANAGE
	add_dish_command ("add_alias", call_add_alias, 5);
	add_dish_help	 ("add_alias", "<alias_name> <object>", FALSE, FALSE,
			  "add an alias entry,");
	add_dish_command ("del_alias", call_del_alias, 5);
	add_dish_help	 ("del_alias", "<object>", FALSE, FALSE, 
			  "delete an alias entry,");
	add_dish_command ("alias_chk", call_alias_chk, 5);
	add_dish_help	 ("alias_chk", "<object>", FALSE, FALSE, 
			  "Check alias or all aliases below <object>,");
#endif

	add_dish_help ("quit","", FALSE, FALSE, "Quit the program.");
	add_dish_help (NULLCP,NULLCP,0,0,NULLCP);

	do_dish ();
}
