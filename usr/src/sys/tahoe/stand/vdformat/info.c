#ifndef lint
static char sccsid[] = "@(#)info.c	1.3 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"

/*
**
*/

info()
{
	extern boolean	read_bad_sector_map();
	boolean		has_map;

	cur.state = inf;
	print("Gathering information for ");
	printf("controller %d, drive %d.\n\n", cur.controller, cur.drive);

	has_map = read_bad_sector_map();
	if(has_map == true) {
		print("Module serial number is %d.\n", bad_map->bs_id);
		print("Drive contains a%s bad sector map.\n",
		    (bad_map == &offset_bad_map) ? "n old-style" : "");
	} else
		print("Drive does not contain a bad sector map.\n");
	print_bad_sector_list();
}

