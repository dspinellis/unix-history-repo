#ifndef lint
static char sccsid[] = "@(#)info.c	1.2 (Berkeley/CCI) %G%";
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
	print("Module serial number is %d.\n", bad_map->bs_id);
	print("Drive type is %s.\n", lab->d_typename);
	if(has_map == true)
		print("Drive contains a bad sector map.\n");
	else
		print("Drive does not contain a bad sector map.\n");
	print_bad_sector_list();
}

