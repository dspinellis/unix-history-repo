#ifndef lint
static char sccsid[] = "@(#)exercise.c	1.2 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"

#define	verbose	1

/*
**
*/

exercise()
{
	int		cyl, trk;
	dskadr		ead, sad;

	print("Starting disk exercise on ");
	printf("controller %d, drive %d, ", cur.controller, cur.drive);
	printf("type %s.\n",CURRENT->vc_name);

	if(read_bad_sector_map() == true) {
		if(bad_map->bs_id != D_INFO.id) {
			print("Module serial numbers do not match!\n");
			print("Use `info' to find the real serial number.\n");
			_longjmp(abort_environ, 1);
		}
	}
	else if(is_formatted() == false) {
		print("Can not exercise unformatted drives!\n");
		_longjmp(abort_environ, 1);
	}
	print("Starting read test.\n");
	cur.state = exec;
	sad.track = sad.sector = 0;
	indent();
	for(sad.cylinder=0; sad.cylinder<CURRENT->vc_ncyl; sad.cylinder++) {
		print("pass %d...\n", sad.cylinder);
		for(cyl=0; cyl<CURRENT->vc_ncyl-NUMSYS; cyl++){
			ead.cylinder = cyl;
			for(trk=0; trk<CURRENT->vc_ntrak; trk++) {
				ead.track = trk;
				ead.sector = 0;
				access_dsk((char *)scratch, &sad,
				    VDOP_SEEK, 1, 1);
				verify_track(&ead, 16, verbose);
				if(kill_processes == true)
					goto exit;
			}
		}
	}
	exdent(2);
exit:	sync_bad_sector_map();
	printf("Exercise completed successfully.\n");
}

