#ifndef lint
static char sccsid[] = "%W% (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"

/*
**
*/

format()
{
	boolean	read_bad_sector_map();
	cur.state = fmt;
	print("Starting format on ");
	printf("controller %d, drive %d, ", cur.controller, cur.drive);
	printf("type %s.\n", lab->d_typename);

	/* Read the flaw map from the disk (where ever it may be) */
	if(read_bad_sector_map() == true) {
		if(bad_map->bs_id != D_INFO->id) {
			print("Module serial numbers do not match!\n");
#ifdef notdef
			print("Use `info' to find the real serial number.\n");
			_longjmp(abort_environ, 1);
#else
			printf("Using serial number from drive, %d\n",
			    bad_map->bs_id);
			D_INFO->id = bad_map->bs_id;
#endif
		}
	}
	else
		bad_map->bs_id = D_INFO->id;

	/* Re-Initialize bad sector map relocation pointers */
	zero_bad_sector_map();
	write_bad_sector_map();
	if(kill_processes == true)
		_longjmp(quit_environ, 1);

	/* format the disk surface */
	format_relocation_area();
	format_maintenence_area();
	format_users_data_area();
	if(kill_processes == true)
		_longjmp(quit_environ, 1);


	/* verify the surface */
	verify_relocation_area();
	verify_maintenence_area();
	verify_users_data_area();

	(void) writelabel();
}


/*
**
*/

format_relocation_area()
{
	register long		sector_count;
	dskadr			dskaddr;

	cur.substate = sub_fmt;
	dskaddr.cylinder = (short)(lab->d_ncylinders - NUMSYS);
	dskaddr.track = (char)0;
	dskaddr.sector = (char)0;
	sector_count = (long)(NUMREL * lab->d_ntracks * lab->d_nsectors);
	format_sectors(&dskaddr, &dskaddr, NRM, sector_count);
}


/*
**
*/

format_users_data_area()
{
	register long		sector_count;
	dskadr			dskaddr;
	register int		cyl;

	cur.substate = sub_fmt;
	sector_count = (long)(lab->d_ntracks * lab->d_nsectors);
	dskaddr.track = (char)0;
	dskaddr.sector = (char)0;
	for(cyl=0; cyl < (lab->d_ncylinders - NUMSYS); cyl++) {
		dskaddr.cylinder = cyl;
		format_sectors(&dskaddr, &dskaddr, NRM, sector_count);
		if (kill_processes)
			return;
	}
}


/*
**
*/

format_maintenence_area()
{
	register long		sector_count;
	dskadr			dskaddr;

	cur.substate = sub_fmt;
	dskaddr.cylinder = (short)(lab->d_ncylinders - NUMMNT - NUMMAP);
	dskaddr.track = (char)0;
	dskaddr.sector = (char)0;
	sector_count = (long)(NUMMNT * lab->d_ntracks * lab->d_nsectors);
	format_sectors(&dskaddr, &dskaddr, NRM, sector_count);
}


/*
**
*/

boolean is_formatted()
{
	extern boolean	align_buf();
	dskadr		dskaddr;

	dskaddr.cylinder = 0;
	dskaddr.track = 0;
	dskaddr.sector = 0;
	if(C_INFO->type == VDTYPE_SMDE) {
		access_dsk((char *)save, &dskaddr, VDOP_RDRAW, 1, 1);
		if(align_buf((unsigned long *)save, CDCSYNC) == false)
			return true;
		return	false;
	}
	else if(access_dsk((char *)save, &dskaddr, VDOP_RD, 1, 1)&HEADER_ERROR)
		return false;
	return true;
}


/*
**	Vdformat_sectors is used to do the actual formatting of a block.
*/

format_sectors(dskaddr, hdraddr, flags, count)
dskadr	*dskaddr, *hdraddr;
short	flags;
long	count;
{
	struct format_op fop;

	cur.daddr.cylinder = dskaddr->cylinder & 0xfff;
	cur.daddr.track = dskaddr->track;

	bzero((char *)&fop, sizeof(fop));
	fop.df_buf = (char *)scratch; 
	fop.df_count = count;
	fop.df_startblk = ((dskaddr->cylinder & 0xfff) * lab->d_ntracks +
	    dskaddr->track) * lab->d_nsectors + dskaddr->sector;
	fop.df_reg[0] = VDOP_FSECT;		/* format sector command */
	fop.df_reg[1] = *(int *)&hdraddr;
	fop.df_reg[2] = flags;

	(void) ioctl(diskfd, DIOCWFORMAT, &fop);
	dcb.operrsta = fop.df_reg[0];		/* XXX */
	dcb.err_code = fop.df_reg[1];		/* XXX */

	if (dcb.operrsta & DCBS_HARD)
		vd_error("format");
	if(kill_processes == true)
		_longjmp(quit_environ, 1);
}
