#ifndef lint
static char sccsid[] = "@(#)format.c	1.4 (Berkeley/CCI) %G%";
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
	printf("type %s.\n",CURRENT->vc_name);

	/* Read the flaw map from the disk (where ever it may be) */
	if(read_bad_sector_map() == true) {
		if(bad_map->bs_id != D_INFO.id) {
			print("Module serial numbers do not match!\n");
			print("Use `info' to find the real serial number.\n");
			_longjmp(abort_environ, 1);
		}
	}
	else
		bad_map->bs_id = D_INFO.id;

	/* Re-Initialize bad sector map relocation pointers */
	zero_bad_sector_map();
	write_bad_sector_map();
	if(kill_processes == true)
		_longjmp(quit_environ, 1);

	/* format the disk surface */
	format_relocation_area();
	format_maintainence_area();
	format_users_data_area();
	if(kill_processes == true)
		_longjmp(quit_environ, 1);


	/* verify the surface */
	verify_relocation_area();
	verify_maintainence_area();
	verify_users_data_area();
}


/*
**
*/

format_relocation_area()
{
	register long		sector_count;
	dskadr			dskaddr;

	cur.substate = sub_fmt;
	dskaddr.cylinder = (short)(CURRENT->vc_ncyl - NUMSYS);
	dskaddr.track = (char)0;
	dskaddr.sector = (char)0;
	sector_count = (long)(NUMREL * CURRENT->vc_ntrak * CURRENT->vc_nsec);
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
	sector_count = (long)(CURRENT->vc_ntrak * CURRENT->vc_nsec);
	dskaddr.track = (char)0;
	dskaddr.sector = (char)0;
	for(cyl=0; cyl < (CURRENT->vc_ncyl - NUMSYS); cyl++) {
		dskaddr.cylinder = cyl;
		format_sectors(&dskaddr, &dskaddr, NRM, sector_count);
		if (kill_processes)
			return;
	}
}


/*
**
*/

format_maintainence_area()
{
	register long		sector_count;
	dskadr			dskaddr;

	cur.substate = sub_fmt;
	dskaddr.cylinder = (short)(CURRENT->vc_ncyl - NUMMNT - NUMMAP);
	dskaddr.track = (char)0;
	dskaddr.sector = (char)0;
	sector_count = (long)(NUMMNT * CURRENT->vc_ntrak * CURRENT->vc_nsec);
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
	if(C_INFO.type == VDTYPE_SMDE) {
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
	cur.daddr.cylinder = dskaddr->cylinder & 0xfff;
	cur.daddr.track = dskaddr->track;
	dcb.opcode = VDOP_FSECT;		/* format sector command */
	dcb.intflg = DCBINT_NONE;
	dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	dcb.devselect = (char)cur.drive;
	dcb.trailcnt = (char)(sizeof(struct trfmt) / sizeof(long));
	dcb.trail.fmtrail.addr = (char *)scratch; 
	dcb.trail.fmtrail.nsectors = count;
	dcb.trail.fmtrail.disk.cylinder = dskaddr->cylinder | flags;
	dcb.trail.fmtrail.disk.track = dskaddr->track;
	dcb.trail.fmtrail.disk.sector = dskaddr->sector;
	dcb.trail.fmtrail.hdr.cylinder = hdraddr->cylinder | flags;
	dcb.trail.fmtrail.hdr.track = hdraddr->track;
	dcb.trail.fmtrail.hdr.sector = hdraddr->sector;
	mdcb.mdcb_head = &dcb;
	mdcb.mdcb_status = 0;
	VDGO(C_INFO.addr, (u_long)&mdcb, C_INFO.type);
	poll((int)(((count+849)/850)+120));
	if(vdtimeout <= 0) {
		printf(" while formatting sectors.\n");
		_longjmp(abort_environ, 1);
	}
}
