#ifndef lint
static char sccsid[] = "@(#)verify.c	1.3 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"

#define	verbose	1

/*
**
*/

verify()
{
	extern boolean	read_bad_sector_map();

	cur.state = vfy;
	print("Starting verification on ");
	printf("controller %d, drive %d, ", cur.controller, cur.drive);
	printf("type %s.\n",CURRENT->vc_name);

	if(is_formatted() == true) {
		if(read_bad_sector_map() == true) {
			if(bad_map->bs_id == D_INFO.id) {
				verify_users_data_area();
				return;
			}
		}
		print("I can't verify drives with old formats.\n");
		return;
	}
	print("I can't verify unformatted drives.\n");
}


/*
**
*/

load_verify_patterns()
{
	register int index;
	register struct flawpat *fp = CURRENT->vc_pat;

	/* Init bad block pattern array */
	for(index=0; index<TRKSIZ; index++) {
		pattern_0[index] = fp->fp_pat[0];
		pattern_1[index] = fp->fp_pat[1];
		pattern_2[index] = fp->fp_pat[2];
		pattern_3[index] = fp->fp_pat[3];
		pattern_4[index] = fp->fp_pat[4];
		pattern_5[index] = fp->fp_pat[5];
		pattern_6[index] = fp->fp_pat[6];
		pattern_7[index] = fp->fp_pat[7];
		pattern_8[index] = fp->fp_pat[8];
		pattern_9[index] = fp->fp_pat[9];
		pattern_10[index] = fp->fp_pat[10];
		pattern_12[index] = fp->fp_pat[12];
		pattern_13[index] = fp->fp_pat[13];
		pattern_14[index] = fp->fp_pat[14];
		pattern_15[index] = fp->fp_pat[15];
	}
}


/*
**
*/

verify_relocation_area()
{
	cur.substate = sub_vfy;
	verify_cylinders((int)CURRENT->vc_ncyl - NUMSYS, NUMREL, 16);
	sync_bad_sector_map();
}


/*
**
*/

verify_users_data_area()
{
	int	pats = ops_to_do[cur.controller][cur.drive].numpat;

	cur.substate = sub_vfy;
	verify_cylinders(0, (int)CURRENT->vc_ncyl - NUMSYS, pats);
	sync_bad_sector_map();
}


/*
**
*/

verify_maintainence_area()
{
	cur.substate = sub_vfy;
	verify_cylinders(CURRENT->vc_ncyl - NUMSYS + NUMREL, NUMMNT, 16);
	sync_bad_sector_map();
}


/*
**	verify_cylinders does full track certification for every track
** on the cylinder.
*/

verify_cylinders(base_cyl, cyl_count, pats)
int	base_cyl, cyl_count, pats;
{
	dskadr		dskaddr;

	if (pats == 0)
		return;
	/* verify each track of each cylinder */
	for (dskaddr.cylinder = base_cyl;
	    dskaddr.cylinder < base_cyl + cyl_count; dskaddr.cylinder++)
		for (dskaddr.track = 0; dskaddr.track < CURRENT->vc_ntrak;
		    dskaddr.track++)
			verify_track(&dskaddr, pats, verbose);
}


/*
**	verify_track verifies a single track.  If a full-track write fails,
** the sector is flagged; if a full-track read fails, then each sector
** is read individually to determine which sectors are really bad.
** If a sector is bad it is flagged as bad by flag_sector.
*/

verify_track(dskaddr, pats, verbosity)
dskadr	*dskaddr;
int	pats;
int	verbosity;
{
	register int	index, i;
	register int	count;
	register long	before;
	register long	*after;
	register long	offset = SECSIZ / sizeof(long);
	int		pattern_count = pats;
	int		sectorflagged = -1;

	if (pats == 0)
		return;
	dskaddr->sector = (char)0;
	access_dsk((char *)pattern_address[0], dskaddr, VDOP_WD,
	    CURRENT->vc_nsec, 1);
	for (index = 0; index < pattern_count; index++) {
		if (!data_ok()) {
			if (dcb.operrsta & HEADER_ERROR)  {
		 
				flag_sector(dskaddr, dcb.operrsta,
				    dcb.err_code, verbosity);
				break;
			}
			if (dcb.operrsta & DATA_ERROR)
				pattern_count = 16;
		}
		access_dsk((char *)scratch, dskaddr, VDOP_RD,
		    CURRENT->vc_nsec, 1);
		if (!data_ok()) {
			if (dcb.operrsta & HEADER_ERROR)  {
				flag_sector(dskaddr, dcb.operrsta,
				    dcb.err_code, verbosity);
				break;
			}
			for (i = 0; i < CURRENT->vc_nsec; i++) {
				register long	*next;

				dskaddr->sector = i;
				next = &scratch[i * offset];
				access_dsk((char *)next, dskaddr, VDOP_RD, 1,1);
				if (!data_ok())
					flag_sector(dskaddr, dcb.operrsta,
					    dcb.err_code, verbosity);
			}
			dskaddr->sector = (char)0;
		}
		if (index+1 < pattern_count)
			access_dsk((char *)pattern_address[index+1],
			    dskaddr, VDOP_WD, CURRENT->vc_nsec, 0);
		count = CURRENT->vc_nsec * offset;
		before = *pattern_address[index];
		after = scratch;
		for (i = 0; i < count; i++) {
			if (before != *(after++)) {
				dskaddr->sector = (char)(i / offset);
				if (dskaddr->sector != sectorflagged)
					flag_sector(dskaddr, 0, 0,
					    verbosity);
				sectorflagged = dskaddr->sector;
			}
		}
		if (index+1 < pattern_count) {
			poll(60);
			if (vdtimeout <= 0) {
				printf(" while verifing track.\n");
				_longjmp(abort_environ, 1);
			}
		}
		if (kill_processes == true) {
			sync_bad_sector_map();
			_longjmp(quit_environ, 1);
		}
	}
}


flag_sector(dskaddr, status, ecode, verbosity)
dskadr	*dskaddr;
long	status;
int	ecode;
int	verbosity;
{
	fmt_err		error;
	bs_entry	entry;

	indent();
	if (verbosity != 0) {
		print("Error at sector %d (cyl %d trk %d sect %d),\n",
		    to_sector(*dskaddr), dskaddr->cylinder, dskaddr->track,
		    dskaddr->sector);
		if (status)
			print("  status=%b", status, VDERRBITS);
		else
			printf("  data comparison error");
		if (C_INFO.type == VDTYPE_SMDE && ecode)
			printf(", ecode=0x%x", ecode);
		printf(".\n  Sector will be relocated.\n");
	}
	if(is_in_map(dskaddr) == false) {
		error.err_adr = *dskaddr;
		error.err_stat = status;
		entry = (*C_INFO.code_pos)(error);
		add_flaw(&entry);
	}
	exdent(1);
}
