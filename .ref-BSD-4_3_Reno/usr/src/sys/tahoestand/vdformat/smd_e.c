#ifndef lint
static char sccsid[] = "@(#)smd_e.c	1.3 (Berkeley/CCI) 6/7/88";
#endif

#include	"vdfmt.h"


/*
**	The sector format of the SMD controller looks like this:
**  32 bytes sector gap			(header error)
**   1 byte sync (0x19)			(header error)
**   2 bytes cylinder address		(header error)
**   1 byte track address		(header error)
**   1 byte sector address		(header error)
**   2 bytes alt cylinder address	(header error)
**   1 byte alt track address		(header error)
**   1 byte alt sector address		(header error)
**   4 bytes header crc			(header error)
**  34 bytes header gap			(data error)
** 512 bytes data			(data error)
**   4 byte ecc				(data error)
**  ?? trailing pad			(data error)
*/

smd_e_decode_position(bad_entry, error)
register bs_entry *bad_entry;
register fmt_err *error;
{
	int	sector_length = lab->d_traksize / lab->d_nsectors;
	int	offset = (bad_entry->bs_offset-2) % sector_length;
	int	bytes = (bad_entry->bs_length / 8) + 4;

	error->err_adr.cylinder = bad_entry->bs_cyl;
	error->err_adr.track = bad_entry->bs_trk;
	error->err_adr.sector = (bad_entry->bs_offset-2) / sector_length;
	if(error->err_adr.sector >= lab->d_nsectors) {
		error->err_adr.sector = lab->d_nsectors - 1;
		error->err_stat = DATA_ERROR;
	}
	else if((offset < 45) || ((offset+bytes) > sector_length))
		error->err_stat = HEADER_ERROR;
	else 
		error->err_stat = DATA_ERROR;
}


/*
**
*/

smd_e_code_position(error, badent)
register fmt_err *error;
register bs_entry *badent;
{
	int		sector_length = lab->d_traksize / lab->d_nsectors;

	badent->bs_length = 1;
	badent->bs_cyl = error->err_adr.cylinder;
	badent->bs_trk = error->err_adr.track;
	badent->bs_offset = error->err_adr.sector * sector_length;
	if(error->err_stat & HEADER_ERROR) {
		badent->bs_offset += 1;
	}
	else {
		badent->bs_offset += 60;
	}
	badent->bs_alt.cylinder = 0;
	badent->bs_alt.track = 0;
	badent->bs_alt.sector = 0;
	badent->bs_how = scanning;
}
