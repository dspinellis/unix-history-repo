#ifndef lint
static char sccsid[] = "@(#)smd.c	1.2 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"



/*
**	The sector format of the SMD controller looks like this:
**  28 bytes sector gap		(header error)
**   1 byte sync (0x19)		(header error)
**   2 bytes cylinder address	(header error)
**   1 byte track address	(header error)
**   1 byte sector address	(header error)
**   2 bytes header crc		(header error)
**  18 bytes header gap		(data error)
** 512 bytes data		(data error)
**   4 byte ecc			(data error)
**  ?? trailing pad		(data error)
*/

fmt_err smd_decode_position(bad_entry)
bs_entry bad_entry;
{
	fmt_err	error;
	int	sector_length = lab->d_traksize / lab->d_nsectors;
	int	offset = (bad_entry.bs_offset-2) % sector_length;
	int	bytes = ((bad_entry.bs_length / 8)) + 4;

	error.err_adr.cylinder = bad_entry.bs_cyl;
	error.err_adr.track = bad_entry.bs_trk;
	error.err_adr.sector = (bad_entry.bs_offset-2) / sector_length;
	if(error.err_adr.sector >= lab->d_nsectors) {
		error.err_adr.sector = lab->d_nsectors - 1;
		error.err_stat = DATA_ERROR;
	}
	else if((offset < 35) || ((offset+bytes) > sector_length))
		error.err_stat = HEADER_ERROR;
	else 
		error.err_stat = DATA_ERROR;
	return	error;
}


/*
**
*/

bs_entry smd_code_position(error)
fmt_err	error;
{
	int		sector_length = lab->d_traksize / lab->d_nsectors;
	bs_entry	temp;

	temp.bs_length = 1;
	temp.bs_cyl = error.err_adr.cylinder;
	temp.bs_trk = error.err_adr.track;
	temp.bs_offset = error.err_adr.sector * sector_length;
	if(error.err_stat & HEADER_ERROR) {
		temp.bs_offset += 1;
	}
	else {
		temp.bs_offset += 50;
	}
	temp.bs_alt.cylinder = 0;
	temp.bs_alt.track = 0;
	temp.bs_alt.sector = 0;
	temp.bs_how = scanning;
	return	temp;
}
