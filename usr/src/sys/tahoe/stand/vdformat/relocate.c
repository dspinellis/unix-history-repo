#ifndef lint
static char sccsid[] = "@(#)relocate.c	1.7 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"
#include	"cmd.h"

/*
**
*/

relocate()
{
	extern boolean	is_formatted();

	cur.state = rel;
	print("Adding flaws to bad sector map on ");
	printf("controller %d, drive %d, ", cur.controller, cur.drive);
	printf("type %s.\n", lab->d_typename);

	indent();
	if(is_formatted() == true) {
		if(read_bad_sector_map() == true)
			if(bad_map->bs_id != D_INFO->id) {
				print("Drive serial numbers do not match!\n");
				exdent(1);
				_longjmp(abort_environ, 1);
			}
		get_new_relocations();
		cur.substate = sub_wmap;
		sync_bad_sector_map();
	}
	else
		print("Drive must be formatted befor relocations are done.\n");
	exdent(1);
}


/*
**
*/

rel_help()
{
	indent();
	print("Relocation commands are in the following form:\n");
	indent();
	print("[a-h] (block)   -  UNIX file system block (%s-byte) number.\n",
	    DEV_BSIZE);
	print("SEctor (sector) -  Absolute sector number on disk.\n");
	print("Track (track)   -  Absolute disk track number.\n");
	print("(cylinder) (head) (offset) (length) - CDC flaw map format.\n");
	print("STARt           -  Starts relocation process.\n\n");
	exdent(2);
}


/*
**
*/

get_new_relocations()
{
	char		line[256];
	char		*ptr;
	bs_entry	entry;
	dskadr		dskaddr;
	fmt_err		dskerr;
	int		max_track;
	register int	block;

	dskaddr.cylinder = lab->d_ncylinders - 1;
	dskaddr.track = lab->d_ntracks - 1;
	max_track = to_track(dskaddr);
	for(;;) {
		print("Location? ");
		get_string_cmd(line, rel_help);
		if(kill_processes == true)
			_longjmp(quit_environ, 1);
		if(line[0] == '\0')
			continue;
		ptr = line;
		trim_white(ptr);
		if(!strncmp(ptr, "he", 2) || !strncmp(ptr, "?", 1) ||
		    !strncmp(ptr, "stat", 4) || !strncmp(ptr, "!", 1))
			continue;
		indent();
		if((*ptr >= 'a') && (*ptr <= 'h')) {
			register char	par = *(ptr++);

			block = get_next_digit(ptr);
			dskerr.err_adr = *from_unix((unsigned char)par,
			    (unsigned int)block);
			if((dskerr.err_adr.cylinder == -1) || (block == -1)) {
				print("Invalid UNIX block number!\n");
				goto next;
			}
			print("Confirm block %d on file-system '%c'",block,par);
			dskerr.err_stat = DATA_ERROR;
		doreloc:
			printf(" (cn %d tn %d sn %d)", dskerr.err_adr.cylinder,
			    dskerr.err_adr.track, dskerr.err_adr.sector);
			if(get_yes_no("") == true) {
				(*C_INFO->code_pos)(&dskerr, &entry);
				add_user_relocations(&entry);
			}
		}
		else if(*ptr == 't') {
			block = get_next_digit(ptr);
			if((block == -1) || (block >= max_track)) {
				print("Invalid track number!\n");
				goto next;
			}
			dskerr.err_adr = *from_track(block);
			dskerr.err_stat = HEADER_ERROR;
			print("Confirm track %d", block);
			goto doreloc;
		}
		else if(!strncmp(ptr, "se", 2)) {
			block = get_next_digit(ptr);

			if((block == -1) ||
			    ((lab->d_nsectors*lab->d_ntracks*lab->d_ncylinders)<block)){
				print("Invalid sector number!\n");
				goto next;
			}
			dskerr.err_adr = *from_sector((unsigned int)block);
			dskerr.err_stat = DATA_ERROR;
			goto doreloc;
		}
		else if(is_digit(*ptr)) {
			entry.bs_cyl = get_next_digit(ptr);
			skipdigits(ptr);
			finddigit(ptr);
			entry.bs_trk = get_next_digit(ptr);
			skipdigits(ptr);
			finddigit(ptr);
			entry.bs_offset = get_next_digit(ptr);
			skipdigits(ptr);
			finddigit(ptr);
			entry.bs_length = get_next_digit(ptr);
			if((entry.bs_trk != -1) && (entry.bs_offset != -1) &&
			    (entry.bs_length != -1)) {
				if(entry.bs_cyl >= lab->d_ncylinders)
					print("Cylinder number to high!\n");
				else if(entry.bs_trk >= lab->d_ntracks)
					print("Head number to high!\n");
				else if(entry.bs_offset >= lab->d_traksize)
					print("Offset too long!\n");
				else if(entry.bs_length == 0)
					print("Can't have a 0 length error!\n");
				else {
					print("Confirm  Cyl %d, ",entry.bs_cyl);
					printf("Head %d, ", entry.bs_trk);
					printf("offset %d, ", entry.bs_offset);
					printf("length %d", entry.bs_length);
					if(get_yes_no("") == true)
						add_user_relocations(&entry);
				}
			}
			else
				goto bad;
		}
		else if(!strncmp(ptr, "start", 4)) {
			exdent(1);
			break;
		}
		else
bad:			print("What?\n");
next:		exdent(1);
	}
}

dskadr check_track_for_relocations(entry, i)
bs_entry	entry;
register int	i;
{
	register int	j = i;
	fmt_err		temp, cmp;
	boolean		bad_track = false;

	(*C_INFO->decode_pos)(&entry, &cmp);
	/* Check to see if a alternate track is or will be on this track */
	while((bad_map->list[j].bs_cyl == entry.bs_cyl) &&
	    (bad_map->list[j].bs_trk == entry.bs_trk)) {
		(*C_INFO->decode_pos)(&bad_map->list[j], &temp);
		if(temp.err_stat & HEADER_ERROR) {
			bad_track = true;
			/* if track was mapped out (it can't be us) */
			if(((bad_map->list[j].bs_alt.cylinder != 0)) ||
		    	    (bad_map->list[j].bs_alt.track != 0) ||
		    	    (bad_map->list[j].bs_alt.sector != 0)) {
				return cmp.err_adr;
			}
		}
		j++;
	}
	/*
	**    If it was a bad track and it was not the current entry
	** that produced it then then map it
	** to itself and forget about it for now since it will be taken
	** care of later.
	**
	**    If it was the current entry return zero and the track will be
	** mapped out correctly.
	*/
	if(bad_track == true) {
		if(cmp.err_stat & HEADER_ERROR)
			return entry.bs_alt; /* better known as zero */
		else
			return cmp.err_adr;
	}
	/*
	**   if we made it through all the bad track stuff then check for
	** multiple errors on the same sector that are already mapped!
	*/
	j = i;
	while((bad_map->list[j].bs_cyl == entry.bs_cyl) &&
	    (bad_map->list[j].bs_trk == entry.bs_trk)) {
		(*C_INFO->decode_pos)(&bad_map->list[j], &temp);
		if(temp.err_adr.sector == cmp.err_adr.sector) {
			/* if it is not really the current entry */
			if((bad_map->list[j].bs_offset != entry.bs_offset) ||
			    (bad_map->list[j].bs_length != entry.bs_length)) { 
				/* if the sector is already mapped out */
				if(((bad_map->list[j].bs_alt.cylinder != 0)) ||
		   	 	    (bad_map->list[j].bs_alt.track != 0) ||
		    		    (bad_map->list[j].bs_alt.sector != 0)) {
					return temp.err_adr;
				}
			}
		}
		j++;
	}
	return	entry.bs_alt;
}


/*
**
*/

dskadr	is_relocated(entry)
bs_entry	entry;
{
	register int	i;

	for(i=0; i<bad_map->bs_count; i++)
		if((bad_map->list[i].bs_cyl == entry.bs_cyl) &&
		    (bad_map->list[i].bs_trk == entry.bs_trk))
			return check_track_for_relocations(entry, i);
	return entry.bs_alt;
}



/*
**
*/

sync_bad_sector_map()
{
	register int	i;
	dskadr		dskaddr;

	/*
	** do all the relocation cylinders first to allocate all flaws in
	** relocation area.
	*/
	for(i=bad_map->bs_count-1; i>=0; i--) {
		if((bad_map->list[i].bs_cyl >= lab->d_ncylinders-NUMSYS) &&
		    (bad_map->list[i].bs_cyl < lab->d_ncylinders-NUMMAP-NUMMNT)) {
			if((bad_map->list[i].bs_alt.cylinder == 0) &&
			    (bad_map->list[i].bs_alt.track == 0) &&
			    (bad_map->list[i].bs_alt.sector == 0)) {
				bad_map->list[i].bs_alt = 
				    *new_location(&bad_map->list[i]);
			}
		}
	}
	for(i=bad_map->bs_count-1; i>=0; i--) {
		if((bad_map->list[i].bs_alt.cylinder == 0) &&
		    (bad_map->list[i].bs_alt.track == 0) &&
		    (bad_map->list[i].bs_alt.sector == 0)) {
			dskaddr = is_relocated(bad_map->list[i]);
			if((dskaddr.cylinder == 0) && (dskaddr.track == 0) &&
			    (dskaddr.sector == 0)) {
				bad_map->list[i].bs_alt = 
				    *new_location(&bad_map->list[i]);
				do_relocation(bad_map->list[i]);
			}
			else
				bad_map->list[i].bs_alt = dskaddr;
		}
	}
	write_bad_sector_map();
}



/*
**
*/

do_relocation(entry)
bs_entry	entry;
{
	fmt_err	temp;

	if(entry.bs_cyl >= lab->d_ncylinders-NUMSYS)
		if(entry.bs_cyl != (lab->d_ncylinders - NUMMAP - NUMMNT))
			return;
	(*C_INFO->decode_pos)(&entry, &temp);
	if((entry.bs_alt.cylinder == 0) && (entry.bs_alt.track == 0) &&
	    (entry.bs_alt.sector == 0))
		print_unix_block(temp.err_adr);
	else if(temp.err_stat & HEADER_ERROR)
		if(C_INFO->type == VDTYPE_VDDC) {
			print("Can't relocate tracks on VDDC controllers.\n");
			print_unix_block(temp.err_adr);
		}
		else
			relocate_track(entry);
	else
		relocate_sector(entry);
}


/*
**
*/

relocate_sector(entry)
bs_entry	entry;
{
	dskadr		phys, reloc;
	fmt_err		temp;
	register long	status;

	(*C_INFO->decode_pos)(&entry, &temp);
	phys = temp.err_adr;
	reloc = entry.bs_alt;
	format_sectors(&phys, &reloc, RELOC_SECTOR, (long)1);
	
	format_sectors(&reloc, &phys, ALT_SECTOR, (long)1);
	status = access_dsk((char *)save, &temp.err_adr, VDOP_WD, 1, 1);
	if(!((status & DCBS_ATA) && !(status & (DCBS_HARD|DCBS_SOFT)))) {
		print(
		"Sector relocation failed (c %d t %d s %d).  Status = 0x%x.\n",
		    phys.cylinder, phys.track, phys.sector, status);
		print_unix_block(phys);
	}
}



/*
**
*/

relocate_track(entry)
bs_entry	entry;
{
	dskadr		phys, reloc;
	fmt_err		temp;
	register long	status;

	(*C_INFO->decode_pos)(&entry, &temp);
	temp.err_adr.sector = 0;
	phys = temp.err_adr;
	reloc = entry.bs_alt;
	reloc.sector = 0xff;
	format_sectors(&phys, &reloc, RELOC_SECTOR, (long)lab->d_nsectors);
	
	reloc.sector = 0x00;
	format_sectors(&reloc, &phys, ALT_SECTOR, (long)lab->d_nsectors);
	status = access_dsk((char *)save, &temp.err_adr, VDOP_WD, lab->d_nsectors, 1);
	if(!((status & DCBS_ATA) && !(status & (DCBS_HARD|DCBS_SOFT)))) {
		print("Track relocation failed.  Status = 0x%x.\n", status);
		print_unix_block(phys);
	}
}
