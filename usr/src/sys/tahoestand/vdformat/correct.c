#ifndef lint
static char sccsid[] = "@(#)correct.c	1.3 (Berkeley/CCI) 6/7/88";
#endif

#include	"vdfmt.h"
#include	"cmd.h"

/*
**
*/

correct()
{
	cur.state = cor;
	print("Making corrections to bad sector map on ");
	printf("controller %d, drive %d, ", cur.controller, cur.drive);
	printf("type %s.\n", lab->d_typename);

	indent();
	if(is_formatted() == true)
		if(read_bad_sector_map() == true) {
			get_corrections();
			cur.substate = sub_wmap;
			sync_bad_sector_map();
		}
		else 
			print("There is no bad sector map on this drive!\n");
	else
		print("Drive must be formatted befor corrections are done.\n");
	exdent(1);
}


/*
**
*/

cor_help()
{
	indent();
	print("Correction commands are in the following form:\n");
	indent();
	print("ID              -  Correct module serial number.\n");
	print("[a-h] (block)   -  UNIX file system format.\n");
	print("SEctor (sector) -  Absolute sector number on disk.\n");
	print("Track (track)   -  Absolute disk track number.\n");
	print("(cylinder) (head) (offset) (length) - CDC flaw map format.\n");
	print("CLEAR           -  Remove all relocations not from flaw map.\n");
	print("STARt           -  Ends correction process.\n\n");
	exdent(2);
}


/*
**
*/

get_corrections()
{
	extern int	id_help();
	char		line[256];
	char		*ptr;
	bs_entry	entry;
	dskadr		dskaddr;
	fmt_err		dskerr;
	int		max_track;
	register int	block;

	dskaddr.cylinder = lab->d_ncylinders - 1;
	dskaddr.cylinder = lab->d_ntracks - 1;
	max_track = to_track(dskaddr);
	indent();
	for(;;) {
		print("Location? ");
		get_string_cmd(line, cor_help);
		if(kill_processes == true)
			break;
		if(line[0] == '\0')
			continue;
		ptr = line;
		trim_white(ptr);
		if(!strncmp(ptr, "he", 2) || !strncmp(ptr, "?", 1) ||
		    !strncmp(ptr, "stat", 4) || !strncmp(ptr, "!", 1))
			continue;
		indent();
		if(!strncmp(ptr, "id", 2)) {
			register int	temp;

			for(;;) {
				print("Pack ID is %d. Change to? ",
				    bad_map->bs_id);
				temp = get_digit_cmd(id_help);
				if(temp > 0)
					break;
			}
			D_INFO->id = bad_map->bs_id = temp;
		} else if (!strcmp(ptr, "clear")) {
			print(
		     "Confirm removal of ALL relocations installed manually\n");
			if (get_yes_no("or by verification") == true)
				clear_relocations(true);
		}
		else if((*ptr  >= 'a') && (*ptr <= 'h')) {
			register char	par = *ptr++;

			block = get_next_digit(ptr);
			dskerr.err_adr = *from_unix((unsigned char)par,
			    (unsigned int)block);
			if((dskerr.err_adr.cylinder == -1) || (block == -1)) {
				print("Invalid UNIX block number!\n");
				goto	next;
			}
			print("Confirm block %d on file-system '%c'",block,par);
			dskerr.err_stat = DATA_ERROR;
		doreloc:
			printf(" (cn %d tn %d bn %d)", dskerr.err_adr.cylinder,
			    dskerr.err_adr.track, dskerr.err_adr.sector);
			if(get_yes_no("") == true) {
				(*C_INFO->code_pos)(&dskerr, &entry);
				remove_user_relocations(&entry);
			}
		}
		else if(*ptr == 't') {
			block = get_next_digit(ptr);
			if((block == -1) || (block >= max_track)) {
				print("Invalid track number!\n");
				goto	next;
			}
			dskerr.err_adr = *from_track(block);
			dskerr.err_stat = HEADER_ERROR;
			print("Confirm track %d", block);
			goto doreloc;
		}
		else if(!strncmp(ptr, "se", 2)) {
			block = get_next_digit(ptr);
			if (block == -1 ||
			    block > lab->d_nsectors*lab->d_ntracks*lab->d_ncylinders) {
				print("Invalid sector number!\n");
				goto	next;
			}
			dskerr.err_adr = *from_sector((unsigned int)block);
			dskerr.err_stat = DATA_ERROR;
			print("Confirm sector %d", block);
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
						remove_user_relocations(&entry);
				}
			}
			else
				goto bad;
		} else if(!strncmp(ptr, "star", 4)) {
			exdent(1);
			break;
		}
		else
bad:			print("What?\n");
next:		exdent(1);
	}
	exdent(1);
}

