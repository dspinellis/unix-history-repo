/*
 * Change MSDOS file attribute flags
 *
 * Emmet P. Gray			US Army, HQ III Corps & Fort Hood
 * ...!uunet!uiucuxc!fthood!egray	Attn: AFZF-DE-ENV
 * fthood!egray@uxc.cso.uiuc.edu	Directorate of Engineering & Housing
 * 					Environmental Management Office
 * 					Fort Hood, TX 76544-5057
 */

#include <stdio.h>
#include "msdos.h"
#include "patchlevel.h"

int fd = -1;				/* the file descriptor for the device */
int dir_start;				/* starting sector for directory */
int dir_len;				/* length of directory (in sectors) */
int dir_entries;			/* number of directory entries */
int clus_size;				/* cluster size (in sectors) */
char *mcwd;				/* the Current Working Directory */
int fat_error;				/* FAT error detected? */

#define ADD	1
#define REMOVE	(-1)
#define LEAVE	0

main(argc, argv)
int argc;
char *argv[];
{
	int entry, ismatch, oops, fargn, read_only, hidden, sys, archive;
	int i, action;
	char *filename, *newfile, *unix_name(), drive, get_drive();
	char *get_path(), *pathname, *get_name(), *fix_mcwd(), last_drive;
	void exit(), dir_write(), dir_flush(), disk_flush();
	struct directory *dir, *dir_read();

	oops = 0;
	fargn = -1;
	archive = LEAVE;
	hidden = LEAVE;
	read_only = LEAVE;
	sys = LEAVE;
					/* can't use getopt(3)... */
	for (i = 1; i < argc; i++) {
		switch (argv[i][0]) {
			case '-':
				action = REMOVE;
				break;
			case '+':
				action = ADD;
				break;
			default:
				fargn = i;
				break;
		}
		if (fargn != -1)
			break;

		switch (argv[i][1]) {
			case 'a':
			case 'A':
				archive = action;
				break;
			case 'h':
			case 'H':
				hidden = action;
				break;
			case 'r':
			case 'R':
				read_only = action;
				break;
			case 's':
			case 'S':
				sys = action;
				break;
			default:
				oops++;
				break;
		}
		if (oops)
			break;
	}
	if (argc < 3 || argv[fargn][0] == '\0' || oops) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s [-a|+a] [-h|+h] [-r|+r] [-s|+s] msdosfile [msdosfiles...]\n", argv[0]);
		exit(1);
	}
	last_drive = 'x';
	mcwd = fix_mcwd();

	for (i = fargn; i < argc; i++) {
		drive = get_drive(argv[i]);
		if (last_drive != drive) {
			if (init(drive, 2)) {
				fprintf(stderr, "%s: Cannot initialize '%c:'\n", argv[0], drive);
				continue;
			}
			last_drive = drive;
		}
		filename = get_name(argv[i]);
		pathname = get_path(argv[i]);
		if (subdir(drive, pathname))
			continue;

					/* see if exists and do it */
		ismatch = 0;
		for (entry = 0; entry < dir_entries; entry++) {
			dir = dir_read(entry);
					/* if empty */
			if (dir->name[0] == 0x0)
				break;
					/* if erased */
			if (dir->name[0] == 0xe5)
				continue;
					/* if dir or volume label */
			if ((dir->attr & 0x10) || (dir->attr & 0x08))
				continue;

			newfile = unix_name(dir->name, dir->ext);

					/* do it... */
			if (match(newfile, filename)) {
				if (archive == ADD)
					dir->attr |= 0x20;
				if (archive == REMOVE)
					dir->attr &= ~0x20;
				if (hidden == ADD)
					dir->attr |= 0x02;
				if (hidden == REMOVE)
					dir->attr &= ~0x02;
				if (read_only == ADD)
					dir->attr |= 0x01;
				if (read_only == REMOVE)
					dir->attr &= ~0x01;
				if (sys == ADD)
					dir->attr |= 0x04;
				if (sys == REMOVE)
					dir->attr &= ~0x04;
				dir_write(entry, dir);
				ismatch++;
			}
		}
		if (!ismatch)
			fprintf(stderr, "%s: File \"%s\" not found\n", argv[0], filename);
	}
	dir_flush();
	disk_flush();
	close(fd);
	exit(0);
}
