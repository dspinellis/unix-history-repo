/*
 * Display contents of an MSDOS file
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

main(argc, argv)
int argc;
char *argv[];
{
	extern int optind;
	extern char *optarg;
	int i, ismatch, entry, c, oops, textmode, stripmode;
	unsigned int fat;
	long size;
	char *filename, *newfile, *get_name(), *unix_name(), *pathname;
	char *get_path(), drive, get_drive(), last_drive, *fix_mcwd();
	void exit();
	struct directory *dir, *dir_read();

					/* get command line options */
	oops = 0;
	stripmode = 0;
	textmode = 0;
	while ((c = getopt(argc, argv, "st")) != EOF) {
		switch (c) {
			case 's':
				stripmode = 1;
				break;
			case 't':
				textmode = 1;
				break;
			default:
				oops = 1;
				break;
		}
	}

	if (oops || (argc - optind) < 1) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s [-st] msdosfile [msdosfiles...]\n", argv[0]);
		exit(1);
	}
	last_drive = 'x';
	mcwd = fix_mcwd();

	for (i = optind; i < argc; i++) {
		drive = get_drive(argv[i]);
		if (drive != last_drive) {
			if (init(drive, 0)) {
				fprintf(stderr, "%s: Cannot initialize '%c:'\n", argv[0], drive);
				continue;
			}
			last_drive = drive;
		}
		filename = get_name(argv[i]);
		pathname = get_path(argv[i]);
		if (subdir(drive, pathname))
			continue;

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

					/* see it if matches the pattern */
			if (match(newfile, filename)) {
				fat = dir->start[1] * 0x100 + dir->start[0];
				size = dir->size[3] * 0x1000000L + dir->size[2] * 0x10000L + dir->size[1] * 0x100 + dir->size[0];
				if (file_read(stdout, fat, textmode, stripmode, size))
					break;
				ismatch = 1;
			}
		}
		if (fat_error)
			break;

		if (!ismatch)
			fprintf(stderr, "%s: File \"%s\" not found\n", argv[0], filename);
	}
	close(fd);
	exit(0);
}

/*
 * stubs for read-only programs
 */

void
disk_flush()
{
	extern int disk_dirty;

	disk_dirty = 0;
	return;
}

void
dir_flush()
{
	extern int dir_dirty;

	dir_dirty = 0;
	return;
}
