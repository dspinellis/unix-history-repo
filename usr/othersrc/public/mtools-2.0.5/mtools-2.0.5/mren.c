/*
 * Rename an existing MSDOS file
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
	int entry, ismatch, nogo, fargn, verbose;
	char filename[13], *newfile, *strncpy(), *unix_name();
	char new[13], ans[10], *temp, *strcpy(), drive, get_drive();
	char *get_path(), *pathname, *get_name(), *fix_mcwd();
	unsigned char *target, *dos_name();
	void exit(), dir_write(), disk_flush(), dir_flush();
	struct directory *dir, *dir_read();

	fargn = 1;
	verbose = 0;
	if (argc > 1) {
		if (!strcmp(argv[1], "-v")) {
			fargn = 2;
			verbose = 1;
		}
	}
	if (argc != fargn + 2) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s [-v] sourcefile targetfile\n", argv[0]);
		exit(1);
	}
	mcwd = fix_mcwd();

	drive = get_drive(argv[1]);
	if (init(drive, 2)) {
		fprintf(stderr, "%s: Cannot initialize '%c:'\n", argv[0], drive);
		exit(1);
	}
	strcpy(filename, get_name(argv[fargn]));
	pathname = get_path(argv[fargn]);
	if (subdir(drive, pathname))
		exit(1);

	temp = get_name(argv[fargn + 1]);
	target = dos_name(argv[fargn + 1], verbose);

	strcpy(new, unix_name(target, target + 8));
	nogo = 0;
					/* the name supplied may be altered */
	if (strcmp(temp, new) && verbose) {
		while (!nogo) {
			printf("Do you accept \"%s\" as the new filename (y/n) ? ", new);
			gets(ans);
			if (ans[0] == 'y' || ans[0] == 'Y')
				break;
			if (ans[0] == 'n' || ans[0] == 'N')
				nogo = 1;
		}
	}
	if (nogo)
		exit(0);
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
					/* if volume label */
		if ((dir->attr & 0x08))
			continue;
					/* you may rename a directory */
		newfile = unix_name(dir->name, dir->ext);

					/* if the new name already exists */
		if (!strcmp(new, newfile)) {
			fprintf(stderr, "%s: File \"%s\" already exists\n", argv[0], new);
			exit(1);
		}
					/* if the old name exists */
		if (match(newfile, filename)) {
			ismatch = 1;
			break;
		}
	}
	if (!ismatch) {
		fprintf(stderr, "%s: File \"%s\" not found\n", argv[0], filename);
		exit(1);
	}
					/* so go ahead and do it */
	strncpy((char *) dir->name, (char *) target, 8);
	strncpy((char *) dir->ext, (char *) target + 8, 3);
	dir_write(entry, dir);

	dir_flush();
	disk_flush();
	close(fd);
	exit(0);
}
