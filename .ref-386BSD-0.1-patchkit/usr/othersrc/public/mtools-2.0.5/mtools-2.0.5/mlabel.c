/*
 * Make an MSDOS volume label
 *
 * Emmet P. Gray			US Army, HQ III Corps & Fort Hood
 * ...!uunet!uiucuxc!fthood!egray	Attn: AFZF-DE-ENV
 * fthood!egray@uxc.cso.uiuc.edu	Directorate of Engineering & Housing
 * 					Environmental Management Office
 * 					Fort Hood, TX 76544-5057
 */

#include <stdio.h>
#include <ctype.h>
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
	int entry, slot, fargn, verbose, oops;
	char filename[30], *strncpy(), drive, ans[10], *strncat();
	char *strcpy(), *fix_mcwd();
	unsigned char fixed[12], vol[12];
	void exit(), dir_write(), dir_flush(), disk_flush();
	struct directory *dir, *dir_read(), *mk_entry();
	long time(), now;

	fargn = 1;
	verbose = 0;
	oops = 0;
	if (argc > 1) {
		if (!strcmp(argv[1], "-v")) {
			fargn = 2;
			verbose = 1;
		}
		if (argv[1][0] == '-' && !verbose)
			oops++;
	}
	if (argc < 2 || argv[fargn][1] != ':' || oops) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s [-v] drive:\n", argv[0]);
		exit(1);
	}
	mcwd = fix_mcwd();

	drive = argv[fargn][0];
	if (islower(drive))
		drive = toupper(drive);

	if (init(drive, 2)) {
		fprintf(stderr, "%s: Cannot initialize '%c:'\n", argv[0], drive);
		exit(1);
	}
					/* see if a label exists and get slot */
	slot = -1;
	vol[0] = '\0';
	for (entry = 0; entry < dir_entries; entry++) {
		dir = dir_read(entry);
					/* if empty */
		if (dir->name[0] == 0x0) {
			if (slot < 0)
				slot = entry;
			break;
		}
					/* if erased */
		if (dir->name[0] == 0xe5) {
			if (slot < 0)
				slot = entry;
			continue;
		}
					/* if not a volume label */
		if (!(dir->attr & 0x08))
			continue;

		slot = entry;
		strncpy((char *) vol, (char *) dir->name, 8);
		vol[8] = '\0';
		strncat((char *) vol, (char *) dir->ext, 3);
		vol[11] = '\0';

		printf("Volume in drive %c is \"%s\"\n", drive, vol);
		break;
	}
	if (slot < 0) {
		fprintf(stderr, "%s: No directory slots\n", argv[0]);
		exit(1);
	}
	if (vol[0] == '\0')
		printf("Volume in drive %c is unlabeled\n", drive);

					/* ask for new label */
	printf("Enter the new volume label (11 characters): ");
	gets(filename);
	if (filename[0] != '\0') {
		sprintf((char *) fixed, "%-11.11s", filename);
		if (strlen(filename) > 11 && verbose)
			printf("New label is \"%s\"\n", fixed);
	}
	else {
		if (vol[0] == '\0')
			exit(0);

		printf("Delete volume label (y/n): ");
		gets(ans);
		if (ans[0] == 'y' || ans[0] == 'Y') {
			strcpy((char *) fixed, (char *) vol);
			fixed[0] = 0xe5;
		}
		else
			exit(0);
	}
					/* make directory entry */
	time(&now);
	dir = mk_entry(fixed, 0x08, 0, 0L, now);
	dir_write(slot, dir);

	dir_flush();
	disk_flush();
	close(fd);
	exit(0);
}
