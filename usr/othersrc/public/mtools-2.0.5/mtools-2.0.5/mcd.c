/*
 * Change MSDOS directories
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
	FILE *fp;
	char *fix_mcwd(), *strcpy(), newpath[MAX_PATH], *get_name();
	char *get_path(), *pathname, *filename, drive, *strcat();
	char get_drive(), *mcwd_path, *getenv(), *expand();
	void exit();

	if (argc > 2) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s: msdosdirectory\n", argv[0]);
		exit(1);
	}
					/* only report the mcwd */
	mcwd = fix_mcwd();
	if (argc == 1) {
		printf("%s\n", mcwd);
		exit(0);
	}

	drive = get_drive(argv[1]);
	filename = get_name(argv[1]);
	pathname = get_path(argv[1]);

	if (init(drive, 0)) {
		fprintf(stderr, "%s: Cannot initialize '%c:'\n", argv[0], drive);
		exit(1);
	}

	/*
	 * Move to "first guess" subdirectory, so that is_dir() can
	 * search to see if filename is also a directory.
	 */
	if (subdir(drive, pathname)) {
		fprintf(stderr, "%s: Directory not found\n", argv[0]);
		exit(1);
	}
					/* is filename really a subdirectory? */
	strcpy(newpath, pathname);
	if (is_dir(filename)) {
		if (newpath[strlen(newpath) -1] != '/')
			strcat(newpath, "/");
		strcat(newpath, filename);

					/* move to real subdirectory */
		if (subdir(drive, newpath)) {
			fprintf(stderr, "%s: Directory not found\n", argv[0]);
			exit(1);
		}
	}
	else {
		if (*filename != '\0') {
			fprintf(stderr, "%s: Directory not found\n", argv[0]);
			exit(1);
		}
	}
					/* it checked out ok, so save it */
	mcwd_path = getenv("MCWD");
	if (mcwd_path == NULL || *mcwd_path == '\0')
		mcwd_path = "$HOME/.mcwd";

	if (!(fp = fopen(expand(mcwd_path), "w"))) {
		fprintf(stderr, "%s: Can't open '%s' for write\n", argv[0], expand(mcwd_path));
		exit(1);
	}
	fprintf(fp, "%c:%s\n", drive, newpath);
	fclose(fp);
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
