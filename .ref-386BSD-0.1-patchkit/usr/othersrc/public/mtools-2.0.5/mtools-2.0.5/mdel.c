/*
 * Delete an MSDOS file
 *
 * Emmet P. Gray			US Army, HQ III Corps & Fort Hood
 * ...!uunet!uiucuxc!fthood!egray	Attn: AFZF-DE-ENV
 * fthood!egray@uxc.cso.uiuc.edu	Directorate of Engineering & Housing
 * 					Environmental Management Office
 * 					Fort Hood, TX 76544-5057
 */

#include <stdio.h>
#include <signal.h>
#include "msdos.h"
#include "patchlevel.h"

int fd = -1;				/* the file descriptor for the device */
int dir_start;				/* starting sector for directory */
int dir_len;				/* length of directory (in sectors) */
int dir_entries;			/* number of directory entries */
int clus_size;				/* cluster size (in sectors) */
char *mcwd;				/* the Current Working Directory */
int fat_error;				/* FAT error detected? */

static int got_signal();

main(argc, argv)
int argc;
char *argv[];
{
	int i, ismatch, entry, nogo, verbose, fargn;
	unsigned int start;
	char *filename, *newfile, *get_name(), *unix_name(), *get_path();
	char *pathname, ans[10], drive, get_drive(), last_drive, *fix_mcwd();
	void exit(), fat_write(), dir_write(), dir_flush(), disk_flush();
	struct directory *dir, *dir_read();
					/* catch signals */
	signal(SIGINT, (SIG_TYPE(*) ()) got_signal);
	signal(SIGTERM, (SIG_TYPE(*) ()) got_signal);
	signal(SIGQUIT, (SIG_TYPE(*) ()) got_signal);

	if (argc > 1 && !strcmp(argv[1], "-v")) {
		verbose = 1;
		fargn = 2;
	}
	else {
		verbose = 0;
		fargn = 1;
	}
	if (argc < 2 || (argv[1][0] == '-' && !verbose)) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s [-v] msdosfile [msdosfiles...]\n", argv[0]);
		exit(1);
	}
	last_drive = 'x';
	mcwd = fix_mcwd();

	for (i = fargn; i < argc; i++) {
		drive = get_drive(argv[i]);
		if (drive != last_drive) {
			if (last_drive != 'x') {
				fat_write();
				dir_flush();
				disk_flush();
			}

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

		nogo = 0;
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
				if (verbose)
					printf("Removing %s\n", newfile);
				ismatch = 1;
				if (dir->attr & 0x01) {
					while (!nogo) {
						printf("%s: \"%s\" is read only, erase anyway (y/n) ? ", argv[0], newfile);
						gets(ans);
						if (ans[0] == 'y' || ans[0] == 'Y')
							break;
						if (ans[0] == 'n' || ans[0] == 'N')
							nogo = 1;
					}
					if (nogo)
						continue;
				}
				start = dir->start[1] * 0x100 + dir->start[0];
				if (fat_free(start))
					break;
				dir->name[0] = 0xe5;
				dir_write(entry, dir);
			}
		}
		if (fat_error)
			break;

		if (!ismatch)
			fprintf(stderr, "%s: File \"%s\" not found\n", argv[0], filename);
	}
					/* write the FAT, flush the buffers */
	fat_write();
	dir_flush();
	disk_flush();
	close(fd);
	exit(0);
}

/*
 * Do a graceful exit if the program is interrupted.  This will reduce
 * (but not eliminate) the risk of generating a corrupted disk on
 * a user abort.
 */

static int
got_signal()
{
	void exit(), disk_flush(), fat_write(), dir_flush();

	if (fd < 0)
		exit(1);
	fat_write();
	dir_flush();
	disk_flush();
	close(fd);
	exit(1);
}
