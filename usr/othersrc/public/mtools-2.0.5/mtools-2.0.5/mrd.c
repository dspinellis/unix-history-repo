/*
 * Delete an MSDOS subdirectory
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

static int got_signal(), is_empty();

main(argc, argv)
int argc;
char *argv[];
{
	int i, ismatch, entry, oops, empty;
	unsigned int start;
	char *filename, *newfile, *get_name(), drive, *pathname, *get_path();
	char *unix_name(), get_drive(), last_drive, *fix_mcwd();
	void exit(), fat_write(), dir_write(), disk_flush(), dir_flush();
	struct directory *dir, *dir_read();
					/* catch signals */
	signal(SIGINT, (SIG_TYPE(*) ()) got_signal);
	signal(SIGTERM, (SIG_TYPE(*) ()) got_signal);
	signal(SIGQUIT, (SIG_TYPE(*) ()) got_signal);

	if (argc == 1) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s mdsosdirectory [msdosdirectories...]\n", argv[0]);
		exit(1);
	}

	last_drive = 'x';
	mcwd = fix_mcwd();

	for (i = 1; i < argc; i++) {
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

		oops = 0;
		ismatch = 0;
		for (entry = 0; entry < dir_entries; entry++) {
			dir = dir_read(entry);
					/* if empty */
			if (dir->name[0] == 0x0)
				break;
					/* if erased */
			if (dir->name[0] == 0xe5)
				continue;
					/* if not dir */
			if (!(dir->attr & 0x10))
				continue;

			newfile = unix_name(dir->name, dir->ext);
			if (match(newfile, filename)) {
				start = dir->start[1] * 0x100 + dir->start[0];
				if ((empty = is_empty(start)) < 0)
					break;
				if (!empty) {
					fprintf(stderr, "%s: Directory \"%s\" is not empty\n", argv[0], filename);
					oops++;
					break;
				}
				if (!start) {
					fprintf(stderr, "%s: Can't remove root directory\n", argv[0]);
					oops++;
					break;
				}
				if (fat_free(start))
					break;
				dir->name[0] = 0xe5;
				dir_write(entry, dir);
				ismatch = 1;
			}
		}
		if (fat_error)
			break;

		if (oops)
			continue;

		if (!ismatch)
			fprintf(stderr, "%s: Directory \"%s\" not found\n", argv[0], filename);
	}
					/* write the FAT, flush the buffers */
	fat_write();
	dir_flush();
	disk_flush();
	close(fd);
	exit(0);
}

/*
 * See if directory is empty.  Returns 1 if empty, 0 if not, and -1 on error.
 * Can't use subdir() and dir_read() as it would clobber the globals.
 */

static int
is_empty(fat)
unsigned int fat;
{
	register int i;
	int next, buflen;
	long sector;
	extern unsigned int last_fat, fat_decode();
	unsigned char tbuf[MAX_CLUSTER];
	void disk_read();

	/* CONSTCOND */
	while (1) {
		sector = (long) (fat - 2) * clus_size + dir_start + dir_len;
		buflen = clus_size * MSECTOR_SIZE;
		disk_read(sector, tbuf, buflen);

					/* check first character of name */
		for (i = 0; i < MSECTOR_SIZE; i += MDIR_SIZE) {
			if (tbuf[i] == '.')
				continue;
			if (tbuf[i] != 0x0 && tbuf[i] != 0xe5)
				return(0);
		}
					/* get next cluster number */
		next = fat_decode(fat);
		if (next == 1) {
			fprintf(stderr, "is_empty: FAT problem\n");
			return(-1);
		}
					/* end of cluster chain */
		if (next >= last_fat)
			break;
		fat = next;
	}
	return(1);
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
