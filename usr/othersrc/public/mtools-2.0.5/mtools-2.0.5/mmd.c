/*
 * Make an MSDOS subdirectory
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
static void empty_dir();

main(argc, argv)
int argc;
char *argv[];
{
	int i, entry, slot, fargn, verbose, oops;
	extern unsigned int end_fat;
	unsigned int fat, dot, next_fat();
	char filename[13], *newfile, drive, get_drive(), *get_path();
	char *strcpy(), *fix_mcwd(), *pathname, *unix_name(), last_drive;
	unsigned char *fixed, *dos_name();
	void exit(), fat_write(), dir_write(), disk_flush(), dir_flush();
	struct directory *dir, *dir_read(), *mk_entry();
	long time(), now;
					/* catch signals */
	signal(SIGINT, (SIG_TYPE(*) ()) got_signal);
	signal(SIGTERM, (SIG_TYPE(*) ()) got_signal);
	signal(SIGQUIT, (SIG_TYPE(*) ()) got_signal);

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
	if (argc == 1 || oops) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s [-v] msdosdirectory [msdosdirectories...]\n", argv[0]);
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
					/* serves the get_name() function too */
		fixed = dos_name(argv[i], verbose);

		strcpy(filename, unix_name(fixed, fixed + 8));
		pathname = get_path(argv[i]);

		if (subdir(drive, pathname))
			continue;
					/* see if exists and get slot */
		slot = -1;
		dot = 0;
		oops = 0;
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
					/* if not a directory */
			if (!(dir->attr & 0x10))
				continue;

			newfile = unix_name(dir->name, dir->ext);
					/* save the 'dot' directory info */
			if (!strcmp(".", newfile))
				dot = dir->start[1] * 0x100 + dir->start[0];

			if (!strcmp(filename, newfile)) {
				fprintf(stderr, "%s: Directory \"%s\" already exists\n", argv[0], filename);
				oops++;
				break;
			}
		}
		if (oops)
			continue;
					/* no '.' entry means root directory */
		if (dot == 0 && slot < 0) {
			fprintf(stderr, "%s: No directory slots\n", argv[0]);
			continue;
		}
					/* make the directory grow */
		if (dot && slot < 0) {
			if (dir_grow(dot)) {
				fprintf(stderr, "%s: Disk full\n", argv[0]);
				continue;
			}
					/* first slot in the new part */
			slot = entry;
		}
					/* grab a starting cluster */
		if ((fat = next_fat(0)) == 1) {
			fprintf(stderr, "%s: Disk full\n", argv[0]);
			continue;
		}
					/* make directory entry */
		time(&now);
		dir = mk_entry(fixed, 0x10, fat, 0L, now);
		dir_write(slot, dir);
					/* write the cluster */
		empty_dir(fat, dot);
		fat_encode(fat, end_fat);
	}
					/* write the FAT, flush the buffers */
	fat_write();
	dir_flush();
	disk_flush();
	close(fd);
	exit(0);
}

/*
 * Write an empty directory 'template' to the cluster starting at 'dot'.
 */

static void
empty_dir(dot, dot_dot)
unsigned int dot, dot_dot;
{
	int buflen;
	long start;
	char buf[MAX_CLUSTER], *memcpy(), *memset();
	struct directory *dir, *mk_entry();
	void disk_write();
	long time(), now;

	start = (long) (dot - 2) * clus_size + dir_start + dir_len;

	buflen = clus_size * MSECTOR_SIZE;
	time(&now);
					/* make the '.' and '..' entries */
	dir = mk_entry((unsigned char *) ".          ", 0x10, dot, 0L, now);
	memcpy(&buf[0], (char *) dir, MDIR_SIZE);
	dir = mk_entry((unsigned char *) "..         ", 0x10, dot_dot, 0L, now);
	memcpy(&buf[MDIR_SIZE], (char *) dir, MDIR_SIZE);

					/* zero the remainder */
	memset(&buf[2 * MDIR_SIZE], '\0', buflen - (2 * MDIR_SIZE));

					/* write the cluster */
	disk_write(start, (unsigned char *) buf, buflen);
	return;
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
