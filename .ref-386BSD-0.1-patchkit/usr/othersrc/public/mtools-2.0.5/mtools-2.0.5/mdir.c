/*
 * Display an MSDOS directory
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

static long getfree();
static char *conv_date(), *conv_time();

main(argc, argv)
int argc;
char *argv[];
{
	int i, entry, files, fargn, wide, faked;
	long size, blocks;
	char *date, *time, last_drive, *fix_mcwd();
	char *strncpy(), newpath[MAX_PATH], *get_name(), *get_path(), *pathname;
	char *newfile, *filename, *unix_name(), volume[12], drive, *strpbrk();
	char *strcpy(), *strcat(), newname[13], *strncat(), get_drive();
	void exit();
	struct directory *dir, *dir_read();

	fargn = 1;
	wide = 0;
	files = 0;
					/* first argument */
	if (argc > 1) {
		if (!strcmp(argv[1], "-w")) {
			wide = 1;
			fargn = 2;
		}
		if (argv[1][0] == '-' && !wide) {
			fprintf(stderr, "%s: illegal option -- %c\n", argv[0], argv[1][1]);
			fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
			fprintf(stderr, "Usage: %s: [-w] msdosdirectory\n", argv[0]);
			fprintf(stderr, "       %s: [-w] msdosfile [msdosfiles...]\n", argv[0]);
			exit(1);
		}
	}
					/* fake an argument */
	faked = 0;
	if (argc == fargn) {
		faked++;
		argc++;
	}
	last_drive = 'x';
	mcwd = fix_mcwd();

	for (i = fargn; i < argc; i++) {
		if (faked) {
			drive = get_drive("");
			filename = get_name("");
			pathname = get_path("");
		}
		else {
			drive = get_drive(argv[i]);
			filename = get_name(argv[i]);
			pathname = get_path(argv[i]);
		}
					/* is this a new device? */
		if (drive != last_drive) {
			if (last_drive != 'x') {
				blocks = getfree() * MSECTOR_SIZE;
				if (!files)
					printf("File \"%s\" not found\n\n", newname);
				else
					printf("     %3d File(s)     %6ld bytes free\n\n", files, blocks);
			}
			if (init(drive, 0)) {
				fprintf(stderr, "%s: Cannot initialize '%c:'\n", argv[0], drive);
				continue;
			}
			last_drive = drive;
			files = 0;
					/* find the volume label */
			volume[0] = '\0';
			for (entry = 0; entry < dir_entries; entry++) {
				dir = dir_read(entry);

					/* if empty */
				if (dir->name[0] == 0x0)
					break;

					/* if erased */
				if (dir->name[0] == 0xe5)
					continue;

					/* if not volume label */
				if (!(dir->attr & 0x08))
					continue;

				strncpy(volume, (char *) dir->name, 8);
				volume[8] = '\0';
				strncat(volume, (char *) dir->ext, 3);
				volume[11] = '\0';
				break;
			}
			if (volume[0] == '\0')
				printf(" Volume in drive %c has no label\n", drive);
			else
				printf(" Volume in drive %c is %s\n", drive, volume);
		}

		/*
		 * Move to "first guess" subdirectory, so that is_dir() can
		 * search to see if filename is also a directory.
		 */
		if (subdir(drive, pathname))
			continue;

		/*
		 * Under MSDOS, wildcards that match directories don't
		 * display the contents of that directory.  So I guess I'll
		 * do that too.
		 */
		if ((strpbrk(filename, "*[?") == NULL) && is_dir(filename)) {
			strcpy(newpath, pathname);
			if (newpath[strlen(newpath) -1] != '/')
				strcat(newpath, "/");
			strcat(newpath, filename);

					/* move to real subdirectory */
			if (subdir(drive, newpath))
				continue;

			strcpy(newname, "*");
		}
		else {
			strcpy(newpath, pathname);
			strcpy(newname, filename);
		}
					/* if no files, assume '*' */
		if (*filename == '\0')
			strcpy(newname, "*");

		printf(" Directory for %c:%s\n\n", drive, newpath);

		for (entry = 0; entry < dir_entries; entry++) {
			dir = dir_read(entry);
					/* if empty */
			if (dir->name[0] == 0x0)
				break;
					/* if erased */
			if (dir->name[0] == 0xe5)
				continue;
					/* if a volume label */
			if (dir->attr & 0x08)
				continue;

			newfile = unix_name(dir->name, dir->ext);
			if (!match(newfile, newname))
				continue;

			files++;
			if (wide && files != 1) {
				if (!((files - 1) % 5))
					putchar('\n');
			}
			date = conv_date(dir->date[1], dir->date[0]);
			time = conv_time(dir->time[1], dir->time[0]);
			size = dir->size[3] * 0x1000000L + dir->size[2] * 0x10000L + dir->size[1] * 0x100 + dir->size[0];
					/* is a subdirectory */
			if (dir->attr & 0x10) {
				if (wide)
					printf("%-8.8s %-3.3s   ", dir->name, dir->ext);
				else
					printf("%-8.8s %-3.3s     <DIR>     %s  %s\n", dir->name, dir->ext, date, time);
				continue;
			}
			if (wide)
				printf("%-8.8s %-3.3s   ", dir->name, dir->ext);
			else
				printf("%-8.8s %-3.3s    %8ld   %s  %s\n", dir->name, dir->ext, size, date, time);
		}
		if (argc > 2)
			putchar('\n');
	}
	if (fd < 0)
		exit(1);

	blocks = getfree() * MSECTOR_SIZE;
	if (!files)
		printf("File \"%s\" not found\n", newname);
	else
		printf("     %3d File(s)     %6ld bytes free\n", files, blocks);
	close(fd);
	exit(0);
}

/*
 * Get the amount of free space on the diskette
 */

static long
getfree()
{
	register unsigned int i;
	long total;
	extern unsigned int num_clus;
	unsigned int fat_decode();

	total = 0L;
	for (i = 2; i < num_clus + 2; i++) {
					/* if fat_decode returns zero */
		if (!fat_decode(i))
			total += clus_size;
	}
	return(total);
}

/*
 * Convert an MSDOS directory date stamp to ASCII
 */

static char *
conv_date(date_high, date_low)
unsigned date_high, date_low;
{
/*
 *	    hi byte     |    low byte
 *	|7|6|5|4|3|2|1|0|7|6|5|4|3|2|1|0|
 *      | | | | | | | | | | | | | | | | |
 *      \   7 bits    /\4 bits/\ 5 bits /
 *         year +80      month     day
 */
	static char ans[9];
	unsigned char year, month_hi, month_low, day;

	year = (date_high >> 1) + 80;
	month_hi = (date_high & 0x1) << 3;
	month_low = date_low >> 5;
	day = date_low & 0x1f;
	sprintf(ans, "%2d-%02d-%02d", month_hi + month_low, day, year);
	return(ans);
}

/*
 * Convert an MSDOS directory time stamp to ASCII.
 */

static char *
conv_time(time_high, time_low)
unsigned time_high, time_low;
{
/*
 *	    hi byte     |    low byte
 *	|7|6|5|4|3|2|1|0|7|6|5|4|3|2|1|0|
 *      | | | | | | | | | | | | | | | | |
 *      \  5 bits /\  6 bits  /\ 5 bits /
 *         hour      minutes     sec*2
 */
	static char ans[7];
	char am_pm;
	unsigned char hour, min_hi, min_low;

	hour = time_high >> 3;
	am_pm = (hour >= 12) ? 'p' : 'a';
	if (hour > 12)
		hour = hour - 12;
	if (hour == 0)
		hour = 12;
	min_hi = (time_high & 0x7) << 3;
	min_low = time_low >> 5;
	sprintf(ans, "%2d:%02d%c", hour, min_hi + min_low, am_pm);
	return(ans);
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
