/*
 * Read (copy) an MSDOS file to Unix
 *
 * Emmet P. Gray			US Army, HQ III Corps & Fort Hood
 * ...!uunet!uiucuxc!fthood!egray	Attn: AFZF-DE-ENV
 * fthood!egray@uxc.cso.uiuc.edu	Directorate of Engineering & Housing
 * 					Environmental Management Office
 * 					Fort Hood, TX 76544-5057
 */

#define LOWERCASE

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef BSD
#include <sys/time.h>
#else /* BSD */
#include <time.h>
#endif /* BSD */
#include "msdos.h"
#include "patchlevel.h"

int fd = -1;				/* the file descriptor for the device */
int dir_start;				/* starting sector for directory */
int dir_len;				/* length of directory (in sectors) */
int dir_entries;			/* number of directory entries */
int clus_size;				/* cluster size (in sectors) */
char *mcwd;				/* the Current Working Directory */
int fat_error;				/* FAT error detected? */

static void set_mtime();
static FILE *open_file();
static long conv_stamp();

main(argc, argv)
int argc;
char *argv[];
{
	FILE *fp;
	extern int optind;
	extern char *optarg;
	int i, ismatch, entry, single, c, oops, preserve, nowarn, textmode;
	unsigned int fat;
	long size, mtime;
	char *filename, *newfile, *get_name(), *unix_name(), *pathname;
	char *get_path(), *target, tmp[MAX_PATH], *strcat(), *strcpy(), drive;
	char get_drive(), last_drive, *fix_mcwd(), *s;
	void exit();
	struct directory *dir, *dir_read();
	struct stat stbuf;

					/* get command line options */
	oops = 0;
	preserve = 0;
	nowarn = 0;
	textmode = 0;
	while ((c = getopt(argc, argv, "tnmv")) != EOF) {
		switch (c) {
			case 't':
				textmode = 1;
				break;
			case 'n':
				nowarn = 1;
				break;
			case 'm':
				preserve = 1;
				break;
			case 'v':	/* dummy option for mcopy */
				break;
			default:
				oops = 1;
				break;
		}
	}

	if (oops || (argc - optind) < 2) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s [-tnm] msdosfile unixfile\n", argv[0]);
		fprintf(stderr, "       %s [-tnm] msdosfile [msdosfiles...] unixdirectory\n", argv[0]);
		exit(1);
	}
	last_drive = 'x';
	mcwd = fix_mcwd();
					/* only 1 file to copy... */
	single = 1;
	target = argv[argc - 1];
					/* ...unless last arg is a directory */
	if (!stat(target, &stbuf)) {
		if ((stbuf.st_mode & S_IFMT) == S_IFDIR)
			single = 0;
	}
					/* too many arguments */
	if (single && (argc - optind) != 2) {
		fprintf(stderr, "%s: Too many arguments or destination directory omitted\n", argv[0]);
		exit(1);
	}

	for (i = optind; i < argc - 1; i++) {
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

					/* if single file */
			if (single) {
				if (!strcmp(newfile, filename)) {
					fat = dir->start[1] * 0x100 + dir->start[0];
					size = dir->size[3] * 0x1000000L + dir->size[2] * 0x10000L + dir->size[1] * 0x100 + dir->size[0];
					if (preserve)
						mtime = conv_stamp(dir->time, dir->date);
					else
						mtime = 0L;
					if ((fp = open_file(target, nowarn))) {
						if (file_read(fp, fat, textmode, 0, size)) {
							fclose(fp);
							break;
						}
						fclose(fp);
						set_mtime(target, mtime);
					}
					ismatch = 1;
					break;
				}
			}
					/* if multiple files */
			else {
				if (match(newfile, filename)) {
					fat = dir->start[1] * 0x100 + dir->start[0];
					size = dir->size[3] * 0x1000000L + dir->size[2] * 0x10000L + dir->size[1] * 0x100 + dir->size[0];
					if (preserve)
						mtime = conv_stamp(dir->time, dir->date);
					else
						mtime = 0L;
					printf("Copying %s\n", newfile);
#ifdef LOWERCASE
					s = newfile;
					while (*s) {
						if (isupper(*s))
							*s = tolower(*s);
						s++;
					}
#endif /* LOWERCASE */
					strcpy(tmp, target);
					strcat(tmp, "/");
					strcat(tmp, newfile);
					if ((fp = open_file(tmp, nowarn))) {
						if (file_read(fp, fat, textmode, 0, size)) {
							fclose(fp);
							break;
						}
						fclose(fp);
						set_mtime(tmp, mtime);
					}
					ismatch = 1;
				}
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
 * Open the named Unix file for write.
 */

static FILE *
open_file(target, nowarn)
char *target;
int nowarn;
{
	static FILE *fp;
	char ans[10];
	struct stat stbuf;

	if (!nowarn) {
		if (!access(target, 0)) {
			/* CONSTCOND */
			while (1) {
				printf("File \"%s\" exists, overwrite (y/n) ? ", target);
				gets(ans);
				if (ans[0] == 'n' || ans[0] == 'N')
					return(NULL);
				if (ans[0] == 'y' || ans[0] == 'Y')
					break;
			}
					/* sanity checking */
			if (!stat(target, &stbuf)) {
				if ((stbuf.st_mode & S_IFREG) != S_IFREG) {
					fprintf(stderr, "\"%s\" is not a regular file\n", target);
					return(NULL);
				}
			}
		}
	}

	if (!(fp = fopen(target, "w"))) {
		fprintf(stderr, "Can't open \"%s\" for write\n", target);
		return(NULL);
	}
	return(fp);
}

/*
 * Convert an MSDOS time & date stamp to the Unix time() format
 */

static long
conv_stamp(time_field, date_field)
unsigned char *time_field, *date_field;
{
#ifdef BSD
	struct timeval tv;
	struct timezone tz;
#else /* BSD */
	extern long timezone;
	void tzset();
#endif /* BSD */
	struct tm *tmbuf, *localtime();
	int year, mon, mday, hour, min, sec, old_leaps;
	long answer, sec_year, sec_mon, sec_mday, sec_hour, sec_min, sec_leap;
	long tzone, dst;
	static int month[] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304,
	334};
					/* dissect the parts */
	year = (date_field[1] >> 1) + 1980;
	mon = (((date_field[1] & 0x1) << 3) + (date_field[0] >> 5));
	mday = date_field[0] & 0x1f;
	hour = time_field[1] >> 3;
	min = (((time_field[1] & 0x7) << 3) + (time_field[0] >> 5));
	sec = (time_field[0] & 0x1f) * 2;
					/* how many previous leap years */
	old_leaps = (year - 1972) / 4L;
	sec_leap = old_leaps * 24L * 60L * 60L;
					/* back off 1 day if before 29 Feb */
	if (!(year % 4) && mon < 3)
		sec_leap -= 24L * 60L * 60L;
	sec_year = (year - 1970) * 365L * 24L * 60L * 60L;
	sec_mon = month[mon - 1] * 24L * 60L * 60L;
	sec_mday = mday * 24L * 60L * 60L;
	sec_hour = hour * 60L * 60L;
	sec_min = min * 60L;
					/* correct for Time Zone */
#ifdef BSD
	gettimeofday(&tv, &tz);
	tzone = tz.tz_minuteswest * 60L;
#else /* BSD */
	tzset();
	tzone = timezone;
#endif /* BSD */

	answer = sec_leap + sec_year + sec_mon + sec_mday + sec_hour + sec_min + sec + tzone;
					/* correct for Daylight Saving Time */
	tmbuf = localtime(&answer);
	dst = (tmbuf->tm_isdst) ? (-60L * 60L) : 0L;
	answer += dst;
	
	return(answer);
}

/*
 * Preserve the file modification times after the fclose()
 */

static void
set_mtime(target, mtime)
char *target;
long mtime;
{
#ifdef BSD
	struct timeval tv[2];

	if (mtime != 0L) {
		tv[0].tv_sec = mtime;
		tv[0].tv_usec = 0;
		tv[1].tv_sec = mtime;
		tv[1].tv_usec = 0;
		utimes(target, tv);
	}
#else /* BSD */
	struct {
		time_t actime;
		time_t modtime;
	} utbuf;

	if (mtime != 0L) {
		utbuf.actime = mtime;
		utbuf.modtime = mtime;
		utime(target, &utbuf);
	}
#endif /* BSD */
	return;
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
