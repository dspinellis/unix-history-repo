/*
 * Write (copy) a Unix file to MSDOS
 *
 * Emmet P. Gray			US Army, HQ III Corps & Fort Hood
 * ...!uunet!uiucuxc!fthood!egray	Attn: AFZF-DE-ENV
 * fthood!egray@uxc.cso.uiuc.edu	Directorate of Engineering & Housing
 * 					Environmental Management Office
 * 					Fort Hood, TX 76544-5057
 */

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "msdos.h"
#include "patchlevel.h"

int fd = -1;				/* the file descriptor for the device */
int dir_start;				/* starting sector for directory */
int dir_len;				/* length of directory (in sectors) */
int dir_entries;			/* number of directory entries */
int clus_size;				/* cluster size (in sectors) */
char *mcwd;				/* the Current Working Directory */
int fat_error;				/* FAT error detected? */

int full = 0;
int textmode = 0;
int nowarn = 0;
static int got_signal();
static struct directory *writeit();
static long free_space();

main(argc, argv)
int argc;
char *argv[];
{
	extern int optind;
	extern char *optarg;
	int i, entry, ismatch, nogo, slot, single;
	int c, oops, verbose, first, mod_time;
	unsigned int dot, start;
	char *filename, *newfile, *get_name(), get_drive();
	char *unix_name(), ans[10], *pathname, *get_path(), *fix_mcwd();
	char tmp[MAX_PATH], target[13], *strcat(), *strcpy(), drive;
	unsigned char *fixed, *dos_name();
	void exit(), fat_write(), dir_write(), disk_flush(), dir_flush();
	struct directory *dir, *dir_read();
					/* catch signals */
	signal(SIGINT, (SIG_TYPE(*) ()) got_signal);
	signal(SIGTERM, (SIG_TYPE(*) ()) got_signal);
	signal(SIGQUIT, (SIG_TYPE(*) ()) got_signal);
					/* get command line options */
	oops = 0;
	verbose = 0;
	mod_time = 0;
	while ((c = getopt(argc, argv, "tnvm")) != EOF) {
		switch (c) {
			case 't':
				textmode = 1;
				break;
			case 'n':
				nowarn = 1;
				break;
			case 'v':
				verbose = 1;
				break;
			case 'm':
				mod_time = 1;
				break;
			default:
				oops = 1;
				break;
		}
	}

	if (oops || (argc - optind) < 2) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s [-tnvm] unixfile msdosfile\n", argv[0]);
		fprintf(stderr, "       %s [-tnvm] unixfile [unixfiles...] msdosdirectory\n", argv[0]);
		exit(1);
	}
	mcwd = fix_mcwd();

	drive = get_drive(argv[argc - 1]);
	if (init(drive, 2)) {
		fprintf(stderr, "%s: Cannot initialize '%c:'\n", argv[0], drive);
		exit(1);
	}

	filename = get_name(argv[argc - 1]);
	pathname = get_path(argv[argc - 1]);

	/*
	 * Move to "first guess" directory so we can see if filename is also
	 * a directory.
	 */
	if (subdir(drive, pathname))
		exit(1);
					/* test if last argv is a dir */
	if (is_dir(filename) || *filename == '\0') {
		if (*filename) {
			strcpy(tmp, pathname);
			if (tmp[strlen(tmp) -1] != '/')
				strcat(tmp, "/");
			strcat(tmp, filename);

			if (subdir(drive, tmp))
				exit(1);
		}
		single = 0;
	}
	else {
		single = 1;
					/* too many arguments */
		if ((argc - optind) != 2) {
			fprintf(stderr, "%s: Too many arguments or destination directory omitted\n", argv[0]);
			exit(1);
		}
	}

	for (i = optind; i < argc - 1; i++) {
		if (single)
			fixed = dos_name(argv[argc - 1], verbose);
		else
			fixed = dos_name(argv[i], verbose);

		strcpy(target, unix_name(fixed, fixed + 8));
					/* see if exists and get slot */
		ismatch = 0;
		slot = -1;
		dot = 0;
		nogo = 0;
		first = 1;
		for (entry = 0; entry < dir_entries; entry++) {
			dir = dir_read(entry);
					/* save the '.' entry info */
			if (first) {
				first = 0;
				if ((dir->attr & 0x10) && dir->name[0] == '.') {
					dot = dir->start[1] * 0x100 + dir->start[0];
					continue;
				}
			}
					/* is empty */
			if (dir->name[0] == 0x0) {
				if (slot < 0)
					slot = entry;
				break;
			}
					/* is erased */
			if (dir->name[0] == 0xe5) {
				if (slot < 0)
					slot = entry;
				continue;
			}
					/* is dir or volume label */
			if ((dir->attr & 0x10) || (dir->attr & 0x08))
				continue;

			newfile = unix_name(dir->name, dir->ext);

					/* if file exists, delete it first */
			if (!strcmp(target, newfile)) {
				ismatch = 1;
				start = dir->start[1] * 0x100 + dir->start[0];
				if (nowarn) {
					if (fat_free(start))
						break;
					dir->name[0] = 0xe5;
					dir_write(entry, dir);
					if (slot < 0)
						slot = entry;
				}
				else {
					/* CONSTCOND */
					while (1) {
						printf("File \"%s\" exists, overwrite (y/n) ? ", target);
						gets(ans);
						if (ans[0] == 'n' || ans[0] == 'N') {
							nogo = 1;
							break;
						}
						if (ans[0] == 'y' || ans[0] == 'Y') {
							if (fat_free(start))
								break;
							dir->name[0] = 0xe5;
							dir_write(entry, dir);
							if (slot < 0)
								slot = entry;
							break;
						}
					}
				}
			}
			if (ismatch)
				break;
		}
		if (fat_error)
			break;

		if (nogo)		/* chickened out... */
			continue;
					/* no '.' entry means root directory */
		if (dot == 0 && slot < 0) {
			fprintf(stderr, "%s: No directory slots\n", argv[0]);
			break;
		}
					/* make the directory grow */
		if (dot && slot < 0) {
			if (dir_grow(dot)) {
				fprintf(stderr, "%s: Disk full\n", argv[0]);
				break;
			}
					/* first entry in 'new' directory */
			slot = entry;
		}
					/* write the file */
		if (dir = writeit(fixed, argv[i], verbose, mod_time, single, target))
			dir_write(slot, dir);

		if (full) {
			fprintf(stderr, "%s: Disk full\n", argv[0]);
			break;
		}
		if (single)
			break;
	}
					/* write the FAT, flush the buffers */
	fat_write();
	dir_flush();
	disk_flush();
	close(fd);
	exit(0);
}

/*
 * Open the named file for read, create the cluster chain, return the
 * directory structure or NULL on error.
 */

static struct directory *
writeit(fixed, path, verbose, mod_time, single, target)
unsigned char *fixed;
char *path;
int verbose, mod_time, single;
char *target;
{
	FILE *fp;
	unsigned int fat, next_fat();
	long filesize, file_write(), size, time(), now;
	struct directory *dir, *mk_entry();
	struct stat stbuf;

	if (stat(path, &stbuf) < 0) {
		fprintf(stderr, "Can't stat \"%s\"\n", path);
		return(NULL);
	}

	if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
		if (verbose)
			fprintf(stderr, "\"%s\" is a directory\n", path);
		return(NULL);
	}

	if ((stbuf.st_mode & S_IFREG) != S_IFREG) {
		if (verbose)
			fprintf(stderr, "\"%s\" is not a regular file\n", path);
		return(NULL);
	}

	if (!(fp = fopen(path, "r"))) {
		fprintf(stderr, "Can't open \"%s\" for read\n", path);
		return(NULL);
	}

	if (!single)
		printf("Copying %s\n", target);

					/* will it fit? */
	filesize = stbuf.st_size;
	if (filesize > free_space()) {
		full = 1;
		return(NULL);
	}
					/* preserve mod time? */
	if (mod_time)
		now = stbuf.st_mtime;
	else
		time(&now);

					/* if a zero length file */
	if (filesize == 0L) {
		dir = mk_entry(fixed, 0x20, 0, 0L, now);
		return(dir);
	}

	if ((fat = next_fat(0)) == 1) {
		full = 1;
		fclose(fp);
		return(NULL);
	}
	if ((size = file_write(fp, fat, filesize, textmode)) < 0) {
		fclose(fp);
		return(NULL);
	}
	fclose(fp);

	dir = mk_entry(fixed, 0x20, fat, size, now);
	return(dir);
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


/*
 * Get the amount of remaining free space
 */

static long
free_space()
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
	total *= MSECTOR_SIZE;
	return(total);
}
