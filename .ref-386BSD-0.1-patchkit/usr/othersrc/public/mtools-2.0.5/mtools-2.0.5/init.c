/*
 * Initialize an MSDOS diskette.  Read the boot sector, and switch to the
 * proper floppy disk device to match the format on the disk.  Sets a bunch
 * of global variables.  Returns 0 on success, or 1 on failure.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "msdos.h"

#define FULL_CYL
#define WORD(x) ((boot->x)[0] + ((boot->x)[1] << 8))
#define DWORD(x) ((boot->x)[0] + ((boot->x)[1] << 8) + ((boot->x)[2] << 16) + ((boot->x)[3] << 24))

unsigned int num_clus;			/* total number of cluster */
int num_fat;				/* the number of FAT tables */
long disk_offset;			/* skip this many bytes */
int fat_bits;				/* the FAT encoding scheme */

extern int fd, fat_len, dir_len, dir_start, clus_size, dir_dirty, disk_dirty;
extern int fat_error, disk_size;
extern long disk_current;
extern char *mcwd;
extern unsigned char *fat_buf, *disk_buf, *dir_buf;
extern struct device devices[];
static struct bootsector *read_boot();

int
init(drive, mode)
char drive;
int mode;
{
	int fat_start, tracks, heads, sectors, old_dos;
	char *malloc(), *name, *expand();
	void perror(), exit(), reset_chain(), free(), fat_read();
	struct bootsector *boot;
	struct device *dev;

	if (fd != -1) {
		close(fd);
		free((char *) fat_buf);
		free((char *) disk_buf);
		free((char *) dir_buf);
	}
					/* check out the drive letter */
	dev = devices;
	while (dev->drive) {
		if (dev->drive == drive)
			break;
		dev++;
	}
	if (!dev->drive) {
		fprintf(stderr, "Drive '%c:' not supported\n", drive);
		return(1);
	}
					/* open the device */
	while (dev->name) {
		if (dev->drive != drive)
			break;

		name = expand(dev->name);
		if ((fd = open(name, mode | dev->mode)) < 0) {
			perror("init: open");
			exit(1);
		}
					/* set default parameters, if needed */
		if (dev->gioctl) {
			if ((*(dev->gioctl)) (fd, dev->tracks, dev->heads, dev->sectors))
				goto try_again;
		}
					/* read the boot sector */
		disk_offset = dev->offset;
		if ((boot = read_boot()) == NULL)
			goto try_again;

		heads = WORD(nheads);
		sectors = WORD(nsect);
		if (heads && sectors)
			tracks = WORD(psect) / (unsigned) (heads * sectors);

					/* sanity checking */
		old_dos = 0;
		if (!heads || heads > 100 || !sectors || sectors > 500 || tracks > 5000 || !boot->clsiz) {
			/*
			 * The above technique will fail on diskettes that
			 * have been formatted with very old MSDOS, so we
			 * resort to the old table-driven method using the
			 * media signature (first byte in FAT).
			 */
			unsigned char temp[MSECTOR_SIZE];
			if (read(fd, (char *) temp, MSECTOR_SIZE) != MSECTOR_SIZE)
				temp[0] = '0';

			switch (temp[0]) {
				case 0xfe:	/* 160k */
					tracks = 40;
					sectors = 8;
					heads = 1;
					dir_start = 3;
					dir_len = 4;
					clus_size = 1;
					fat_len = 1;
					num_clus = 313;
					break;
				case 0xfc:	/* 180k */
					tracks = 40;
					sectors = 9;
					heads = 1;
					dir_start = 5;
					dir_len = 4;
					clus_size = 1;
					fat_len = 2;
					num_clus = 351;
					break;
				case 0xff:	/* 320k */
					tracks = 40;
					sectors = 8;
					heads = 2;
					dir_start = 3;
					dir_len = 7;
					clus_size = 2;
					fat_len = 1;
					num_clus = 315;
					break;
				case 0xfd:	/* 360k */
					tracks = 40;
					sectors = 9;
					heads = 2;
					dir_start = 5;
					dir_len = 7;
					clus_size = 2;
					fat_len = 2;
					num_clus = 354;
					break;
				default:
					fprintf(stderr, "Probable non-MSDOS disk\n");
					close(fd);
					fd = -1;
					return(1);
			}
			fat_start = 1;
			num_fat = 2;
			old_dos = 1;
		}
					/* check the parameters */
		if (dev->tracks && !dev->gioctl) {
			if (dev->tracks == tracks && dev->heads == heads && dev->sectors == sectors)
				break;
		}
		else
			break;

try_again:	close(fd);
		fd = -1;
		dev++;
	}
	if (fd == -1) {
		if (boot != NULL && dev->tracks)
			fprintf(stderr, "No support for %d tracks, %d heads, %d sector diskettes\n", tracks, heads, sectors);
		return(1);
	}
					/* set new parameters, if needed */
	if (dev->gioctl) {
		if ((*(dev->gioctl)) (fd, tracks, heads, sectors)) {
			fprintf(stderr, "Can't set disk parameters\n");
			close(fd);
			fd = -1;
			return(1);
		}
	}

	/*
	 * all numbers are in sectors, except num_clus (which is in clusters)
	 */
	if (!old_dos) {
		clus_size = boot->clsiz;
		fat_start = WORD(nrsvsect);
		fat_len = WORD(fatlen);
		dir_start = fat_start + (boot->nfat * fat_len);
		dir_len = WORD(dirents) * MDIR_SIZE / (unsigned) MSECTOR_SIZE;
		/*
		 * For DOS partitions > 32M
		 */
		if (WORD(psect) == 0)
			num_clus = (unsigned int) (DWORD(bigsect) - dir_start - dir_len) / clus_size;
		else
			num_clus = (unsigned int) (WORD(psect) - dir_start - dir_len) / clus_size;
		num_fat = boot->nfat;
	}
					/* more sanity checking */
	if (clus_size * MSECTOR_SIZE > MAX_CLUSTER) {
		fprintf(stderr, "Cluster size of %d is larger than max of %d\n", clus_size * MSECTOR_SIZE, MAX_CLUSTER);
		close(fd);
		fd = -1;
		return(1);
	}
	if (!old_dos && WORD(secsiz) != MSECTOR_SIZE) {
		fprintf(stderr, "Sector size of %d is not supported\n", WORD(secsiz));
		close(fd);
		fd = -1;
		return(1);
	}
					/* full cylinder buffering */
#ifdef FULL_CYL
	disk_size = (dev->tracks) ? (sectors * heads) : 1;
#else /* FULL_CYL */
	disk_size = (dev->tracks) ? sectors : 1;
#endif /* FULL_CYL */

/*
 * The driver in Dell's SVR4 v2.01 is unreliable with large writes.
 */
#ifdef DELL
	disk_size = 1;
#endif /* DELL */

	disk_buf = (unsigned char *) malloc((unsigned int) disk_size * MSECTOR_SIZE);
	if (disk_buf == NULL) {
		perror("init: malloc");
		exit(1);
	}
					/* read the FAT sectors */
	disk_current = -1000L;
	disk_dirty = 0;
	fat_error = 0;
	fat_bits = dev->fat_bits;
	fat_read(fat_start);
					/* set dir_chain[] to root directory */
	dir_dirty = 0;
	reset_chain(NEW);
	return(0);
}

/*
 * Fix the info in the MCWD file to be a proper directory name.  Always
 * has a leading separator.  Never has a trailing separator (unless it is
 * the path itself).
 */

char *
fix_mcwd()
{
	FILE *fp;
	struct stat sbuf;
	char *s, *strcpy(), *strcat(), *mcwd_path, *getenv(), *strncpy();
	char buf[BUFSIZ], *file, *expand();
	static char ans[MAX_PATH];
	long now, time();

	mcwd_path = getenv("MCWD");
	if (mcwd_path == NULL || *mcwd_path == '\0')
		mcwd_path = "$HOME/.mcwd";

	file = expand(mcwd_path);
	if (stat(file, &sbuf) < 0)
		return("A:/");
	/*
	 * Ignore the info, if the file is more than 6 hours old
	 */
	time(&now);
	if (now - sbuf.st_mtime > 6 * 60 * 60) {
		fprintf(stderr, "Warning: \"%s\" is out of date, contents ignored\n", file);
		return("A:/");
	}
	
	if (!(fp = fopen(file, "r")))
		return("A:/");

	if (!fgets(buf, BUFSIZ, fp))
		return("A:/");

	buf[strlen(buf) -1] = '\0';
	fclose(fp);
					/* drive letter present? */
	s = buf;
	if (buf[0] && buf[1] == ':') {
		strncpy(ans, buf, 2);
		ans[2] = '\0';
		s = &buf[2];
	}
	else 
		strcpy(ans, "A:");
					/* add a leading separator */
	if (*s != '/' && *s != '\\') {
		strcat(ans, "/");
		strcat(ans, s);
	}
	else
		strcat(ans, s);
					/* translate to upper case */
	for (s = ans; *s; ++s) {
		if (islower(*s))
			*s = toupper(*s);
		if (*s == '\\')
			*s = '/';
	}
					/* if only drive, colon, & separator */
	if (strlen(ans) == 3)
		return(ans);
					/* zap the trailing separator */
	if (*--s == '/')
		*s = '\0';
	return(ans);
}

/*
 * Read the boot sector.  We glean the disk parameters from this sector.
 */

static struct bootsector *
read_boot()
{
	long lseek();
	static struct bootsector boot;

	if (lseek(fd, disk_offset, 0) < 0)
		return(NULL);
					/* read the first sector */
	if (read(fd, (char *) &boot, MSECTOR_SIZE) != MSECTOR_SIZE)
		return(NULL);

	return(&boot);
}
