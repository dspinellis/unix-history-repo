/*
 * Add an MSDOS filesystem to a low level formatted diskette.
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

int fd, dir_dirty, dir_entries;
long dir_chain[MAX_DIR_SECS];
unsigned char *dir_buf;

main(argc, argv)
int argc;
char *argv[];
{
	extern int optind;
	extern char *optarg;
	extern struct device devices[];
	struct bootsector boot;
	int i, c, oops, tracks, heads, sectors, fat_len, dir_len, clus_size;
	int tot_sectors, num_clus, fat_guess;
	long time(), now, lseek();
	char drive, *name, *expand();
	char *strncpy(), *memset(), *memcpy();
	unsigned char media, label[12], buf[MSECTOR_SIZE];
	struct device *dev;
	struct directory *dir, *mk_entry();
	void exit(), perror();

	oops = 0;
	tracks = 0;
	heads = 0;
	sectors = 0;
	label[0] = '\0';
					/* get command line options */
	while ((c = getopt(argc, argv, "t:h:s:l:")) != EOF) {
		switch (c) {
			case 't':
				tracks = atoi(optarg);
				break;
			case 'h':
				heads = atoi(optarg);
				break;
			case 's':
				sectors = atoi(optarg);
				break;
			case 'l':
				sprintf((char *) label, "%-11.11s", optarg);
				break;
			default:
				oops = 1;
				break;
		}
	}

	if (oops || (argc - optind) != 1) {
		fprintf(stderr, "Mtools version %s, dated %s\n", VERSION, DATE);
		fprintf(stderr, "Usage: %s [-t tracks] [-h heads] [-s sectors] [-l label] device\n", argv[0]);
		exit(1);
	}

	drive = argv[argc -1][0];
	if (islower(drive))
		drive = toupper(drive);

					/* check out the drive letter */
	dev = devices;
	while (dev->drive) {
		if (dev->drive == drive)
			break;
		dev++;
	}
	if (!dev->drive) {
		fprintf(stderr, "Drive '%c:' not supported\n", drive);
		exit(1);
	}
	if (dev->tracks == 0) {
		fprintf(stderr, "Non-removable media is not supported\n");
		exit(1);
	}
					/* find the right one */
	if (!dev->gioctl) {
		while (dev->drive == drive) {
			if ((!tracks || dev->tracks == tracks) && (!heads || dev->heads == heads) && (!sectors || dev->sectors == sectors))
				break;
			dev++;
		}
	}
	if (dev->drive != drive) {
		fprintf(stderr, "%s: Paramaters not supported\n", argv[0]);
		exit(1);
	}
					/* open the device */
	name = expand(dev->name);
	if ((fd = open(name, 2 | dev->mode)) < 0) {
		perror("init: open");
		exit(1);
	}
					/* fill in the blanks */
	if (!tracks)
		tracks = dev->tracks;
	if (!heads)
		heads = dev->heads;
	if (!sectors)
		sectors = dev->sectors;

					/* set parameters, if needed */
	if (dev->gioctl) {
		if ((*(dev->gioctl)) (fd, tracks, heads, sectors))
			exit(1);
	}
					/* do a "test" read */
	if (read(fd, (char *) buf, MSECTOR_SIZE) != MSECTOR_SIZE) {
		fprintf(stderr, "%s: Error reading from '%s', wrong parameters?\n", argv[0], name);
		exit(1);
	}
					/* get the parameters */
	tot_sectors = tracks * heads * sectors;
	switch (tot_sectors) {
		case 320:		/* 40t * 1h * 8s = 160k */
			media = 0xfe;
			clus_size = 1;
			dir_len = 4;
			fat_len = 1;
			break;
		case 360:		/* 40t * 1h * 9s = 180k */
			media = 0xfc;
			clus_size = 1;
			dir_len = 4;
			fat_len = 2;
			break;
		case 640:		/* 40t * 2h * 8s = 320k */
			media = 0xff;
			clus_size = 2;
			dir_len = 7;
			fat_len = 1;
			break;
		case 720:		/* 40t * 2h * 9s = 360k */
			media = 0xfd;
			clus_size = 2;
			dir_len = 7;
			fat_len = 2;
			break;
		case 1440:		/* 80t * 2h * 9s = 720k */
			media = 0xf9;
			clus_size = 2;
			dir_len = 7;
			fat_len = 3;
			break;
		case 2400:		/* 80t * 2h * 15s = 1.2m */
			media = 0xf9;
			clus_size = 1;
			dir_len = 14;
			fat_len = 7;
			break;
		case 2880:		/* 80t * 2h * 18s = 1.44m */
			media = 0xf0;
			clus_size = 1;
			dir_len = 14;
			fat_len = 9;
			break;
		default:		/* a non-standard format */
			media = 0xf0;
			if (heads == 1)
				clus_size = 1;
			else
				clus_size = (tot_sectors > 2000) ? 1 : 2;
			if (heads == 1)
				dir_len = 4;
			else
				dir_len = (tot_sectors > 2000) ? 14 : 7;
			/*
			 * Estimate the fat length, then figure it out.  The
			 * 341 is the number of 12 bit fat entries in a sector.
			 */
			fat_guess = ((tot_sectors / clus_size) / 341.0) + 0.95;
			num_clus = (tot_sectors -dir_len - (2 * fat_guess) -1) / clus_size;
			fat_len = (num_clus / 341.0) + 1;
			break;
	}
					/* the boot sector */
	memset((char *) &boot, '\0', MSECTOR_SIZE);
	boot.jump[0] = 0xeb;
	boot.jump[1] = 0x44;
	boot.jump[2] = 0x90;
	strncpy((char *) boot.banner, "Mtools  ", 8);
	boot.secsiz[0] = 512 % 0x100;
	boot.secsiz[1] = 512 / 0x100;
	boot.clsiz = (unsigned char) clus_size;
	boot.nrsvsect[0] = 1;
	boot.nrsvsect[1] = 0;
	boot.nfat = 2;
	boot.dirents[0] = (dir_len * 16) % 0x100;
	boot.dirents[1] = (dir_len * 16) / 0x100;
	boot.psect[0] = tot_sectors % 0x100;
	boot.psect[1] = tot_sectors / 0x100;
	boot.descr = media;
	boot.fatlen[0] = fat_len % 0x100;
	boot.fatlen[1] = fat_len / 0x100;
	boot.nsect[0] = sectors % 0x100;
	boot.nsect[1] = sectors / 0x100;
	boot.nheads[0] = heads % 0x100;
	boot.nheads[1] = heads / 0x100;

					/* write the boot */
	lseek(fd, 0L, 0);
	write(fd, (char *) &boot, MSECTOR_SIZE);
					/* first fat */
	memset((char *) buf, '\0', MSECTOR_SIZE);
	buf[0] = media;
	buf[1] = 0xff;
	buf[2] = 0xff;
	write(fd, (char *) buf, MSECTOR_SIZE);
	memset((char *) buf, '\0', MSECTOR_SIZE);
	for (i = 1; i < fat_len; i++)
		write(fd, (char *) buf, MSECTOR_SIZE);
					/* second fat */
	buf[0] = media;
	buf[1] = 0xff;
	buf[2] = 0xff;
	write(fd, (char *) buf, MSECTOR_SIZE);
	memset((char *) buf, '\0', MSECTOR_SIZE);
	for (i = 1; i < fat_len; i++)
		write(fd, (char *) buf, MSECTOR_SIZE);
					/* the root directory */
	if (label[0] != '\0') {
		time(&now);
		dir = mk_entry(label, 0x08, 0, 0L, now);
		memcpy((char *) buf, (char *) dir, MDIR_SIZE);
	}
	write(fd, (char *) buf, MSECTOR_SIZE);
	memset((char *) buf, '\0', MSECTOR_SIZE);
	for (i = 1; i < dir_len; i++)
		write(fd, (char *) buf, MSECTOR_SIZE);
	close(fd);
	exit(0);
}

/* hooks for the missing parts */
void disk_write() {}
