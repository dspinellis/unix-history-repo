#ifndef lint
static char sccsid[] = "@(#)enpload.c	5.3 (Berkeley from CCI) %G%";
#endif

/*
 * CMC Ethernet ``Microcode'' Loader.
 */
#include <stdio.h>
#include <a.out.h>

#include <sys/types.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <tahoeif/if_enpreg.h>

char	*dev;

main(argc, argv)
	int argc;
	char *argv[];
{
	int enp = -1, fd, first = 1, nostart = 0;

	argc--, argv++;
	if (argc > 0) {
		enp = open(dev = argv[0], O_RDWR);
		if (enp < 0) {
			fprintf(stderr, "enpload: ");
			perror(dev);
			exit(-1);
		}
		argc--, argv++;
	}
	for (; argc > 0; argc--, argv++) {
		if (strcmp(argv[0], "-s") == 0 || strcmp(argv[0], "-S") == 0) {
			nostart++;
			continue;
		}
		if (first) {
			/*
			 * Reset device before first file is loaded.
			 */
			if (ioctl(enp, ENPIORESET) < 0) {
				fprintf(stderr, "enpload: %s: ", dev);
				perror("ioctl (ENPIORESET)");
				exit(-1);
			}
			first = !first;
		}
		if ((fd = open(argv[0], O_RDONLY)) < 0) {
			fprintf(stderr, "enpload: "), perror(argv[0]);
			exit(1);
		}
		enpload(enp, fd, argv[0]);
		close(fd);
	}
	if (enp != -1 && !nostart && ioctl(enp, ENPIOGO) < 0) {
		fprintf(stderr, "enpload: ");
		perror("ioctl (ENPIOGO)");
		exit(-1);
	}
	exit(0);
}

#define	RELO		0x03FFFF	/* relocation offset */
#define	ENPMSTART	0x0		/* start of memory */
#define	BSIZE		512		/* buffer size */
char	buff[BSIZE];
char	zbuf[BSIZE];

enpload(enp, fd, filename)
	int enp, fd;
	char *filename;
{
	int cnt, size, lstart;
	struct exec hdr;

	if (read(fd, &hdr, sizeof (hdr)) != sizeof (hdr)) {
		fprintf(stderr, "enpload: %s: Read short (header).\n",
		   filename);
		exit(1);
	}
	if (N_BADMAG(hdr)) {
		fprintf(stderr, "enpload: %s: Bad magic number.\n", filename);
		exit(1);
	}
	size = hdr.a_text + hdr.a_data;
	lstart = (ENPMSTART + (hdr.a_entry & RELO)) - 0x1000;

	printf("%s: Loading %s...", dev, filename);
	(void) lseek(enp, lstart + size, L_SET);
	while (hdr.a_bss >= BSIZE) {
		if (write(enp, zbuf, BSIZE) != BSIZE) {
			fprintf(stderr, "enpload: Bss write error.\n");
			exit(-1);
		}
		hdr.a_bss -= BSIZE;
	}
	if (hdr.a_bss > 0 && write(enp, zbuf, hdr.a_bss) != hdr.a_bss) {
		fprintf(stderr, "enpload: Bss write error.\n");
		exit(-1);
	}
	(void) lseek(enp, lstart, L_SET);
	while (size > BSIZE) {
		cnt = read(fd, buff, BSIZE);
		size -= cnt;
		if (write(enp, buff, cnt) != cnt) {
			fprintf(stderr, "enpload: Write error.\n");
			exit(-1);
		}
	}
	if (size > 0) {
		cnt = read(fd, buff, size);
		if (write(enp, buff, cnt) != cnt) {
			fprintf(stderr, "enpload: Write error.\n");
			exit(-1);
		}
	}
	printf("done.\n");
}
