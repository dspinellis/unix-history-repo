/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)rotate.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Rotate a Varian/Versatec font.
 */

#include <stdio.h>
#include <vfont.h>
#include <sys/types.h>
#include <sys/stat.h>

char	*chp;
char	*sbrk();

main(argc,argv)
char **argv;
{
	struct header h;
	struct dispatch d[256], nd;
	struct stat stb;
	off_t tell();
	int i,size;
	int beg;
	char scr[2048];

	argc--, argv++;
	if (argc > 0) {
		close(0);
		if (open(argv[0], 0) < 0)
			perror(argv[0]), exit(1);
	}
	if (read(0, &h, sizeof(h)) != sizeof(h)) {
		fprintf(stderr, "header read error\n");
		exit(1);
	}
	if (h.magic != 0436) {
		fprintf(stderr, "bad magic number\n");
		exit(1);
	}
	if (read(0, d, sizeof(d)) != sizeof(d)) {
		fprintf(stderr, "dispatch read error\n");
		exit(1);
	}
	fstat(0, &stb);
	size = stb.st_size - tell(0);
	fprintf(stderr, "%d bytes of characters\n", size);
	chp = sbrk(size + 1024);
	read(0, chp, size);
	write(1, &h, sizeof (h));
	write(1, d, sizeof (d));
	beg = tell(1);
	for (i = 0; i < 256; i++)
		if (d[i].nbytes) {
			if (d[i].addr + d[i].nbytes > size) {
				fprintf(stderr, "char %d out of range\n", i);
				continue;
			}
			cvt(&d[i], chp+d[i].addr, &nd, scr);
			d[i] = nd;
			d[i].addr = tell(1) - beg;
			write(1, scr, d[i].nbytes);
		}
	fprintf(stderr, "done, new size %d\n", tell(1) - beg);
	h.size = tell(1) - beg;
	lseek(1, 0, 0);
	write(1, &h, sizeof (h));
	write(1, d, sizeof (d));
}

cvt(odp, ocp, dp, cp)
	struct dispatch *odp, *dp;
	register char *ocp, *cp;
{
	int max;
	int bpl;
	int row,byte,bit;
	register char *ep;
	register int bitoff;
	register int bits;
	int extra;

	max = (odp->up+odp->down+7)/8;
	extra = max*8 - (odp->down+odp->up);
	dp->down = odp->down;
	dp->up = odp->up;
	dp->left = odp->left;
	dp->right = odp->right;
	dp->nbytes = max*(dp->right+dp->left);
	ep = cp;
	for (byte = 0; byte < dp->nbytes; byte++)
		*ep++ = 0;
	bpl = (dp->right+dp->left+7)/8;
	for (row = 0; row < odp->up+odp->down; row++) {
		for (byte = 0; byte < bpl; byte++) {
			bits = *ocp++;
			for (bit = 0; bit < 8; bit++) {
				if (bits & 0x80) {
					ep = cp + max*(byte*8+bit);
					bitoff = max*8 - row - 1 - extra;
					ep += (bitoff/8);
					*ep |= 0x80 >> (bitoff%8);
				}
				bits <<= 1;
			}
		}
	}
}
