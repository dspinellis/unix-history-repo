#include <X/mit-copyright.h>

/*
 * XPR - process xwd(1) files for the LN03 or LA100 printer
 *
 * Author: Michael R. Gretzinger, MIT Project Athena
 * Copyright (C) 1985, Massachusetts Institute of Technology
 */

#ifndef lint
static char *rcsid_xpr_c = "$Header: xpr.c,v 10.6 86/11/30 16:51:01 jg Rel $";
#endif

#include <sys/types.h>
#include <sys/file.h>
#include <sys/uio.h>
#include <stdio.h>
#include "lncmd.h"
#include <X/XWDFile.h>

int debug = 0;

enum device {LN01, LN03, LA100};
enum orientation {PORTRAIT, LANDSCAPE};

#define W_MAX 2400
#define H_MAX 3150
#define W_MARGIN 75
#define H_MARGIN 37
#define W_PAGE 2550
#define H_PAGE 3225

#ifdef NOINLINE
#define min(x,y) (((x)<(y))?(x):(y))
#endif NOINLINE

#define F_PORTRAIT 1
#define F_LANDSCAPE 2
#define F_DUMP 4
#define F_NOSIXOPT 8
#define F_APPEND 16
#define F_NOFF 32
#define F_REPORT 64


main(argc, argv)
char **argv;
{
    XWDFileHeader win;
    register unsigned char (*sixmap)[];
    register int i;
    register int iw;
    register int ih;
    register int sixel_count;
    char *w_name;
    char *filename;
    char *output_filename;
    int scale, width, height, flags, split;
    int left, top;
    int top_margin, left_margin;
    int wpad, hpad;
    char *header, *trailer;
    enum orientation orientation;
    enum device device;
    
    parse_args (argc, argv, &scale, &width, &height, &left, &top, &device, 
		&flags, &split, &header, &trailer);

    /* read in window header */
    read(0, &win, sizeof win);
    w_name = (char *)malloc(win.header_size - sizeof win);
    read(0, w_name, win.header_size - sizeof win);
    
    /* calculate orientation and scale */
    setup_layout(device, win.pixmap_width, win.pixmap_height, flags, width, 
		 height, header, trailer, &scale, &orientation);

    /* calculate w and h cell count */
    wpad = (16 - (win.pixmap_width & 15)) & 15;
    iw = win.pixmap_width;
    ih = (win.pixmap_height + 5) / 6;
    hpad = (ih * 6) - win.pixmap_height;

    /* build pixcells from input file */
    sixel_count = iw * ih;
    sixmap = (unsigned char (*)[])malloc(sixel_count);
    build_sixmap(iw, ih, sixmap, wpad, hpad);

    /* output commands and sixel graphics */
    if (device == LN03) {
/*	ln03_grind_fonts(sixmap, iw, ih, scale, &pixmap); */
	ln03_setup(iw, ih, orientation, scale, left, top,
		   &left_margin, &top_margin, flags, header, trailer);
	ln03_output_sixels(sixmap, iw, ih, (flags & F_NOSIXOPT), split, 
			   scale, top_margin, left_margin);
	ln03_finish();
    } else if (device == LA100) {
	la100_setup(iw, ih, scale);
	la100_output_sixels(sixmap, iw, ih, (flags & F_NOSIXOPT));
	la100_finish();
    } else {
	fprintf(stderr, "xwd2: device not supported\n");
    }
    
    /* print some statistics */
    if (flags & F_REPORT) {
	fprintf(stderr, "Name: %s\n", w_name);
	fprintf(stderr, "Width: %d, Height: %d\n", win.pixmap_width, 
		win.pixmap_height);
	fprintf(stderr, "Orientation: %s, Scale: %d\n", 
		(orientation==PORTRAIT) ? "Portrait" : "Landscape", scale);
    }
    if (flags & F_DUMP) dump_sixmap(sixmap, iw, ih);
}

parse_args(argc, argv, scale, width, height, left, top, device, flags, 
	   split, header, trailer)
register int argc;
register char **argv;
int *scale;
int *width;
int *height;
int *left;
int *top;
enum device *device;
int *flags;
int *split;
char **header;
char **trailer;
{
    register char *output_filename;
    register int f;
    register int len;
    register int pos;
    double atof();
    int atoi();

    output_filename = NULL;
    *device = LN03;	/* default */
    *flags = 0;
    *split = 1;
    *width = -1;
    *height = -1;
    *top = -1;
    *left = -1;
    *header = NULL;
    *trailer = NULL;
    
    argc--;
    argv++;

    while (argc > 0 && argv[0][0] == '-') {
	len = strlen(*argv);
	switch (argv[0][1]) {
	case 'a':		/* -append <filename> */
	    if (!bcmp(*argv, "-append", len)) {
		argc--; argv++;
		output_filename = *argv;
		*flags |= F_APPEND;
	    }
	    break;

	case 'd':		/* -device {ln03 | la100} | -dump */
	    if (len <= 2) {
		fprintf(stderr, "xwd2: ambiguous option: \"%s\"\n", *argv);
		exit(1);
	    }
	    if (!bcmp(*argv, "-device", len)) {
		argc--; argv++;
		len = strlen(*argv);
		if (!bcmp(*argv, "ln03", len)) {
		    *device = LN03;
		} else if (!bcmp(*argv, "la100", len)) {
		    *device = LA100;
		} else {
		    fprintf(stderr, 
			    "xwd2: device \"%s\" not supported\n", *argv);
		    exit(1);
		}
	    } else if (!bcmp(*argv, "-dump", len)) {
		*flags |= F_DUMP;
	    }
	    break;

	case 'h':		/* -height <inches> */
	    if (len <= 3) {
		fprintf(stderr, "xwd2: ambiguous option: \"%s\"\n", *argv);
		exit(1);
	    }
	    if (!bcmp(*argv, "-height", len)) {
		argc--; argv++;
		*height = (int)(300.0 * atof(*argv));
	    } else if (!bcmp(*argv, "-header", len)) {
		argc--; argv++;
		*header = *argv;
	    }
	    break;

	case 'l':		/* -landscape | -left <inches> */
	    if (!bcmp(*argv, "-landscape", len)) {
		*flags |= F_LANDSCAPE;
	    } else if (!bcmp(*argv, "-left", len)) {
		argc--; argv++;
		*left = (int)(300.0 * atof(*argv));
	    }
	    break;

	case 'n':		/* -nosixopt | -noff */
	    if (len <= 3) {
		fprintf(stderr, "xwd2: ambiguous option: \"%s\"\n", *argv);
		exit(1);
	    }
	    if (!bcmp(*argv, "-nosixopt", len)) {
		*flags |= F_NOSIXOPT;
	    } else if (!bcmp(*argv, "-noff", len)) {
		*flags |= F_NOFF;
	    }
	    break;

	case 'o':		/* -output <filename> */
	    if (!bcmp(*argv, "-output", len)) {
		argc--; argv++;
		output_filename = *argv;
	    }
	    break;

	case 'p':		/* -portrait */
	    if (!bcmp(*argv, "-portrait", len)) {
		*flags |= F_PORTRAIT;
	    }
	    break;

	case 'r':		/* -report */
	    if (!bcmp(*argv, "-report", len)) {
		*flags |= F_REPORT;
	    }
	    break;

	case 's':		/* -scale <scale> | -split <n-pages> */
	    if (!bcmp(*argv, "-scale", len)) {
		argc--; argv++;
		*scale = atoi(*argv);
	    } else if (!bcmp(*argv, "-split", len)) {
		argc--; argv++;
		*split = atoi(*argv);
	    }
	    break;

	case 't':		/* -top <inches> */
	    if (len <= 2) {
		fprintf(stderr, "xwd2: ambigous option: \"%s\"\n", *argv);
		exit(1);
	    }
	    if (!bcmp(*argv, "-top", len)) {
		argc--; argv++;
		*top = (int)(300.0 * atof(*argv));
	    } else if (!bcmp(*argv, "-trailer", len)) {
		argc--; argv++;
		*trailer = *argv;
	    }
	    break;

	case 'w':		/* -width <inches> */
	    if (!bcmp(*argv, "-width", len)) {
		argc--; argv++;
		*width = (int)(300.0 * atof(*argv));
	    }
	    break;

	}
	argc--; argv++;
    }

    if (argc > 0) {
	f = open(*argv, O_RDONLY, 0);
	if (f < 0) {
	    fprintf(stderr, "xwd2: error opening \"%s\" for input\n", *argv);
	    perror("");
	    exit(1);
	}
	dup2(f, 0);
	close(f);
/*	if (output_filename == NULL) {
	    output_filename = (char *)malloc(strlen(*argv)+10);
	    build_output_filename(*argv, *device, output_filename);
	} */
    }

    if (output_filename != NULL) {
	if (!(*flags & F_APPEND)) {
	    f = open(output_filename, O_CREAT|O_WRONLY|O_TRUNC, 0664);
	} else {
	    f = open(output_filename, O_WRONLY, 0);
	}
	if (f < 0) {
	    fprintf(stderr, "xwd2: error opening \"%s\" for output\n", 
		    output_filename);
	    perror("xwd2");
	    exit(1);
	}
	if (*flags & F_APPEND) {
	    pos = lseek(f, 0, 2);          /* get eof position */
	    if (*flags & F_NOFF) pos -= 3; /* set position before trailing */
					   /*     formfeed and reset */
	    lseek(f, pos, 0);              /* set pointer */
	}
	dup2(f, 1);
	close(f);
    }
}

setup_layout(device, win_width, win_height, flags, width, height, 
	     header, trailer, scale, orientation)
enum device device;
int win_width;
int win_height;
int flags;
int width;
int height;
char *header;
char *trailer;
int *scale;
enum orientation *orientation;
{
    register int w_scale;
    register int h_scale;
    register int iscale = *scale;
    register int w_max;
    register int h_max;

    if (header != NULL) win_height += 75;
    if (trailer != NULL) win_height += 75;

    /* check maximum width and height; set orientation and scale*/
    if (device == LN03) {
	if ((win_width < win_height || (flags & F_PORTRAIT)) && 
	    !(flags & F_LANDSCAPE)) {
	    *orientation = PORTRAIT;
	    w_max = (width > 0)? width : W_MAX;
	    h_max = (height > 0)? height : H_MAX;
	    w_scale = w_max / win_width;
	    h_scale = h_max / win_height;
	    *scale = min(w_scale, h_scale);
	} else {
	    *orientation = LANDSCAPE;
	    w_max = (width > 0)? width : H_MAX;
	    h_max = (height > 0)? height : W_MAX;
	    w_scale = w_max / win_width;
	    h_scale = h_max / win_height;
	    *scale = min(w_scale, h_scale);
	}
    } else {			/* device == LA100 */
	*orientation = PORTRAIT;
	*scale = W_MAX / win_width;
    }
    if (*scale == 0) *scale = 1;
    if (*scale > 6) *scale = 6;
    if (iscale > 0 && iscale < *scale) *scale = iscale;
}

dump_sixmap(sixmap, iw, ih)
register unsigned char (*sixmap)[];
int iw;
int ih;
{
    register int i, j;
    register unsigned char *c;

    c = (unsigned char *)sixmap;
    fprintf(stderr, "Sixmap:\n");
    for (i = 0; i < ih; i++) {
	for (j = 0; j < iw; j++) {
	    fprintf(stderr, "%02X ", *c++);
	}
	fprintf(stderr, "\n\n");
    }
}

build_sixmap(iw, ih, sixmap, wpad, hpad)
int ih;
int iw;
unsigned char (*sixmap)[];
int wpad;
int hpad;
{
    int iwb, iww;
    int rsize, cc;
    int w, maxw;
    struct iovec linevec[6];
    unsigned char line[6][150];
    register unsigned char *c;
    register int i, j, k, m;
    register int sixel;
    static int mask[] = {~1, ~2, ~4, ~8, ~16, ~32, ~64, ~128};

    c = (unsigned char *)sixmap;
    iwb = (iw + wpad) / 8;

    for (i = 0; i <= 5; i++) {
	linevec[i].iov_base = (caddr_t)line[i];
	linevec[i].iov_len = iwb;
    }

    while (--ih >= 0) {
	if (ih > 0 || hpad == 0) {
	    rsize = iwb * 6;
	    while (rsize > 0) {
		cc = readv(0, linevec, 6);
		if (cc == 0) break;
		rsize -= cc;
	    }
	} else {
	    i = 6 - hpad;
	    rsize = iwb * i;
	    while (rsize > 0) {
		cc = readv(0, linevec, i);
		if (cc == 0) break;
		rsize -= cc;
	    }
	    for (; i < 6; i++)
		for (j = 0; j < iwb; j++) line[i][j] = 0xFF;
	}

#ifndef NOINLINE
	for (i = 0; i < iw; i++) {
	    sixel =  extzv(line[0], i, 1);
	    sixel |= extzv(line[1], i, 1) << 1;
	    sixel |= extzv(line[2], i, 1) << 2;
	    sixel |= extzv(line[3], i, 1) << 3;
	    sixel |= extzv(line[4], i, 1) << 4;
	    sixel |= extzv(line[5], i, 1) << 5;
	    *c++ = sixel;
	}
#else
	for (i = 0, w = iw; w > 0; i++) {
	    for (j = 0; j <= 7; j++) {
		m = mask[j];
		k = -j;
		sixel =  ((line[0][i] & ~m) << k++);
		sixel |= ((line[1][i] & ~m) << k++);
		sixel |= ((line[2][i] & ~m) << k++);
		sixel |= ((line[3][i] & ~m) << k++);
		sixel |= ((line[4][i] & ~m) << k++);
		sixel |= ((line[5][i] & ~m) << k);
		*c++ = sixel;
		if (--w == 0) break;
	    }
	}
#endif
    }
}

build_output_filename(name, device, oname)
register char *name, *oname;
enum device device;
{
    while (*name && *name != '.') *oname++ = *name++;
    switch (device) {
    case LN03:	bcopy(".ln03", oname, 6); break;
    case LA100:	bcopy(".la100", oname, 7); break;
    }
}


ln03_grind_fonts(sixmap, iw, ih, scale, pixmap)
unsigned char (*sixmap)[];
int iw;
int ih;
int scale;
struct pixmap (**pixmap)[];
{
}


ln03_setup(iw, ih, orientation, scale, left, top, left_margin, top_margin, 
	   flags, header, trailer)
int iw;
int ih;
enum orientation orientation;
int scale;
int left;
int top;
int *left_margin;
int *top_margin;
int flags;
char *header;
char *trailer;
{
    register int i;
    register int lm, tm, xm;
    char fontname[6];
    char buf[256];
    register char *bp = buf;
	
    if (!(flags & F_APPEND)) {
	sprintf(bp, LN_RIS); bp += 2;
	sprintf(bp, LN_SSU, 7); bp += 5;
	sprintf(bp, LN_PUM_SET); bp += sizeof LN_PUM_SET - 1;
    }

    if (orientation == PORTRAIT) {
	lm = (left > 0)? left : (((W_MAX - scale * iw) / 2) + W_MARGIN);
	tm = (top > 0)? top : (((H_MAX - scale * ih * 6) / 2) + H_MARGIN);
	sprintf(bp, LN_PFS, "?20"); bp += 7;
	sprintf(bp, LN_DECOPM_SET); bp += sizeof LN_DECOPM_SET - 1;
	sprintf(bp, LN_DECSLRM, lm, W_PAGE - lm); bp += strlen(bp);
    } else {
	lm = (left > 0)? left : (((H_MAX - scale * iw) / 2) + H_MARGIN);
	tm = (top > 0)? top : (((W_MAX - scale * ih * 6) / 2) + W_MARGIN);
	sprintf(bp, LN_PFS, "?21"); bp += 7;
	sprintf(bp, LN_DECOPM_SET); bp += sizeof LN_DECOPM_SET - 1;
	sprintf(bp, LN_DECSLRM, lm, H_PAGE - lm); bp += strlen(bp);
    }

    if (header != NULL) {
	sprintf(bp, LN_VPA, tm - 100); bp += strlen(bp);
	i = strlen(header);
	xm = (((scale * iw) - (i * 30)) / 2) + lm;
	sprintf(bp, LN_HPA, xm); bp += strlen(bp);
	sprintf(bp, LN_SGR, 3); bp += strlen(bp);
	bcopy(header, bp, i);
	bp += i;
    }
    if (trailer != NULL) {
	sprintf(bp, LN_VPA, tm + (scale * ih * 6) + 75); bp += strlen(bp);
	i = strlen(trailer);
	xm = (((scale * iw) - (i * 30)) / 2) + lm;
	sprintf(bp, LN_HPA, xm); bp += strlen(bp);
	sprintf(bp, LN_SGR, 3); bp += strlen(bp);
	bcopy(trailer, bp, i);
	bp += i;
    }

    sprintf(bp, LN_HPA, lm); bp += strlen(bp);
    sprintf(bp, LN_VPA, tm); bp += strlen(bp);
    sprintf(bp, LN_SIXEL_GRAPHICS, 9, 0, scale); bp += strlen(bp);
    sprintf(bp, "\"1;1"); bp += 4; /* Pixel aspect ratio */
    write(1, buf, bp-buf);
    *top_margin = tm;
    *left_margin = lm;
}

#define LN03_RESET "\033c"

ln03_finish()
{
    write(1, LN03_RESET, sizeof LN03_RESET - 1);
}

la100_setup(iw, ih, scale)
{
    unsigned char buf[256];
    register unsigned char *bp;
    int lm, tm;

    bp = buf;
    lm = ((80 - (int)((double)iw / 6.6)) / 2) - 1;
    if (lm < 1) lm = 1;
    tm = ((66 - (int)((double)ih / 2)) / 2) - 1;
    if (tm < 1) tm = 1;
    sprintf(bp, "\033[%d;%ds", lm, 81-lm); bp += strlen(bp);
    sprintf(bp, "\033[?7l"); bp += 5;
    sprintf(bp, "\033[%dd", tm); bp += strlen(bp);
    sprintf(bp, "\033[%d`", lm); bp += strlen(bp);
    sprintf(bp, "\033P0q"); bp += 4;
    write(1, buf, bp-buf);
}

#define LA100_RESET "\033[1;80s\033[?7h"

la100_finish()
{
    write(1, LA100_RESET, sizeof LA100_RESET - 1);
}

ln03_alter_background(sixmap, iw, ih)
unsigned char (*sixmap)[];
int iw;
int ih;
{
    register int size;
    register unsigned char *c, *stopc;
    register unsigned char *startc;
    register int n;

    c = (unsigned char *)sixmap;
    stopc = c + (iw * ih);
    n = 0;
    while (c < stopc) {
	switch (*c) {
	case 0x08: case 0x11: case 0x04: case 0x22:
	case 0x20: case 0x21: case 0x24: case 0x00:
	    if (n == 0) startc = c;
	    n++;
	    break;

	default:
	    if (n >= 2) {
		while (n-- > 0) *startc++ = 0x00;
	    } else {
		n = 0;
	    }
	    break;
	}
	c++;
    }
}

ln03_output_sixels(sixmap, iw, ih, nosixopt, split, scale, top_margin, 
		   left_margin)
unsigned char (*sixmap)[];
int iw;
int ih;
int nosixopt;
int split;
int top_margin;
int left_margin;
{
    unsigned char *buf;
    register unsigned char *bp;
    int i;
    int j;
    register int k;
    register unsigned char *c;
    register int lastc;
    register int count;
    char snum[6];
    register char *snp;

    bp = (unsigned char *)malloc(iw*ih+512);
    buf = bp;
    count = 0;
    lastc = -1;
    c = (unsigned char *)sixmap;
    split = ih / split;		/* number of lines per page */

    iw--;			/* optimization */
    for (i = 0; i < ih; i++) {
	for (j = 0; j <= iw; j++) {
	    if (!nosixopt) {
		if (*c == lastc && j < iw) {
		    count++;
		    c++;
		    continue;
		}
		if (count >= 3) {
		    bp--;
		    count++;
		    *bp++ = '!';
		    snp = snum;
		    while (count > 0) {
			k = count / 10;
			*snp++ = count - (k * 10) + '0';
			count = k;
		    }
		    while (--snp >= snum) *bp++ = *snp;
		    *bp++ = (~lastc & 0x3F) + 0x3F;
		} else if (count > 0) {
		    lastc = (~lastc & 0x3F) + 0x3F;
		    do {
			*bp++ = lastc;
		    } while (--count > 0);
		}
	    }
	    lastc = *c++;
	    *bp++ = (~lastc & 0x3F) + 0x3F;
	}
	*bp++ = '-';		/* New line */
	lastc = -1;
	if ((i % split) == 0 && i != 0) {
	    sprintf(bp, LN_ST); bp += sizeof LN_ST - 1;
	    *bp++ = '\f';
	    sprintf(bp, LN_VPA, top_margin + (i * 6 * scale)); bp += strlen(bp);
	    sprintf(bp, LN_HPA, left_margin); bp += strlen(bp);
	    sprintf(bp, LN_SIXEL_GRAPHICS, 9, 0, scale); bp += strlen(bp);
	    sprintf(bp, "\"1;1"); bp += 4;
	}
    }

    sprintf(bp, LN_ST); bp += sizeof LN_ST - 1;
    *bp++ = '\f';
    write(1, buf, bp-buf);
}

la100_output_sixels(sixmap, iw, ih)
unsigned char (*sixmap)[];
int iw;
int ih;
{
    unsigned char *buf;
    register unsigned char *bp;
    int i;
    register int j, k;
    register unsigned char *c;
    register int lastc;
    register int count;
    char snum[6];

    bp = (unsigned char *)malloc(iw*ih+512);
    buf = bp;
    count = 0;
    lastc = -1;
    c = (unsigned char *)sixmap;

    for (i = 0; i < ih; i++) {
	for (j = 0; j < iw; j++) {
	    if (*c == lastc && (j+1) < iw) {
		count++;
		c++;
		continue;
	    }
	    if (count >= 2) {
		bp -= 2;
		count = 2 * (count + 1);
		*bp++ = '!';
		k = 0;
		while (count > 0) {
		    snum[k++] = (count % 10) + '0';
		    count /= 10;
		}
		while (--k >= 0) *bp++ = snum[k];
		*bp++ = (~lastc & 0x3F) + 0x3F;
		count = 0;
	    } else if (count > 0) {
		lastc = (~lastc & 0x3F) + 0x3F;
		do {
		    *bp++ = lastc;
		    *bp++ = lastc;
		} while (--count > 0);
	    }
	    lastc = (~*c & 0x3F) + 0x3F;
	    *bp++ = lastc;
	    *bp++ = lastc;
	    lastc = *c++;
	}
	*bp++ = '-';		/* New line */
	lastc = -1;
    }

    sprintf(bp, LN_ST); bp += sizeof LN_ST - 1;
    *bp++ = '\f';
    write(1, buf, bp-buf);
}
