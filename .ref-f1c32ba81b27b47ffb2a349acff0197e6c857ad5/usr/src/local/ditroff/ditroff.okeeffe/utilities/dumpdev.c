#ifndef lint
static char sccsid[] = "@(#)dumpdev.c	1.2 (CWI) 85/10/24";
#endif lint

/*
 * inverse of makethev, dump de information of the (binary) device information
 * will also dump the font info of standard (default) mounted fonts.
 *
 * Usage:
 * dumpdev [flags] device
 *
 * flags:
 *	-f name;	take info from dir name instead of default dir
 *	-Tdevice;	device for wich the dump is made;
 *	-D;		dump only the device info, not the font info
 *	-F F1 F2 ...;	dump only the named fonts info
 *	-d;		give extra debug info on error output
 *
 * Author: jaap akkerhuis, Mathematisch Centrum, Oc 1982
 *
 */

# include "../dev.h"
# include <stdio.h>

# define BMASK	0377
# define FATAL	1

struct dev	dev;
struct Font	font;

char *fontdir	= "/usr/local/lib/ditroff/font";
char *devname	= "har";	/* devicename */

int	nfonts;
int	nsizes;
int	nchtab;

# define NSIZE	100	/* maximum number of sizes */

# define NCH	256	/* maximum number of characters with funny names */

# define FSIZE	200	/* size of physical font */

# define NFONT	10	/* Maximum number of default fonts */

int	dbg;
int 	Dflag;
int	Fflag;

main(argc, argv)
int	argc;	char *argv[];
{	FILE *fp;

	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'f':
			fontdir = argv[2];
			argv++;
			argc--;
			break;
		case 'd':
			dbg ++;
			break;
		case 'T':
			devname = &argv[1][2];
			break;
		case 'D':
			Dflag++;
			break;
		case 'F':
			Fflag++;
			break;
		default:
			fprintf( stderr, "Unknown option %c\n", argv[1][1]);
			break;
		}
		argv++;
		argc--;
	}

	if(devname == NULL)
		error(FATAL,"No device specified");

	getdesc();

	if(Dflag)
		exit(0);

	if( Fflag)
		while ( argc > 1) {
			getfont( argv[1] );
			argv++;
			argc--;
		}
}

error(f, s, a1, a2, a3, a4, a5, a6, a7) {
	fprintf(stderr, "dumpdev: ");
	fprintf(stderr, s, a1, a2, a3, a4, a5, a6, a7);
	fprintf(stderr, "\n");
	if (f)
		exit(1);
}

/*
 * structure of a device file.
 *
 * the first part consists of the structure dev.
 *
 * Notes: dev.filesize contains the size of the file minus the strcture dev
 * 	dev.nchtab contains the nimber of funny charnames +1
 *
 * then follows a list of sizes (shorts), ended with a zero.
 *
 * then follows a table of dev.nchtab pointers ( shorts  ),
 * 	these will point to the strings with all the funnynames.
 *	this is called chtab.
 *
 * after this is the table of funny names (chname) which is dev.lnchname
 *	bytes big.
 *
 * So up uo here the device charactistics are read.
 *
 * Then follows the default mounted font info, dev.nfont times (max NFONT).
 *
 * first the font structure.
 *
 * font.nwfonts is the amount of widths of the font, so it will be used
 * as the amount of characters in the font as well.
 * 
 * so now will follow:
 *	the widthtable (font.nwfonts bytes) containg the widths info
 *	the kerntable (font.nwfonts bytes) containing the de- & ascender info
 *	the codetable (font.nwfonts bytes) containing the codes for the chars
 *	the fitable (dev.nchtab+128-32 bytes) containing indexes to the
 *		previous three tables.
 *	
 *	if font.fonttab == 1
 *		will also follow the fcodetable (font.nwfonts (sizeof(short))
 *		containing the physical font numbers ( see also the comment
 *		added by jna in makedev.c)
 *
 * for info about the use of this tables, see the comment at dumpfont.
 *
 */

char *chname;
short *chtab;

getdesc()
{	
	char *malloc(), *filebase, *p;
	struct	Font	*fontbase[NFONT];
	short *pstab, *p1;
	int i, fin, nw;
	char temp[60];

	sprintf(temp, "%s/dev%s/DESC.out", fontdir, devname);

	if((fin = open(temp, 0)) < 0)
		error(FATAL, "can't open DESC.out for %s\n", temp);

	printf("# Dump of device %s (%s)\n", devname, temp);

	if((read(fin, &dev, sizeof(struct dev))) != sizeof(struct dev))
		error(FATAL, "read error reading devstruct %s", temp);
	nfonts = dev.nfonts;
	nsizes = dev.nsizes;
	nchtab = dev.nchtab;
	if(nfonts > NFONT)
		error(!FATAL,"More (%d) fonts then possible (%d)",
			nfonts, NFONT);
	if(nsizes > NSIZE)
		error(!FATAL,"More (%d) sizes then possible (%d)",
			nsizes, NSIZE);
	if(nchtab > NCH)
		error(!FATAL,"More (%d) names then possible (%d)",
			nchtab, NCH);
	if(dbg) {
		fprintf(stderr,
	"filesize %d, default fonts %d, sizes %d, funny names %d lchname %d\n",
		dev.filesize, dev.nfonts, dev.nsizes, dev.nchtab, dev.lchname);
		fprintf(stderr,
	"sizescale %d, paperwidth %d, paperlenght %d, spare1 %d, spare2 %d\n",
		dev.sizescale, dev.paperwidth, dev.paperlength, dev.spare1,
		dev.spare2);
	}

	printf("res %d\nhor %d\nvert %d\nunitwidth %d\n",
			dev.res, dev.hor, dev.vert, dev.unitwidth);
	if( dev.sizescale)
		printf("sizescale %d\n", dev.sizescale);
	if(dev.paperwidth)
		printf("paperwidth %d\n", dev.paperwidth);
	if(dev.paperlength)
		printf("paperlength %d\n", dev.paperlength);
	if(dev.spare1)
		printf("spare1 %d\n", dev.spare1);
	if(dev.spare2)
		printf("spare2 %d\n", dev.spare2);

	filebase = malloc(dev.filesize);	/* enough room for whole file */
	if((read(fin, filebase, dev.filesize)) != dev.filesize)	/* all at once */
		error(FATAL, "read error reading fontinfo %s", temp);
	pstab = (short *) filebase;
	
	printf("sizes ");
	i = 0;
	for( p1 = pstab; *p1; p1++) {
		i++;
		printf("%d ",*p1);
	}
	printf("\n");
	if ( i != nsizes)
		error(!FATAL, "%s sizes (%d) then expected (%d)\n",
			i > nsizes ? "More" : "Less", i, nsizes);

	chtab = pstab + nsizes + 1;	/* table of indexes in chname */
	chname = (char *) (chtab + dev.nchtab);	/* start of name table */
	p = chname + dev.lchname;	/* beginning of first font */

	for ( i = 0; i < nfonts; i++) {	/* pickup the font names */
		fontbase[i] = (struct Font *) p;
		nw = *p & BMASK;	/* first thing is width count */
		p += sizeof(struct Font);
		p += 3 * nw + dev.nchtab + 128 - 32;
		if(fontbase[i]->fonttab == 1)
			p += nw * sizeof( short );
	}
	printf("fonts %d", nfonts);
	for ( i = 0; i < nfonts; i++)
		printf(" %s",fontbase[i]->namefont);
	printf("\n");

	if(dbg) {
		fprintf(stderr, "Indexes:");
		p1 = chtab;
		i = 0;
		for( p1 = chtab; p1 < chtab + dev.nchtab - 1; p1++) {
			i++;
			fprintf(stderr, " %d", *p1);
			if( i == 16) {
				fprintf(stderr,"\n");
				i = 0;
			}
		}
		if( i != 0)
			fprintf(stderr, "\n");
	}

	printf("charset\n");
	i = 0;
	for( p1 = chtab; p1 < chtab + dev.nchtab -1; p1++) {
		int i2;
		i++;
		printf("  %s", chname + *p1);
		if( i == 16 || (i2 == 0 & i == 4)) {
			printf("\n");
			i = 0;
			if( i2 == 0)
				i2++;
		}
	}
	if( i != 0)
		printf("\n");

	if( !Dflag)
		if ( !Fflag)
			for( i = 0; i < nfonts ; i++ )
				dumpfont( fontbase[i]);
	close( fin );
}

/*
 * How to use the tables
 *
 * the fitable (font index table) contains indexes to the information about
 * all the characters of a device that can be printed.
 * The device is supposed to have all (128-32) printable ascii chars.
 * We rely on thus idea
 * There are also an unknown numer (den.nchtab -1) funny chars.
 * So this make it clear why fitab is device dependent and not font dependent. 
 *
 * For ascii characters you get your information by:
 *	fitab[inputchar-32] will have the index in the tables,
 *	if the index is 0, the char doesn't exist on this font
 *		so f.i. codetab[fitab[inputchar-32]] will give you the
 *		outputcode.
 *
 * For funny chars:
 * 	Compare the string of the funny char with strings in nchname table
 *	if the nth string are the same, you can find the index by
 *	fitab[n + 128-32]
 *	if the index is 0, the char doesn't exist on this font
 *	and the kerning info f.i. by
 *	kerntab[fitab[n + 128-32]]
 *	if font.fonttab == 1
 *		There will also be a font code table, this is found by
 *		the same ways.
 *	if n >= dev.nchtab, the funny name was illegal.
 *
 * Note:
 *	Width[0] contains the spacesize, set by or the spacesize command
 *	while constructing the font file, or the default spacesize.
 *
 */

dumpfont( font )
struct Font *font;
{	char *p;
	int nw;
	int i, c;
	char *kerntab, *fitab, *widthtab, *codetab;
	short *fcode;

	p = (char *) font;

	nw = *p & BMASK;

	p += sizeof(struct Font);

	widthtab = p;
	p += nw;
	kerntab = p;
	p += nw;
	codetab = p;
	p += nw;
	fitab = p;
	if( font->fonttab == 1) {	/* the fcode tab is here */
		p += nchtab + 128 -32;
		fcode = (short *) p;
	}

	printf("# Fontinfo for %s\n", devname);
	printf("name %s\n", font->namefont);
	printf("internalname %s\n", font->intname);
	if( font->specfont )
		printf("special\n");
	mklig( font->ligfont );
	if( font->fonttab )
		printf("fonttab\n");
	if( font->slant )
		printf("slant %d\n", font-> slant);
	if( *widthtab )	/* widthtab[0] contains the spacewidth */
		printf( "spacewidth %d\n", *widthtab & BMASK);

	/* now print the contents of this font */

	/* first the ascii chars */
	for( c = 0; c < 128 - 32; c++) {
		i = fitab[c];
		if( i ) {
			printf("%c\t%d\t%o\t0%o",
			  (c + 32) & BMASK, widthtab[i] & BMASK,
			   kerntab[i] & BMASK, codetab[i] & BMASK);
			if(font->fonttab == 1)
				printf("\t%d\n", fcode[i]);
			else
				printf("\n");
		}
	}

	/* and now the special ones */
	for( c = 0; c < nchtab; c++) {
		i = (unsigned char)fitab[c + 128 - 32];
		if(i) {
			printf("%s\t%d\t%o\t0%o",
			  &chname[chtab[c]], widthtab[i] & BMASK,
			   kerntab[i] & BMASK, codetab[i] & BMASK);
			if(font->fonttab == 1)
				printf("\t%d\n", fcode[i]);
			else
				printf("\n");
		}
	}
}


mklig( c )
char c;
{
	if( !c )
		return;

	printf("ligatures");
	
	if( c & LFF)
		printf(" ff");
	if( c & LFI)
		printf(" fi");
	if( c & LFL)
		printf(" fl");
	if( c & LFFI)
		printf(" ffi");
	if( c & LFFL)
		printf(" ffl");
	printf(" 0\n");
}

getfont( fname )
char *fname;
{	char *malloc(), *p;
	int fin, size;
	char temp[60];

	sprintf(temp,"%s/dev%s/%s.out", fontdir, devname, fname);

	if((fin = open(temp, 0)) < 0)
		error(FATAL, "can't open %s\n", temp);

	printf("# Dump of font %s (%s)\n", fname, temp);

	size = lseek(fin, 0L, 2);	/*get size of file*/
	if(dbg)
		fprintf(stderr, "Size of font %s: %d\n", temp, size);

	lseek(fin, 0L, 0);

	p = malloc( size);

	if((read(fin, p, size)) != size )
		error(FATAL, "read error at %s\n", temp);

	dumpfont(p);
	free( p );
	close (fin);
}
