/* main.c	1.8	83/07/06
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *	This file contains the main and file system dependent routines
 * for producing hard copy from gremlin files.  It is extensively modified
 * from the vplot source.
 */

#include "gprint.h"
#include <signal.h>
#include <vfont.h>

#define LPR "/usr/ucb/lpr"

extern char *mktemp();
extern char *malloc();
extern char *rindex();

/* imports */
extern HGtline(), HGArc(), HGPutText(), HGMove(), HGSetFont();
extern HGSetBrush(), HGInitFont(), HGPrintElt();
extern int style[], thick[];
extern char *tfont[], *tsize[];
 
/* database imports */

extern ELT *DBInit(), *DBRead();
extern POINT *PTInit(), *PTMakePoint();

int	linethickness = 0;	/* brush styles */
int	linmod = SOLID;
char	*obuf;			/* output buffer NumOfLin x DevRange/8 */
int	bufsize;		/* output buffer size */
int	lastx;
int	lasty;
int	angle, startx, starty, endx, endy;
double	scale = 4.0;		/* Variables used to map gremlin screen */
double	orgx = 0.0;		/* x and y origin of gremlin picture */
double	orgy = 0.0;

FILE	*pfp = stdout;		/* file descriptor of current picture */
char	picture[] = "/usr/tmp/rastAXXXXXX";
int	run = 13;		/* index of 'a' in picture[] */
int	DevRange = Vxlen;	/* plot dimensions in pixels */
int	DevRange8 = Vxlen/8;
int	BytPrLin = Vbytperlin;	/* Bytes per raster line. (not DevRange8) */
int	NumOfLin = Vylen;

int	lparg = 6;		/* index into lpargs */
char	*lpargs[50] = { "lpr", "-Pvarian", "-v", "-s", "-r", "-J", };

int	Orientation;		/* variables used to print from font file */
int	cfont = 0;
int	csize = 0;
struct	header header;
struct	dispatch dispatch[256];
char	*bits = NULL;
char	*fontdir = "/usr/lib/vfont/";

main(argc, argv)
int argc;
char *argv[];
{
	FILE *fp, *fopen();
	ELT *PICTURE, *e;
	POINT *p1, pos;
	char *file[50], string[10], *arg;
	char c, string1[50], string2[50], string3[50], string4[50], 
		string5[50], string6[50], string7[50], string8[50]; 
	extern int cleanup();
	float mult;
	int WriteRaster = FALSE;
	register char *cp1, *cp2;
	register int i, k;
	int brsh, gfil = 0;

	/* Parse the command line. */

	argc--;
	argv++;
	while (argc--) {
		if (*(arg = *argv++) != '-')
			file[gfil++] = arg;
		else switch (*++arg) {
		case 'W':	/* Print to wide (versatec) device */
			DevRange = Wxlen;
			DevRange8 = Wxlen/8;
			BytPrLin = Wbytperlin;
			NumOfLin = Wylen;
			lpargs[1] = "-Pversatec";
			break;
		case 'V':	/* Print to narrow (varian) device */
			DevRange = Vxlen;
			DevRange8 = Vxlen/8;
			BytPrLin = Vbytperlin;
			NumOfLin = Vylen;
			lpargs[1] = "-Pvarian";
			break;
		case '1':	/* select size 1 */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			tsize[0] = arg;
			break;
		case '2':	/* select size 2 */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			tsize[1] = arg;
			break;
		case '3':	/* select size 3 */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			tsize[2] = arg;
			break;
		case '4':	/* select size 4 */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			tsize[3] = arg;
			break;
		case 'R':	/* select Roman font */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			tfont[0] = arg;
			break;
		case 'I':	/* select italics font */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			tfont[1] = arg;
			break;
		case 'B':	/* select bold font */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			tfont[2] = arg;
			break;
		case 'S':	/* select special font */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			tfont[3] = arg;
			break;
		case 'N':	/* select narrow brush width */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			(void) sscanf(arg, "%d", &brsh);
			thick[0] = thick[1] = thick[3] = thick[4] = brsh;
			break;
		case 'T':	/* select thick brush width */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			(void) sscanf(arg, "%d", &brsh);
			thick[2] = brsh;
			break;
		case 'M':	/* select medium brush width */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			(void) sscanf(arg, "%d", &brsh);
			thick[5] = brsh;
			break;
		case 't':	/* send raster to standard output */
			WriteRaster = TRUE;
			break;
		case 'x':	/* select scale */
			if (*++arg == '\0' && argc--)
				arg = *argv++;
			sscanf(arg,"%f", &mult);
			scale *= mult;
			break;
		case 'p':	/* prompt for font and size parameters */
			printf("Roman font name? (%s): ", tfont[0]);
			gets(string1);
			if (*string1 != '\0') tfont[0] = string1;
			printf("Italic font name? (%s): ", tfont[1]);
			gets(string2);
			if (*string2 != '\0') tfont[1] = string2;
			printf("Bold font name? (%s): ", tfont[2]);
			gets(string3);
			if (*string3 != '\0') tfont[2] = string3;
			printf("Special font name? (%s): ", tfont[3]);
			gets(string4);
			if (*string4 != '\0') tfont[3] = string4;
			printf("font size 1? (%s): ", tsize[0]);
			gets(string5);
			if (*string5 != '\0') tsize[0] = string5;
			printf("font size 2? (%s): ", tsize[1]);
			gets(string6);
			if (*string6 != '\0') tsize[1] = string6;
			printf("font size 3? (%s): ", tsize[2]);
			gets(string7);
			if (*string7 != '\0') tsize[2] = string7;
			printf("font size 4? (%s): ", tsize[3]);
			gets(string8);
			if (*string8 != '\0') tsize[3] = string8;
			printf("narrow brush size? (%d): ", thick[0]);
			gets(string);
			if (*string != '\0') {
			    sscanf(string, "%d", &brsh);
			    thick[0] = thick[1] = thick[3] = thick[4] = brsh;
			}
			printf("medium brush size? (%d): ", thick[5]);
			gets(string);
			if (*string != '\0') {
			    sscanf(string, "%d", &brsh);
			    thick[5] = brsh;
			}
			printf("thick brush size? (%d): ", thick[2]);
			gets(string);
			if (*string != '\0') {
			    sscanf(string, "%d", &brsh);
			    thick[2] = brsh;
			}
			break;
		default:
			fprintf(stderr, "unknown switch: %c\n", *arg);
		}
	}

	if (WriteRaster) {		/* over-ride dimension settings */
		DevRange = Vxlen;	/* if printing to file to be gdumped */
		DevRange8 = Vxlen/8;
		BytPrLin = DevRange8;
	}

	signal(SIGTERM, cleanup);
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, cleanup);
	mktemp(picture);
	if ((obuf = malloc(bufsize = NumOfLin * DevRange8)) == NULL) {
		fprintf(stderr,"gprint: ran out of memory for output buffer\n");
		exit(1);
	}
	if (gfil == 0) {	/* no filename, use standard input */
		file[0] = NULL;
		gfil++;
	}
	for (k=0; k<gfil; k++) {
		if (file[k] != NULL) {
			if ((fp = fopen(file[k], "r")) == NULL) {
			    fprintf(stderr, "gprint: can't open %s\n", file[k]);
			    continue;
			}
			if (k == 0) {
				if ((arg = rindex(file[k], '/')) == NULL)
					arg = file[k];
				else
					arg++;
				lpargs[lparg++] = arg;
			}
		} else {
			fp = stdin;
			lpargs[lparg++] = "gremlin";
		}
		/* read picture file */
		PICTURE = DBRead(fp, &Orientation, &pos);
		fclose(fp);
		if (DBNullelt(PICTURE))
			continue;

		if (!WriteRaster) {
			umask(022);
			if ((pfp = fopen(picture, "w")) == NULL) {
			    fprintf(stderr,"gprint: can't create %s\n",picture);
			    cleanup();
			}
		}
		i = strlen(picture) + 1;
		if ((arg = malloc(i)) == NULL) {
			fprintf(stderr, "gprint: ran out of memory\n");
			cleanup();
		}
		strcpy(arg, picture);
		lpargs[lparg++] = arg;
		picture[run]++;
		cp2 = &obuf[bufsize];
		for (cp1 = obuf; cp1 < cp2; )
			*cp1++ = 0;
		e = PICTURE;
		while (!DBNullelt(e)) {
			HGPrintElt(e);	/* traverse picture;  print elements */
			e = DBNextElt(e);
		}
		if (WriteRaster)	/* if -t then cut image length */
		    while (!*--cp2);
		for (cp1 = obuf; cp1 < cp2; ) {			/* write file */
			for (i = DevRange8; i--; cp1++)
				putc(*cp1, pfp);
			for (i = BytPrLin - DevRange8; i--; )
				putc('\0', pfp);
		}
		if (!WriteRaster)
			fclose(pfp);
	}
	if (!WriteRaster) {
		lpargs[lparg] = 0;
		execv(LPR, lpargs);
		fprintf(stderr, "gprint: can't exec %s\n", LPR);
		cleanup();
	}
	exit(0);
}

cleanup()
{
	do
		unlink(picture);
	while (picture[run]-- != 'A');
	exit(1);
}

/*
 * Points should be in the range 0 <= x < DevRange, 0 <= y < NumOfLin.
 * The origin is the top left-hand corner with increasing x towards the
 * right and increasing y going down.
 * The output array is NumOfLin x DevRange/8 pixels.
 */
point(x, y)
register int x, y;
{
	register unsigned byte;

	if ((unsigned) x < DevRange && (unsigned) y < NumOfLin) {
		byte = y * DevRange8 + (x >> 3);
		obuf[byte] |= 1 << (7 - (x & 07));
	}
}
