/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)vfontinfo.c	5.1 (Berkeley) %G%";
#endif /* not lint */

/* Font Information for VCat-style fonts
 *      Andy Hertzfeld  4/79
 *
 *	Modified to print Ascii chars 1/80 by Mark Horton
 *	Zoom option added 5/81 by Steve Stone with tables from Mark Horton.
 *	Message option added 5/31 by Mark Horton
 */
#include <stdio.h>
#include <ctype.h>
#include <vfont.h>

struct header FontHeader;
struct dispatch disptable[256];

char	IName[100];
char *	rdchar();
long	fbase;

char	defascii[256];
char	*charswanted = defascii;
int	verbose;
char	charbits[4000];
int	H, W, WB, base;
int 	zoom = 1;

char msgout[24][80];
int msgflag = 0;
int curline, curcol;	/* cursor, numbered from lower left corner */
int minline=24, maxline=0, maxcol=0;

main(argc,argv)
int argc;
char **argv;

{
	int FID,i,j;

	while (argc > 1 && argv[1][0] == '-') {
		switch(argv[1][1]) {
		case 'v':
			verbose++;
			break;
		case 'z':
			zoom = argv[1][2] - '0';
			break;
		case 'm':
			msgflag = 1;
			zoom = 2;
			for (i=0; i<24; i++)
				for (j=0; j<80; j++)
					msgout[i][j] = ' ';
			curline = 5; curcol = 0;
			break;
		default:
			printf("Bad flag: %s\n", argv[1]);
		}
		argc--; argv++;
	}
	if (argc < 2) {
		fprintf(stderr,"Usage: %s filename", argv[0]);
		exit(2);
	}

	for (i=0; i<128; i++)
		defascii[i] = i;
	if (argc >= 3)
		charswanted = argv[2];

	sprintf(IName,"/usr/lib/vfont/%s",argv[1]);
	if ((FID = open(argv[1],0)) < 0)
		if ((FID = open(IName,0)) < 0) { 
			printf("Can't find %s\n",argv[1]);
			exit(8); 
		};

	if (read(FID,&FontHeader,sizeof FontHeader) != sizeof FontHeader)
		error("Bad header in Font file.");

	if (read(FID,&disptable[0],sizeof disptable) != sizeof disptable)
		error("Bad dispatch table in Font file");

	fbase = sizeof FontHeader + sizeof disptable;

	if (FontHeader.magic != 0436)
		printf("Magic number %o wrong\n", FontHeader.magic);
	if (!msgflag) {
		printf("Font %s, ",argv[1]);
		printf("raster size %d, ",FontHeader.size);
		printf("max width %d, max height %d, xtend %d\n",
			FontHeader.maxx, FontHeader.maxy,FontHeader.xtend);
		printf("\n");
		for (i = strlen(argv[1]) + 1; i > 0; --i)
			printf(" ");
		printf("ASCII     offset    size  left    right   up     down    width \n");
	}

	for (i=0; i<256; i++) {
		j = charswanted[i];
		if (i>0 && j==0)
			break;
		if (disptable[j].nbytes != 0) {
			if (!msgflag)
				printf("%s  %3o %2s     %4d   %4d   %4d   %4d   %4d   %4d   %5d\n",
					argv[1],
					j, rdchar(j),
					disptable[j].addr,
					disptable[j].nbytes,
					disptable[j].left,
					disptable[j].right,
					disptable[j].up,
					disptable[j].down,
					disptable[j].width);
			if (verbose || msgflag) {
				int len = disptable[j].nbytes;
				int k, l, last;

				lseek(FID, fbase+disptable[j].addr, 0);
				read(FID, charbits, len);
				H = (disptable[j].up) + (disptable[j].down);
				W = (disptable[j].left) + (disptable[j].right);
				base = disptable[j].up;
				WB = (W+7)/8;
				if (zoom < 0) {
					/*
					 * Old 1 for 1 code.  The aspect ratio
					 * is awful, so we don't use it.
					 */
					for (k=0; k<H; k++) {
						for (last=W-1; last >= 0; last--)
							if (fbit(k, last))
								break;
						for (l=0; l<=W-1; l++) {
							printf("%c", fbit(k,l)?'M':' ');
						}
						printf("\n");
					}
					printf("\n");
				} else {
					shozoom();
					if (msgflag) {
						k = disptable[j].width;
						if (zoom == 0) k *= 2;
						else if (zoom == 2) k /= 2;
						curcol += k;
					}
				}
			}
		}
	}
	if (msgflag) {
		for (i=maxline; i>=minline; i--) {
			for (j=0; j<maxcol; j++)
				putchar(msgout[i][j]);
			putchar('\n');
		}
	}
}

error(string)
char *string;

{ 
	printf("\nvfontinfo: %s\n",string);
	exit(8);
};

char *rdchar(c)
char c;
{
	static char ret[3];
	ret[0] = isprint(c) ? ' ' : '^';
	ret[1] = isprint(c) ?  c  : c^0100;
	ret[2] = 0;
	return (ret);
}

int
fbit(row, col)
int row, col;
{
	int thisbyte, thisbit, ret;

	if (row<0 || row>=H || col>=W) return(0);
	thisbyte = charbits[row*WB + (col>>3)] & 0xff;
	thisbit = 0x80 >> (col&7);
	ret = thisbyte & thisbit;
	return (ret != 0);
}


/*
The implementation would work like this:
	zoom level	method
	0		2 chars/pixel, 1 is "[]", 0 is "  ".
	1		2 pixels/char 2x1, using " " "," "'" "|"
	2		8 pixels/char 4x2, using 16x16 table
	3		32 pixels/char 8x4, mapped into (2)
	4 and up	similar, mapped into (2)

The 16x16 table maps a 4x2 pattern into a printing ascii character which
most closely approximates that pattern, e.g. the pattern
	|'
	''
would be represented by the character "[".  I have such a table worked out.

Grainer zoom levels would take the rule of reducing it into a smaller bitmap,
or-ing the bits together.  (e.g. level 3 would take a 2x2 chunk and map it
into a single pixel: 0 if all 4 are 0, 1 otherwise.)  These pixels would be
displayed as in 2.
*/

/*
 * graphtab: a table for rudimentary graphics on ordinary terminals.
 * For each 4x2 bit pattern of the form:
 *	ae
 *	bf
 *	cg
 *	dh
 * form the 4 bit quantities abcd and efgh and get table entry
 *	graphtab[abcd][efgh]
 * to display in that character position.
 *
 * General philosophies: the dh bits are intended for descenders where
 * possible.  Characters with radically different appearance on different
 * terminals (e.g. _ and ^) are avoided.
 *
 * Version 1.0, March 1981, Mark Horton.
 */

char tab1[4] = {
	' ', ',', '\'', '|'
};

char graphtab[16][16] = {
' ', '.', '.', ',', '.', ';', ':', 'j', '\'', ':', ':', ';', '\'', ';', '!', '|',
'.', '.', ':', ',', ';', ';', ';', 'j', '/', ';', ';', ';', 'j', 'j', 'j', 'j',
'.', ',', '~', ',', 'r', '<', 'j', 'q', '/', ';', 'I', ';', '/', '|', 'I', '|',
',', ',', 'r', 'x', '/', '/', '/', 'd', '/', '/', '/', 'd', '/', '/', '/', 'd',
'.', ':', '\\', ';', '-', '=', 'v', 'q', '\'', ':', '<', '|', '\'', ':', '+', '+',
';', ';', '>', ';', '=', '=', 'g', 'g', '\'', ':', 'S', 'S', '/', '/', '/', '+',
':', '\\', '\\', '\\', 'r', '<', 'w', 'q', '/', '<', '6', '4', '/', '/', 'd', '+',
'l', 'L', '+', 'b', 'y', '[', 'p', 'g', '/', '<', '/', '6', '/', '/', '/', '+',
'`', ':', ':', ';', '`', '\\', '\\', '\\', '"', ':', ':', ';', '`', '\\', 'Y', 'T',
';', ';', ';', ';', '`', '2', '>', '\\', ':', '=', ';', ';', '?', '?', ']', ']',
':', ';', ';', ';', '>', '2', '>', '\\', 'F', ';', 'O', ';', '7', '?', ']', '7',
';', ';', ';', ';', '?', '2', '>', 'b', ';', ';', ';', ';', '?', '?', ']', '#',
'\'', '\\', '\\', '\\', '`', '\\', '\\', '\\', '\'', '\'', '<', '5', '"', '"', 'v', 'q',
';', '\\', '\\', '\\', '`', '=', '\\', '\\', '\'', '\'', '5', '5', '"', '?', 'g', 'g',
'I', 'L', 'L', 'L', 'D', '\\', 'b', 'f', 'F', '[', '[', '[', 'P', '?', '#', 'M',
'|', '|', '|', '|', '|', '#', '+', '#', 'T', '[', 'F', 'F', 'P', '?', 'P', 'M'
};


shozoom()
{
	register i;

	if (zoom == 0) 
		sho0();
	else if (zoom == 1)
		sho1();
	else if (zoom == 2)
		sho2();
}

sho0()
{
	register k,l;

	for (k=0; k<H; k++) {
		for (l=0; l<W; l++)
			printf("%s", fbit(k,l)?"[]": "  ");
		printf("\n");
	}
	printf("\n");
}

sho1()
{
	register i,k,l;

	k = 0;
	while (k < H) {
		for(l=0;l<W;l++) {
			i = fbit(k,l)*2 + fbit(k+1,l);
			printf("%c",tab1[i]);
			l++;
		}
		printf("\n");
		k += 2;
	}
	printf("\n");
}

sho2()
{
	register i,j,k,l;
	int line = curline + (base+3)/4;
	int col;

	k = base%4;
	if (k > 0) k -= 4;
	while (k < H) {
		l = 0;
		col = curcol;
		while (l<W) {
			i = fbit(k,l)*8 + fbit(k+1,l)*4 + 
			    fbit(k+2,l)*2 + fbit(k+3,l);
			l++;
			j = fbit(k,l)*8 + fbit(k+1,l)*4 + 
			    fbit(k+2,l)*2 + fbit(k+3,l);

			if (msgflag) {
				if (graphtab[i][j] != ' ') {
					if (line > maxline) maxline = line;
					if (line < minline) minline = line;
					if (col > maxcol)   maxcol  = col;
				}
				msgout[line][col] = graphtab[i][j];
			} else
				printf("%c",graphtab[i][j]);
			l++;
			col++;
		}
		if (msgflag == 0)
			printf("\n");
		k += 4;
		line--;
	}
	if (msgflag == 0)
		printf("\n");
}
