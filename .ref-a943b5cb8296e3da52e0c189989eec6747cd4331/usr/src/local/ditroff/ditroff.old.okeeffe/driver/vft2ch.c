/*	vft2ch.c	(Berkeley)	1.3	85/07/03
 *
 * Font translation for vfont-style fonts to character format.
 *
 *	Use:  vft2ch  fontfile  [ character_list ]
 *
 *		Reads "fontfile" from current directory (or if not found,
 *	from BITDIR defined below) and converts it to a character font format
 *	editable by real people, and convertable BACK to vfont format by the
 *	ch2vft program.  Output goes to stdout.
 */

#include <stdio.h>
#include <vfont.h>


#define	DIRSIZ	256
#define	MAGICNO	0436
#ifndef BITDIR
#define BITDIR		"/usr/lib/vfont"
#endif


struct header FontHeader;
struct dispatch disptable[DIRSIZ];

char	IName[100];
long	fbase;

unsigned char	defascii[DIRSIZ];
unsigned char	*charswanted = defascii;
char	charbits[20000];
int	height, width, bwidth;


main(argc, argv)
int argc;
char **argv;
{
	register int FID;
	register int i;
	register int j;

	if (argc < 2 || argc > 3)
		error("usage: %s filename [ character-list ]", argv[0]);

	for (i = 0; i < DIRSIZ; i++)
		defascii[i] = i;
	if (argc >= 3)
		charswanted = (unsigned char *) argv[2];

	++argv;
	sprintf(IName, "%s/%s", BITDIR, *argv);
	if ((FID = open(*argv, 0)) < 0)
		if ((FID = open(IName, 0)) < 0)
			error("Can't find %s", *argv);

	if (read(FID, &FontHeader,sizeof FontHeader) != sizeof FontHeader)
		error("bad header in Font file.");
	if (FontHeader.magic != MAGICNO)
		error("magic number %o wrong", FontHeader.magic);
	if (read(FID,&disptable[0],sizeof disptable) != sizeof disptable)
		error("bad dispatch table in Font file");
	fbase = sizeof FontHeader + sizeof disptable;

	while (**argv && (**argv < '0' || **argv > '9'))
		(*argv)++;
	printf("fontheader\nsize 46\n");
	if (**argv)
		printf("mag 1000\ndesiz %d\n", atoi(*argv));
	printf("rot 0\ncadv 0\nladv 1\nid 0\nres 200\n");

	for (i = 0; i < DIRSIZ; i++) {
		j = charswanted[i];
		if (i > 0 && j == 0)
			break;
		if (disptable[j].nbytes != 0) {
			register int k, l;
			register int left = disptable[j].left;
			register int right = disptable[j].right;
			register int up = disptable[j].up;
			register int down = disptable[j].down;


			printf(":%d, width = %d.00\n", j, disptable[j].width);

			lseek(FID, fbase + disptable[j].addr, 0);
			read(FID, charbits, disptable[j].nbytes);
			height = up + down;
			width = left + right;
#ifdef sun
			bwidth = ((width + 15) / 16) * 2;
#else
			bwidth = (width + 7) / 8;
#endif
			for (k = up; k < 0; k++) {
			    for (l = left; l < 0; l++)
				pbit(l, k, l==left && k==up);
			    for (l = 0; l <= left; l++)
				pbit(l, k, l==left && k==up);
			    while (l < width)
				pbit(l++, k, 0);
			    printf("\n");
			}
			for (k = 0; k <= up; k++) {
			    for (l = left; l < 0; l++)
				pbit(l, k, l==left && k==up);
			    for (l = 0; l <= left; l++)
				pbit(l, k, l==left && k==up);
			    while (l < width)
				pbit(l++, k, 0);
			    printf("\n");
			}
			while (k < height) {
			    for (l = left; l < 0; l++)
				pbit(l, k, 0);
			    for (l = 0; l <= left; l++)
				pbit(l, k, 0);
			    while (l < width)
				pbit(l++, k, 0);
			    printf("\n");
			    k++;
			}
			printf("\n");
		}
	}
	exit(0);
}


/*----------------------------------------------------------------------------*
 | Routine:	error (format_string, argument1, argument2.... )
 |
 | Results:	fprints a message to standard error, then exits with error
 |		code 1
 |
 | Side Efct:	This routine does NOT return
 *----------------------------------------------------------------------------*/

/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
	fprintf(stderr, "vft2ch: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(1);
}


/*----------------------------------------------------------------------------*
 | Routine:	pbit (column, row, reference_point_flag)
 |
 | Results:	prints, to standard output, the appropriate representation
 |		for the dot at (row, column), given a flag noting whether
 |		or not the dot is the reference point.  Column and row need
 |		not be within the bounds of the bit-map rectangle (delimited
 |		by the global variables height and width).
 *----------------------------------------------------------------------------*/

pbit(col, row, flag)
int col, row, flag;
{
	if (col < 0 || row < 0 || row >= height || col >= width) {
		printf(flag ? "x" : ".");
		return;
	}
	if ((charbits[row * bwidth + (col >> 3)] & 0xff) & (0x80 >> (col & 7)))
		printf(flag ? "X" : "@");
	else
		printf(flag ? "x" : ".");
}
