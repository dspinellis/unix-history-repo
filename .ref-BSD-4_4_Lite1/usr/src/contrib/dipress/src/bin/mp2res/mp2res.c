/*
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 *    This module reads a compressed bitmap file in the MacPaint
 *    format and outputs an RES file.
 *
 *    For a description of RES (Raster Encoding Standard) see
 *    the booklet entitled "Raster Encoding Standard" XNS Standard 178506
 *    (June, 1985)
 *
 * HISTORY
 * 21-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added the ability to generate IP masters, too.
 *
 * 07-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Converted for use with getopt.
 *
 * 03-Jun-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Created mp2res from readmac.c .
 *
 *
 * K. Knox, 10-Dec-84 18:51:22, Created readmac.
 */

#include <stdio.h>

#include "iptokens.h"
#include "literal.h"
#include "operator.h"

#define err0 "usage: readmac filename\n"
#define err1 "mp2res: Could not open, %s.\n"

#define TRUE	1
#define FALSE	0

/* External procedures. */
extern char *malloc();

/*
 * Main program
 *	parse command line
 *	call ProcessData to do the work
 */

main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int	firstRow = 0,
		lastRow = (720 - 1),
		firstColumn = 0,
		lastColumn = (576 - 1),
		outputFileFD,
		makeInterpress = FALSE;
	extern char *optarg;
	extern optind;
	FILE *inputFileDesc;

	outputFileFD = 1;		/* standard out */

	while ((c = getopt(argc, argv, "it:b:l:o:r:")) != EOF)
	    switch (c) {
		case 'i':
		    makeInterpress = TRUE;
		    break;

		case 't':		/* top */
		    firstRow = atoi(optarg);
		    break;

		case 'b':		/* bottom */
		    lastRow = atoi(optarg);
		    break;

		case 'l':		/* left */
		    firstColumn = atoi(optarg);
		    break;

		case 'o':		/* output file */
		    if( (outputFileFD = creat(optarg, 0664)) < 0) {
			fprintf(stderr, "mp2res: can't open %s for writing\n", optarg);
			perror(optarg);
			exit(1);
		    }
		    break;

		case 'r':		/* right */
		    lastColumn = atoi(optarg);
		    break;

	    default:
		printf("ipmetrics: option '%c' not allowed\n");
	    }

	/* do we read from standard input? */
	if (argc == optind) {
		inputFileDesc = stdin;
	} else {
		/* open the MacPaint file */
		if ((inputFileDesc = fopen(argv[optind], "r")) == NULL)
			fprintf(stderr, err1, argv[optind]);
	}

	if (! makeInterpress )
	    MakeRES(inputFileDesc, outputFileFD,
		firstRow, lastRow, firstColumn, lastColumn);
	else
	    MakeIP(inputFileDesc, outputFileFD,
		firstRow, lastRow, firstColumn, lastColumn);

	exit(0);
}


/*
 * Convert a MacPaint document to an RES file
 *	don't copy the data that the user doesn't want
 */

MakeRES(inputFileDesc, outputFileFD,
	firstRow, lastRow, firstColumn, lastColumn)
FILE *inputFileDesc;
int outputFileFD,
	firstRow, lastRow, firstColumn, lastColumn;
{
	int i,
		bufferSize;
	int	pixelsPerScanLine,
		numberOfScanLines,
		bytesPerScanLine;
	unsigned char *image, *p;

	pixelsPerScanLine = lastColumn - firstColumn + 1;
	numberOfScanLines = lastRow - firstRow + 1;

	if( pixelsPerScanLine % 32 != 0 )
		fprintf(stderr, "internal error: %d is not a multiple of 32\n", pixelsPerScanLine);

	bytesPerScanLine = (pixelsPerScanLine + 7)/8;

	/* skip over MacPaint header */
	for (i=0; i < 512; i++)
		getc(inputFileDesc);

	bufferSize = bytesPerScanLine * numberOfScanLines;
	image = (unsigned char *) malloc((unsigned) bufferSize);

	res_select(outputFileFD);

	AppendOp(OP_beginBlock);

	/* element 1: imageScale */
	AppendRational(254L, 72L*100*100);	/* assume 72 spots/inch */
	AppendOp(OP_dup);
	AppendInteger(2L);
	AppendOp(OP_makevec);

	/* element 2: xDimension */
	AppendInteger((long) pixelsPerScanLine);		/* xPixels */

	/* element 3: yDimension */
	AppendInteger((long) numberOfScanLines);		/* yPixels */

	/* element 4: maskImage */
	AppendInteger(0L);				/* maskImage */

	/* element 5: colorImage */
	AppendInteger((long) numberOfScanLines);		/* yPixels */
	AppendInteger((long) pixelsPerScanLine);		/* xPixels */
	AppendInteger(1L);		/* sampelsPerPixel */
	AppendInteger(1L);		/* maxSampleValue */
	AppendInteger(1L);		/* samplesInterleaved */

	/* enter transformation to bring it to row major order */
	AppendInteger(-90L);
	AppendOp(OP_rotate);
	AppendInteger(0L);
	AppendInteger((long) numberOfScanLines);
	AppendOp(OP_translate);
	AppendOp(OP_concat);

	/* skip over as many rows as needed */
	for (i = 0; i < firstRow; i++)
		readscan(inputFileDesc, image);

	/* read the scan lines in */
	for (i = firstRow, p = image; i <= lastRow; i++, p += bytesPerScanLine)
		readscan(inputFileDesc, p);

	AppendPPVector(bufferSize, 1, pixelsPerScanLine, (unsigned char *) image);

	Op(makepixelarray);		/* make the array */

	/* element 6: colorOperator */
	AppendInteger(0L);

	/* element 7: image Properties */
	AppendIdentifier("imageDescription");
	AppendString("made from a MacPaint file");
	Makevec(2);

	/* element 8: signature */
	AppendInteger(13086L);

	AppendOp(OP_endBlock);

	ip_flush();

	free((char *) image);
}

MakeIP(inputFileDesc, outputFileFD,
	firstRow, lastRow, firstColumn, lastColumn)
FILE *inputFileDesc;
int outputFileFD,
	firstRow, lastRow, firstColumn, lastColumn;
{
	int i,
		bufferSize;
	int	pixelsPerScanLine,
		numberOfScanLines,
		bytesPerScanLine;
	unsigned char *image, *p;

	pixelsPerScanLine = lastColumn - firstColumn + 1;
	numberOfScanLines = lastRow - firstRow + 1;

	if( pixelsPerScanLine % 32 != 0 )
		fprintf(stderr, "%d is not a multiple of 32\n", pixelsPerScanLine);

	bytesPerScanLine = (pixelsPerScanLine + 7)/8;

	for (i=0; i < 512; i++)
		getc(inputFileDesc);

	bufferSize = bytesPerScanLine * numberOfScanLines;
	image = (unsigned char *) malloc((unsigned) bufferSize);

	ip_select(outputFileFD);

	AppendOp(OP_beginBlock);
	AppendOp(OP_beginBody);
	AppendOp(OP_endBody);	/* end preamble */
	AppendOp(OP_beginBody);	/* page 1 (and only) */

	AppendRational(254L, 72L*100*100);	/* assume 72 spots/inch */
	AppendOp(OP_scale);
	Translate(.5*.0254, .25*.0254);
	AppendOp(OP_concat);
	AppendOp(OP_concatt);

	AppendInteger((long) numberOfScanLines);		/* yPixels */
	AppendInteger((long) pixelsPerScanLine);		/* xPixels */
	AppendInteger(1L);		/* sampelsPerPixel */
	AppendInteger(1L);		/* maxSampleValue */
	AppendInteger(1L);		/* samplesInterleaved */

	/* enter transformation to bring it to row major order */
	AppendInteger(-90L);
	AppendOp(OP_rotate);
	AppendInteger(0L);
	AppendInteger((long) numberOfScanLines);
	AppendOp(OP_translate);
	AppendOp(OP_concat);

	/* skip over as many rows as needed */
	for (i = 0; i < firstRow; i++)
		readscan(inputFileDesc, image);

	for (i = firstRow, p = image; i <= lastRow; i++, p += bytesPerScanLine)
		readscan(inputFileDesc, p);

	/* output the result to the Interpress file */
	AppendPPVector(bufferSize, 1, pixelsPerScanLine, (unsigned char *) image);

	Op(makepixelarray);		/* make the array */

	/* draw a box around the pixel array */
	AppendInteger(1L);		/* unit stroke width */
	AppendInteger(15L);		/* stroke width imager variable */
	AppendOp(OP_iset);
	AppendRational(-1L, 2L);
	AppendRational(-1L, 2L);
	AppendOp(OP_moveto);
	AppendInteger((long) (numberOfScanLines + 1));
	AppendOp(OP_linetoy);
	AppendInteger((long) (pixelsPerScanLine + 1));
	AppendOp(OP_linetox);
	AppendRational(-1L, 2L);
	AppendOp(OP_linetoy);
	AppendRational(-1L, 2L);
	AppendOp(OP_linetox);
	AppendOp(OP_maskstroke);

	/* prepare to show pixel array */
	AppendInteger(0L);
	AppendInteger(0L);
	AppendOp(OP_setxy);
	Op(maskpixel);			/* show it */

	AppendOp(OP_endBody);
	AppendOp(OP_endBlock);

	ip_flush();

	free((char *) image);
}


/*
 * Read the next scan line from the MacPaint file
 */

readscan(inputFileDesc, image)
FILE *inputFileDesc;
unsigned char *image;
{
	int in_pos, count, data_byte;

	in_pos = 0;

	while (in_pos < 72)
	{
		count = getc(inputFileDesc);

		if (count > 127)
			count -= 256;

		if (count >= 0) {
			/* run of raw bytes */
			count++;                            /* # of bytes to read */

			while (count--)
				image[in_pos++] = getc(inputFileDesc);
		} 
		else {
			/* run of repeated byte */
			count = -count+1;	                 /* repetition factor */
			data_byte = getc(inputFileDesc);              /* byte to repeat */

			while (count--)
				image[in_pos++] = data_byte;
		}
	}
}
