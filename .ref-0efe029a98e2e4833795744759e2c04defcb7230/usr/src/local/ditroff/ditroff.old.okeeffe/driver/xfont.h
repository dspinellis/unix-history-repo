/*	xfont.h	1.1	87/02/05
 *
 * This file defines the structure that lives at the beginning of the
 * "strike" format font file - the font format used for the X window system.
 * The strike bitmap has the following structure.  Characters are layed
 * out logically right next to each other in consecutive bits in the bitmap.
 * The "leftArray" contains the bit offset of the character in the stream
 * of bits.  A sample bitmap and resulting offsets are shown below:
 *
 * leftArray:	0     6     12   17    23        33   38    44   49  53   58    
 *		|     |     |    |     |         |    |     |    |   |    |   
 * start:	..@...@@@@...@@@.@@@@...@@...@@..@@@@..@@@@.@..@.@@@.@@@@. . .
 * each line	.@.@..@...@.@.....@..@.@..@.@..@.@....@.....@..@..@....@..
 * continues	@...@.@@@@..@.....@..@.@...@...@.@@@..@.@@@.@@@@..@....@... . .
 * at the next	@@@@@.@...@.@.....@..@..@@...@@..@....@...@.@..@..@..@.@..
 * short after	@...@.@@@@...@@@.@@@@.....@.@....@.....@@@@.@..@.@@@.@.@.. . .
 * the last line...........................@..............@...........@...
 */
struct FontData {
	int	bitmapPtr;	/* offset to strike bitmap in the file */
	short	bmWidth;	/* width of the bitmap (in pixels) */
	short	bmHeight;	/* bitmap height (pixels) */
	short	bitsPerPixel;	/* for color? */
	short	firstChar;	/* first character in the font (usually 0) */
	short	lastChar;	/* index of last character (usually 127) */
	short	leftArray;	/* offset in file to array of char offsets */
	short	waste;
	short	baseline;	/* offset from top to baseline (or zero) */
	short	spaceIndex;	/* the space character (always 32?) */
	short	fixedWidth;	/* the width of each character or zero */
};
