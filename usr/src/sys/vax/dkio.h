/*	dkio.h	6.1	83/07/29	*/
/*
 * Structures and definitions for disk io control commands
 *
 * THIS WHOLE AREA NEEDS MORE THOUGHT.  FOR NOW JUST IMPLEMENT
 * ENOUGH TO READ AND WRITE HEADERS ON MASSBUS DISKS.  EVENTUALLY
 * SHOULD BE ABLE TO DETERMINE DRIVE TYPE AND DO OTHER GOOD STUFF.
 */

/* disk io control commands */
#define DKIOCHDR	_IO(d, 1)	/* next I/O will read/write header */
