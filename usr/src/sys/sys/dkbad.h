/*	dkbad.h	4.1	81/05/08	*/

/*
 * Definitions needed to perform bad block
 * revectoring ala DEC STD 144.
 *
 * The bad block information is located in the
 * first 5 even numbered sectors of the last
 * track of the disk pack.  There are five
 * identical copies of the information, described
 * by the dkbad structure.
 *
 * Replacement blocks are allocated starting with
 * the first block before the bad block information
 * and working backwards towards the beginning of
 * the disk.  A maximum of 126 bad blocks are supported.
 * The position of the bad block in the bad block table
 * determines which replacement block it corresponds to.
 *
 * The bad block information and replacement blocks
 * are conventionally only accessable through the
 * 'c' file system partition of the disk.  If that
 * partition is used for a file system, the user is
 * responsible for making sure that it does not overlap
 * the bad block information or any replacement blocks.
 */

struct dkbad {
	long	bt_csn;			/* cartridge serial number */
	u_short	bt_magic;		/* magic number for sanity check */
#define	BADMAGIC	0122155
	u_short	bt_flag;		/* -1 => alignment cartridge */
	struct {
		u_short	bt_cyl;		/* cylinder number of bad block */
		u_short	bt_trksec;	/* track and sector number */
	} bt_bad[126];
};

#define	ECC	0
#define	SSE	1
#define	BSE	2
#define	CONT	3
