/*
 * Structures and definitions for mag tape io control commands
 */

/* structure for MTIOCTOP - mag tape op command */

struct	mtop	{
	short	mt_op;		/* operations defined below */
	short	mt_count;	/* how many of them */
};

/* operations: */
#define MTWEOF	0	/* write an end-of-file record */
#define MTFSF	1	/* forward space file */
#define MTBSF	2	/* backward space file */
#define MTFSR	3	/* forward space record */
#define MTBSR	4	/* backward space record */
#define MTREW	5	/* rewind */
#define MTOFFL	6	/* rewind and put the drive offline */

/* structure for MTIOCGET - mag tape get status command */

struct	mtget	{
	short	mt_dsreg;	/* drive status register */
	short	mt_erreg;	/* error register */
	short	mt_resid;	/* residual count */
/* the following two are not yet implemented */
	short	mt_fileno;	/* file number of current position */
	short	mt_blkno;	/* block number of current position */
};

/* unfortunately the bits in these registers are grossly device dependent */
/* should be defined here but aren't */
