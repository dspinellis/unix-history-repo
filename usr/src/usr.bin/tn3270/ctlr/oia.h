/*
 * This file describes the Operator Information Area in the 3270.
 */

#define	INCLUDED_OIA

#define	OIA_READY_3274	0xF4
#define	OIA_ONLINE_A	0xCC
#define	OIA_OWNERSHIP_MYJOB	0xCF
#define	OIA_OWNERSHIP_SYSTEM_OPERATOR	0xF0
#define	OIA_OWNERSHIP_UNOWNED		0xF1
#define	OIA_INSERT_ON	0x3a

typedef struct {
    unsigned char
	ready,			/* 01-01 Is 3274 ready? */
	online,			/* 02-02 Online in which mode (a, b) */
	ownership,		/* 03-03 Who owns us (job, operator, unowned) */
	test2_4[3],		/* 04-06 Rest of TEST string */
	reserved[2],		/* 07-08 */
	x,			/* 09-09 X - input inhibited */
	xnull,			/* 10-10 Space */
	xwhy[7],		/* 11-17 Why are we inhibited? */
	reserved2[14],		/* 18-31 */
	num[3],			/* 32-34 Numeric lock */
	upshift,		/* 35-35 Shift state */
	insert,			/* 36-36 Insert mode */
	apl[3];			/* 37-39 APL mode */
} OIA;
