/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scsi.c	7.3 (Berkeley) %G%
 */

/*
 * SCSI utility routines for making SCSI device drivers easier.
 */

#include <sys/param.h>

#include <mips/dev/device.h>
#include <mips/dev/scsi.h>

/*
 * The error codes for class 0-6 sense data are class specific.
 * The follow arrays of strings are used to print error messages.
 */
static char *Class0Errors[] = {
	"No sense data",
	"No index signal",
	"No seek complete",
	"Write fault",
	"Drive not ready",
	"Drive not selected",
	"No Track 00",
	"Multiple drives selected",
	"No address acknowledged",
	"Media not loaded",
	"Insufficient capacity",
	"Drive timeout",
};
static char *Class1Errors[] = {
	"ID CRC error",
	"Unrecoverable data error",
	"ID address mark not found",
	"Data address mark not found",
	"Record not found",
	"Seek error",
	"DMA timeout error",
	"Write protected",
	"Correctable data check",
	"Bad block found",
	"Interleave error",
	"Data transfer incomplete",
	"Unformatted or bad format on drive",
	"Self test failed",
	"Defective track (media errors)",
};
static char *Class2Errors[] = {
	"Invalid command",
	"Illegal block address",
	"Aborted",
	"Volume overflow",
};
static char *Class7Errors[] = {
	"No sense data",
	"Recoverable error",
	"Drive not ready",
	"Media error",
	"Hardware error",
	"Illegal request",
	"Unit attention",
	"Write protected",
	"Blank check error",
	"Vendor error",
	"Powerup failure",
	"Abort",
	"Equal",
	"Overflow",
	"Reserved14/miscompare",
};
static int scsiNumErrors[] = {
	sizeof(Class0Errors) / sizeof(char *),
	sizeof(Class1Errors) / sizeof(char *),
	sizeof(Class2Errors) / sizeof(char *),
	0, 0, 0, 0, 0,
};
static char **scsiErrors[] = {
	Class0Errors,
	Class1Errors,
	Class2Errors,
};

/*
 * Decode the sense data and print a suitable message.
 */
scsiPrintSense(sp, len)
	register ScsiClass7Sense *sp;
	int len;
{
	ScsiClass0Sense *sp0;
	int class, code;

	if (sp->error7 != 0x70) {
		sp0 = (ScsiClass0Sense *)sp;
		class = sp0->error >> 4;
		code = sp0->error & 0xF;
		if (code >= scsiNumErrors[class])
			printf("sense error 0x%x", sp0->error);
		else
			printf("%s", scsiErrors[class][code]);
		if (sp->valid)
			printf(", blk %d", (sp0->highAddr << 16) |
				(sp0->midAddr << 8) | sp0->lowAddr);
	} else {
		if (sp->key >= sizeof(Class7Errors) / sizeof(char *))
			printf("sense class 7 error 0x%x", sp->key);
		else
			printf("%s", Class7Errors[sp->key]);
		if (sp->fileMark)
			printf(", file mark seen");
		if (sp->endOfMedia)
			printf(", end of media seen");
		if (sp->badBlockLen)
			printf(", block length mis-match");
		if (sp->valid)
			printf(", blk %d", (sp->info1 << 24) |
				(sp->info2 << 16) | (sp->info3 << 8) |
				sp->info4);
	}
	printf("\n");
}

/*
 * Setup a command block for a SCSI Group0 command.
 */
void
scsiGroup0Cmd(cmd, lun, block, count, c)
	unsigned cmd;			/* group0 SCSI command */
	unsigned lun;			/* Logical Unit Number */
	register unsigned block;	/* starting block number for transfer */
	unsigned count;			/* # of sectors/bytes to transfer */
	register ScsiGroup0Cmd *c;	/* command to be filled in */
{

	c->command = cmd;
	c->unitNumber = lun;
	c->highAddr = block >> 16;
	c->midAddr = block >> 8;
	c->lowAddr = block;
	c->blockCount = count;
	c->control = 0;
}

/*
 * Setup a command block for a SCSI Group1 command.
 */
void
scsiGroup1Cmd(cmd, lun, block, count, c)
	unsigned cmd;			/* group0 SCSI command */
	unsigned lun;			/* Logical Unit Number */
	register unsigned block;	/* starting block number for transfer */
	unsigned count;			/* # of sectors/bytes to transfer */
	register ScsiGroup1Cmd *c;	/* command to be filled in */
{

	c->command = cmd;
	c->unitNumber = lun;
	c->pad1 = 0;
	c->highAddr = block >> 24;
	c->midHighAddr = block >> 16;
	c->midLowAddr = block >> 8;
	c->lowAddr = block;
	c->pad2 = 0;
	c->highBlockCount = count >> 8;
	c->lowBlockCount = count;
	c->control = 0;
}
