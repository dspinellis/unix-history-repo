/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)scsi.c	8.1 (Berkeley) 6/10/93
 */

/*
 * SCSI utility routines for making SCSI device drivers easier.
 */

#include <sys/param.h>

#include <pmax/dev/device.h>
#include <pmax/dev/scsi.h>

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
