/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
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
 * scsi.c -- front end of SCSI test commands
 * by A.Fujita, FEB-09-1992
 */

#include <sys/param.h>
#include <luna68k/dev/scsireg.h>
#include <luna68k/stand/status.h>


int scsi_device = 6;

#define SENSBUFF 8					/* 6/10/93P%$%98.1i%$%P$G%;%s%9%G!<%? */
							/* $ND9$5$r#8/usr/src/sys/luna68k/stand/SCCS/s.scsi.c$%H0JFb$K8GDj$7$F */
u_char	sensbuff[SENSBUFF];				/* #80J>e$OL50UL#$G$"$k!#         */

static struct scsi_inquiry inquirybuf;
static struct scsi_fmt_cdb inquiry = {
	6,
	CMD_INQUIRY, 0, 0, 0, sizeof(inquirybuf), 0
};

static u_long capacitybuf[2];
struct scsi_fmt_cdb capacity = {
	10,
	CMD_READ_CAPACITY, 0, 0, 0, 0, 0, 0, 0, 0, 0
};


int
scsi(argc, argv)
	int   argc;
	char *argv[];
{
	register char *p;
	register int i, status;

	if (argc < 2) {
		printf("This command is required subcommand !!\n");
		return(ST_ERROR);
	}

	if (!strcmp(argv[1], "device")) {
		if (argc > 2) {
			i = 0;
			for (p = argv[2]; *p != NULL; p++) {
				i = i * 10 + *p - '0';
			}
			if ( i < 8 && i >= 0) {
				scsi_device = i;
			}
		}
		printf("Current Device ID: %d\n", scsi_device);
	} else if (!strcmp(argv[1], "test_unit_rdy")) {
				/* CTLR  SLAVE  LUN */
		scsi_test_unit_rdy(   0, scsi_device,   0);
	} else if (!strcmp(argv[1], "request_sense")) {
			        /* CTLR  SLAVE  LUN */
		scsi_request_sense(   0, scsi_device,   0, sensbuff, SENSBUFF);
	} else if (!strcmp(argv[1], "inquiry")) {
		if (scsi_immed_command(   0, scsi_device,   0, &inquiry,
				       (u_char *) &inquirybuf, sizeof(inquirybuf)) == 0) {
			printf("Type:\t0x%x\n",		inquirybuf.type);
			printf("Qualifier:\t0x%x\n",	inquirybuf.qual);
			printf("Version:\t0x%x\n",	inquirybuf.version);
			printf("RDF:\t0x%x\n",		inquirybuf.rsvd);
			
			printf("Vender ID:\t");
			for (i = 0; i < 8; i++)
				printf("%c", inquirybuf.vendor_id[i]);
			printf("\n");
			
			printf("Product ID:\t");
			for (i = 0; i < 16; i++)
				printf("%c", inquirybuf.product_id[i]);
			printf("\n");
			
			printf("Revision:\t");
			for (i = 0; i < 4; i++)
				printf("%c", inquirybuf.rev[i]);
			printf("\n");
		}
	} else if (!strcmp(argv[1], "read_capacity")) {
		if (scsi_immed_command(   0, scsi_device,   0, &capacity,
				       (u_char *) &capacitybuf, sizeof(capacitybuf)) == 0) {
			printf("Logical Block Address:\t%d (0x%x)\n",
			       capacitybuf[0], capacitybuf[0]);
			printf("Block Length:\t\t%d (0x%x)\n",
			       capacitybuf[1], capacitybuf[1]);
		}
	} else if (!strcmp(argv[1], "trace")) {
		for (i = 0; i < 7; i++) {
			printf("SCSI ID %d .... ", i);
			status = scsi_test_unit_rdy( 0, i, 0);
			if (status >= 0)
				printf("found.\n");
			else
				printf("no.\n");
		}
	} else if (!strcmp(argv[1], "format_unit")) {
		i = 0;
		while (i == 0) {
			printf("Do you really want to format SCSI %d device ? [y/n]: ",
			       scsi_device);
			i = cngetc();
			printf("\n");
			if ((i != 'y') && (i != 'Y') && (i != 'n') && (i != 'N'))
				i = 0;
		}

		if ((i == 'y') || (i == 'Y'))
			status = scsi_format_unit( 0, scsi_device, 0);
	}

	return(ST_NORMAL);
}

static struct scsi_fmt_cdb scsi_cdb = {
	10,
	0,  0, 0, 0, 0, 0, 0, 0, 0, 0
};

int
scsi_read_raw(target, blk, nblk, buff, len)
	u_int   target;
	u_int   blk;
	u_int   nblk;
	u_char *buff;
	u_int   len;
{
	register struct scsi_fmt_cdb *cdb = &scsi_cdb;

	cdb->cdb[0] = CMD_READ_EXT;
	
	cdb->cdb[2] = (blk & 0xff000000) >> 24;
	cdb->cdb[3] = (blk & 0x00ff0000) >> 16;
	cdb->cdb[4] = (blk & 0x0000ff00) >>  8;
	cdb->cdb[5] = (blk & 0x000000ff);
	
	cdb->cdb[7] = (nblk & 0xff00) >> 8;
	cdb->cdb[8] = (nblk & 0x00ff);
	
	if (scsi_immed_command(0, target, 0, cdb, buff, len) == 0)
		return(1);
	else
		return(0);
}

int
scsi_read(blk, buff, len)
	u_int   blk;
	u_char *buff;
	u_int   len;
{
	u_int   nblk = len >> DEV_BSHIFT;
	
	return(scsi_read_raw(scsi_device, blk, nblk, buff, len));
}

int
scsi_write(blk, buff, len)
	u_int   blk;
	u_char *buff;
	u_int   len;
{
	register struct scsi_fmt_cdb *cdb = &scsi_cdb;

	cdb->cdb[0] = CMD_WRITE_EXT;
	
	cdb->cdb[2] = (blk & 0xff000000) >> 24;
	cdb->cdb[3] = (blk & 0x00ff0000) >> 16;
	cdb->cdb[4] = (blk & 0x0000ff00) >>  8;
	cdb->cdb[5] = (blk & 0x000000ff);
	
	cdb->cdb[7] = ((len >> DEV_BSHIFT) & 0xff00) >> 8;
	cdb->cdb[8] = ((len >> DEV_BSHIFT) & 0x00ff);
	
	if (scsi_immed_command(0, scsi_device, 0, cdb, buff, len) == 0)
		return(1);
	else
		return(0);
}
