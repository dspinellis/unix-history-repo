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
 *	@(#)device.h	8.1 (Berkeley) 6/10/93
 */

/*
 * This structure is used to encapsulate the routines for a device driver.
 * This allows an "object oriented" approach so a controller device driver
 * can support multiple attached devices or a device can be attached to
 * different types of controllers.
 */
struct driver {
	char	*d_name;	/* device driver name (e.g., "rz") */
	int	(*d_init)();	/* routine to probe & initialize device */
	void	(*d_start)();	/* routine to start operation */
	void	(*d_done)();	/* routine to call when operation complete */
	void	(*d_intr)();	/* routine to call when interrupt is seen */
};

/*
 * This structure describes controllers directly connected to CPU
 * and is partially initialized in "ioconf.c" by the 'config' program.
 */
struct pmax_ctlr {
	struct driver	*pmax_driver;	/* controller driver routines */
	int		pmax_unit;	/* controller number */
	char		*pmax_addr;	/* address of controller */
	int		pmax_pri;	/* interrupt priority */
	int		pmax_flags;	/* flags */

	int		pmax_alive;	/* true if init routine succeeded */
};

/*
 * This structure describes devices connected to a SCSI interface
 * and is partially initialized in "ioconf.c" by the 'config' program.
 */
struct scsi_device {
	struct driver	*sd_driver;	/* SCSI device driver routines */
	struct driver	*sd_cdriver;	/* SCSI interface driver routines */
	int		sd_unit;	/* device unit number */
	int		sd_ctlr;	/* SCSI interface number */
	int		sd_drive;	/* SCSI address number */
	int		sd_slave;	/* LUN if device has multiple units */
	int		sd_dk;		/* used for disk statistics */
	int		sd_flags;	/* flags */

	int		sd_alive;	/* true if init routine succeeded */
};

/* Define special unit types used by the config program */
#define QUES	-1	/* -1 means '?' */
#define	UNKNOWN -2	/* -2 means not set yet */

/*
 * This structure contains information that a SCSI interface controller 
 * needs to execute a SCSI command.
 */
typedef struct ScsiCmd {
	struct	scsi_device *sd; /* device requesting the command */
	int	unit;		/* unit number passed to device done routine */
	int	flags;		/* control flags for this command (see below) */
	int	buflen;		/* length of the data buffer in bytes */
	char	*buf;		/* pointer to data buffer for this command */
	int	cmdlen;		/* length of data in cmdbuf */
	u_char	*cmd;		/* buffer for the SCSI command */
} ScsiCmd;

/*
 * Define flags for controlling the SCSI command.
 *
 * SCSICMD_DATA_TO_DEVICE
 *	TRUE -> data is to be transferred to the device.
 *	FALSE -> data is to be transferred from the device.
 *	meaningless if buflen is 0.
 * SCSICMD_USE_SYNC
 *	Attempt to negotiate for a synchronous data transfer.
 */
#define SCSICMD_DATA_TO_DEVICE	0x01
#define SCSICMD_USE_SYNC	0x02

#ifdef KERNEL
extern struct pmax_ctlr pmax_cinit[];
extern struct scsi_device scsi_dinit[];
#endif
