/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)device.h	7.2 (Berkeley) %G%
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
