/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)volhdr.h	7.1 (Berkeley) 5/8/90
 */

/*
 * vohldr.h: volume header for "LIF" format volumes
 */

struct	lifvol {
	short	vol_id;
	char	vol_label[6];
	int	vol_addr;
	short	vol_oct;
	short	vol_dummy;
	int	vol_dirsize;
	short	vol_version;
	short	vol_zero;
	int	vol_huh1;
	int	vol_huh2;
	int	vol_length;
};

struct	lifdir {
	char	dir_name[10];
	short	dir_type;
	int	dir_addr;
	int	dir_length;
	char	dir_toc[6];
	short	dir_flag;
	int	dir_exec;
};

/* load header for boot rom */
struct load {
	int address;
	int count;
};

#define VOL_ID		-32768
#define VOL_OCT		4096
#define	DIR_TYPE	-5822
#define DIR_FLAG	0x8001	/* dont ask me! */
#define	SECTSIZE	256
