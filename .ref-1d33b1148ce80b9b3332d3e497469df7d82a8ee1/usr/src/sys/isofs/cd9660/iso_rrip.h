/*-
 * Copyright (c) 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley
 * by Pace Willisson (pace@blitz.com).  The Rock Ridge Extension
 * Support code is derived from software contributed to Berkeley
 * by Atsushi Murai (amurai@spec.co.jp).
 *
 * %sccs.include.redist.c%
 *
 *	@(#)iso_rrip.h	8.2 (Berkeley) %G%
 */


/*
 *	Analyze function flag (similar to RR field bits)
 */
#define	ISO_SUSP_ATTR		0x0001
#define	ISO_SUSP_DEVICE		0x0002
#define	ISO_SUSP_SLINK		0x0004
#define	ISO_SUSP_ALTNAME	0x0008
#define	ISO_SUSP_CLINK		0x0010
#define	ISO_SUSP_PLINK		0x0020
#define	ISO_SUSP_RELDIR		0x0040
#define	ISO_SUSP_TSTAMP		0x0080
#define	ISO_SUSP_IDFLAG		0x0100
#define	ISO_SUSP_EXTREF		0x0200
#define	ISO_SUSP_CONT		0x0400
#define	ISO_SUSP_OFFSET		0x0800
#define	ISO_SUSP_STOP		0x1000
#define	ISO_SUSP_UNKNOWN	0x8000

typedef struct {
	struct iso_node	*inop;
	int		fields;		/* interesting fields in this analysis */
	daddr_t		iso_ce_blk;	/* block of continuation area */
	off_t		iso_ce_off;	/* offset of continuation area */
	int		iso_ce_len;	/* length of continuation area */
	struct iso_mnt	*imp;		/* mount structure */
	ino_t		*inump;		/* inode number pointer */
	char		*outbuf;	/* name/symbolic link output area */
	u_short		*outlen;	/* length of above */
	u_short		maxlen;		/* maximum length of above */
	int		cont;		/* continuation of above */
} ISO_RRIP_ANALYZE;

int cd9660_rrip_analyze __P((struct iso_directory_record *isodir,
			    struct iso_node *inop, struct iso_mnt *imp));
int cd9660_rrip_getname __P((struct iso_directory_record *isodir,
			    char *outbuf, u_short *outlen,
			    ino_t *inump, struct iso_mnt *imp));
int cd9660_rrip_getsymname __P((struct iso_directory_record *isodir,
			       char *outbuf, u_short *outlen,
			       struct iso_mnt *imp));
int cd9660_rrip_offset __P((struct iso_directory_record *isodir,
			   struct iso_mnt *imp));
