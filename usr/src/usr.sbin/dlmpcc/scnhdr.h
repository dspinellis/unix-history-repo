/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)scnhdr.h	5.3 (Berkeley) 6/1/90
 */

struct scnhdr {
	char		s_name[8];	/* section name */
	long		s_paddr;	/* physical address */
	long		s_vaddr;	/* virtual address */
	long		s_size;		/* section size */
	long		s_scnptr;	/* file ptr to raw data for section */
	long		s_relptr;	/* file ptr to relocation */
	long		s_lnnoptr;	/* file ptr to line numbers */
	unsigned short	s_nreloc;	/* number of relocation entries */
	unsigned short	s_nlnno;	/* number of line number entries */
	long		s_flags;	/* flags */
	};

#define	SCNHDR	struct scnhdr
#define	SCNHSZ	sizeof(SCNHDR)




/*
 * Define constants for names of "special" sections
 */

#define _TEXT ".text"
#define _DATA ".data"
#define _BSS  ".bss"
#define _TV  ".tv"




/*
 * The low 4 bits of s_flags is used as a section "type"
 */

#define STYP_REG	0x00		/* "regular" section:
						allocated, relocated, loaded */
#define STYP_DSECT	0x01		/* "dummy" section:
						not allocated, relocated,
						not loaded */
#define STYP_NOLOAD	0x02		/* "noload" section:
						allocated, relocated,
						 not loaded */
#define STYP_GROUP	0x04		/* "grouped" section:
						formed of input sections */
#define STYP_PAD	0x08		/* "padding" section:
						not allocated, not relocated,
						 loaded */
#define STYP_COPY	0x10		/* "copy" section:
						for decision function used
						by field update;  not
						allocated, not relocated,
						loaded;  reloc & lineno
						entries processed normally */
#define	STYP_TEXT	0x20		/* section contains text only */
#define STYP_DATA	0x40		/* section contains data only */
#define STYP_BSS	0x80		/* section contains bss only */

/*
 *  In a minimal file or an update file, a new function
 *  (as compared with a replaced function) is indicated by S_NEWFCN
 */

#define S_NEWFCN  0x10

/*
 * In 3b Update Files (output of ogen), sections which appear in SHARED
 * segments of the Pfile will have the S_SHRSEG flag set by ogen, to inform
 * dufr that updating 1 copy of the proc. will update all process invocations.
 */

#define S_SHRSEG	0x20
