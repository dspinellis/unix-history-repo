/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* The tmp file is indexed in chunks of CH_SIZE characters.  CH_SIZE is
   (1 << CH_BITS).  New lines are added to the end of the tmp file.  The
   file is not garbage collected because that would be too painful.  As a
   result, commands like Yank and Kill are really easy; basically all we
   do is make copies of the disk addresses of the lines (as opposed to
   the contents).  So, putline(buf) writes BUF to the disk and returns a
   new disk address.  Getline(addr, buf) is the opposite of putline().
   f_getputl(line, fp) reads from open FP directly into the tmp file (into
   the buffer cache (see below)) and stores the address in LINE.  This is
   used during read_file to minimize compying.

   Lines do NOT cross block bounderies in the tmp file so that accessing
   the contents of lines can be much faster.  Pointers to offsets into
   disk buffers are returned instead of copying the contents into local
   arrays and then using them.  This cuts down on the amount of copying a
   great deal, at the expense of less efficiency.  The lower bit of disk
   addresses is used for marking lines as needing redisplay done.

   There is a buffer cache of NBUF buffers (64 on !SMALL machines and the
   3 on small ones).  The blocks are stored in LRU order and each block
   is also stored in a hash table by block #.  When a block is requested
   it can quickly be looked up in the hash table.  If it's not there the
   LRU block is assigned the new block #.  If it finds that the LRU block
   is dirty (i.e., has pending IO) it syncs the WHOLE tmp file, i.e.,
   does all the pending writes.  This works much better on floppy disk
   systems, like the IBM PC, if the blocks are sorted before sync'ing. */

#ifdef SMALL
#   define CH_BITS		4
#   if BUFSIZ == 512
#	define MAX_BLOCKS	1024
#   else
#	define MAX_BLOCKS	512
#   endif
#else
#   define CH_BITS		0
#   define MAX_BLOCKS		4096	/* basically unlimited */
#endif /* SMALL */

#if BUFSIZ == 512
#   define BNO_SHIFT		(9 - CH_BITS)
#else
#   define BNO_SHIFT		(10 - CH_BITS)
#endif

/* CH_SIZE is how big each chunk is.  For each 1 the DFree pointer
   is incremented we extend the tmp file by CH_SIZE characters.
   CH_PBLOCK is the # of chunks per block.  RND_MASK is used to mask
   off the lower order bits of the daddr to round down to the beginning
   of a block.  OFF_MASK masks off the higher order bits so we can get
   at the offset into the disk buffer.

   NOTE:  It's pretty important that these numbers be multiples of
	  2.  Be careful if you change things. */
#ifndef MAC
#define CH_SIZE			(1 << CH_BITS)
#define CH_PBLOCK		(BUFSIZ / CH_SIZE)
#define RND_MASK		(CH_PBLOCK - 1)
#define OFF_MASK		(BUFSIZ - 1)
#define BNO_MASK		(MAX_BLOCKS - 1)
#define blk_round(daddr)	(daddr & ~RND_MASK)
#define forward_block(daddr)	(daddr + CH_PBLOCK)
#define da_to_bno(daddr)	((daddr >> BNO_SHIFT) & BNO_MASK)
#define da_to_off(daddr)	((daddr << CH_BITS) & OFF_MASK)
#define da_too_huge(daddr)	((daddr >> BNO_SHIFT) >= MAX_BLOCKS)
#else
#define CH_SIZE			((disk_line)1 << CH_BITS)
#define CH_PBLOCK		((disk_line)BUFSIZ / CH_SIZE)
#define RND_MASK		((disk_line)CH_PBLOCK - 1)
#define OFF_MASK		((disk_line)BUFSIZ - 1)
#define BNO_MASK		((disk_line)MAX_BLOCKS - 1)
#define blk_round(daddr)	((disk_line)daddr & ~RND_MASK)
#define forward_block(daddr)	((disk_line)daddr + CH_PBLOCK)
#define da_to_bno(daddr)	((disk_line)(daddr >> BNO_SHIFT) & BNO_MASK)
#define da_to_off(daddr)	((disk_line)(daddr << CH_BITS) & OFF_MASK)
#define da_too_huge(daddr)	((disk_line)(daddr >> BNO_SHIFT) >= MAX_BLOCKS)
#endif
