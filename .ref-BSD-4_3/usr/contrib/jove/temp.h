/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

/* This algorithm is just like the VI and ED ones.  There are several
   differences though.  The first is that I don't just have THREE or TWO
   incore blocks of the tmp file.  Instead there is a buffer cache of NBUF
   buffers (64 on VM machines and the normal 3 on smaller ones).  Each block
   is stored in LRU order and in a hash table by block #.  When a block is
   requested it can quickly be looked up in the hash table.  If it's not
   there the LRU block is used.  If it finds that the LRU block is dirty it
   syncs the whole tmp file, i.e., does all the pending writes.  This works
   really well on floppy disk systems, like the IBM PC, if the blocks are
   sorted first.

   The constants below are sorta hard to grok because they are in disguise,
   but the basic idea is this:  The tmp file is allocated in chunks of
   BNDRY/2 (or is it BNDRY? I can't remember) characters.  New lines are
   added to the end of the tmp file.  The file is not garbage collected
   because that would be too painful.  As a result, commands like Yank and
   Kill are really easy.  Basically all we do is make copies of the disk
   addresses of the lines.  It's fast--very.  So, putline(buf) writes BUF to
   the disk and returns a new disk address.  Getline(addr, buf) is the
   opposite of putline().  Lines do NOT cross block bounderies (as in VI and
   ED) so that accessing the contents of lines can be much faster.  Pointers
   to offsets into disk buffers are returned instead of copying the contents
   into local arrays and then using them.  This cut down on the amount of
   copying a great deal, at the expense of less efficiency.  But it's not a
   big deal, really.  Incrementing the logical disk pointer by INCRMT is
   like incrementing the physical disk pointer by a block.  The lower bit is
   left alone, so JOVE uses that to mark lines as needing redisplay done to
   them. */

#ifndef VMUNIX

#if BUFSIZ == 512
#	define	BLKMSK	01777
#	define	BNDRY	16
#	define	INCRMT	0100
#	define	LBTMSK	0760
#	define	NMBLKS	1018
#	define	OFFBTS	6
#	define	OFFMSK	077
#	define	SHFT	3
#else
#	define	BLKMSK	0777
#	define	BNDRY	16
#	define	INCRMT	0200
#	define	LBTMSK	01760
#	define	NMBLKS	506
#	define	OFFBTS	7
#	define	OFFMSK	0177
#	define	SHFT	3
#endif

#else

#define	BLKMSK	077777
#define	BNDRY	2
#define	INCRMT	02000
#define	LBTMSK	01776
#define	NMBLKS	077770
#define	OFFBTS	10
#define	OFFMSK	01777
#define	SHFT	0

#endif VMUNIX

extern int	nleft,		/* Number of good characters left in current block */
		tmpfd;
extern disk_line
		tline;	/* Pointer to end of tmp file */

extern char	*tfname;
