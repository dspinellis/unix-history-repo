/* Copyright (c) 1981 Regents of the University of California */

/* "@(#)dumprestore.h 1.3 %G%" */

/*
 * TP_BSIZE is the size of file blocks on the dump tapes.
 * Note that TP_BSIZE must be a multiple of DEV_BSIZE.
 *
 * NTREC is the number of TP_BSIZE blocks that are written
 * in each tape record.
 *
 * BLKING calculates the number of TP_BSIZE blocks in a file
 * system frag.
 *
 * TP_NINDIR is the number of indirect pointers in a TS_INODE
 * or TS_ADDR record. Note that it must be a power of two.
 */
#define TP_BSIZE	1024
#define NTREC   	10
#define BLKING(fs)	((fs)->fs_fsize/TP_BSIZE)
#define TP_NINDIR	(TP_BSIZE/2)

#define TS_TAPE 	1
#define TS_INODE	2
#define TS_BITS 	3
#define TS_ADDR 	4
#define TS_END  	5
#define TS_CLRI 	6
#define MAGIC   	(int)60011
#define CHECKSUM	(int)84446

union u_spcl {
	char dummy[TP_BSIZE];
	struct	s_spcl {
		int	c_type;
		time_t	c_date;
		time_t	c_ddate;
		int	c_volume;
		daddr_t	c_tapea;
		ino_t	c_inumber;
		int	c_magic;
		int	c_checksum;
		struct	dinode	c_dinode;
		int	c_count;
		char	c_addr[TP_NINDIR];
	} s_spcl;
} u_spcl;

#define spcl u_spcl.s_spcl

struct	idates {
	char	id_name[16];
	char	id_incno;
	time_t	id_ddate;
};

#define	DUMPOUTFMT	"%-16s %c %s"		/* for printf */
						/* name, incno, ctime(date) */
#define	DUMPINFMT	"%16s %c %[^\n]\n"	/* inverse for scanf */
