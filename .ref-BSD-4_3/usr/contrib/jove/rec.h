/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

struct rec_head {
	int	Uid,		/* Uid of owner. */
		Pid;		/* Pid of jove process. */
	time_t	UpdTime;	/* Last time this was updated. */
	int	Nbuffers;	/* Number of buffers. */
};

struct rec_entry {
	char	r_bname[128],
		r_fname[128];
	int	r_nlines;
};

