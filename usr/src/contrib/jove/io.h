/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

extern char	*HomeDir;

extern size_t	HomeLen;

extern int	DOLsave;	/* Do Lsave flag.  If lines aren't being saved
				   when you think they should have been, this
				   flag is probably not being set, or is being
				   cleared before lsave() was called. */

extern daddr	DFree;  /* pointer to end of tmp file */

extern int	Jr_Len;		/* length of Just Read Line */

extern char
	*lbptr proto((struct line *line)),
	*pr_name proto((char *fname,int okay_home)),
	*pwd proto((void)),
	*sprint proto((char *, ...));

extern struct _file
	*open_file proto((char *fname,char *buf,int how,int complainifbad,int loudness));

extern void
	setCWD proto((char *d)),
	getCWD proto((void)),
	PathParse proto((char *name,char *intobuf)),
	SyncTmp proto((void)),
	close_file proto((struct _file *fp)),
	d_cache_init proto((void)),
	file_write proto((char *fname,int app)),
	filemunge proto((char *newname)),
	getline proto((daddr addr,char *buf)),
	lsave proto((void)),
	putreg proto((struct _file *fp,struct line *line1,int char1,struct line *line2,int char2,int makesure)),
	read_file proto((char *file,int is_insert)),
	tmpclose proto((void)),
	tmpinit proto((void)),

	WriteFile proto((void));

extern int
	chkCWD proto((char *dn));

extern daddr
	f_getputl proto((struct line *line,struct _file *fp)),
	putline proto((char *buf));

#if !(defined(MSDOS) || defined(MAC))
extern void
	chk_mtime proto((Buffer *thisbuf, char *fname, char *how));
#endif
