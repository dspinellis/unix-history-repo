/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

extern char	*HomeDir;

extern size_t	HomeLen;

extern bool	DOLsave;	/* Do Lsave flag.  If lines aren't being saved
				   when you think they should have been, this
				   flag is probably not being set, or is being
				   cleared before lsave() was called. */

extern daddr	DFree;  /* pointer to end of tmp file */

extern int	Jr_Len;		/* length of Just Read Line */

extern char
	*lbptr proto((struct line *line)),
	*pr_name proto((char *fname,int okay_home)),
	*pwd proto((void)),
	*sprint proto((const char *, ...));

extern struct FileStruct
	*open_file proto((char *fname,char *buf,int how,int complainifbad,int loudness));

extern void
	setCWD proto((char *d)),
	getCWD proto((void)),
	PathParse proto((char *name,char *intobuf)),
	SyncTmp proto((void)),
	close_file proto((struct FileStruct *fp)),
	d_cache_init proto((void)),
	file_write proto((char *fname, bool app)),
	filemunge proto((char *newname)),
	getline proto((daddr addr,char *buf)),
	lsave proto((void)),
	putreg proto((struct FileStruct *fp,struct line *line1,int char1,struct line *line2,int char2,bool makesure)),
	read_file proto((char *file, bool is_insert)),
	tmpclose proto((void)),
	tmpremove proto((void)),

	WriteFile proto((void));

extern bool
	chkCWD proto((char *dn));

extern daddr
	putline proto((char *buf));

#ifdef	MAC
#define	chk_mtime(thisbuf, fname, how)	{ }
#else
#ifdef	MSDOS
#define	chk_mtime(thisbuf, fname, how)	{ }
#else
extern void
	chk_mtime proto((Buffer *thisbuf, char *fname, char *how));
#endif
#endif
