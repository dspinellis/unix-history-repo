/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

struct variable {
	int	Type;		/* in this case a variable */
	char	*Name;		/* name is always second */
	UnivPtr	v_value;
	int	v_flags;
};

/* variable types/flags */
#define V_BASE10	01	/* is integer in base 10 */
#define V_BASE8		02	/* is integer in base 8 */
#define V_BOOL		04	/* is a boolean */
#define V_STRING	010	/* is a string */
#define V_CHAR		020	/* is a character */
#define V_FILENAME	040	/* a file name (implies V_STRING) */
#define V_TYPEMASK	077	/* mask off the extra bits */
#define V_MODELINE	0100	/* update modeline */
#define V_CLRSCREEN	0200	/* clear and redraw screen */
#define V_TTY_RESET	0400	/* redo the tty modes */

extern const struct variable	variables[];

#ifdef	MAC
# ifdef	TXT_TO_C
int		/* kludge, so setmaps will compile with variables */
# else
extern int
# endif	/* TXT_TO_C */
#else
extern int
#endif	/* MAC */

#ifndef	MAC
	phystab,		/* terminal's tabstop settings */
#endif
	tabstop,		/* expand tabs to this number of spaces */
	RMargin,		/* right margin */
	LMargin,		/* left margin */
	ScrollStep,		/* how should we scroll */
	MarkThresh,		/* moves greater than MarkThresh will SetMark */
	PDelay,			/* paren flash delay in tenths of a second */
	CArgIndent,		/* how to indent arguments to C functions */
	CIndIncrmt,		/* how much each indentation level pushes
				   over in C mode */
	CreatMode,		/* default mode for creat'ing files */
	SyncFreq,		/* how often to sync the file pointers */
	UpdFreq,		/* how often to update modeline */
	MailInt,		/* mail check interval */
	SExitChar,		/* type this to stop i-search */
	AbortChar,		/* cancels command input */
	IntChar,		/* ttysets this to generate QUIT */
#ifdef	IBMPC
	Fgcolor,
	Bgcolor,
	Mdcolor,
#endif	/* IBMPC */
#ifndef	MAC
	EWSize;			/* size to make the error window */
#else
	Macmode;	/* see mac.c */
#endif	/* MAC */

#ifdef	MAC
# ifdef	TXT_TO_C	/* kludge, for setmaps with variables */
bool
# else
extern bool
# endif	/* TXT_TO_C */
#else
extern bool
#endif	/* MAC */
	OKXonXoff,		/* disable start/stop characters */
	OkayBadChars,		/* allow bad characters in files created
				   by JOVE */
#ifdef	ABBREV
	AutoCaseAbbrev,		/* automatically do case on abbreviations */
#endif
	CaseIgnore,		/* case ignore search */
#ifdef	BIFF
	BiffChk,		/* turn off/on biff with entering/exiting jove */
#endif
#ifdef	F_COMPLETION
	DispBadFs,		/* display filenames with bad extensions? */
#endif
	DoEVexpand,		/* treat $foo as environment variable */
	EndWNewline,		/* end files with a blank line */
#ifdef	MAC
	Keyonly,
	Bufchange,
	Modechange,
	EventCmd,
	Windchange,
	Macmode,
#endif	/* MAC */
#ifdef	BACKUPFILES
	BkupOnWrite,		/* make backup files when writing */
#endif
	MarksShouldFloat,	/* adjust marks on insertion/deletion */
	UseRE,			/* use regular expressions in search */
	MetaKey,		/* this terminal has a meta key */
	BriteMode,		/* make the mode line inverse? */
	ScrollAll,		/* we current line scrolls, scroll whole window? */
	UseBuffers,		/* use buffers with Typeout() */
#ifdef	ID_CHAR
	UseIC,			/* whether or not to use i/d char
				   processesing */
#endif
	VisBell,		/* use visible bell (if possible) */
#ifndef	MAC
	WtOnMk,			/* write files on compile-it command */
#endif
	WrapScan;		/* make searches wrap */



#ifdef	MAC
# ifdef	TXT_TO_C	/* kludge, for setmaps with variables */
char
# else
extern char
# endif	/* TXT_TO_C */
#else
extern char
#endif	/* MAC */

#ifndef	MAC
	ErrFmtStr[256],		/* format string for parse errors */
#endif
#ifdef	IPROCS
	proc_prompt[128],	/* process prompt */
	dbx_parse_fmt[128],	/* dbx-mode parse string */
#endif
#ifdef	F_COMPLETION
	BadExtensions[128],	/* extensions (e.g., ".o" to ignore) */
#endif
#ifdef	CMT_FMT
	CmtFmt[80],
#endif
	ModeFmt[120],		/* mode line format string */
#ifdef	UNIX
	Mailbox[FILESIZE],		/* mailbox name */
#endif	/* UNIX */
	TmpFilePath[FILESIZE],	/* directory/device to store tmp files */
	TagFile[FILESIZE],		/* default tag file */
	Shell[FILESIZE];		/* shell to use */
