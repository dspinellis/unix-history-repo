/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

extern char
/* proc.c */
	*MakeName proto((char *command)),

/* ask.c */
	*ask_file proto((const char *prmt, char *def, char *buf)),
	*ask proto((char *, char *, ...)),
	*do_ask proto((char *, bool (*) proto((int)), char *, const char *, ...));

extern int
	yes_or_no_p proto((char *, ...));

extern void
	minib_add proto((char *str, bool movedown));

extern void
	isprocbuf proto((char *bufname)),
	DoAutoExec proto((char *new, char *old));

extern void
	/* extend.c */
	vpr_aux proto((const struct variable *, char *, size_t)),

	/* proc.c */
#ifndef	MSDOS
	dowait proto((int pid, int *status)),
#endif
	get_FL_info proto((char *, char *)),
	ChkErrorLines proto((void)),
	ErrFree proto((void));

extern SIGRESULT
/* jove.c */
	finish proto((int code));	/* doesn't return at all! */

extern int
/* ctype.h */
	ismword proto((int c)),

/* proc.c */
	UnixToBuf proto((char *, char *, bool, int, bool, ...)),

/* extend.c */
	addgetc proto((void)),
	ask_int proto((char *prompt, int base)),
	chr_to_int proto((char *cp, int base, int allints, int *result)),
	complete proto((char **possible, const char *prompt, int flags)),

/* jove.c */
#ifdef	MAC
	win_reshape proto((int /*junk*/)),
#endif
	charp proto((void)),
	getch proto((void)),
	jgetchar proto((void)),
	getrawinchar proto((void));

#ifdef	LOAD_AV
extern int
	get_la proto((void));	/* integer, units of .01 */

extern void
	closekmem proto((void));
#endif	/* LOAD_AV */

extern bool
	joverc proto((char *file));

extern void
	raw_scream proto((const char *)),
	error proto((const char *, ...)),
	complain proto((const char *, ...)),
	raw_complain proto((const char *, ...)),
	confirm proto((const char *, ...)),
	ttyset proto((bool n)),
	tty_reset proto ((void)),
	Ungetc proto((int c));


#ifdef	IBMPC
extern bool
	rawkey_ready proto((void));
extern char
	switchar proto((void));
#endif

extern Bufpos
/* insert.c */
	*DoYank proto((struct line *fline, int fchar, struct line *tline, int tchar, struct line *atline, int atchar, struct buffer *whatbuf));

extern void
	LineInsert proto((int num));

/* c.c */
extern Bufpos
	*c_indent proto((int brace)),
	*m_paren proto((int p_type, int dir, int can_mismatch, int can_stop));

extern void
	mp_error proto((void));

extern unsigned char
	chpl proto((void)),
	lpp proto((void));

extern void
	DoJustify proto((struct line *l1, int c1, struct line *l2, int c2, bool scrunch, int indent)),

	/* abbrev.c */
#ifdef	ABBREV
	AbbrevExpand proto((void)),
#endif

	/* disp.c */
	message proto((char *)),

	/* insert.c */
#ifdef	ABBREV
	MaybeAbbrevExpand proto((void)),
#endif
	GCchunks proto((void)),

	/* fmt.c */
	format proto((char *buf, size_t len, const char *fmt, char *ap)),
	add_mess proto((const char *, ...)),
	f_mess proto((const char *, ...)),
	fwritef proto((struct FileStruct *, const char *, ...)),
	writef proto((const char *, ...)),
	s_mess proto((const char *, ...)),
	swritef proto((char *, size_t, const char *, ...)),

	/* keymaps.c */
	InitKeymaps proto((void)),

	/* paragraph.c */
	do_rfill proto((bool ulm)),

	/* macros.c */
	mac_init proto((void)),

	/* misc.c */
	skip_wht_space proto((void)),
	put_bufs proto((bool askp)),

	/* term.c */
#ifndef	IBMPC
	getTERM proto((void)),
#endif
	putpad proto((char *str, int lines)),
	putargpad proto((char *str, int arg, int lines)),
	settout proto((char *ttbuf));

#ifdef	IBMPC
extern void
	write_em proto((char *s)),
	write_emc proto((char *s, int n)),
	write_emif proto((char *s));
#endif

#ifdef	MAC

extern int
	creat proto((char *, int)),
	open proto((const char *, int, ...)),
	close proto((int)),
	read proto((int, const char *, unsigned)),
	write proto((int, const char *, unsigned)),
	unlink proto((const char *)),
	chdir proto((const char *)),
	getArgs proto((char ***));

extern bool
	rawchkc proto((void));

extern long
	lseek proto((int, long, unsigned));

extern time_t
	time proto((time_t *));

#endif	/* MAC */

extern char
	*pwd proto((void)),
#ifdef	MAC
	*getwd proto((void)),	/* OOPS: same name, different type! */
#else
	*getwd proto((char *)),
#endif
	*pfile proto((char *)),
	*gfile proto((char *));

#ifdef	MAC
extern void
	MacInit proto((void)),
	InitBinds proto((void)),
	NPlacur proto((int, int)),
	i_lines proto((int, int, int)),
	d_lines proto((int, int, int)),
	clr_page proto((void)),
	clr_eoln proto((void)),
	docontrols proto((void)),
	RemoveScrollBar proto((Window *)),
	InitEvents proto((void)),
	menus_on proto((void)),
	menus_off proto((void));
#endif

/* Wired Commands */

extern void
	UnbindC proto((void)),
	ShowVersion proto((void)),
	WVisSpace proto((void)),
	AppReg proto((void)),
	Apropos proto((void)),
	BackChar proto((void)),
	BList proto((void)),
	FList proto((void)),
	BUpList proto((void)),
	FDownList proto((void)),
	BSexpr proto((void)),
	BackWord proto((void)),
	Bof proto((void)),
	Bol proto((void)),
	Bos proto((void)),
	Bow proto((void)),
	BindAKey proto((void)),
	BindMac proto((void)),
	BufPos proto((void)),
#ifdef	MSDOS
	Buf0Select proto((void)),
	Buf1Select proto((void)),
	Buf2Select proto((void)),
	Buf3Select proto((void)),
	Buf4Select proto((void)),
	Buf5Select proto((void)),
	Buf6Select proto((void)),
	Buf7Select proto((void)),
	Buf8Select proto((void)),
	Buf9Select proto((void)),
	Buf10Select proto((void)),
#endif	/* MSDOS */
	CasRegLower proto((void)),
	CasRegUpper proto((void)),
	CapChar proto((void)),
	CapWord proto((void)),
	LowWord proto((void)),
	UppWord proto((void)),
	Chdir proto((void)),
	prCWD proto((void)),
	prDIRS proto((void)),
	Pushd proto((void)),
	Popd proto((void)),
	prCTIME proto((void)),
	ChrToOct proto((void)),
	ClAndRedraw proto((void)),
#ifndef	MAC
	MakeErrors proto((void)),
	ErrParse proto((void)),
#endif
	CopyRegion proto((void)),
	BufSelect proto((void)),
	DelBlnkLines proto((void)),
	DelNChar proto((void)),
	DelNWord proto((void)),
	OneWindow proto((void)),
	DelPChar proto((void)),
	DelPWord proto((void)),
	DelReg proto((void)),
	KillSome proto((void)),
	DelWtSpace proto((void)),
	DelCurWindow proto((void)),
	KeyDesc proto((void)),
	Digit proto((void)),
	Digit0 proto((void)),
	Digit1 proto((void)),
	Digit2 proto((void)),
	Digit3 proto((void)),
	Digit4 proto((void)),
	Digit5 proto((void)),
	Digit6 proto((void)),
	Digit7 proto((void)),
	Digit8 proto((void)),
	Digit9 proto((void)),
	DescBindings proto((void)),
	DescCom proto((void)),
	Eof proto((void)),
	Eol proto((void)),
	Eos proto((void)),
	Eow proto((void)),
	ForPara proto((void)),
	BackPara proto((void)),
	BufErase proto((void)),
	PtToMark proto((void)),
	Extend proto((void)),
	ExecMacro proto((void)),
	RunMacro proto((void)),
	Leave proto((void)),
	FindFile proto((void)),
	WindFind proto((void)),
	FindTag proto((void)),
	FDotTag proto((void)),
	ToIndent proto((void)),
	ForChar proto((void)),
	FSexpr proto((void)),
	ForWord proto((void)),
	TimesFour proto((void)),
	GoLine proto((void)),
	GrowWindowCmd proto((void)),
	IncFSearch proto((void)),
	IncRSearch proto((void)),
	InsFile proto((void)),
	Justify proto((void)),
	RegJustify proto((void)),
	SetLMargin proto((void)),
	SetRMargin proto((void)),
	LRShift proto((void)),
	RRShift proto((void)),
	BufKill proto((void)),
	KillBos proto((void)),
	KillEos proto((void)),
	KillEOL proto((void)),
	KillExpr proto((void)),
	BufList proto((void)),
	NotModified proto((void)),
	NameMac proto((void)),
	DelMacro proto((void)),
	Newline proto((void)),
	OpenLine proto((void)),
	LineAI proto((void)),
#ifndef	MAC
	ShowErr proto((void)),
	NextError proto((void)),
	PrevError proto((void)),
	ParseAll proto((void)),
#endif	/* MAC */
#ifdef	MSDOS
	PageScrollUp proto((void)),
	PageScrollDown proto((void)),
#endif	/* MSDOS */
	NextLine proto((void)),
	NextPage proto((void)),
	NextWindow proto((void)),
	Recur proto((void)),
	PopMark proto((void)),
	PageNWind proto((void)),
	Tab proto((void)),
	DoParen proto((void)),
#ifdef	SPELL
	SpelWords proto((void)),
#endif
#ifdef	JOB_CONTROL
	PauseJove proto((void)),
#endif
	PrevLine proto((void)),
	PrevPage proto((void)),
	PrevWindow proto((void)),
#ifndef	MAC
	jcloseall proto((void)),
	Push proto((void)),
#endif
	RegReplace proto((void)),
	QRepSearch proto((void)),
	QuotChar proto((void)),
	ReadFile proto((void)),
	DefKBDMac proto((void)),
	RedrawDisplay proto((void)),
	ReNamBuf proto((void)),
	RepSearch proto((void)),
	DownScroll proto((void)),
	UpScroll proto((void)),
	ForSearch proto((void)),
	FSrchND proto((void)),
	RevSearch proto((void)),
	RSrchND proto((void)),
	SelfInsert proto((void)),
	SetVar proto((void)),
	SetMark proto((void)),
#ifndef	MAC
	ShellCom proto((void)),
	ShNoBuf proto((void)),
	Shtypeout proto((void)),
	ShToBuf proto((void)),
	FilterRegion proto((void)),
#endif
	ShrWindow proto((void)),
	Source proto((void)),
#ifdef	SPELL
	SpelBuffer proto((void)),
#endif
	SplitWind proto((void)),
	GotoWind proto((void)),
	Remember proto((void)),
	Forget proto((void)),
	StrLength proto((void)),
	TransChar proto((void)),
	TransLines proto((void)),
	SaveFile proto((void)),
	WtModBuf proto((void)),
	WriteMacs proto((void)),
	XtermMouse proto((void)),
	WrtReg proto((void)),
	Yank proto((void)),
	YankPop proto((void)),
	PrVar proto((void)),
	WNumLines proto((void)),
#ifdef	IPROCS
	ShellProc proto((void)),
	ProcInt proto((void)),
	ProcQuit proto((void)),
	ProcKill proto((void)),
#  ifdef	PTYPROCS
	ProcEof proto((void)),
	ProcStop proto((void)),
	ProcCont proto((void)),
	ProcDStop proto((void)),
#  endif
	ProcSendData proto((void)),
	ProcNewline proto((void)),
	ProcList proto((void)),
	ProcBind proto((void)),
	Iprocess proto((void)),
	DBXpoutput proto((void)),
#endif

#ifdef	LISP
	GSexpr proto((void)),	/* Grind S Expression. */
	AddSpecial proto((void)),	/* add lisp special form */
#endif
	CAutoExec proto((void)),
	MAutoExec proto((void)),

	DefMAbbrev proto((void)),
	DefGAbbrev proto((void)),
	SaveAbbrevs proto((void)),
	RestAbbrevs proto((void)),
	EditAbbrevs proto((void)),
	BindMtoW proto((void)),

#ifdef	CMT_FMT
	Comment proto((void)),
#endif

	ScrollLeft proto((void)),
	ScrollRight proto((void)),

	MakeKMap proto((void)),
	KmBind proto((void)),
	ProcKmBind proto((void)),

	MacInter proto((void));		/* This is the last one. */

/*==== Declarations of Library/System Routines ====*/

extern int	errno;	/* Redundant if declared in <errno.h> -- DHR */
extern char *strerror proto((int));	/* errno.h or string.h? */

/* General Utilities: <stdlib.h> */

extern int	abs proto((int));

extern void	abort proto((void));
extern void	exit proto((int));

extern int	atoi proto((const char */*nptr*/));

extern void	qsort proto((UnivPtr /*base*/, size_t /*nmemb*/,
	size_t /*size*/, int (*/*compar*/)(UnivConstPtr, UnivConstPtr)));

extern char	*getenv proto((const char *));
extern int	system proto((const char *));

extern void
	free proto((UnivPtr));

extern UnivPtr
	calloc proto((unsigned int, unsigned int)),
	malloc proto((size_t)),
	realloc proto((UnivPtr, size_t));

/* Date and Time <time.h> */

extern time_t	time proto((time_t */*tloc*/));
extern char	*ctime proto((const time_t *));

/* UNIX */

#ifdef	IBMPC
#define const	/* the const's in the following defs conflict with MSC 5.1 */
#endif

#ifdef	POSIX_UNISTD
# include <unistd.h>
#else	/* !POSIX_UNISTD */
extern int	chdir proto((const char */*path*/));
extern int	access proto((const char */*path*/, int /*mode*/));
extern int	creat proto((const char */*path*/, int /*mode*/));
	/* Open may have an optional third argument, int mode */
extern int	open proto((const char */*path*/, int /*flags*/, ...));


#ifdef	IBMPC
extern int	read proto((int /*fd*/, char * /*buf*/, size_t /*nbytes*/));
extern int	write proto((int /*fd*/, const char * /*buf*/, size_t /*nbytes*/));
#else
extern int	read proto((int /*fd*/, UnivPtr /*buf*/, size_t /*nbytes*/));
extern int	write proto((int /*fd*/, UnivConstPtr /*buf*/, size_t /*nbytes*/));
#endif

extern int	execl proto((const char */*name*/, const char */*arg0*/, ...));
extern int	execlp proto((const char */*name*/, const char */*arg0*/, ...));
extern int	execv proto((const char */*name*/, const char */*argv*/[]));
extern int	execvp proto((const char */*name*/, const char */*argv*/[]));

#ifdef	IBMPC
#undef const
#endif

extern void	_exit proto((int));	/* exit(), without flush, etc */

extern unsigned	alarm proto((unsigned /*seconds*/));

extern int	pipe proto((int *));
extern int	close proto((int));
extern int	dup proto((int));
extern int	dup2 proto((int /*old_fd*/, int /*new_fd*/));
extern long	lseek proto((int /*fd*/, long /*offset*/, int /*whence*/));
extern int	fchmod proto((int /*fd*/, int /*mode*/));
extern int	fork proto((void));

extern int	getpid proto((void));
extern int	getuid proto((void));

extern int	setuid proto((int));
extern int	chown proto((const char *, int, int));

extern int	kill proto((int /*pid*/, int /*sig*/));
extern void	perror proto((const char *));

#endif	/* POSIX_UNISTD */

extern int	unlink proto((const char */*path*/));
extern int	fsync proto((int));


extern char	*mktemp proto((char *));


/* BSD UNIX
 *
 * Note: in most systems, declaration of a non-existant function is
 * OK if the function is never actually called.  The parentheses around
 * the name prevent any macro expansion.
 */

extern int	UNMACRO(vfork) proto((void));

#ifdef	BSD_SIGS
#ifndef BSD386
extern int	UNMACRO(killpg) proto((int /*pgrp*/, int /*sig*/));
#endif
#endif

#ifndef BSD386
extern int	UNMACRO(setpgrp) proto((int /*pid*/, int /*pgrp*/));	/* Sys V takes no arg */
#endif

#ifdef	REALSTDC
struct timeval;	/* forward declaration preventing prototype scoping */
#endif

#ifndef BSD386
extern int	UNMACRO(select) proto((int /*width*/, fd_set */*readfds*/, fd_set */*writefds*/,
	fd_set */*exceptfds*/, struct timeval */*timeout*/));
#endif

extern void	UNMACRO(bcopy) proto((UnivConstPtr, UnivPtr, size_t));
extern void	UNMACRO(bzero) proto((UnivPtr, size_t));

/* termcap */

extern int	UNMACRO(tgetent) proto((char */*buf*/, const char */*name*/));
extern int	UNMACRO(tgetflag) proto((const char */*id*/));
extern int	UNMACRO(tgetnum) proto((const char */*id*/));
extern char	*UNMACRO(tgetstr) proto((const char */*id*/, char **/*area*/));
extern void	UNMACRO(tputs) proto((const char *, int, void (*) proto((int))));
extern char	*UNMACRO(tgoto) proto((const char *, int /*destcol*/, int /*destline*/));
