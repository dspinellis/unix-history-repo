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
	*ask_file proto((char *prmt, char *def, char *buf)),
	*ask proto((char *, char *, ...)),
	*do_ask proto((char *, int (*) proto((int)), char *, char *, ...));

extern int
	yes_or_no_p proto((char *, ...));

extern void
	minib_add proto((char *str,int movedown));

extern void
	isprocbuf proto((char *bufname)),
	DoAutoExec proto((char *new,char *old)),
	RegToUnix proto((struct buffer *outbuf,char *cmd));

extern void
	/* extend.c */
	vpr_aux proto((const struct variable *, char *)),

	/* proc.c */
#ifndef	MSDOS
	dowait proto((int pid, int *status)),
#endif
	get_FL_info proto((char *, char *)),
	ErrFree proto((void));

extern int
/* jove.c */
	finish proto((int code)),	/* doesn't return at all! */
/* ctype.h */
	ismword proto((int c)),

/* proc.c */
	UnixToBuf proto((char *, int, int, int, ...)),

/* extend.c */
	addgetc proto((void)),
	ask_int proto((char *prompt,int base)),
	aux_complete proto((int c)),
	chr_to_int proto((char *cp,int base,int allints, int *result)),
	complete proto((char * *possible,char *prompt,int flags)),
	joverc proto((char *file)),

/* jove.c */
#ifdef	MAC
	win_reshape proto((int /*junk*/)),
#endif
	charp proto((void)),
	getch proto((void)),
	jgetchar proto((void)),
	getrawinchar proto((void));

extern void
	error proto((char *, ...)),
	complain proto((char *, ...)),
	confirm proto((char *, ...)),
	ttyset proto((int n)),
	tty_reset proto ((void)),
	ToError proto((int forward)),
	Ungetc proto((int c));


#ifdef IBMPC
extern int
	lower proto((char *c)),
	rawkey_ready proto((void));
extern char
	switchar proto((void));
#endif

extern Bufpos
/* insert.c */
	*DoYank proto((struct line *fline,int fchar,struct line *tline,int tchar,struct line *atline,int atchar,struct buffer *whatbuf)),
	*lisp_indent proto((void));

extern void
	Insert proto((int c)),
	LineInsert proto((int num));

/* c.c */
extern Bufpos
	*c_indent proto((int brace)),
	*m_paren proto((int p_type,int dir,int can_mismatch,int can_stop));

extern void
	mp_error proto((void));

extern unsigned char
	chpl proto((void)),
	lpp proto((void));

extern void
	DoJustify proto((struct line *l1,int c1,struct line *l2,int c2,int scrunch,int indent)),

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
	format proto((char *buf, size_t len, char *fmt, char *ap)),
	add_mess proto((char *, ...)),
	f_mess proto((char *, ...)),
	fwritef proto((struct _file *, char *, ...)),
	writef proto((char *, ...)),
	s_mess proto((char *, ...)),
	swritef proto((char *, char *, ...)),

	/* keymaps.c */
	InitKeymaps proto((void)),

	/* paragraph.c */
	do_rfill proto((int ulm)),

	/* macros.c */
	mac_init proto((void)),

	/* misc.c */
	skip_wht_space proto((void)),
	put_bufs proto((int askp)),

	/* term.c */
#ifndef	IBMPC
	getTERM proto((void)),
#endif
	putpad proto((char *str,int lines)),
	putargpad proto((char *str, int arg, int lines)),
	settout proto((char *ttbuf));

#if defined(IBMPC)
extern void
	write_em proto((char *s)),
	write_emc proto((char *s,int n)),
	write_emif proto(()),
	write_emif proto((char *s));
#endif

#ifdef MAC

extern int
	creat proto((char *,int)),
	open proto((char *,int)),
	close proto((int)),
	read proto((int,char *,unsigned)),
	write proto((int,char *,unsigned)),
	unlink proto((char *)),
	chdir proto((char *)),
	rawchkc proto((void)),
	getArgs proto((char ***));

extern long
	lseek proto((int,long,unsigned));

extern time_t
 	time proto((time_t *));

#endif /* MAC */

extern char
	*pwd proto((void)),
#ifdef MAC
	*getwd(),
#else
	*getwd proto((char *)),
#endif
	*pfile proto((char *)),
	*gfile proto((char *));

#ifdef MAC
extern void
	MacInit proto((void)),
	InitBinds proto((void)),
	NPlacur proto((int,int)),
	i_lines proto((int,int,int)),
	d_lines proto((int,int,int)),
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
#if defined(MSDOS)
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
#endif /* MSDOS */
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
#if !defined(MAC)
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
#ifndef _mac	/* conflicts with MacTraps version */
	GrowWindow proto((void)),
#endif
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
#if !defined(MAC)
	ShowErr proto((void)),
	NextError proto((void)),
#endif /* MAC */
#if defined(MSDOS)
	PageScrollUp proto((void)),
	PageScrollDown proto((void)),
#endif /* MSDOS */
#if !defined(MAC)
	PrevError proto((void)),
#endif /* MAC */
	NextLine proto((void)),
	NextPage proto((void)),
	NextWindow proto((void)),
	Recur proto((void)),
	PopMark proto((void)),
	PageNWind proto((void)),
	Tab proto((void)),
	DoParen proto((void)),
#if !defined(MAC)
	ParseAll proto((void)),
#endif
#if defined(SPELL)
	SpelWords proto((void)),
#endif
#if defined(JOB_CONTROL)
	PauseJove proto((void)),
#endif
	PrevLine proto((void)),
	PrevPage proto((void)),
	PrevWindow proto((void)),
#if !defined(MAC)
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
#if !defined(MAC)
	ShellCom proto((void)),
	ShNoBuf proto((void)),
	Shtypeout proto((void)),
	ShToBuf proto((void)),
#endif
	ShrWindow proto((void)),
	Source proto((void)),
#if defined(SPELL)
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
	WrtReg proto((void)),
	Yank proto((void)),
	YankPop proto((void)),
	PrVar proto((void)),
#if !defined(MAC)
	FilterRegion proto((void)),
#endif
	WNumLines proto((void)),
#if defined(IPROCS)
	ShellProc proto((void)),
	ProcInt proto((void)),
	ProcQuit proto((void)),
	ProcKill proto((void)),
#  if !defined(PIPEPROCS)
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

#if defined(LISP)
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

#if defined(CMT_FMT)
	Comment proto((void)),
#endif

	ScrollLeft proto((void)),
	ScrollRight proto((void)),

	MakeKMap proto((void)),
	KmBind proto((void)),
	ProcKmBind proto((void)),

	MacInter proto((void));		/* This is the last one. */

/*==== Declarations of Library/System Routines ====*/

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

extern char
	*calloc proto((unsigned int, unsigned int)),
	*malloc proto((size_t)),
	*realloc proto((UnivPtr, size_t));

/* Date and Time <time.h> */

extern time_t	time proto((time_t */*tloc*/));
extern char	*ctime proto((const time_t *));

/* UNIX */

#ifdef IBMPC
#define const	/* the const's in the following defs conflict with MSC 5.1 */
#endif

extern int	chdir proto((const char */*path*/));
extern int	access proto((const char */*path*/, int /*mode*/));
extern int	creat proto((const char */*path*/, int /*mode*/));
	/* Open may have an optional third argument, int mode */
extern int	open proto((const char */*path*/, int /*flags*/, ...));

#ifdef IBMPC
extern int	read proto((int /*fd*/, char * /*buf*/, size_t /*nbytes*/));
extern int	write proto((int /*fd*/, char * /*buf*/, size_t /*nbytes*/));
#else
extern int	read proto((int /*fd*/, UnivPtr /*buf*/, size_t /*nbytes*/));
extern int	write proto((int /*fd*/, UnivPtr /*buf*/, size_t /*nbytes*/));
#endif

extern int	execl proto((const char */*name*/, const char */*arg0*/, ...));
extern int	execlp proto((const char */*name*/, const char */*arg0*/, ...));
extern int	execv proto((const char */*name*/, const char */*argv*/[]));
extern int	execvp proto((const char */*name*/, const char */*argv*/[]));

#ifdef IBMPC
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
extern int	unlink proto((const char */*path*/));
extern int	fsync proto((int));

extern int	fork proto((void));

extern int	getpid proto((void));
extern int	getuid proto((void));

extern int	kill proto((int /*pid*/, int /*sig*/));

extern char	*mktemp proto((char *));

/* BSD UNIX
 *
 * Note: in most systems, declarations of non-existant functions is
 * OK if they are never actually called.  The parentheses around the
 * name prevents any macro expansion.
 */

extern int	UNDEF(vfork) proto((void));

#ifdef BSD_SIGS
extern int	UNDEF(killpg) proto((int /*pgrp*/, int /*sig*/));
#endif
extern int	UNDEF(setpgrp) proto((int /*pid*/, int /*pgrp*/));	/* Sys V takes no arg */

#ifdef	__STDC__
struct timeval;	/* forward declaration */
#endif	/* __STDC__ */
extern int	UNDEF(select) proto((int /*width*/, long */*readfds*/, long */*writefds*/,
	long */*exceptfds*/, struct timeval */*timeout*/));

extern void	UNDEF(bcopy) proto((UnivConstPtr, UnivPtr, size_t));
extern void	UNDEF(bzero) proto((UnivPtr, size_t));
extern int	UNDEF(ffs) proto((long));

/* termcap */

extern int	UNDEF(tgetent) proto((char */*buf*/, const char */*name*/));
extern int	UNDEF(tgetflag) proto((const char */*id*/));
extern int	UNDEF(tgetnum) proto((const char */*id*/));
extern char	UNDEF(*tgetstr) proto((const char */*id*/, char **/*area*/));
extern void	UNDEF(tputs) proto((const char *, int, void (*) proto((int))));
extern char	UNDEF(*tgoto) proto((const char *, int /*destcol*/, int /*destline*/));


