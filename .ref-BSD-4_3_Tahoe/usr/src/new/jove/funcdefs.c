/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "ctype.h"

#if !defined(TXT_TO_C) 
extern void
	EscPrefix(),
	CtlxPrefix(),
	MiscPrefix(),
	UnbindC(),
	ShowVersion(),
	WVisSpace(),
#ifdef ANSICODES
	AnsiCodes(),
#endif
	AppReg(),
	Apropos(),
	BackChar(),
	BList(),
	FList(),
	BUpList(),
	FDownList(),
	BSexpr(),
	BackWord(),
	Bof(),
	Bol(),
	Bos(),
	Bow(),
	BindAKey(),
	BindMac(),
	BufPos(),
#ifdef MSDOS
	Buf1Select(),
	Buf2Select(),
	Buf3Select(),
	Buf4Select(),
	Buf5Select(),
	Buf6Select(),
	Buf7Select(),
	Buf8Select(),
	Buf9Select(),
	Buf10Select(),
#endif /* MSDOS */
	CasRegLower(),
	CasRegUpper(),
	CapChar(),
	CapWord(),
	LowWord(),
	UppWord(),
#ifdef CHDIR
	Chdir(),
	prCWD(),
	prDIRS(),
	Pushd(),
	Popd(),
#endif
	prCTIME(),
	ChrToOct(),
	ClAndRedraw(),
#ifndef MAC
	MakeErrors(),
#endif
	CopyRegion(),
	BufSelect(),
	DelBlnkLines(),
	DelNChar(),
	DelNWord(),
	OneWindow(),
	DelPChar(),
	DelPWord(),
	DelReg(),
	KillSome(),
	DelWtSpace(),
	DelCurWindow(),
	KeyDesc(),
	Digit(),
	Digit0(),
	Digit1(),
	Digit2(),
	Digit3(),
	Digit4(),
	Digit5(),
	Digit6(),
	Digit7(),
	Digit8(),
	Digit9(),
	DescBindings(),
	DescCom(),
	Eof(),
	Eol(),
	Eos(),
	Eow(),
	ForPara(),
	BackPara(),
	BufErase(),
	PtToMark(),
	Extend(),
	ExecMacro(),
	RunMacro(),
	Leave(),
	FindFile(),
	WindFind(),
	FindTag(),
	FDotTag(),
	ToIndent(),
	ForChar(),
	FSexpr(),
	ForWord(),
	TimesFour(),
	GoLine(),
	GrowWindow(),
	IncFSearch(),
	IncRSearch(),
	InsFile(),
	Justify(),
	RegJustify(),
	SetLMargin(),
	SetRMargin(),
	BufKill(),
	KillBos(),
	KillEos(),
	KillEOL(),
	KillExpr(),
	BufList(),
	NotModified(),
	NameMac(),
	DelMacro(),
	Newline(),
	OpenLine(),
	LineAI(),
#ifndef MAC
	ShowErr(),
	NextError(),
#endif /* MAC */
#ifdef MSDOS
	PageScrollUp(),
	PageScrollDown(),
#endif /* MSDOS */
#ifndef MAC
	PrevError(),
#endif /* MAC */
	NextLine(),
	NextPage(),
	NextWindow(),
	Recur(),
	PopMark(),
	PageNWind(),
	Tab(),
	DoParen(),
#ifndef MAC
	ParseAll(),
#endif
#ifdef SPELL
	SpelWords(),
#endif
#ifdef JOB_CONTROL
	PauseJove(),
#endif
	PrevLine(),
	PrevPage(),
	PrevWindow(),
#ifndef MAC
	Push(),
#endif
	RegReplace(),
	QRepSearch(),
	QuotChar(),
	ReadFile(),
	DefKBDMac(),
	RedrawDisplay(),
	ReNamBuf(),
	RepSearch(),
	DownScroll(),
	UpScroll(),
	ForSearch(),
	FSrchND(),
	RevSearch(),
	RSrchND(),
	SelfInsert(),
	SetVar(),
 	SetMark(),
#ifndef MAC
	ShellCom(),
	ShNoBuf(),
	Shtypeout(),
	ShToBuf(),
#endif
	ShrWindow(),
	Source(),
#ifdef SPELL
	SpelBuffer(),
#endif
	SplitWind(),
	GotoWind(),
	Remember(),
	Forget(),
	StrLength(),
	TransChar(),
	TransLines(),
	SaveFile(),
	WtModBuf(),
	WriteFile(),
	WriteMacs(),
	WrtReg(),
	Yank(),
	YankPop(),	PrVar(),
#ifndef MAC
	FilterRegion(),
#endif
	WNumLines(),
#ifdef IPROCS
	ShellProc(),
	ProcInt(),
	ProcQuit(),
	ProcKill(),
#  ifndef PIPEPROCS
	ProcEof(),
	ProcStop(),
	ProcCont(),
	ProcDStop(),
#  endif
	ProcSendData(),
	ProcNewline(),
	ProcList(),
	ProcBind(),
	Iprocess(),
#endif

#ifdef LISP
	GSexpr(),	/* Grind S Expression. */
	AddSpecial(),	/* add lisp special form */
#endif
	CAutoExec(),
	MAutoExec(),

	DefMAbbrev(),
	DefGAbbrev(),
	SaveAbbrevs(),
	RestAbbrevs(),
	EditAbbrevs(),
	BindMtoW(),

#ifdef CMT_FMT
	Comment(),
#endif

	ScrollLeft(),
	ScrollRight(),
	MacInter();		/* This is the last one. */


#ifdef MAC
#	define WIRED_CMD(c) c,'\0','\0'	/* for About Jove... */
#else
#	define WIRED_CMD(c)	c
#endif /* MAC */

#else /* TXT_TO_C */

#ifdef MAC
#	define WIRED_CMD(c) 0,'\0','\0'
#else
#	define WIRED_CMD(c)	0
#endif
#endif /* TXT_TO_C */

struct cmd	commands[] = {
#ifdef LISP
	FUNCTION, "add-lisp-special", WIRED_CMD(AddSpecial),
#endif
#ifdef ANSICODES
	FUNCTION, "ansi-codes", WIRED_CMD(AnsiCodes),
#endif
	FUNCTION, "append-region", WIRED_CMD(AppReg),
	FUNCTION, "apropos", WIRED_CMD(Apropos),
	FUNCTION, "auto-execute-command", WIRED_CMD(CAutoExec),
	FUNCTION, "auto-execute-macro", WIRED_CMD(MAutoExec),
	DefMinor(Fill), "auto-fill-mode", WIRED_CMD(0),
	DefMinor(Indent), "auto-indent-mode", WIRED_CMD(0),
	FUNCTION, "backward-character", WIRED_CMD(BackChar),
	FUNCTION, "backward-list", WIRED_CMD(BList),
	FUNCTION, "backward-paragraph", WIRED_CMD(BackPara),
	FUNCTION, "backward-s-expression", WIRED_CMD(BSexpr),
	FUNCTION, "backward-sentence", WIRED_CMD(Bos),
	FUNCTION, "backward-up-list", WIRED_CMD(BUpList),
	FUNCTION, "backward-word", WIRED_CMD(BackWord),
	FUNCTION, "begin-kbd-macro", WIRED_CMD(Remember),
	FUNCTION, "beginning-of-file", WIRED_CMD(Bof),
	FUNCTION, "beginning-of-line", WIRED_CMD(Bol),
	FUNCTION, "beginning-of-window", WIRED_CMD(Bow),
	FUNCTION, "bind-to-key", WIRED_CMD(BindAKey),
	FUNCTION, "bind-macro-to-key", WIRED_CMD(BindMac),
#ifdef ABBREV
	FUNCTION, "bind-macro-to-word-abbrev", WIRED_CMD(BindMtoW),
#endif
	FUNCTION, "buffer-position", WIRED_CMD(BufPos),
	DefMajor(CMODE), "c-mode", WIRED_CMD(0),
	FUNCTION, "case-character-capitalize", WIRED_CMD(CapChar),
	FUNCTION, "case-region-lower", WIRED_CMD(CasRegLower),
	FUNCTION, "case-region-upper", WIRED_CMD(CasRegUpper),
	FUNCTION, "case-word-capitalize", WIRED_CMD(CapWord),
	FUNCTION, "case-word-lower", WIRED_CMD(LowWord),
	FUNCTION, "case-word-upper", WIRED_CMD(UppWord),
	FUNCTION, "character-to-octal-insert", WIRED_CMD(ChrToOct),
#ifdef CHDIR
	FUNCTION, "cd", WIRED_CMD(Chdir),
#endif
	FUNCTION, "clear-and-redraw", WIRED_CMD(ClAndRedraw),
#ifndef MAC
	FUNCTION, "compile-it", WIRED_CMD(MakeErrors),
#endif
#ifdef IPROCS
#  ifndef PIPEPROCS
#    ifdef TIOCSLTC
	FUNCTION, "continue-process", WIRED_CMD(ProcCont),
#    endif
#  endif
#endif
	FUNCTION, "copy-region", WIRED_CMD(CopyRegion),
#ifndef MAC
	FUNCTION, "current-error", WIRED_CMD(ShowErr),
#endif
	FUNCTION, "date", WIRED_CMD(prCTIME),
#ifdef ABBREV
	FUNCTION, "define-global-word-abbrev", WIRED_CMD(DefGAbbrev),
	FUNCTION, "define-mode-word-abbrev", WIRED_CMD(DefMAbbrev),
#endif
	FUNCTION, "define-macro", WIRED_CMD(DefKBDMac),
	FUNCTION, "delete-blank-lines", WIRED_CMD(DelBlnkLines),
	FUNCTION, "delete-buffer", WIRED_CMD(BufKill),
	FUNCTION, "delete-macro", WIRED_CMD(DelMacro),
	FUNCTION, "delete-next-character", WIRED_CMD(DelNChar),
	FUNCTION, "delete-other-windows", WIRED_CMD(OneWindow),
	FUNCTION, "delete-previous-character", WIRED_CMD(DelPChar),
	FUNCTION, "delete-white-space", WIRED_CMD(DelWtSpace),
	FUNCTION, "delete-current-window", WIRED_CMD(DelCurWindow),
	FUNCTION, "describe-bindings", WIRED_CMD(DescBindings),
	FUNCTION, "describe-command", WIRED_CMD(DescCom),
	FUNCTION, "describe-key", WIRED_CMD(KeyDesc),
	FUNCTION, "describe-variable", WIRED_CMD(DescCom),
	FUNCTION, "digit", WIRED_CMD(Digit),
	FUNCTION, "digit-1", WIRED_CMD(Digit1),
	FUNCTION, "digit-2", WIRED_CMD(Digit2),
	FUNCTION, "digit-3", WIRED_CMD(Digit3),
	FUNCTION, "digit-4", WIRED_CMD(Digit4),
	FUNCTION, "digit-5", WIRED_CMD(Digit5),
	FUNCTION, "digit-6", WIRED_CMD(Digit6),
	FUNCTION, "digit-7", WIRED_CMD(Digit7),
	FUNCTION, "digit-8", WIRED_CMD(Digit8),
	FUNCTION, "digit-9", WIRED_CMD(Digit9),
	FUNCTION, "digit-0", WIRED_CMD(Digit0),
#ifdef CHDIR
	FUNCTION, "dirs", WIRED_CMD(prDIRS),
#endif
	FUNCTION, "down-list", WIRED_CMD(FDownList),
#ifdef IPROCS
#  ifndef PIPEPROCS
#    ifdef TIOCSLTC
	FUNCTION, "dstop-process", WIRED_CMD(ProcDStop),
#    endif
#  endif
#endif
#ifdef ABBREV
	FUNCTION, "edit-word-abbrevs", WIRED_CMD(EditAbbrevs),
#endif
	FUNCTION, "end-kbd-macro", WIRED_CMD(Forget),
	FUNCTION, "end-of-file", WIRED_CMD(Eof),
	FUNCTION, "end-of-line", WIRED_CMD(Eol),
	FUNCTION, "end-of-window", WIRED_CMD(Eow),
#ifdef IPROCS
#  ifndef PIPEPROCS
	FUNCTION, "eof-process", WIRED_CMD(ProcEof),
#  endif
#endif
	FUNCTION, "erase-buffer", WIRED_CMD(BufErase),
	FUNCTION, "exchange-point-and-mark", WIRED_CMD(PtToMark),
	FUNCTION, "execute-named-command", WIRED_CMD(Extend),
	FUNCTION, "execute-kbd-macro", WIRED_CMD(ExecMacro),
	FUNCTION, "execute-macro", WIRED_CMD(RunMacro),
	FUNCTION, "exit-jove", WIRED_CMD(Leave),
#ifdef CMT_FMT
 	FUNCTION, "fill-comment", WIRED_CMD(Comment),
#endif /* CMT_FMT */
	FUNCTION, "fill-paragraph", WIRED_CMD(Justify),
	FUNCTION, "fill-region", WIRED_CMD(RegJustify),
#ifndef MAC
	FUNCTION, "filter-region", WIRED_CMD(FilterRegion),
#endif
	FUNCTION, "find-file", WIRED_CMD(FindFile),
	FUNCTION, "find-tag", WIRED_CMD(FindTag),
	FUNCTION, "find-tag-at-point", WIRED_CMD(FDotTag),
	FUNCTION, "first-non-blank", WIRED_CMD(ToIndent),
	FUNCTION, "forward-character", WIRED_CMD(ForChar),
	FUNCTION, "forward-list", WIRED_CMD(FList),
	FUNCTION, "forward-paragraph", WIRED_CMD(ForPara),
	FUNCTION, "forward-s-expression", WIRED_CMD(FSexpr),
	FUNCTION, "forward-sentence", WIRED_CMD(Eos),
	FUNCTION, "forward-word", WIRED_CMD(ForWord),
	DefMajor(FUNDAMENTAL), "fundamental-mode", WIRED_CMD(0),
	FUNCTION, "gather-numeric-argument", WIRED_CMD(TimesFour),
#ifdef LISP
	FUNCTION, "grind-s-expr", WIRED_CMD(GSexpr),
#endif
	FUNCTION, "goto-line", WIRED_CMD(GoLine),
	FUNCTION, "goto-window-with-buffer", WIRED_CMD(GotoWind),
	FUNCTION, "grow-window", WIRED_CMD(GrowWindow),
	FUNCTION, "handle-tab", WIRED_CMD(Tab),
	FUNCTION, "i-search-forward", WIRED_CMD(IncFSearch),
	FUNCTION, "i-search-reverse", WIRED_CMD(IncRSearch),
	FUNCTION, "insert-file", WIRED_CMD(InsFile),
#ifdef IPROCS
	FUNCTION, "interrupt-process", WIRED_CMD(ProcInt),
	FUNCTION, "i-shell-command", WIRED_CMD(Iprocess),
#endif
	FUNCTION, "kill-next-word", WIRED_CMD(DelNWord),
	FUNCTION, "kill-previous-word", WIRED_CMD(DelPWord),
#ifdef IPROCS
	FUNCTION, "kill-process", WIRED_CMD(ProcKill),
#endif
	FUNCTION, "kill-region", WIRED_CMD(DelReg),
	FUNCTION, "kill-s-expression", WIRED_CMD(KillExpr),
	FUNCTION, "kill-some-buffers", WIRED_CMD(KillSome),
	FUNCTION, "kill-to-beginning-of-sentence", WIRED_CMD(KillBos),
	FUNCTION, "kill-to-end-of-line", WIRED_CMD(KillEOL),
	FUNCTION, "kill-to-end-of-sentence", WIRED_CMD(KillEos),
	FUNCTION, "left-margin-here", WIRED_CMD(SetLMargin),
#ifdef LISP
	DefMajor(LISPMODE), "lisp-mode", WIRED_CMD(0),
#endif
	FUNCTION, "list-buffers", WIRED_CMD(BufList),
#ifdef IPROCS
	FUNCTION, "list-processes", WIRED_CMD(ProcList),
#endif
	FUNCTION, "make-buffer-unmodified", WIRED_CMD(NotModified),
	FUNCTION, "make-macro-interactive", WIRED_CMD(MacInter),
	FUNCTION, "name-kbd-macro", WIRED_CMD(NameMac),
	FUNCTION, "newline", WIRED_CMD(Newline),
	FUNCTION, "newline-and-backup", WIRED_CMD(OpenLine),
	FUNCTION, "newline-and-indent", WIRED_CMD(LineAI),
#ifndef MAC
	FUNCTION, "next-error", WIRED_CMD(NextError),
#endif
	FUNCTION, "next-line", WIRED_CMD(NextLine),
	FUNCTION, "next-page", WIRED_CMD(NextPage),
	FUNCTION, "next-window", WIRED_CMD(NextWindow),
	FUNCTION, "number-lines-in-window", WIRED_CMD(WNumLines),
	DefMinor(OverWrite), "over-write-mode", WIRED_CMD(0),
	FUNCTION, "page-next-window", WIRED_CMD(PageNWind),
	FUNCTION, "paren-flash", WIRED_CMD(DoParen),
#ifndef MAC
	FUNCTION, "parse-errors", WIRED_CMD(ErrParse),
#endif
#ifdef SPELL
	FUNCTION, "parse-spelling-errors-in-buffer", WIRED_CMD(SpelWords),
#endif
#ifdef JOB_CONTROL
	FUNCTION, "pause-jove", WIRED_CMD(PauseJove),
#else
#	ifndef MAC
	FUNCTION, "pause-jove", WIRED_CMD(Push),
#	endif
#endif
	FUNCTION, "pop-mark", WIRED_CMD(PopMark),
#ifdef CHDIR
	FUNCTION, "popd", WIRED_CMD(Popd),
#endif
	FUNCTION, "prefix-1", WIRED_CMD(EscPrefix),
	FUNCTION, "prefix-2", WIRED_CMD(CtlxPrefix),
	FUNCTION, "prefix-3", WIRED_CMD(MiscPrefix),
#ifndef MAC
	FUNCTION, "previous-error", WIRED_CMD(PrevError),
#endif
	FUNCTION, "previous-line", WIRED_CMD(PrevLine),
	FUNCTION, "previous-page", WIRED_CMD(PrevPage),
	FUNCTION, "previous-window", WIRED_CMD(PrevWindow),
	FUNCTION, "print", WIRED_CMD(PrVar),
#ifdef IPROCS
	FUNCTION, "process-bind-to-key", WIRED_CMD(ProcBind),
	FUNCTION, "process-newline", WIRED_CMD(ProcNewline),
	FUNCTION, "process-send-data-no-return", WIRED_CMD(ProcSendData),
#endif
#ifndef MAC
	FUNCTION, "push-shell", WIRED_CMD(Push),
#endif
#ifdef CHDIR
	FUNCTION, "pushd", WIRED_CMD(Pushd),
	FUNCTION, "pwd", WIRED_CMD(prCWD),
#endif
	FUNCTION, "query-replace-string", WIRED_CMD(QRepSearch),
#ifdef IPROCS
	FUNCTION, "quit-process", WIRED_CMD(ProcQuit),
#endif
	FUNCTION, "quoted-insert", WIRED_CMD(QuotChar),
#ifdef ABBREV
	FUNCTION, "read-word-abbrev-file", WIRED_CMD(RestAbbrevs),
#endif
	FUNCTION, "redraw-display", WIRED_CMD(RedrawDisplay),
	FUNCTION, "recursive-edit", WIRED_CMD(Recur),
	FUNCTION, "rename-buffer", WIRED_CMD(ReNamBuf),
	FUNCTION, "replace-in-region", WIRED_CMD(RegReplace),
	FUNCTION, "replace-string", WIRED_CMD(RepSearch),
	FUNCTION, "right-margin-here", WIRED_CMD(SetRMargin),
	FUNCTION, "save-file", WIRED_CMD(SaveFile),
	FUNCTION, "scroll-down", WIRED_CMD(DownScroll),
	FUNCTION, "scroll-left", WIRED_CMD(ScrollLeft),
#ifdef MSDOS
	FUNCTION, "scroll-next-page", WIRED_CMD(PageScrollUp),
	FUNCTION, "scroll-previous-page", WIRED_CMD(PageScrollDown),
#endif /* MSDOS */
	FUNCTION, "scroll-right", WIRED_CMD(ScrollRight),
	FUNCTION, "scroll-up", WIRED_CMD(UpScroll),
	FUNCTION, "search-forward", WIRED_CMD(ForSearch),
	FUNCTION, "search-forward-nd", WIRED_CMD(FSrchND),
	FUNCTION, "search-reverse", WIRED_CMD(RevSearch),
	FUNCTION, "search-reverse-nd", WIRED_CMD(RSrchND),
	FUNCTION, "select-buffer", WIRED_CMD(BufSelect),
#ifdef MSDOS
	FUNCTION, "select-buffer-1", WIRED_CMD(Buf1Select),
	FUNCTION, "select-buffer-2", WIRED_CMD(Buf2Select),
	FUNCTION, "select-buffer-3", WIRED_CMD(Buf3Select),
	FUNCTION, "select-buffer-4", WIRED_CMD(Buf4Select),
	FUNCTION, "select-buffer-5", WIRED_CMD(Buf5Select),
	FUNCTION, "select-buffer-6", WIRED_CMD(Buf6Select),
	FUNCTION, "select-buffer-7", WIRED_CMD(Buf7Select),
	FUNCTION, "select-buffer-8", WIRED_CMD(Buf8Select),
	FUNCTION, "select-buffer-9", WIRED_CMD(Buf9Select),
	FUNCTION, "select-buffer-10", WIRED_CMD(Buf10Select),
#endif /* MSDOS */
	FUNCTION, "self-insert", WIRED_CMD(SelfInsert),
	FUNCTION, "set", WIRED_CMD(SetVar),
	FUNCTION, "set-mark", WIRED_CMD(SetMark),
#ifdef IPROCS	/* for GNU compatibility */
	FUNCTION, "shell", WIRED_CMD(ShellProc),
#endif
#ifndef MAC
	FUNCTION, "shell-command", WIRED_CMD(ShellCom),
	FUNCTION, "shell-command-no-buffer", WIRED_CMD(ShNoBuf),
	FUNCTION, "shell-command-to-buffer", WIRED_CMD(ShToBuf),
	FUNCTION, "shell-command-with-typeout", WIRED_CMD(Shtypeout),
#endif
	DefMinor(ShowMatch), "show-match-mode", WIRED_CMD(0),
	FUNCTION, "shrink-window", WIRED_CMD(ShrWindow),
	FUNCTION, "source", WIRED_CMD(Source),
#ifdef SPELL
	FUNCTION, "spell-buffer", WIRED_CMD(SpelBuffer),
#endif
	FUNCTION, "split-current-window", WIRED_CMD(SplitWind),
	FUNCTION, "start-remembering", WIRED_CMD(Remember),
#ifdef IPROCS
#  ifndef PIPEPROCS
	FUNCTION, "stop-process", WIRED_CMD(ProcStop),
#  endif
#endif
	FUNCTION, "stop-remembering", WIRED_CMD(Forget),
	FUNCTION, "string-length", WIRED_CMD(StrLength),
#ifdef JOB_CONTROL
	FUNCTION, "suspend-jove", WIRED_CMD(PauseJove),
#endif
	DefMajor(TEXT), "text-mode", WIRED_CMD(0),
	FUNCTION, "transpose-characters", WIRED_CMD(TransChar),
	FUNCTION, "transpose-lines", WIRED_CMD(TransLines),
	FUNCTION, "unbind-key", WIRED_CMD(UnbindC),
	FUNCTION, "version", WIRED_CMD(ShowVersion),
	FUNCTION, "visible-spaces-in-window", WIRED_CMD(WVisSpace),
	FUNCTION, "visit-file", WIRED_CMD(ReadFile),
	FUNCTION, "window-find", WIRED_CMD(WindFind),
#ifdef ABBREV
	DefMinor(Abbrev), "word-abbrev-mode", WIRED_CMD(0),
	FUNCTION, "write-word-abbrev-file", WIRED_CMD(SaveAbbrevs),
#endif
	FUNCTION, "write-file", WIRED_CMD(WriteFile),
	FUNCTION, "write-macros-to-file", WIRED_CMD(WriteMacs),
	FUNCTION, "write-modified-files", WIRED_CMD(WtModBuf),
	FUNCTION, "write-region", WIRED_CMD(WrtReg),
	FUNCTION, "yank", WIRED_CMD(Yank),
	FUNCTION, "yank-pop", WIRED_CMD(YankPop),
	FUNCTION, 0, 0
};

#ifndef TXT_TO_C
data_obj *
findcom(prompt)
char	*prompt;
{
	/* This is for faster startup.  This just reads until a space or a
	   tab or a newline character is reached, and then does a
	   semi-hashed lookup on that string.  This should be much faster
	   than initializing the minibuffer for each line. */
	if (InJoverc) {
		char	cmdbuf[128];
		register struct cmd	*cmd;
		register char	*cp = cmdbuf;
#if !(defined(IBMPC) || defined(MAC))
		register int	c;
#else
		int c;
#endif		
		struct cmd	*which;
		int	cmdlen,
			found = 0;
		static struct cmd	*cmdhash[26];
		static int	beenhere = NO;

/* special case for prefix commands--only upper case ones */
#define hash(c)	(c - 'a')

		/* initialize the hash table */
		if (beenhere == NO) {
			int	lastc = 0;

			for (cmd = commands; cmd->Name != 0; cmd++)
				if (lastc != cmd->Name[0]) {
					lastc = cmd->Name[0];
					cmdhash[hash(lastc)] = cmd;
				}
			beenhere = YES;
		}
#ifdef MAC
		menus_off();	/* Block menu choices during input */
#endif
		/* gather the cmd name */
		while (((c = getch()) != EOF) && !index(" \t\r\n", c)) {
#if (defined(IBMPC) || defined(MAC))
			lower(&c);
#else			
			if (isupper(c))
				c = tolower(c);
#endif
			*cp++ = c;
		}
		if (c == EOF)
			return 0;
		*cp = '\0';
		cmdlen = cp - cmdbuf;
		if (cmdlen == 0)
			return 0;

		/* look it up (in the reduced search space) */
		if (islower(cmdbuf[0]))
		    for (cmd = cmdhash[hash(cmdbuf[0])]; cmd != 0 && cmd->Name[0] == cmdbuf[0]; cmd++) {
			if (strncmp(cmd->Name, cmdbuf, cmdlen) == 0) {
				if (strcmp(cmd->Name, cmdbuf) == 0)
					return (data_obj *) cmd;
				found += 1;
				which = cmd;
			}
		    }
		if (found > 1)
			complain("[\"%s\" ambiguous]", cmdbuf);
		else if (found == 0)
			complain("[\"%s\" unknown]", cmdbuf);
		else
			return (data_obj *) which;
	} else {
		static char	*strings[(sizeof commands) / sizeof (commands[0])];
		static int	beenhere = NO;
		register int	com;

		if (beenhere == NO) {
			register char	**strs = strings;
			register struct cmd	*c;

			for (c = commands; c->Name != 0; c++)
				*strs++ = c->Name;
			*strs = 0;
			beenhere = YES;
		}

		if ((com = complete(strings, prompt, CASEIND)) < 0)
			return 0;
		return (data_obj *) &commands[com];
	}
	/* NOTREACHED */
}
#endif
