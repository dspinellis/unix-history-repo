/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "ctype.h"

#if !defined(MSDOS) && !defined(SYSV) && !defined(MAC)
# include <sgtty.h>
#endif

#if !defined(TXT_TO_C)

#if defined(MAC)
#	define WIRED_CMD(c) (c),'\0','\0'	/* for About Jove... */
#else
#	define WIRED_CMD(c)	(c)
#endif /* MAC */

#else /* TXT_TO_C */

#if defined(MAC)
#	define WIRED_CMD(c) 0,'\0','\0'
#else
#	define WIRED_CMD(c)	0
#endif
#endif /* TXT_TO_C */

const struct cmd	commands[] = {
#if defined(LISP)
	FUNCTION, "add-lisp-special", WIRED_CMD(AddSpecial),
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
	FUNCTION, "bind-keymap-to-key", WIRED_CMD(KmBind),
	FUNCTION, "bind-macro-to-key", WIRED_CMD(BindMac),
#if defined(ABBREV)
	FUNCTION, "bind-macro-to-word-abbrev", WIRED_CMD(BindMtoW),
#endif
	FUNCTION, "buffer-position", WIRED_CMD(BufPos),
	DefMajor(CMODE), "c-mode", WIRED_CMD(0),
	MODFUNC, "case-character-capitalize", WIRED_CMD(CapChar),
	MODFUNC, "case-region-lower", WIRED_CMD(CasRegLower),
	MODFUNC, "case-region-upper", WIRED_CMD(CasRegUpper),
	MODFUNC, "case-word-capitalize", WIRED_CMD(CapWord),
	MODFUNC, "case-word-lower", WIRED_CMD(LowWord),
	MODFUNC, "case-word-upper", WIRED_CMD(UppWord),
	MODFUNC, "character-to-octal-insert", WIRED_CMD(ChrToOct),
	FUNCTION, "cd", WIRED_CMD(Chdir),
	FUNCTION, "clear-and-redraw", WIRED_CMD(ClAndRedraw),
#if !defined(MAC)
	FUNCTION, "compile-it", WIRED_CMD(MakeErrors),
#endif
#if defined(IPROCS) && !defined(PIPEPROCS) && defined(TIOCSLTC)
	FUNCTION, "continue-process", WIRED_CMD(ProcCont),
#endif
	FUNCTION, "copy-region", WIRED_CMD(CopyRegion),
#if !defined(MAC)
	FUNCTION, "current-error", WIRED_CMD(ShowErr),
#endif
	FUNCTION, "date", WIRED_CMD(prCTIME),
#if defined(IPROCS)
	FUNCTION, "process-dbx-output", WIRED_CMD(DBXpoutput),
#endif
#if defined(ABBREV)
	FUNCTION, "define-global-word-abbrev", WIRED_CMD(DefGAbbrev),
	FUNCTION, "define-mode-word-abbrev", WIRED_CMD(DefMAbbrev),
#endif
	FUNCTION, "define-macro", WIRED_CMD(DefKBDMac),
	MODFUNC, "delete-blank-lines", WIRED_CMD(DelBlnkLines),
	FUNCTION, "delete-buffer", WIRED_CMD(BufKill),
	FUNCTION, "delete-macro", WIRED_CMD(DelMacro),
	MODFUNC, "delete-next-character", WIRED_CMD(DelNChar),
	FUNCTION, "delete-other-windows", WIRED_CMD(OneWindow),
	MODFUNC, "delete-previous-character", WIRED_CMD(DelPChar),
	MODFUNC, "delete-white-space", WIRED_CMD(DelWtSpace),
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
	FUNCTION, "dirs", WIRED_CMD(prDIRS),
	FUNCTION, "down-list", WIRED_CMD(FDownList),
#if defined(IPROCS) && !defined(PIPEPROCS) && defined(TIOCSLTC)
	FUNCTION, "dstop-process", WIRED_CMD(ProcDStop),
#endif
#if defined(ABBREV)
	FUNCTION, "edit-word-abbrevs", WIRED_CMD(EditAbbrevs),
#endif
	FUNCTION, "end-kbd-macro", WIRED_CMD(Forget),
	FUNCTION, "end-of-file", WIRED_CMD(Eof),
	FUNCTION, "end-of-line", WIRED_CMD(Eol),
	FUNCTION, "end-of-window", WIRED_CMD(Eow),
#if defined(IPROCS) && !defined(PIPEPROCS)
	FUNCTION, "eof-process", WIRED_CMD(ProcEof),
#endif
	FUNCTION, "erase-buffer", WIRED_CMD(BufErase),
	FUNCTION, "exchange-point-and-mark", WIRED_CMD(PtToMark),
	FUNCTION, "execute-named-command", WIRED_CMD(Extend),
	FUNCTION, "execute-kbd-macro", WIRED_CMD(ExecMacro),
	FUNCTION, "execute-macro", WIRED_CMD(RunMacro),
	FUNCTION, "exit-jove", WIRED_CMD(Leave),
#if defined(CMT_FMT)
	MODFUNC, "fill-comment", WIRED_CMD(Comment),
#endif /* CMT_FMT */
	MODFUNC, "fill-paragraph", WIRED_CMD(Justify),
	MODFUNC, "fill-region", WIRED_CMD(RegJustify),
#if !defined(MAC)
	MODFUNC, "filter-region", WIRED_CMD(FilterRegion),
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
#if defined(LISP)
	MODFUNC, "grind-s-expr", WIRED_CMD(GSexpr),
#endif
	FUNCTION, "goto-line", WIRED_CMD(GoLine),
	FUNCTION, "goto-window-with-buffer", WIRED_CMD(GotoWind),
	FUNCTION, "grow-window", WIRED_CMD(GrowWindow),
	MODFUNC, "handle-tab", WIRED_CMD(Tab),
	FUNCTION, "i-search-forward", WIRED_CMD(IncFSearch),
	FUNCTION, "i-search-reverse", WIRED_CMD(IncRSearch),
	MODFUNC, "insert-file", WIRED_CMD(InsFile),
#if defined(IPROCS)
	FUNCTION, "interrupt-process", WIRED_CMD(ProcInt),
	FUNCTION, "i-shell-command", WIRED_CMD(Iprocess),
#endif
	MODFUNC, "kill-next-word", WIRED_CMD(DelNWord),
	MODFUNC, "kill-previous-word", WIRED_CMD(DelPWord),
#if defined(IPROCS)
	FUNCTION, "kill-process", WIRED_CMD(ProcKill),
#endif
	MODFUNC, "kill-region", WIRED_CMD(DelReg),
	MODFUNC, "kill-s-expression", WIRED_CMD(KillExpr),
	FUNCTION, "kill-some-buffers", WIRED_CMD(KillSome),
	MODFUNC, "kill-to-beginning-of-sentence", WIRED_CMD(KillBos),
	MODFUNC, "kill-to-end-of-line", WIRED_CMD(KillEOL),
	MODFUNC, "kill-to-end-of-sentence", WIRED_CMD(KillEos),
	FUNCTION, "left-margin-here", WIRED_CMD(SetLMargin),
#if defined(LISP)
	DefMajor(LISPMODE), "lisp-mode", WIRED_CMD(0),
#endif
	FUNCTION, "list-buffers", WIRED_CMD(BufList),
#if defined(IPROCS)
	FUNCTION, "list-processes", WIRED_CMD(ProcList),
#endif
	FUNCTION, "make-buffer-unmodified", WIRED_CMD(NotModified),
	FUNCTION, "make-keymap", WIRED_CMD(MakeKMap),
	FUNCTION, "make-macro-interactive", WIRED_CMD(MacInter),
	FUNCTION, "name-kbd-macro", WIRED_CMD(NameMac),
	MODFUNC, "newline", WIRED_CMD(Newline),
	MODFUNC, "newline-and-backup", WIRED_CMD(OpenLine),
	MODFUNC, "newline-and-indent", WIRED_CMD(LineAI),
#if !defined(MAC)
	FUNCTION, "next-error", WIRED_CMD(NextError),
#endif
	FUNCTION, "next-line", WIRED_CMD(NextLine),
	FUNCTION, "next-page", WIRED_CMD(NextPage),
	FUNCTION, "next-window", WIRED_CMD(NextWindow),
	FUNCTION, "number-lines-in-window", WIRED_CMD(WNumLines),
	DefMinor(OverWrite), "over-write-mode", WIRED_CMD(0),
	FUNCTION, "page-next-window", WIRED_CMD(PageNWind),
	MODFUNC, "paren-flash", WIRED_CMD(DoParen),
#if !defined(MAC)
	FUNCTION, "parse-errors", WIRED_CMD(ErrParse),
#endif
#if defined(SPELL)
	FUNCTION, "parse-spelling-errors-in-buffer", WIRED_CMD(SpelWords),
#endif
#if defined(JOB_CONTROL)
	FUNCTION, "pause-jove", WIRED_CMD(PauseJove),
#else
#	ifndef MAC
	FUNCTION, "pause-jove", WIRED_CMD(Push),
#	endif
#endif
	FUNCTION, "pop-mark", WIRED_CMD(PopMark),
	FUNCTION, "popd", WIRED_CMD(Popd),
#if !defined(MAC)
	FUNCTION, "previous-error", WIRED_CMD(PrevError),
#endif
	FUNCTION, "previous-line", WIRED_CMD(PrevLine),
	FUNCTION, "previous-page", WIRED_CMD(PrevPage),
	FUNCTION, "previous-window", WIRED_CMD(PrevWindow),
	FUNCTION, "print", WIRED_CMD(PrVar),
#if defined(IPROCS)
	FUNCTION, "process-bind-keymap-to-key", WIRED_CMD(ProcKmBind),
	FUNCTION, "process-bind-to-key", WIRED_CMD(ProcBind),
	MODFUNC, "process-newline", WIRED_CMD(ProcNewline),
	FUNCTION, "process-send-data-no-return", WIRED_CMD(ProcSendData),
#endif
#if !defined(MAC)
	FUNCTION, "push-shell", WIRED_CMD(Push),
#endif
	FUNCTION, "pushd", WIRED_CMD(Pushd),
	FUNCTION, "pwd", WIRED_CMD(prCWD),
	MODFUNC, "query-replace-string", WIRED_CMD(QRepSearch),
#if defined(IPROCS)
	FUNCTION, "quit-process", WIRED_CMD(ProcQuit),
#endif
	MODFUNC, "quoted-insert", WIRED_CMD(QuotChar),
	DefMinor(ReadOnly), "read-only-mode", WIRED_CMD(0),
#if defined(ABBREV)
	FUNCTION, "read-word-abbrev-file", WIRED_CMD(RestAbbrevs),
#endif
	FUNCTION, "redraw-display", WIRED_CMD(RedrawDisplay),
	FUNCTION, "recursive-edit", WIRED_CMD(Recur),
	FUNCTION, "rename-buffer", WIRED_CMD(ReNamBuf),
	MODFUNC, "replace-in-region", WIRED_CMD(RegReplace),
	MODFUNC, "replace-string", WIRED_CMD(RepSearch),
	FUNCTION, "right-margin-here", WIRED_CMD(SetRMargin),
	FUNCTION, "save-file", WIRED_CMD(SaveFile),
	FUNCTION, "scroll-down", WIRED_CMD(DownScroll),
	FUNCTION, "scroll-left", WIRED_CMD(ScrollLeft),
#if defined(MSDOS)
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
#if defined(MSDOS)
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
	MODFUNC, "self-insert", WIRED_CMD(SelfInsert),
	FUNCTION, "set", WIRED_CMD(SetVar),
	FUNCTION, "set-mark", WIRED_CMD(SetMark),
#if defined(IPROCS)	/* for GNU compatibility */
	FUNCTION, "shell", WIRED_CMD(ShellProc),
#endif
#if !defined(MAC)
	FUNCTION, "shell-command", WIRED_CMD(ShellCom),
	FUNCTION, "shell-command-no-buffer", WIRED_CMD(ShNoBuf),
	FUNCTION, "shell-command-to-buffer", WIRED_CMD(ShToBuf),
	FUNCTION, "shell-command-with-typeout", WIRED_CMD(Shtypeout),
#endif
	MODFUNC, "shift-region-left", WIRED_CMD(LRShift),
	MODFUNC, "shift-region-right", WIRED_CMD(RRShift),
	DefMinor(ShowMatch), "show-match-mode", WIRED_CMD(0),
	FUNCTION, "shrink-window", WIRED_CMD(ShrWindow),
	FUNCTION, "source", WIRED_CMD(Source),
#if defined(SPELL)
	FUNCTION, "spell-buffer", WIRED_CMD(SpelBuffer),
#endif
	FUNCTION, "split-current-window", WIRED_CMD(SplitWind),
	FUNCTION, "start-remembering", WIRED_CMD(Remember),
#if defined(IPROCS) && !defined(PIPEPROCS)
	FUNCTION, "stop-process", WIRED_CMD(ProcStop),
#endif
	FUNCTION, "stop-remembering", WIRED_CMD(Forget),
	FUNCTION, "string-length", WIRED_CMD(StrLength),
#if defined(JOB_CONTROL)
	FUNCTION, "suspend-jove", WIRED_CMD(PauseJove),
#endif
	DefMajor(TEXT), "text-mode", WIRED_CMD(0),
	MODFUNC, "transpose-characters", WIRED_CMD(TransChar),
	MODFUNC, "transpose-lines", WIRED_CMD(TransLines),
	FUNCTION, "unbind-key", WIRED_CMD(UnbindC),
	FUNCTION, "version", WIRED_CMD(ShowVersion),
	FUNCTION, "visible-spaces-in-window", WIRED_CMD(WVisSpace),
	FUNCTION, "visit-file", WIRED_CMD(ReadFile),
	FUNCTION, "window-find", WIRED_CMD(WindFind),
#if defined(ABBREV)
	DefMinor(Abbrev), "word-abbrev-mode", WIRED_CMD(0),
	FUNCTION, "write-word-abbrev-file", WIRED_CMD(SaveAbbrevs),
#endif
	FUNCTION, "write-file", WIRED_CMD(WriteFile),
	FUNCTION, "write-macros-to-file", WIRED_CMD(WriteMacs),
	FUNCTION, "write-modified-files", WIRED_CMD(WtModBuf),
	FUNCTION, "write-region", WIRED_CMD(WrtReg),
	MODFUNC, "yank", WIRED_CMD(Yank),
	MODFUNC, "yank-pop", WIRED_CMD(YankPop),
	FUNCTION, 0, 0
};

#if !defined(TXT_TO_C)
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
		register const struct cmd	*cmd;
		register char	*cp = cmdbuf;
#if !(defined(IBMPC) || defined(MAC))
		register int	c;
#else
		int c;
#endif
		const struct cmd	*which;
		size_t	cmdlen;
		int	found = 0;
		static const struct cmd	*cmdhash[26];
		static int	beenhere = NO;

/* special case for prefix commands--only upper case ones */
#define hash(c)	((c) - 'a')

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
		while (((c = getch()) != EOF) && !strchr(" \t\r\n", c)) {
#if (defined(IBMPC) || defined(MAC))
			lower((char *) &c);
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
		if (found > 1) {
			complain("[\"%s\" ambiguous]", cmdbuf);
			/* NOTREACHED */
		} else if (found == 0) {
			complain("[\"%s\" unknown]", cmdbuf);
			/* NOTREACHED */
		}else
			return (data_obj *) which;
	} else {
		static char	*strings[(sizeof commands) / sizeof (commands[0])];
		static int	beenhere = NO;
		register int	com;

		if (beenhere == NO) {
			register char	**strs = strings;
			register const struct cmd	*c;

			for (c = commands; c->Name != 0; c++)
				*strs++ = c->Name;
			*strs = 0;
			beenhere = YES;
		}

		if ((com = complete(strings, prompt, CASEIND)) < 0)
			return 0;
		return (data_obj *) &commands[com];
	}
#undef	hash
}
#endif
