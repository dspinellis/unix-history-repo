/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#ifndef	TXT_TO_C	/* so that jove.h isn't included twice in setmaps */
#include "jove.h"
#endif

#ifndef IBMPC
const
#endif
struct variable	variables[] = {
	VARIABLE, "abort-char", (UnivPtr) &AbortChar, V_CHAR,
#ifdef	UNIX
	VARIABLE, "allow-^S-and-^Q", (UnivPtr) &OKXonXoff, V_BOOL|V_TTY_RESET,
#endif	/* UNIX */
	VARIABLE, "allow-bad-filenames", (UnivPtr) &OkayBadChars, V_BOOL,
#ifdef	ABBREV
	VARIABLE, "auto-case-abbrev", (UnivPtr) &AutoCaseAbbrev, V_BOOL,
#endif
#ifdef	IBMPC
	VARIABLE, "background-color", (UnivPtr) &Bgcolor, V_BASE10|V_CLRSCREEN,
#endif	/* IBMPC */
#ifdef	F_COMPLETION
	VARIABLE, "bad-filename-extensions", (UnivPtr)BadExtensions, V_STRING,
#endif
	VARIABLE, "c-argument-indentation", (UnivPtr) &CArgIndent, V_BASE10,
	VARIABLE, "c-indentation-increment", (UnivPtr) &CIndIncrmt, V_BASE10,
	VARIABLE, "case-ignore-search", (UnivPtr) &CaseIgnore, V_BOOL,
#ifdef	CMT_FMT
	VARIABLE, "comment-format", (UnivPtr)CmtFmt, V_STRING,
#endif
#ifdef	IPROCS
	VARIABLE, "dbx-format-string", (UnivPtr)dbx_parse_fmt, V_STRING,
#endif
#ifdef	BIFF
	VARIABLE, "disable-biff", (UnivPtr) &BiffChk, V_BOOL,
#endif
#ifdef	F_COMPLETION
	VARIABLE, "display-bad-filenames", (UnivPtr) &DispBadFs, V_BOOL,
#endif
#ifndef	MAC
	VARIABLE, "error-format-string", (UnivPtr)ErrFmtStr, V_STRING,
	VARIABLE, "error-window-size", (UnivPtr) &EWSize, V_BASE10,
#endif
	VARIABLE, "expand-environment-variables", (UnivPtr) &DoEVexpand, V_BOOL,
	VARIABLE, "file-creation-mode", (UnivPtr) &CreatMode, V_BASE8,
	VARIABLE, "files-should-end-with-newline", (UnivPtr) &EndWNewline, V_BOOL,
#ifdef	IBMPC
	VARIABLE, "foreground-color", (UnivPtr) &Fgcolor, V_BASE10|V_CLRSCREEN,
#endif	/* IBMPC */
	VARIABLE, "internal-tabstop", (UnivPtr) &tabstop, V_BASE10|V_CLRSCREEN,
	VARIABLE, "interrupt-character", (UnivPtr) &IntChar, V_CHAR|V_TTY_RESET,
	VARIABLE, "left-margin", (UnivPtr) &LMargin, V_BASE10,
#ifdef	MAC
	VARIABLE, "macify", (UnivPtr) &Macmode, V_BOOL,
#endif
#ifdef	UNIX
	VARIABLE, "mail-check-frequency", (UnivPtr) &MailInt, V_BASE10,
	VARIABLE, "mailbox", (UnivPtr)Mailbox, V_FILENAME,
#endif	/* UNIX */
#ifdef	BACKUPFILES
	VARIABLE, "make-backup-files", (UnivPtr) &BkupOnWrite, V_BOOL,
#endif
	VARIABLE, "mark-threshold", (UnivPtr) &MarkThresh, V_BASE10,
	VARIABLE, "marks-should-float", (UnivPtr) &MarksShouldFloat, V_BOOL,
	VARIABLE, "match-regular-expressions", (UnivPtr) &UseRE, V_BOOL,
	VARIABLE, "meta-key", (UnivPtr) &MetaKey, V_BOOL|V_TTY_RESET,
	VARIABLE, "mode-line", (UnivPtr)ModeFmt, V_STRING|V_MODELINE,
#ifdef	IBMPC
	VARIABLE, "mode-line-color", (UnivPtr) &Mdcolor, V_BASE10|V_MODELINE,
#endif
	VARIABLE, "mode-line-should-standout", (UnivPtr) &BriteMode, V_BOOL|V_MODELINE,
	VARIABLE, "paren-flash-delay", (UnivPtr) &PDelay, V_BASE10,
#ifndef	MAC
	VARIABLE, "physical-tabstop", (UnivPtr) &phystab, V_BASE10|V_CLRSCREEN,
#endif
#ifdef	IPROCS
	VARIABLE, "process-prompt", (UnivPtr)proc_prompt, V_STRING,
#endif
	VARIABLE, "right-margin", (UnivPtr) &RMargin, V_BASE10,
	VARIABLE, "scroll-all-lines", (UnivPtr) &ScrollAll, V_BOOL,
	VARIABLE, "scroll-step", (UnivPtr) &ScrollStep, V_BASE10,
	VARIABLE, "search-exit-char", (UnivPtr) &SExitChar, V_CHAR,
	VARIABLE, "send-typeout-to-buffer", (UnivPtr) &UseBuffers, V_BOOL,
#ifndef	MAC
	VARIABLE, "shell", (UnivPtr)Shell, V_FILENAME,
	VARIABLE, "shell-flags", (UnivPtr)ShFlags, V_STRING,
#endif
#ifndef	MSDOS
	VARIABLE, "sync-frequency", (UnivPtr) &SyncFreq, V_BASE10,
#endif	/* MSDOS */
	VARIABLE, "tag-file", (UnivPtr)TagFile, V_FILENAME,
#ifndef	MAC
	VARIABLE, "tmp-file-pathname", (UnivPtr)TmpFilePath, V_FILENAME,
#endif
#ifdef	UNIX
	VARIABLE, "update-time-frequency", (UnivPtr) &UpdFreq, V_BASE10,
#endif	/* UNIX */
#ifdef	ID_CHAR
	VARIABLE, "use-i/d-char", (UnivPtr) &UseIC, V_BOOL,
#endif
	VARIABLE, "visible-bell", (UnivPtr) &VisBell, V_BOOL,
	VARIABLE, "wrap-search", (UnivPtr) &WrapScan, V_BOOL,
#ifndef	MAC
	VARIABLE, "write-files-on-make", (UnivPtr) &WtOnMk, V_BOOL,
#endif
	VARIABLE, NULL, NULL, 0
};

#ifndef	TXT_TO_C
data_obj *
findvar(prompt)
const char	*prompt;
{
	static char	*strings[(sizeof variables) / sizeof (struct variable)];
	static bool	beenhere = FALSE;
	register int	com;

	if (!beenhere) {
		register char	**strs = strings;
		register const struct variable	*v = variables;

		beenhere = TRUE;
		for (; v->Name; v++)
			*strs++ = v->Name;
		*strs = NULL;
	}

	if ((com = complete(strings, prompt, NOTHING)) < 0)
		return NULL;
	return (data_obj *) &variables[com];
}
#endif
