/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#ifndef TXT_TO_C	/* so that jove.h isn't included twice in setmaps */
#include "jove.h"
#endif

struct variable	variables[] = {
	VARIABLE, "abort-char", &AbortChar, V_CHAR,
#ifdef UNIX
	VARIABLE, "allow-^S-and-^Q", &OKXonXoff, V_BOOL|V_TTY_RESET,
#endif /* UNIX */
	VARIABLE, "allow-bad-filenames", &OkayBadChars, V_BOOL,
#ifdef ABBREV
	VARIABLE, "auto-case-abbrev", &AutoCaseAbbrev, V_BOOL,
#endif
#ifdef IBMPC
	VARIABLE, "background-color", &Bgcolor, V_BASE10|V_CLRSCREEN,
#endif /* IBMPC */
#ifdef F_COMPLETION
	VARIABLE, "bad-filename-extensions", (int *) BadExtensions, V_STRING,
#endif
	VARIABLE, "c-indentation-increment", &CIndIncrmt, V_BASE10,
	VARIABLE, "case-ignore-search", &CaseIgnore, V_BOOL,
#ifdef CMT_FMT
 	VARIABLE, "comment-format", (int *) CmtFmt, V_STRING,
#endif
#ifdef BIFF
	VARIABLE, "disable-biff", &BiffChk, V_BOOL,
#endif
#ifdef F_COMPLETION
	VARIABLE, "display-bad-filenames", &DispBadFs, V_BOOL,
#endif
#ifndef MAC
	VARIABLE, "error-format-string", (int *) ErrFmtStr, V_STRING,
	VARIABLE, "error-window-size", &EWSize, V_BASE10,
#endif
	VARIABLE, "expand-environment-variables", &DoEVexpand, V_BOOL,
	VARIABLE, "file-creation-mode", &CreatMode, V_BASE8,
	VARIABLE, "files-should-end-with-newline", &EndWNewline, V_BOOL,
#ifdef IBMPC
	VARIABLE, "foreground-color", &Fgcolor, V_BASE10|V_CLRSCREEN,
#endif /* IBMPC */
	VARIABLE, "internal-tabstop", &tabstop, V_BASE10|V_CLRSCREEN,
	VARIABLE, "left-margin", &LMargin, V_BASE10,
#ifdef UNIX
	VARIABLE, "mailbox", (int *) Mailbox, V_FILENAME,
	VARIABLE, "mail-check-frequency", (int *) &MailInt, V_BASE10,
#endif /* UNIX */
#ifdef MAC
	VARIABLE, "macify", &Macmode, V_BOOL,
#endif
#ifdef BACKUPFILES
	VARIABLE, "make-backup-files", &BkupOnWrite, V_BOOL,
#endif
	VARIABLE, "mark-threshold", &MarkThresh, V_BASE10,
	VARIABLE, "marks-should-float", &MarksShouldFloat, V_BOOL,
	VARIABLE, "match-regular-expressions", &UseRE, V_BOOL,
	VARIABLE, "meta-key", &MetaKey, V_BOOL|V_TTY_RESET,
	VARIABLE, "mode-line", (int *) ModeFmt, V_STRING|V_MODELINE,
#ifdef IBMPC
	VARIABLE, "mode-line-color", &Mdcolor, V_BASE10|V_MODELINE,
#endif	
	VARIABLE, "mode-line-should-standout", &BriteMode, V_BOOL|V_MODELINE,
	VARIABLE, "paren-flash-delay", &PDelay, V_BASE10,
#ifndef MAC
	VARIABLE, "physical-tabstop", &phystab, V_BASE10|V_CLRSCREEN,
#endif
#ifdef IPROCS
	VARIABLE, "process-prompt", (int *) proc_prompt, V_STRING,
#endif
	VARIABLE, "interrupt-character", &IntChar, V_CHAR|V_TTY_RESET,
	VARIABLE, "right-margin", &RMargin, V_BASE10,
	VARIABLE, "scroll-step", &ScrollStep, V_BASE10,
	VARIABLE, "scroll-all-lines", &ScrollAll, V_BOOL,
	VARIABLE, "search-exit-char", &SExitChar, V_CHAR,
	VARIABLE, "send-typeout-to-buffer", &UseBuffers, V_BOOL,
#ifndef MAC
	VARIABLE, "shell", (int *) Shell, V_STRING,
	VARIABLE, "shell-flags", (int *) ShFlags, V_STRING,
#endif
#ifndef MSDOS
	VARIABLE, "sync-frequency", &SyncFreq, V_BASE10,
#endif /* MSDOS */
	VARIABLE, "tag-file", (int *) TagFile, V_FILENAME,
#ifndef MAC
	VARIABLE, "tmp-file-pathname", (int *) TmpFilePath, V_FILENAME,
#endif
#ifdef UNIX
	VARIABLE, "update-time-frequency", &UpdFreq, V_BASE10,
#endif /* UNIX */
#ifdef ID_CHAR
	VARIABLE, "use-i/d-char", &UseIC, V_BOOL,
#endif
	VARIABLE, "visible-bell", &VisBell, V_BOOL,
	VARIABLE, "wrap-search", &WrapScan, V_BOOL,
#ifndef MAC
	VARIABLE, "write-files-on-make", &WtOnMk, V_BOOL,
#endif
	VARIABLE, 0, 0, 0
};

#ifndef TXT_TO_C
data_obj *
findvar(prompt)
char	*prompt;
{
	static char	*strings[(sizeof variables) / sizeof (struct variable)];
	static int	beenhere = 0;
	register int	com;

	if (beenhere == 0) {
		register char	**strs = strings;
		register struct variable	*v = variables;

		beenhere = 1;
		for (; v->Name; v++)
			*strs++ = v->Name;
		*strs = 0;
	}

	if ((com = complete(strings, prompt, NOTHING)) < 0)
		return 0;
	return (data_obj *) &variables[com];
}
#endif
