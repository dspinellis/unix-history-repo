/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#ifndef TXT_TO_C	/* so that jove.h isn't included twice in setmaps */
#include "jove.h"
#endif

const struct variable	variables[] = {
	VARIABLE, "abort-char", (char *) &AbortChar, V_CHAR,
#ifdef UNIX
	VARIABLE, "allow-^S-and-^Q", (char *) &OKXonXoff, V_BOOL|V_TTY_RESET,
#endif /* UNIX */
	VARIABLE, "allow-bad-filenames", (char *) &OkayBadChars, V_BOOL,
#ifdef ABBREV
	VARIABLE, "auto-case-abbrev", (char *) &AutoCaseAbbrev, V_BOOL,
#endif
#ifdef IBMPC
	VARIABLE, "background-color", (char *) &Bgcolor, V_BASE10|V_CLRSCREEN,
#endif /* IBMPC */
#ifdef F_COMPLETION
	VARIABLE, "bad-filename-extensions", BadExtensions, V_STRING,
#endif
	VARIABLE, "c-argument-indentation", (char *) &CArgIndent, V_BASE10,
	VARIABLE, "c-indentation-increment", (char *) &CIndIncrmt, V_BASE10,
	VARIABLE, "case-ignore-search", (char *) &CaseIgnore, V_BOOL,
#ifdef CMT_FMT
	VARIABLE, "comment-format", CmtFmt, V_STRING,
#endif
#ifdef IPROCS
	VARIABLE, "dbx-format-string", dbx_parse_fmt, V_STRING,
#endif
#ifdef BIFF
	VARIABLE, "disable-biff", (char *) &BiffChk, V_BOOL,
#endif
#ifdef F_COMPLETION
	VARIABLE, "display-bad-filenames", (char *) &DispBadFs, V_BOOL,
#endif
#ifndef MAC
	VARIABLE, "error-format-string", ErrFmtStr, V_STRING,
	VARIABLE, "error-window-size", (char *) &EWSize, V_BASE10,
#endif
	VARIABLE, "expand-environment-variables", (char *) &DoEVexpand, V_BOOL,
	VARIABLE, "file-creation-mode", (char *) &CreatMode, V_BASE8,
	VARIABLE, "files-should-end-with-newline", (char *) &EndWNewline, V_BOOL,
#ifdef IBMPC
	VARIABLE, "foreground-color", (char *) &Fgcolor, V_BASE10|V_CLRSCREEN,
#endif /* IBMPC */
	VARIABLE, "internal-tabstop", (char *) &tabstop, V_BASE10|V_CLRSCREEN,
	VARIABLE, "left-margin", (char *) &LMargin, V_BASE10,
#ifdef UNIX
	VARIABLE, "mailbox", Mailbox, V_FILENAME,
	VARIABLE, "mail-check-frequency", (char *) &MailInt, V_BASE10,
#endif /* UNIX */
#ifdef MAC
	VARIABLE, "macify", (char *) &Macmode, V_BOOL,
#endif
#ifdef BACKUPFILES
	VARIABLE, "make-backup-files", (char *) &BkupOnWrite, V_BOOL,
#endif
	VARIABLE, "mark-threshold", (char *) &MarkThresh, V_BASE10,
	VARIABLE, "marks-should-float", (char *) &MarksShouldFloat, V_BOOL,
	VARIABLE, "match-regular-expressions", (char *) &UseRE, V_BOOL,
	VARIABLE, "meta-key", (char *) &MetaKey, V_BOOL|V_TTY_RESET,
	VARIABLE, "mode-line", ModeFmt, V_STRING|V_MODELINE,
#ifdef IBMPC
	VARIABLE, "mode-line-color", (char *) &Mdcolor, V_BASE10|V_MODELINE,
#endif
	VARIABLE, "mode-line-should-standout", (char *) &BriteMode, V_BOOL|V_MODELINE,
	VARIABLE, "paren-flash-delay", (char *) &PDelay, V_BASE10,
#ifndef MAC
	VARIABLE, "physical-tabstop", (char *) &phystab, V_BASE10|V_CLRSCREEN,
#endif
#ifdef IPROCS
	VARIABLE, "process-prompt", proc_prompt, V_STRING,
#endif
	VARIABLE, "interrupt-character", (char *) &IntChar, V_CHAR|V_TTY_RESET,
	VARIABLE, "right-margin", (char *) &RMargin, V_BASE10,
	VARIABLE, "scroll-step", (char *) &ScrollStep, V_BASE10,
	VARIABLE, "scroll-all-lines", (char *) &ScrollAll, V_BOOL,
	VARIABLE, "search-exit-char", (char *) &SExitChar, V_CHAR,
	VARIABLE, "send-typeout-to-buffer", (char *) &UseBuffers, V_BOOL,
#ifndef MAC
	VARIABLE, "shell", Shell, V_FILENAME,
	VARIABLE, "shell-flags", ShFlags, V_STRING,
#endif
#ifndef MSDOS
	VARIABLE, "sync-frequency", (char *) &SyncFreq, V_BASE10,
#endif /* MSDOS */
	VARIABLE, "tag-file", TagFile, V_FILENAME,
#ifndef MAC
	VARIABLE, "tmp-file-pathname", TmpFilePath, V_FILENAME,
#endif
#ifdef UNIX
	VARIABLE, "update-time-frequency", (char *) &UpdFreq, V_BASE10,
#endif /* UNIX */
#ifdef ID_CHAR
	VARIABLE, "use-i/d-char", (char *) &UseIC, V_BOOL,
#endif
	VARIABLE, "visible-bell", (char *) &VisBell, V_BOOL,
	VARIABLE, "wrap-search", (char *) &WrapScan, V_BOOL,
#ifndef MAC
	VARIABLE, "write-files-on-make", (char *) &WtOnMk, V_BOOL,
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
		register const struct variable	*v = variables;

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
