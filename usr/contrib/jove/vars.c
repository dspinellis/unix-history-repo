/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"

struct variable	variables[] = {
	VARIABLE, "allow-^S-and-^Q", &OKXonXoff, V_BOOL|V_TTY_RESET,
	VARIABLE, "allow-bad-filenames", &OkayBadChars, V_BOOL,
#ifdef ABBREV
	VARIABLE, "auto-case-abbrev", &AutoCaseAbbrev, V_BOOL,
#endif
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
	VARIABLE, "error-window-size", &EWSize, V_BASE10,
	VARIABLE, "file-creation-mode", &CreatMode, V_BASE8,
	VARIABLE, "files-should-end-with-newline", &EndWNewline, V_BOOL,
	VARIABLE, "internal-tabstop", &tabstop, V_BASE10|V_CLRSCREEN,
	VARIABLE, "left-margin", &LMargin, V_BASE10,
	VARIABLE, "mailbox", (int *) Mailbox, V_STRING,
	VARIABLE, "mail-check-frequency", (int *) &MailInt, V_BASE10,
#ifdef BACKUPFILES
	VARIABLE, "make-backup-files", &BkupOnWrite, V_BOOL,
#endif
	VARIABLE, "mark-threshold", &MarkThresh, V_BASE10,
	VARIABLE, "marks-should-float", &MarksShouldFloat, V_BOOL,
	VARIABLE, "match-regular-expressions", &UseRE, V_BOOL,
	VARIABLE, "meta-key", &MetaKey, V_BOOL|V_TTY_RESET,
	VARIABLE, "mode-line", (int *) ModeFmt, V_STRING|V_MODELINE,
	VARIABLE, "mode-line-should-standout", &BriteMode, V_BOOL|V_MODELINE,
	VARIABLE, "paren-flash-delay", &PDelay, V_BASE10,
	VARIABLE, "physical-tabstop", &phystab, V_BASE10|V_CLRSCREEN,
#ifdef IPROCS
	VARIABLE, "process-prompt", (int *) proc_prompt, V_STRING,
#endif
	VARIABLE, "right-margin", &RMargin, V_BASE10,
	VARIABLE, "scroll-step", &ScrollStep, V_BASE10,
	VARIABLE, "search-exit-char", &SExitChar, V_CHAR,
	VARIABLE, "send-typeout-to-buffer", &UseBuffers, V_BOOL,
	VARIABLE, "shell", (int *) Shell, V_STRING,
	VARIABLE, "shell-flags", (int *) ShFlags, V_STRING,
	VARIABLE, "sync-frequency", &SyncFreq, V_BASE10,
	VARIABLE, "tag-file", (int *) TagFile, V_STRING,
	VARIABLE, "update-time-frequency", &UpdFreq, V_BASE10,
#ifdef ID_CHAR
	VARIABLE, "use-i/d-char", &UseIC, V_BOOL,
#endif
	VARIABLE, "visible-bell", &VisBell, V_BOOL,
	VARIABLE, "wrap-search", &WrapScan, V_BOOL,
	VARIABLE, "write-files-on-make", &WtOnMk, V_BOOL,
	VARIABLE, 0, 0, 0
};

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

	if ((com = complete(strings, prompt)) < 0)
		return 0;
	return (data_obj *) &variables[com];
}
