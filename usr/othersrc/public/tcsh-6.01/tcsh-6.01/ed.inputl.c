/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/ed.inputl.c,v 3.15 1991/12/19 22:34:14 christos Exp $ */
/*
 * ed.inputl.c: Input line handling.
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: ed.inputl.c,v 3.15 1991/12/19 22:34:14 christos Exp $")

#include "ed.h"
#include "ed.defns.h"		/* for the function names */
#include "tw.h"			/* for twenex stuff */

#define OKCMD (INBUFSIZE+INBUFSIZE)
extern CCRETVAL e_up_hist();
extern CCRETVAL e_expand_history();

/* ed.inputl -- routines to get a single line from the input. */

extern bool tellwhat;
extern bool MapsAreInited;
extern bool Tty_raw_mode;

/* mismatched first character */
static Char mismatch[] = {'!', '\\', '^', '-', '%', '\0'};

static	int	GetNextCommand	__P((KEYCMD *, Char *));
static	int	SpellLine	__P((int));
static	void	RunCommand	__P((Char *));
static void 	doeval1		__P((Char **));

/* CCRETVAL */
int
Inputl()
{
    CCRETVAL retval;
    KEYCMD  cmdnum = 0;
    extern KEYCMD NumFuns;
    unsigned char tch;		/* the place where read() goes */
    Char    ch;
    int     num;		/* how many chars we have read at NL */
    int	    expnum;
    struct varent *crct = adrof(STRcorrect);
    struct varent *autol = adrof(STRautolist);
    struct varent *matchbeep = adrof(STRmatchbeep);
    struct varent *imode = adrof(STRinputmode);
    Char   *SaveChar, *CorrChar;
    Char    Origin[INBUFSIZE], Change[INBUFSIZE];
    int     matchval;		/* from tenematch() */

    if (!MapsAreInited)		/* double extra just in case */
	ed_InitMaps();

    ClearDisp();		/* reset the display stuff */
    ResetInLine();		/* reset the input pointers */
    if (GettingInput)
	MacroLvl = -1;		/* editor was interrupted during input */

#ifdef FIONREAD
    if (!Tty_raw_mode && MacroLvl < 0) {
	long    chrs = 0;

	(void) ioctl(SHIN, FIONREAD, (ioctl_t) & chrs);
	if (chrs == 0) {
	    if (Rawmode() < 0)
		return 0;
	}
    }
#endif

    GettingInput = 1;
    NeedsRedraw = 0;

    if (tellwhat) {
	copyn(InputBuf, WhichBuf, INBUFSIZE);
	LastChar = InputBuf + (LastWhich - WhichBuf);
	Cursor = InputBuf + (CursWhich - WhichBuf);
	tellwhat = 0;
	Hist_num = HistWhich;
    }
    if (Expand) {
	(void) e_up_hist(0);
	Expand = 0;
    }
    Refresh();			/* print the prompt */

    for (num = OKCMD; num == OKCMD;) {	/* while still editing this line */
#ifdef DEBUG_EDIT
	if (Cursor > LastChar)
	    xprintf("Cursor > LastChar\r\n");
	if (Cursor < InputBuf)
	    xprintf("Cursor < InputBuf\r\n");
	if (Cursor > InputLim)
	    xprintf("Cursor > InputLim\r\n");
	if (LastChar > InputLim)
	    xprintf("LastChar > InputLim\r\n");
	if (InputLim != &InputBuf[INBUFSIZE - 2])
	    xprintf("InputLim != &InputBuf[INBUFSIZE-2]\r\n");
	if ((!DoingArg) && (Argument != 1))
	    xprintf("(!DoingArg) && (Argument != 1)\r\n");
	if (CcKeyMap[0] == 0)
	    xprintf("CcKeyMap[0] == 0 (maybe not inited)\r\n");
#endif

	/* if EOF or error */
	if ((num = GetNextCommand(&cmdnum, &ch)) != OKCMD) {
	    break;
	}

	if (cmdnum >= NumFuns) {/* BUG CHECK command */
#ifdef DEBUG_EDIT
	    xprintf("ERROR: illegal command from key 0%o\r\n", ch);
#endif
	    continue;		/* try again */
	}

	/* now do the real command */
	retval = (*CcFuncTbl[cmdnum]) (ch);

	/* save the last command here */
	LastCmd = cmdnum;

	/* use any return value */
	switch (retval) {

	case CC_REFRESH:
	    Refresh();
	    /* fall through */
	case CC_NORM:		/* normal char */
	    Argument = 1;
	    DoingArg = 0;
	    /* fall through */
	case CC_ARGHACK:	/* Suggested by Rich Salz */
	    /* <rsalz@pineapple.bbn.com> */
	    break;		/* keep going... */

	case CC_EOF:		/* end of file typed */
#ifdef notdef
	    PromptBuf[0] = '\0';
#endif
	    num = 0;
	    break;

	case CC_WHICH:		/* tell what this command does */
	    tellwhat = 1;
	    copyn(WhichBuf, InputBuf, INBUFSIZE);
	    LastWhich = WhichBuf + (LastChar - InputBuf);
	    CursWhich = WhichBuf + (Cursor - InputBuf);
	    *LastChar++ = '\n';	/* for the benifit of CSH */
	    HistWhich = Hist_num;
	    Hist_num = 0;	/* for the history commands */
	    num = LastChar - InputBuf;	/* number characters read */
#ifdef notdef
	    ResetInLine();	/* reset the input pointers */
#endif
	    break;

	case CC_NEWLINE:	/* normal end of line */
	    if (crct && (!Strcmp(*(crct->vec), STRcmd) ||
			 !Strcmp(*(crct->vec), STRall))) {
		copyn(Origin, InputBuf, INBUFSIZE);
		SaveChar = LastChar;
		if (SpellLine(!Strcmp(*(crct->vec), STRcmd)) == 1) {
		    copyn(Change, InputBuf, INBUFSIZE);
		    *Strchr(Change, '\n') = '\0';
		    CorrChar = LastChar;	/* Save the corrected end */
		    LastChar = InputBuf;	/* Null the current line */
		    Beep();
		    printprompt(2, Change);
		    Refresh();
		    (void) read(SHIN, (char *) &tch, 1);
		    ch = tch;
		    if (ch == 'y' || ch == ' ') {
			LastChar = CorrChar;	/* Restore the corrected end */
			xprintf("yes\n");
		    }
		    else {
			(void) copyn(InputBuf, Origin, INBUFSIZE);
			LastChar = SaveChar;
			if (ch == 'e') {
			    xprintf("edit\n");
			    *LastChar-- = '\0';
			    Cursor = LastChar;
			    printprompt(3, NULL);
			    Refresh();
			    break;
			}
			xprintf("no\n");
		    }
		    flush();
		}
	    }			/* end CORRECT code */
	    tellwhat = 0;	/* just in case */
	    Hist_num = 0;	/* for the history commands */
	    num = LastChar - InputBuf;	/* return the number of chars read */
	    /*
	     * For continuation lines, we set the prompt to prompt 2
	     */
	    if (imode) {
		if (!Strcmp(*(imode->vec), STRinsert))
		    inputmode = MODE_INSERT;
		else if (!Strcmp(*(imode->vec), STRoverwrite))
		    inputmode = MODE_REPLACE;
	    }
	    printprompt(1, NULL);
#ifdef notdef
	    ResetInLine();	/* reset the input pointers */
	    PromptBuf[0] = '\0';
#endif
	    break;

	case CC_CORRECT:
	    if (tenematch(InputBuf, INBUFSIZE, Cursor - InputBuf,
			  SPELL) < 0)
		Beep();		/* Beep = No match/ambiguous */
	    if (NeedsRedraw) {
		ClearLines();
		ClearDisp();
		NeedsRedraw = 0;
	    }
	    Refresh();
	    Argument = 1;
	    DoingArg = 0;
	    break;

	case CC_CORRECT_L:
	    if (SpellLine(FALSE) < 0)
		Beep();		/* Beep = No match/ambiguous */
	    if (NeedsRedraw) {
		ClearLines();
		ClearDisp();
		NeedsRedraw = 0;
	    }
	    Refresh();
	    Argument = 1;
	    DoingArg = 0;
	    break;


	case CC_COMPLETE:
	    if (adrof(STRautoexpand))
		(void) e_expand_history(0);
	    /*
	     * Modified by Martin Boyer (gamin@ireq-robot.hydro.qc.ca):
	     * A separate variable now controls beeping after
	     * completion, independently of autolisting.
	     */
	    expnum = Cursor - InputBuf;
	    switch (matchval = 
		    tenematch(InputBuf, INBUFSIZE, Cursor-InputBuf, RECOGNIZE)) {
	    case 1:
		if (non_unique_match && matchbeep &&
		    (Strcmp(*(matchbeep->vec), STRnotunique) == 0))
		    Beep();
		break;
	    case 0:
		if (matchbeep) {
		    if (Strcmp(*(matchbeep->vec), STRnomatch) == 0 ||
			Strcmp(*(matchbeep->vec), STRambiguous) == 0 ||
			Strcmp(*(matchbeep->vec), STRnotunique) == 0)
			Beep();
		}
		else
		    Beep();
		break;
	    default:
		if (matchval < 0) {	/* Error from tenematch */
		    Beep();
		    break;
		}
		if (matchbeep) {
		    if ((Strcmp(*(matchbeep->vec), STRambiguous) == 0 ||
			 Strcmp(*(matchbeep->vec), STRnotunique) == 0))
			Beep();
		}
		else
		    Beep();
		/*
		 * Addition by David C Lawrence <tale@pawl.rpi.edu>: If an 
		 * attempted completion is ambiguous, list the choices.  
		 * (PWP: this is the best feature addition to tcsh I have 
		 * seen in many months.)
		 */
		if (autol && (Strcmp(*(autol->vec), STRambiguous) != 0 || 
				     expnum == Cursor - InputBuf)) {
		    PastBottom();
		    (void) tenematch(InputBuf, INBUFSIZE, Cursor-InputBuf, LIST);
		}
		break;
	    }
	    if (NeedsRedraw) {
		PastBottom();
		ClearLines();
		ClearDisp();
		NeedsRedraw = 0;
	    }
	    Refresh();
	    Argument = 1;
	    DoingArg = 0;
	    break;

	case CC_LIST_CHOICES:
	    /* should catch ^C here... */
	    if (tenematch(InputBuf, INBUFSIZE, Cursor - InputBuf, LIST) < 0)
		Beep();
	    Refresh();
	    Argument = 1;
	    DoingArg = 0;
	    break;

	case CC_LIST_GLOB:
	    if (tenematch(InputBuf, INBUFSIZE, Cursor - InputBuf, GLOB) < 0)
		Beep();
	    Refresh();
	    Argument = 1;
	    DoingArg = 0;
	    break;

	case CC_EXPAND_GLOB:
	    if (tenematch(InputBuf, INBUFSIZE, Cursor - InputBuf,
			  GLOB_EXPAND) <= 0)
		Beep();		/* Beep = No match */
	    if (NeedsRedraw) {
		ClearLines();
		ClearDisp();
		NeedsRedraw = 0;
	    }
	    Refresh();
	    Argument = 1;
	    DoingArg = 0;
	    break;

	case CC_NORMALIZE_PATH:
	    if (tenematch(InputBuf, INBUFSIZE, Cursor - InputBuf,
			  PATH_NORMALIZE) <= 0)
		Beep();		/* Beep = No match */
	    if (NeedsRedraw) {
		ClearLines();
		ClearDisp();
		NeedsRedraw = 0;
	    }
	    Refresh();
	    Argument = 1;
	    DoingArg = 0;
	    break;

	case CC_EXPAND_VARS:
	    if (tenematch(InputBuf, INBUFSIZE, Cursor - InputBuf,
			  VARS_EXPAND) <= 0)
		Beep();		/* Beep = No match */
	    if (NeedsRedraw) {
		ClearLines();
		ClearDisp();
		NeedsRedraw = 0;
	    }
	    Refresh();
	    Argument = 1;
	    DoingArg = 0;
	    break;

	case CC_HELPME:
	    xputchar('\n');
	    /* should catch ^C here... */
	    (void) tenematch(InputBuf, INBUFSIZE, LastChar - InputBuf,
			     PRINT_HELP);
	    Refresh();
	    Argument = 1;
	    DoingArg = 0;
	    break;

	case CC_FATAL:		/* fatal error, reset to known state */
#ifdef DEBUG_EDIT
	    xprintf("*** editor fatal ERROR ***\r\n\n");
#endif				/* DEBUG_EDIT */
	    /* put (real) cursor in a known place */
	    ClearDisp();	/* reset the display stuff */
	    ResetInLine();	/* reset the input pointers */
	    Refresh();		/* print the prompt again */
	    Argument = 1;
	    DoingArg = 0;
	    break;

	case CC_ERROR:
	default:		/* functions we don't know about */
	    DoingArg = 0;
	    Argument = 1;
	    Beep();
	    flush();
	    break;
	}
    }
    (void) Cookedmode();	/* make sure the tty is set up correctly */
    GettingInput = 0;
    flush();			/* flush any buffered output */
    return num;
}

void
PushMacro(str)
    Char   *str;
{
    if (str != NULL && MacroLvl + 1 < MAXMACROLEVELS) {
	MacroLvl++;
	KeyMacro[MacroLvl] = str;
    }
    else {
	Beep();
	flush();
    }
}

/*
 * Like eval, only using the current file descriptors
 */
static Char **gv = NULL;

static void
doeval1(v)
    Char **v;
{
    Char  **oevalvec;
    Char   *oevalp;
    int     my_reenter;
    Char  **savegv;
    jmp_buf osetexit;

    oevalvec = evalvec;
    oevalp = evalp;
    savegv = gv;


    gflag = 0, tglob(v);
    if (gflag) {
	gv = v = globall(v);
	gargv = 0;
	if (v == 0)
	    stderror(ERR_NOMATCH);
	v = copyblk(v);
    }
    else {
	gv = NULL;
	v = copyblk(v);
	trim(v);
    }

    getexit(osetexit);

    /* PWP: setjmp/longjmp bugfix for optimizing compilers */
#ifdef cray
    my_reenter = 1;             /* assume non-zero return val */
    if (setexit() == 0) {
        my_reenter = 0;         /* Oh well, we were wrong */
#else /* !cray */
    if ((my_reenter = setexit()) == 0) {
#endif /* cray */
	evalvec = v;
	evalp = 0;
	process(0);
    }

    evalvec = oevalvec;
    evalp = oevalp;
    doneinp = 0;

    if (gv)
	blkfree(gv);

    gv = savegv;
    resexit(osetexit);
    if (my_reenter)
	stderror(ERR_SILENT);
}

static void
RunCommand(str)
    Char *str;
{
    Char *cmd[2];

    xprintf("\n");	/* Start on a clean line */

    cmd[0] = str;
    cmd[1] = NULL;

    doeval1(cmd);
    
    ClearLines();
    ClearDisp();
    NeedsRedraw = 1;
    Refresh();
}

static int
GetNextCommand(cmdnum, ch)
    KEYCMD *cmdnum;
    register Char *ch;
{
    KEYCMD  cmd = 0;
    int     num;

    while (cmd == 0 || cmd == F_XKEY) {
	if ((num = GetNextChar(ch)) != 1) {	/* if EOF or error */
	    return num;
	}
#ifdef	KANJI
	if (*ch & META) {
	    MetaNext = 0;
	    cmd = CcViMap[' '];
	    break;
	}
	else
#endif /* KANJI */
	if (MetaNext) {
	    MetaNext = 0;
	    *ch |= META;
	}
	cmd = CurrentKeyMap[(unsigned char) *ch];
	if (cmd == F_XKEY) {
	    XmapVal val;
	    switch (GetXkey(ch, &val)) {
	    case XK_CMD:
		cmd = val.cmd;
		break;
	    case XK_STR:
		PushMacro(val.str);
		break;
	    case XK_EXE:
		RunCommand(val.str);
		break;
	    default:
		abort();
		break;
	    }
	}
	if (!AltKeyMap) 
	    CurrentKeyMap = CcKeyMap;
    }
    *cmdnum = cmd;
    return OKCMD;
}

int
GetNextChar(cp)
    register Char *cp;
{
    register int num_read;
#if defined(EWOULDBLOCK) || (defined(POSIX) && defined(EAGAIN))
# if defined(FIONBIO) || (defined(F_SETFL) && defined(O_NDELAY))
#  define TRY_AGAIN
    int     tried = 0;
# endif /* FIONBIO || (F_SETFL && O_NDELAY) */
#endif /* EWOULDBLOCK || (POSIX && EAGAIN) */
    unsigned char tcp;

    for (;;) {
	if (MacroLvl < 0) {
	    if (!Load_input_line())
		break;
	}
	if (*KeyMacro[MacroLvl] == 0) {
	    MacroLvl--;
	    continue;
	}
	*cp = *KeyMacro[MacroLvl]++ & CHAR;
	if (*KeyMacro[MacroLvl] == 0) {	/* Needed for QuoteMode On */
	    MacroLvl--;
	}
	return (1);
    }

    if (Rawmode() < 0)		/* make sure the tty is set up correctly */
	return 0;		/* oops: SHIN was closed */

    while ((num_read = read(SHIN, (char *) &tcp, 1)) == -1)
	switch (errno) {
	    /*
	     * Someone might have set our file descriptor to non blocking From
	     * Gray Watson (gray%antr.uucp@med.pitt.edu), Thanks!!!
	     */
#ifdef EWOULDBLOCK
	case EWOULDBLOCK:
#endif /* EWOULDBLOCK */
#if defined(POSIX) && defined(EAGAIN)
# if defined(EWOULDBLOCK) && EAGAIN != EWOULDBLOCK
	case EAGAIN:
# endif /* EWOULDBLOCK && EAGAIN != EWOULDBLOCK */
#endif /* POSIX && EAGAIN */
#ifdef TRY_AGAIN
	    if (!tried) {
# if defined(F_SETFL) && defined(O_NDELAY)
		(void) fcntl(SHIN, F_SETFL,
			     fcntl(SHIN, F_GETFL, 0) & ~O_NDELAY);
# endif /* F_SETFL && O_NDELAY */
# ifdef FIONBIO
		(void) ioctl(SHIN, FIONBIO, (ioctl_t) & tried);
# endif /* FIONBIO */
		tried = 1;
		break;
	    }
	    *cp = tcp;
	    return (num_read);
#endif /* TRY_AGAIN */
	case EINTR:
	    break;
	default:
#ifdef DEBUG_EDIT
	    xprintf("GetNextChar(): errno == %d\n", errno);
#endif /* DEBUG_EDIT */
	    *cp = tcp;
	    return num_read;
	}
    *cp = tcp;
    return num_read;
}

/*
 * SpellLine - do spelling correction on the entire command line
 * (which may have trailing newline).
 * If cmdonly is set, only check spelling of command words.
 * Return value:
 * -1: Something was incorrectible, and nothing was corrected
 *  0: Everything was correct
 *  1: Something was corrected
 */
static int
SpellLine(cmdonly)
    int     cmdonly;
{
    int     endflag, matchval;
    Char   *argptr, *OldCursor, *OldLastChar;

    OldLastChar = LastChar;
    OldCursor = Cursor;
    argptr = InputBuf;
    endflag = 1;
    matchval = 0;
    do {
	while (ismetahash(*argptr) || iscmdmeta(*argptr))
	    argptr++;
	for (Cursor = argptr;
	     *Cursor != '\0' && !ismetahash(*Cursor) && !iscmdmeta(*Cursor);
	     Cursor++);
	if (*Cursor == '\0') {
	    Cursor = LastChar;
	    if (LastChar[-1] == '\n')
		Cursor--;
	    endflag = 0;
	}
	if (!Strchr(mismatch, *argptr) &&
	    (!cmdonly || starting_a_command(argptr, InputBuf))) {
	    switch (tenematch(InputBuf, INBUFSIZE, Cursor - InputBuf, SPELL)) {
	    case 1:		/* corrected */
		matchval = 1;
		break;
	    case -1:		/* couldn't be corrected */
		if (!matchval)
		    matchval = -1;
		break;
	    default:		/* was correct */
		break;
	    }
	    if (LastChar != OldLastChar) {
		if (argptr < OldCursor)
		    OldCursor += (LastChar - OldLastChar);
		OldLastChar = LastChar;
	    }
	}
	argptr = Cursor;
    } while (endflag);
    Cursor = OldCursor;
    return matchval;
}
