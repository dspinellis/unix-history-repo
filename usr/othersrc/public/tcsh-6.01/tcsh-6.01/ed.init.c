/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/ed.init.c,v 3.23 1991/12/19 22:34:14 christos Exp $ */
/*
 * ed.init.c: Editor initializations
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
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTS_ION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: ed.init.c,v 3.23 1991/12/19 22:34:14 christos Exp $")

#include "ed.h"
#include "ed.term.h"
#include "tc.h"
#include "ed.defns.h"

/* ed.init.c -- init routines for the line editor */
/* #define DEBUG_TTY */

int     Tty_raw_mode = 0;	/* Last tty change was to raw mode */
int     MacroLvl = -1;		/* pointer to current macro nesting level; */
				/* (-1 == none) */
static int Tty_quote_mode = 0;	/* Last tty change was to quote mode */
static unsigned char vdisable;	/* The value of _POSIX_VDISABLE from 
				 * pathconf(2) */

int     Tty_eight_bit = -1;	/* does the tty handle eight bits */

extern bool GotTermCaps;

static ttydata_t extty, edtty, tstty;
#define qutty tstty

extern int insource;
#define SHTTY (insource ? OLDSTD : SHIN)

static unsigned char ttychars[NN_IO][C_NCC] = {
    {
	CINTR,		 CQUIT, 	 CERASE, 	   CKILL,	
	CEOF, 		 CEOL, 		 CEOL2, 	   CSWTCH, 
	CDSWTCH,	 CERASE2,	 CSTART, 	   CSTOP,
	CWERASE, 	 CSUSP, 	 CDSUSP, 	   CREPRINT,
	CDISCARD, 	 CLNEXT,	 CSTATUS,	   CPAGE,
	CPGOFF,		 CKILL2, 	 CBRK, 		   CMIN,
	CTIME
    },
    {
	CINTR, 		 CQUIT, 	  CERASE, 	   CKILL, 
	_POSIX_VDISABLE, _POSIX_VDISABLE, _POSIX_VDISABLE, _POSIX_VDISABLE, 
	_POSIX_VDISABLE, CERASE2,	  CSTART, 	   CSTOP, 	   
	_POSIX_VDISABLE, _POSIX_VDISABLE, _POSIX_VDISABLE, _POSIX_VDISABLE, 
	CDISCARD, 	 _POSIX_VDISABLE, _POSIX_VDISABLE, _POSIX_VDISABLE, 
	_POSIX_VDISABLE, _POSIX_VDISABLE, _POSIX_VDISABLE, 1,
	0
    },
    {	
	0,		 0,		  0,		   0,
	0,		 0,		  0,		   0,
	0,		 0,		  0,		   0,
	0,		 0,		  0,		   0,
	0,		 0,		  0,		   0,
	0,		 0,		  0,		   0,
	0
    }
};

#ifdef SIG_WINDOW
void
check_window_size(force)
    int     force;
{
#ifdef BSDSIGS
    sigmask_t omask;
#endif /* BSDSIGS */
    int     lins, cols;

    /* don't want to confuse things here */
#ifdef BSDSIGS
    omask = sigblock(sigmask(SIG_WINDOW)) & ~sigmask(SIG_WINDOW);
#else /* BSDSIGS */
    (void) sighold(SIG_WINDOW);
#endif /* BSDSIGS */
    /*
     * From: bret@shark.agps.lanl.gov (Bret Thaeler) Avoid sunview bug, where a
     * partially hidden window gets a SIG_WINDOW every time the text is
     * scrolled
     */
    if (GetSize(&lins, &cols) || force) {
	if (GettingInput) {
	    ClearLines();
	    ClearDisp();
	    MoveToLine(0);
	    MoveToChar(0);
	    ChangeSize(lins, cols);
	    Refresh();
	}
	else
	    ChangeSize(lins, cols);
    }
#ifdef BSDSIGS
    (void) sigsetmask(omask);	/* can change it again */
#else				/* BSDSIGS */
    (void) sigrelse(SIG_WINDOW);
#endif /* BSDSIGS */
}

sigret_t
/*ARGSUSED*/
window_change(snum)
int snum;
{
#ifdef UNRELSIGS 
    /* If we were called as a signal handler, restore it. */
    if (snum > 0)
      sigset(snum, window_change);
#endif /* UNRELSIGS */
    check_window_size(0);
#ifndef SIGVOID
    return (snum);
#endif 
}

#endif /* SIG_WINDOW */

void
ed_set_tty_eight_bit()
{
    if (tty_getty(SHTTY, &extty) == -1) {
#ifdef DEBUG_TTY
	xprintf("ed_set_tty_eight_bit: tty_getty: %s\n", strerror(errno));
#endif /* DEBUG_TTY */
	return;
    }
    Tty_eight_bit = tty_geteightbit(&extty);
}

			
int
ed_Setup(rst)
    int rst;
{
    static int havesetup = 0;

    if (havesetup) 	/* if we have never been called */
	return(0);

#if defined(POSIX) && defined(_PC_VDISABLE) && !defined(BSD4_4)
    { 
	long pcret;

	if ((pcret = fpathconf(SHTTY, _PC_VDISABLE)) == -1L)
	    vdisable = _POSIX_VDISABLE;
	else 
	    vdisable = pcret;
	if (vdisable != _POSIX_VDISABLE && rst != 0)
	    for (rst = 0; rst < C_NCC - 2; rst++) {
		if (ttychars[ED_IO][rst] == _POSIX_VDISABLE)
		    ttychars[ED_IO][rst] = vdisable;
		if (ttychars[EX_IO][rst] == _POSIX_VDISABLE)
		    ttychars[EX_IO][rst] = vdisable;
	    }
    }
#else /* ! POSIX || !_PC_VDISABLE && !defined(BSD4_4) */
    vdisable = _POSIX_VDISABLE;
#endif /* POSIX && _PC_VDISABLE */
	
    inputmode = MODE_INSERT;	/* start out in insert mode */
    ed_InitMaps();
    Hist_num = 0;
    Expand = 0;

    if (tty_getty(SHTTY, &extty) == -1) {
#ifdef DEBUG_TTY
	xprintf("ed_Setup: tty_getty: %s\n", strerror(errno));
#endif /* DEBUG_TTY */
	return(-1);
    }

    tstty = edtty = extty;

    T_Speed = tty_getspeed(&extty);
    T_Tabs = tty_gettabs(&extty);
    Tty_eight_bit = tty_geteightbit(&extty);

#if defined(POSIX) || defined(TERMIO)
    extty.d_t.c_iflag &= ~ttylist[EX_IO][M_INPUT].t_clrmask;
    extty.d_t.c_iflag |=  ttylist[EX_IO][M_INPUT].t_setmask;

    extty.d_t.c_oflag &= ~ttylist[EX_IO][M_OUTPUT].t_clrmask;
    extty.d_t.c_oflag |=  ttylist[EX_IO][M_OUTPUT].t_setmask;

    extty.d_t.c_cflag &= ~ttylist[EX_IO][M_CONTROL].t_clrmask;
    extty.d_t.c_cflag |=  ttylist[EX_IO][M_CONTROL].t_setmask;

    extty.d_t.c_lflag &= ~ttylist[EX_IO][M_LINED].t_clrmask;
    extty.d_t.c_lflag |=  ttylist[EX_IO][M_LINED].t_setmask;

# ifdef IRIX3_3
    extty.d_t.c_line = NTTYDISC;
# endif /* IRIX3_3 */

#else	/* GSTTY */		/* V7, Berkeley style tty */

    if (T_Tabs) {	/* order of &= and |= is important to XTABS */
	extty.d_t.sg_flags &= ~(ttylist[EX_IO][M_CONTROL].t_clrmask|XTABS);
	extty.d_t.sg_flags |=   ttylist[EX_IO][M_CONTROL].t_setmask;
    }
    else {
	extty.d_t.sg_flags &= ~ttylist[EX_IO][M_CONTROL].t_clrmask;
	extty.d_t.sg_flags |= (ttylist[EX_IO][M_CONTROL].t_setmask|XTABS);
    }

    extty.d_lb &= ~ttylist[EX_IO][M_LOCAL].t_clrmask;
    extty.d_lb |=  ttylist[EX_IO][M_LOCAL].t_setmask;

#endif /* GSTTY */
    /*
     * Reset the tty chars to reasonable defaults
     * If they are disabled, then enable them.
     */
    if (rst) {
	if (tty_cooked_mode(&tstty)) {
	    tty_getchar(&tstty, ttychars[TS_IO]);
	    /*
	     * Don't affect CMIN and CTIME
	     */
	    for (rst = 0; rst < C_NCC - 2; rst++) {
		if (ttychars[TS_IO][rst] != vdisable &&
		    ttychars[EX_IO][rst] != vdisable)
		    ttychars[EX_IO][rst] = ttychars[TS_IO][rst];
		if (ttychars[TS_IO][rst] != vdisable &&
		    ttychars[ED_IO][rst] != vdisable)
		    ttychars[ED_IO][rst] = ttychars[TS_IO][rst];
	    }
	}
	tty_setchar(&extty, ttychars[EX_IO]);
	if (tty_setty(SHTTY, &extty) == -1) {
#ifdef DEBUG_TTY
	    xprintf("ed_Setup: tty_setty: %s\n", strerror(errno));
#endif /* DEBUG_TTY */
	    return(-1);
	}
    }
    else
	tty_setchar(&extty, ttychars[EX_IO]);

# ifdef SIG_WINDOW
    (void) sigset(SIG_WINDOW, window_change);	/* for window systems */
# endif 
    havesetup = 1;
    return(0);
}

void
ed_Init()
{
    ResetInLine();		/* reset the input pointers */
    GettingInput = 0;		/* just in case */
    LastKill = KillBuf;		/* no kill buffer */

#ifdef DEBUG_EDIT
    CheckMaps();		/* do a little error checking on key maps */
#endif 

    if (ed_Setup(0) == -1)
	return;

    /*
     * if we have been called before but GotTermCaps isn't set, our TERM has
     * changed, so get new termcaps and try again
     */

    if (!GotTermCaps)
	GetTermCaps();		/* does the obvious, but gets term type each
				 * time */

#if defined(TERMIO) || defined(POSIX)
    edtty.d_t.c_iflag &= ~ttylist[ED_IO][M_INPUT].t_clrmask;
    edtty.d_t.c_iflag |=  ttylist[ED_IO][M_INPUT].t_setmask;

    edtty.d_t.c_oflag &= ~ttylist[ED_IO][M_OUTPUT].t_clrmask;
    edtty.d_t.c_oflag |=  ttylist[ED_IO][M_OUTPUT].t_setmask;

    edtty.d_t.c_cflag &= ~ttylist[ED_IO][M_CONTROL].t_clrmask;
    edtty.d_t.c_cflag |=  ttylist[ED_IO][M_CONTROL].t_setmask;

    edtty.d_t.c_lflag &= ~ttylist[ED_IO][M_LINED].t_clrmask;
    edtty.d_t.c_lflag |=  ttylist[ED_IO][M_LINED].t_setmask;


# ifdef IRIX3_3
    edtty.d_t.c_line = NTTYDISC;
# endif /* IRIX3_3 */

#else /* GSTTY */

    if (T_Tabs) {	/* order of &= and |= is important to XTABS */
	edtty.d_t.sg_flags &= ~(ttylist[ED_IO][M_CONTROL].t_clrmask | XTABS);
	edtty.d_t.sg_flags |=   ttylist[ED_IO][M_CONTROL].t_setmask;
    }
    else {
	edtty.d_t.sg_flags &= ~ttylist[ED_IO][M_CONTROL].t_clrmask;
	edtty.d_t.sg_flags |= (ttylist[ED_IO][M_CONTROL].t_setmask | XTABS);
    }

    edtty.d_lb &= ~ttylist[ED_IO][M_LOCAL].t_clrmask;
    edtty.d_lb |=  ttylist[ED_IO][M_LOCAL].t_setmask;
#endif /* POSIX || TERMIO */

    tty_setchar(&edtty, ttychars[ED_IO]);
}

/* 
 * Check and re-init the line. set the terminal into 1 char at a time mode.
 */
int
Rawmode()
{
    if (Tty_raw_mode)
	return (0);

#ifdef _IBMR2
    tty_setdisc(SHTTY, ED_IO);
#endif /* _IBMR2 */

    if (tty_getty(SHTTY, &tstty) == -1) {
#ifdef DEBUG_TTY
	xprintf("Rawmode: tty_getty: %s\n", strerror(errno));
#endif /* DEBUG_TTY */
	return(-1);
    }

    /*
     * We always keep up with the eight bit setting and the speed of the
     * tty. But only we only believe changes that are made to cooked mode!
     */
#if defined(POSIX) || defined(TERMIO)
    Tty_eight_bit = tty_geteightbit(&tstty);
    T_Speed = tty_getspeed(&tstty);

# ifdef POSIX
    /*
     * Fix from: Steven (Steve) B. Green <xrsbg@charney.gsfc.nasa.gov>
     * Speed was not being set up correctly under POSIX.
     */
    if (tty_getspeed(&extty) != T_Speed || tty_getspeed(&edtty) != T_Speed) {
	(void) cfsetispeed(&extty.d_t, T_Speed);
	(void) cfsetospeed(&extty.d_t, T_Speed);
	(void) cfsetispeed(&edtty.d_t, T_Speed);
	(void) cfsetospeed(&edtty.d_t, T_Speed);
    }
# endif /* POSIX */
#else /* GSTTY */

    T_Speed = tty_getspeed(&tstty);
    Tty_eight_bit = tty_geteightbit(&tstty);

    if (extty.d_t.sg_ispeed != tstty.d_t.sg_ispeed) {
	extty.d_t.sg_ispeed = tstty.d_t.sg_ispeed;
	edtty.d_t.sg_ispeed = tstty.d_t.sg_ispeed;
    }

    if (extty.d_t.sg_ospeed != tstty.d_t.sg_ospeed) {
	extty.d_t.sg_ospeed = tstty.d_t.sg_ospeed;
	edtty.d_t.sg_ospeed = tstty.d_t.sg_ospeed;
    }
#endif /* POSIX || TERMIO */

    if (tty_cooked_mode(&tstty)) {
#if defined(POSIX) || defined(TERMIO)
	if (tstty.d_t.c_cflag != extty.d_t.c_cflag) { 
	    extty.d_t.c_cflag  = tstty.d_t.c_cflag;
	    extty.d_t.c_cflag &= ~ttylist[EX_IO][M_CONTROL].t_clrmask;
	    extty.d_t.c_cflag |=  ttylist[EX_IO][M_CONTROL].t_setmask;

	    edtty.d_t.c_cflag  = tstty.d_t.c_cflag;
	    edtty.d_t.c_cflag &= ~ttylist[ED_IO][M_CONTROL].t_clrmask;
	    edtty.d_t.c_cflag |=  ttylist[ED_IO][M_CONTROL].t_setmask;
	}

	if ((tstty.d_t.c_lflag != extty.d_t.c_lflag) &&
	    (tstty.d_t.c_lflag != edtty.d_t.c_lflag)) {
	    extty.d_t.c_lflag = tstty.d_t.c_lflag;
	    extty.d_t.c_lflag &= ~ttylist[EX_IO][M_LINED].t_clrmask;
	    extty.d_t.c_lflag |=  ttylist[EX_IO][M_LINED].t_setmask;

	    edtty.d_t.c_lflag = tstty.d_t.c_lflag;
	    edtty.d_t.c_lflag &= ~ttylist[ED_IO][M_LINED].t_clrmask;
	    edtty.d_t.c_lflag |=  ttylist[ED_IO][M_LINED].t_setmask;
	}

	if ((tstty.d_t.c_iflag != extty.d_t.c_iflag) &&
	    (tstty.d_t.c_iflag != edtty.d_t.c_iflag)) {
	    extty.d_t.c_iflag = tstty.d_t.c_iflag;
	    extty.d_t.c_iflag &= ~ttylist[EX_IO][M_INPUT].t_clrmask;
	    extty.d_t.c_iflag |=  ttylist[EX_IO][M_INPUT].t_setmask;

	    edtty.d_t.c_iflag = tstty.d_t.c_iflag;
	    edtty.d_t.c_iflag &= ~ttylist[ED_IO][M_INPUT].t_clrmask;
	    edtty.d_t.c_iflag |=  ttylist[ED_IO][M_INPUT].t_setmask;
	}

	if ((tstty.d_t.c_oflag != extty.d_t.c_oflag) &&
	    (tstty.d_t.c_oflag != edtty.d_t.c_oflag)) {
	    extty.d_t.c_oflag = tstty.d_t.c_oflag;
	    extty.d_t.c_oflag &= ~ttylist[EX_IO][M_OUTPUT].t_clrmask;
	    extty.d_t.c_oflag |=  ttylist[EX_IO][M_OUTPUT].t_setmask;

	    edtty.d_t.c_oflag = tstty.d_t.c_oflag;
	    edtty.d_t.c_oflag &= ~ttylist[ED_IO][M_OUTPUT].t_clrmask;
	    edtty.d_t.c_oflag |=  ttylist[ED_IO][M_OUTPUT].t_setmask;
	}

	if (tty_gettabs(&extty) == 0) 
	    T_Tabs = 0;
	else 
	    T_Tabs = CanWeTab();

#else /* GSTTY */

	if (((tstty.d_t.sg_flags != extty.d_t.sg_flags) || 
	     (tstty.d_lb != extty.d_lb)) &&
	    ((tstty.d_t.sg_flags != edtty.d_t.sg_flags) || 
	     (tstty.d_lb != edtty.d_lb))) {

	    extty.d_t.sg_flags = tstty.d_t.sg_flags;

	    /*
	     * re-test for some things here (like maybe the user typed 
	     * "stty -tabs"
	     */
	    if (tty_gettabs(&extty) == 0)
		T_Tabs = 0;
	    else 
		T_Tabs = CanWeTab();

	    extty.d_t.sg_flags &= ~ttylist[EX_IO][M_CONTROL].t_clrmask;
	    extty.d_t.sg_flags |=  ttylist[EX_IO][M_CONTROL].t_setmask;

	    if (T_Tabs)		/* order of &= and |= is important to XTABS */
		extty.d_t.sg_flags &= ~XTABS;
	    else 
		extty.d_t.sg_flags |= XTABS;

	    extty.d_lb = tstty.d_lb;
	    extty.d_lb &= ~ttylist[EX_IO][M_LOCAL].t_clrmask;
	    extty.d_lb |= ttylist[EX_IO][M_LOCAL].t_setmask;

	    edtty.d_t.sg_flags = extty.d_t.sg_flags;
	    if (T_Tabs) {	/* order of &= and |= is important to XTABS */
		edtty.d_t.sg_flags &= 
			~(ttylist[ED_IO][M_CONTROL].t_clrmask|XTABS);
		edtty.d_t.sg_flags |=   ttylist[ED_IO][M_CONTROL].t_setmask;
	    }
	    else {
		edtty.d_t.sg_flags &= ~ttylist[ED_IO][M_CONTROL].t_clrmask;
		edtty.d_t.sg_flags |= 
			(ttylist[ED_IO][M_CONTROL].t_setmask|XTABS);
	    }

	    edtty.d_lb = tstty.d_lb;
	    edtty.d_lb &= ~ttylist[ED_IO][M_LOCAL].t_clrmask;
	    edtty.d_lb |= ttylist[ED_IO][M_LOCAL].t_setmask;
	}
# endif /* TERMIO || POSIX */
	{
	    extern int didsetty;
	    int i;

	    tty_getchar(&tstty, ttychars[TS_IO]);
	    /*
	     * Check if the user made any changes.
	     * If he did, then propagate the changes to the
	     * edit and execute data structures.
	     */
	    for (i = 0; i < C_NCC; i++)
		if (ttychars[TS_IO][i] != ttychars[EX_IO][i])
		    break;
		
	    if (i != C_NCC || didsetty) {
		didsetty = 0;
		/*
		 * Propagate changes only to the unprotected chars
		 * that have been modified just now.
		 */
		for (i = 0; i < C_NCC; i++) {
		    if (!((ttylist[ED_IO][M_CHAR].t_setmask & C_SH(i))) &&
			(ttychars[TS_IO][i] != ttychars[EX_IO][i]))
			ttychars[ED_IO][i] = ttychars[TS_IO][i];
		    if (ttylist[ED_IO][M_CHAR].t_clrmask & C_SH(i))
			ttychars[ED_IO][i] = vdisable;
		}
		tty_setchar(&edtty, ttychars[ED_IO]);

		for (i = 0; i < C_NCC; i++) {
		    if (!((ttylist[EX_IO][M_CHAR].t_setmask & C_SH(i))) &&
			(ttychars[TS_IO][i] != ttychars[EX_IO][i]))
			ttychars[EX_IO][i] = ttychars[TS_IO][i];
		    if (ttylist[EX_IO][M_CHAR].t_clrmask & C_SH(i))
			ttychars[EX_IO][i] = vdisable;
		}
		tty_setchar(&extty, ttychars[EX_IO]);
	    }

	}
    }
    if (tty_setty(SHTTY, &edtty) == -1) {
#ifdef DEBUG_TTY
	xprintf("Rawmode: tty_setty: %s\n", strerror(errno));
#endif /* DEBUG_TTY */
	return -1;
    }
    Tty_raw_mode = 1;
    flush();			/* flush any buffered output */
    return (0);
}

int
Cookedmode()
{				/* set tty in normal setup */
    sigret_t(*orig_intr) ();

#ifdef _IBMR2
    tty_setdisc(SHTTY, EX_IO);
#endif /* _IBMR2 */

    if (!Tty_raw_mode)
	return (0);

    /* hold this for reseting tty */
#ifdef BSDSIGS
    orig_intr = (sigret_t (*)()) signal(SIGINT, SIG_IGN);
#else
    orig_intr = (sigret_t (*)()) sigset(SIGINT, SIG_IGN);
#endif /* BSDSIGS */
    if (tty_setty(SHTTY, &extty) == -1) {
#ifdef DEBUG_TTY
	xprintf("Cookedmode: tty_setty: %s\n", strerror(errno));
#endif /* DEBUG_TTY */
	return -1;
    }
    Tty_raw_mode = 0;
#ifdef BSDSIGS
    (void) signal(SIGINT, orig_intr);	/* take these again */
#else
    (void) sigset(SIGINT, orig_intr);	/* take these again */
#endif /* BSDSIGS */
    return (0);
}

void
ResetInLine()
{
    Cursor = InputBuf;		/* reset cursor */
    LastChar = InputBuf;
    InputLim = &InputBuf[INBUFSIZE - 2];
    Mark = InputBuf;
    MetaNext = 0;
    CurrentKeyMap = CcKeyMap;
    AltKeyMap = 0;
    Hist_num = 0;
    DoingArg = 0;
    Argument = 1;
#ifdef notdef
    LastKill = KillBuf;		/* no kill buffer */
#endif 
    LastCmd = F_UNASSIGNED;	/* previous command executed */
    MacroLvl = -1;		/* no currently active macros */
}

static Char *Input_Line = NULL;
int
Load_input_line()
{
    long    chrs = 0;

    if (Input_Line)
	xfree((ptr_t) Input_Line);
    Input_Line = NULL;

    if (Tty_raw_mode)
	return 0;

#ifdef FIONREAD
    (void) ioctl(SHIN, FIONREAD, &chrs);
    if (chrs > 0) {
	char    buf[BUFSIZE];

	chrs = read(SHIN, buf, (size_t) min(chrs, BUFSIZE - 1));
	if (chrs > 0) {
	    buf[chrs] = '\0';
	    Input_Line = Strsave(str2short(buf));
	    PushMacro(Input_Line);
	}
    }
#endif  /* FIONREAD */
    return chrs > 0;
}

/*
 * Bugfix (in Swedish) by:
 * Johan Widen
 * SICS, PO Box 1263, S-163 13 SPANGA, SWEDEN
 * {mcvax,munnari,cernvax,diku,inria,prlb2,penet,ukc,unido}!enea!sics.se!jw
 * Internet: jw@sics.se
 *
 * (via Hans J Albertsson (thanks))
 */
void
QuoteModeOn()
{
    if (MacroLvl >= 0)
	return;

    qutty = edtty;

#if defined(TERMIO) || defined(POSIX)
    qutty.d_t.c_iflag &= ~ttylist[QU_IO][M_INPUT].t_clrmask;
    qutty.d_t.c_iflag |=  ttylist[QU_IO][M_INPUT].t_setmask;

    qutty.d_t.c_oflag &= ~ttylist[QU_IO][M_OUTPUT].t_clrmask;
    qutty.d_t.c_oflag |=  ttylist[QU_IO][M_OUTPUT].t_setmask;

    qutty.d_t.c_cflag &= ~ttylist[QU_IO][M_CONTROL].t_clrmask;
    qutty.d_t.c_cflag |=  ttylist[QU_IO][M_CONTROL].t_setmask;

    qutty.d_t.c_lflag &= ~ttylist[QU_IO][M_LINED].t_clrmask;
    qutty.d_t.c_lflag |=  ttylist[QU_IO][M_LINED].t_setmask;
#else /* GSTTY */
    qutty.d_t.sg_flags &= ~ttylist[QU_IO][M_CONTROL].t_clrmask;
    qutty.d_t.sg_flags |= ttylist[QU_IO][M_CONTROL].t_setmask;
    qutty.d_lb &= ~ttylist[QU_IO][M_LOCAL].t_clrmask;
    qutty.d_lb |= ttylist[QU_IO][M_LOCAL].t_setmask;

#endif /* TERMIO || POSIX */
    if (tty_setty(SHTTY, &qutty) == -1) {
#ifdef DEBUG_TTY
	xprintf("QuoteModeOn: tty_setty: %s\n", strerror(errno));
#endif /* DEBUG_TTY */
	return;
    }
    Tty_quote_mode = 1;
    return;
}

void
QuoteModeOff()
{
    if (!Tty_quote_mode)
	return;
    Tty_quote_mode = 0;
    if (tty_setty(SHTTY, &edtty) == -1) {
#ifdef DEBUG_TTY
	xprintf("QuoteModeOff: tty_setty: %s\n", strerror(errno));
#endif /* DEBUG_TTY */
	return;
    }
    return;
}
