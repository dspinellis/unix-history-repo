/* Included in MH.6 distribution by permission of Mark Nudelman,
   16 September 1985. */

/*
 *		less
 *	Copyright (c) 1984,1985  Mark Nudelman
 *
 *	This program may be freely used and/or modified, 
 *	with the following provisions:
 *	1. This notice and the above copyright notice must remain intact.
 *	2. Neither this program, nor any modification of it,
 *	   may not be sold for profit without written consent of the author.
 *
 *	-----------------------------------------------------------------
 *
 *	This program is a paginator similar to "more", 
 *	but allows you to move both forward and backward in the file.  
 *	Commands are based on "more" and "vi".
 *
 *	----------------------- CHANGES ---------------------------------
 *
 *	    Allowed use on standard input		1/29/84   markn
 *	    Added E, N, P commands			2/1/84    markn
 *	    Added '=' command, 'stop' signal handling	4/17/84   markn
 *	    Added line folding				4/20/84   markn
 *	v2: Fixed '=' command to use BOTTOM_PLUS_ONE, 
 *	    instead of TOP, added 'p' & 'v' commands	4/27/84   markn
 *	v3: Added -m and -t options, '-' command	5/3/84    markn
 *	v4: Added LESS environment variable		5/3/84    markn
 *	v5: New comments, fixed '-' command slightly	5/3/84    markn
 *	v6: Added -Q, visual bell			5/15/84   markn
 *	v7: Fixed jump_back(n) bug: n should count real
 *	    lines, not folded lines.  Also allow number
 *	    on G command.				5/24/84   markn
 *	v8: Re-do -q and -Q commands			5/30/84   markn
 *	v9: Added "+<cmd>" argument			9/25/84   markn
 *	v10: Fixed bug in -b<n> argument processing	10/10/84  markn
 *	v11: Made error() ring bell if \n not entered.	10/18/84  markn
 *	-----------------------------------------------------------------
 *	v12: Reorganized signal handling and made
 *	     portable to 4.2bsd.			2/13/85   mark
 *	v13: Reword error message for '-' command.	2/16/85   mark
 *	v14: Added -bf and -bp variants of -b.		2/22/85   mark
 *	v15: Miscellaneous changes.			2/25/85   mark
 *	v16: Added -u flag for backspace processing.	3/13/85   mark
 *	v17: Added j and k commands, 
 *		changed -t default.			4/13/85   mark
 *	v18: Rewrote signal handling code.		4/20/85   mark
 *	v19: Got rid of "verbose" eq_message().		5/2/85    mark
 *	     Made search() scroll in some cases.
 *	v20: Fixed screen.c ioctls for System V.	5/21/85   mark
 *	v21: Fixed some first_cmd bugs.			5/23/85   mark
 *	v22: Added support for no RECOMP nor REGCMP.	5/24/85   mark
 *	v23: Miscellanous changes and prettying up.	5/25/85   mark
 *      v24: Added ti,te terminal init & de-init       6/3/85 Mike Kersenbrock
 *	v25: Added -U flag, standout mode underlining.	6/8/85    mark
 *	v26: Added -M flag.				6/9/85    mark
 *	     Use underline termcap (us) if it exists.
 *	v27: Renamed some variables to make unique in	6/15/85   mark
 *	     6 chars.  Minor fix to -m.
 *	v28: Fixed right margin bug.			6/28/85   mark
 *	v29: Incorporated M.Rose's changes to signal.c	6/28/85   mark
 *	v30: Fixed stupid bug in argument processing.	6/29/85   mark
 *	v31: Added -p flag, changed repaint algorithm.  7/15/85   mark
 *	     Added kludge for magic cookie terminals.
 *	v32: Added cat_file if output not a tty.	7/16/85   mark
 *	v33: Added -e flag and EDITOR.			7/23/85   mark
 *	v34: Added -s flag.				7/26/85   mark
 *	v35: Rewrote option handling; added option.c.	7/27/85   mark
 *	v36: Fixed -e flag to work if not last file.	7/29/85   mark
 *	v37: Added -x flag.				8/10/85   mark
 *	v38: Changed prompting; created prompt.c.	8/19/85   mark
 *	v39: (Not -p) does not initially clear screen.	8/24/85   mark
 *	v40: Added "skipping" indicator in forw().	8/26/85   mark
 *	v41: ONLY_RETURN, control char commands,	9/17/85   mark
 *	     faster search, other minor fixes.
 *	v42: Added ++ command line syntax;		9/25/85   mark
 *	     ch_fsize for pipes.
 *	v43: Added -h flag, changed prim.c algorithms.	10/15/85  mark
 *	v44: Made END print in all cases of eof;	10/16/85  mark
 *	     ignore SIGTTOU after receiving SIGTSTP.
 *	v45: Never print backspaces unless -u.		10/16/85  mark
 *	v46: Backwards scroll in jump_loc.		10/24/85  mark
 *	v47: Fixed bug in edit(): *first_cmd==0		10/30/85  mark
 *	v48: Use TIOCSETN instead of TIOCSETP.		11/16/85  mark
 *	     Added marks (m and ' commands).
#ifdef	NRTC
 *	v48a: QUIT handling				9/11/85	  mtr
 *	v48b: carat_char a macro			9/11/85	  mtr
#endif	NRTC
 *	-----------------------------------------------------------------
 */

#ifndef	NRTC
char version[] = "@(#) less  version 48";
#else	NRTC
char version[] = "@(#) less  version 48b";
#endif	NRTC
