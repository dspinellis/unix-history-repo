/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mark Nudleman.
 * 
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)version.c	5.1 (Berkeley) %G%";
#endif /* not lint */

/*
 *		less
 *	Copyright (c) 1984,1985  Mark Nudelman
 *
 *	This program may be freely used and/or modified, 
 *	with the following provisions:
 *	1. This notice and the above copyright notice must remain intact.
 *	2. Neither this program, nor any modification of it,
 *	   may be sold for profit without written consent of the author.
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
 * 	v23: Miscellanous changes and prettying up.	5/25/85   mark
 *		Posted to USENET.
 *	-----------------------------------------------------------------
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
 *		Posted to USENET.
 *	-----------------------------------------------------------------
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
 *		Posted to USENET.
 *	-----------------------------------------------------------------
 *	v49: Fixed bug: signal didn't clear mcc.	1/9/86    mark
 *	v50: Added ' (quote) to gomark.			1/15/86   mark
 *	v51: Added + cmd, fixed problem if first_cmd
 *	     fails, made g cmd sort of "work" on pipes
 *	     even if bof is no longer buffered.		1/16/86   mark
 *	v52: Made short files work better.		1/17/86   mark
 *	v53: Added -P option.				1/20/86   mark
 *	v54: Changed help to use HELPFILE.		1/20/86   mark
 *	v55: Messages work better if not tty output.	1/23/86   mark
 *	v56: Added -l option.				1/24/86   mark
 *	v57: Fixed -l to get confirmation before
 *	     overwriting an existing file.		1/31/86   mark
 *	v58: Added filename globbing.			8/28/86   mark
 *	v59: Fixed some bugs with very long filenames.	9/15/86   mark
 *	v60: Incorporated changes from Leith (Casey)
 *	     Leedom for boldface and -z option.		9/26/86   mark
 *	v61: Got rid of annoying repaints after ! cmd.	9/26/86   mark
 *		Posted to USENET.
 *	-----------------------------------------------------------------
 *	v62: Added is_directory(); change -z default to
 *	     -1 instead of 24; cat-and-exit if -e and
 *	     file is less than a screenful.		12/23/86  mark
 *	v63: Fixed bug in cat-and-exit if > 1 file.	1/8/87    mark
 *	v64: Changed puts/putstr, putc/putchr, 
 *	     getc/getchr to avoid name conflict with 
 *	     stdio functions.				1/12/87  mark
 *	v65: Allowed '-' command to change NUMBER
 *	     valued options (thanks to Gary Puckering)	1/26/87  mark
 *	v66: Fixed bug: prepaint should use force=1.	2/13/87  mark
 *	v67: Added !! and % expansion to ! command.	2/24/87  mark
 *	v68: Added SIGWINCH and TIOCGWINSZ support;
 *	     changed is_directory to bad_file.
 *	     (thanks to J. Robert Ward)			2/25/87  mark
 *	v69: Added SIGWIND and WIOCGETD (for Unix PC).	2/25/87  mark
 *	v70: Changed help cmd from 'h' to 'H'; better 
 *	     error msgs in bad_file, errno_message.	3/13/87  mark
 *	v71: Changed -p to -c, made triple -c/-C
 *	     for clear-eol like more's -c.		5/11/87  mark
 *	v72: Added -E, -L, use $SHELL in lsystem().	6/26/87  mark
 *	     (thanks to Steve Spearman)
 *	v73: Allow Examine "#" for previous file.	6/26/87  mark
 *		Posted to USENET 8/25/87.
 *	-----------------------------------------------------------------
 *	v74: Fix conflict in EOF symbol with stdio.h,	9/18/87  mark
 *	     Make os.c more portable to BSD.
 *	v75: Fix problems in get_term (thanks to 	9/23/87  mark
 *	     Paul Eggert); new backwards scrolling in
 *	     jump_loc (thanks to Marion Hakanson).
 *	v76: Added -i flag; allow single "!" to		9/23/87  mark
 *	     invoke a shell (thanks to Franco Barber).
 *	v77: Added -n flag and line number support.	9/24/87  mark
 *	v78: Fixed problem with prompts longer than	9/25/87  mark
 *	     the screen width.	
 *	v79: Added the _ command.			9/29/87  mark
 *	v80: Allow signal to break out of linenum scan.	10/6/87  mark
 *	v81: Allow -b to be changed from within less.	10/6/87  mark
 *	v82: Add cmd_decode to use a table for key	10/7/87  mark
 *	     binding (thanks to David Nason).
 *	v83: Allow .less file for user-defined keys.	10/9/87  mark
 *	v84: Fix -e/-E problems (thanks to Felix Lee).	10/11/87 mark
 *	v85: Search now keeps track of line numbers.	10/15/87 mark
 *	v86: Added -B option and autobuf; fixed		10/20/87 mark
 *	     "pipe error" bug.
 *	v87: Fix bug re BSD signals while reading file.	3/1/88   mark
 *	v88: Use new format for -P option (thanks to	3/12/88  mark
 *	     der Mouse), allow "+-c" without message,
 *	     fix bug re BSD hangup.
 *	v89: Turn off line numbers if linenum scan	3/18/88  mark
 *	     is interrupted.
 *	v90: Allow -P from within less.			3/30/88  mark
 *	v91: Added tags file support (new -t option)	3/30/88  mark
 *	     (thanks to Brian Campbell).
 *	v92: Added -+option syntax.			4/4/88   mark
 *	v93: Add support for slow input (thanks to	4/11/88  mark
 *	     Joe Orost & apologies for taking almost
 *	     3 years to get this in!)
 *	v94: Redo reading/signal stuff.			4/11/88  mark
 *	v95: Repaint screen better after signal.	4/20/88  mark
 *	v96: Add /! and ?! commands.			4/21/88  mark
 *	v97: Allow -l/-L from within less.		5/17/88  mark
 *	     Eliminate some static arrays (use calloc).
 */

char version[] = "@(#) less  version 97";
