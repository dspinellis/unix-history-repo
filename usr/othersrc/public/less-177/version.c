/*
 *		less
 *	Copyright (c) 1984,1985,1989  Mark Nudelman
 *
 *	This program may be freely used and/or modified, 
 *	with the following provisions:
 *	1. This notice and the above copyright notice must remain intact.
 *	2. Neither this program, nor any modification of it,
 *	   may be sold for profit without written consent of the author.
 *
 *	 ---------------------------------------------------------------
 *	|  Special note to the person who ported "less" to the Amiga:	|
 *	|  If you're going to be vain enough to splash your name on	|
 *	|  the screen every time someone runs less, you might at	|
 *	|  least credit the author.					|
 *	 ---------------------------------------------------------------
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
 *		Posted to USENET.
 *	-----------------------------------------------------------------
 *	v98: Fix incorrect calloc call; uninitialized	10/14/88 mark
 *	     var in exec_mca; core dump on unknown TERM.
 *	     Make v cmd work if past last line of file.
 *	     Fix some signal bugs.
 *	v99: Allow space between -X and string,		10/29/88 mark
 *	     when X is a string-valued option.
 *	v100: Fix globbing bug when $SHELL not set;	1/5/89   mark
 *	      allow spaces after -t command.
 *	v101: Fix problem with long (truncated) lines	1/6/89   mark
 *	      in tags file (thanks to Neil Dixon).
 *	v102: Fix bug with E# when no prev file;	1/6/89   mark
 *	      allow spaces after -l command.
 *	v103: Add -N, -f and -? options.  Add z and w	3/14/89  mark
 *	      commands.  Add %L for prompt strings.
 *	v104: Added EDITPROTO.				3/16/89  mark
 *	v105: Fix bug in find_linenum which cached	3/20/89  mark
 *	      incorrectly on long lines.
 *	v106: Added -k option and multiple lesskey      3/31/89  mark
 *	      files.
 *	v107: Add 8-bit char support and -g option.	4/27/89  mark
 *	      Split option code into 3 files.
 *	v108: Allocate position table dynamically       5/5/89   mark
 *	      (thanks to Paul Eggert); change % command
 *	      from "percent" to vi-style brace finder.
 *	v109: Added ESC-% command, split prim.c.	5/10/89  mark
 *	v110: Fixed bug in + option; fixed repaint bug	5/24/89  mark
 *	      under Sun windows (thanks to Paul Eggert).
 *	v111: Generalized # and % expansion; use 	5/25/89  mark
 *	      calloc for some error messages.
 *	v112: Get rid of ESC-%, add {}()[] commands.	5/30/89  mark
 *	v113: Optimize lseeks (thanks to Paul Eggert).	5/31/89  mark
 *	v114: Added ESC-/ and ESC-/! commands.		7/25/89  mark
 *	v115: Added ESC-n command.			7/26/89  mark
 *	v116: Added find_pos to optimize g command.	7/31/89  mark
 *	v117: Change -f option to -r.			8/1/89   mark
 *	v118: Save positions for all previous files,	8/2/89   mark
 *	      not just the immediately previous one.
 *	v119: Save marks across file boundaries.	8/7/89   mark
 *	      Add file handle stuff.
 *	v120: Add :ta command.				8/11/89  mark
 *	v121: Add -f option.				8/16/89  mark
 *	v122: Fix performance with many buffers.	8/30/89  mark
 *	v123: Verbose prompts for string options.	8/31/89  mark
 *		Posted beta to USENET.
 *	-----------------------------------------------------------------
 *	v124: Reorganize search commands,		9/18/89  mark
 *	      N = rev, ESC-n = span, add ESC-N.
 *	v125: Fix tab bug (thanks to Alex Liu).		9/18/89  mark
 *	      Fix EOF bug when both -w and -c.
 *	v126: Add -j option.				10/25/89 mark
 *	v127: Fix problems with blank lines before BOF.	10/27/89 mark
 *	v128: Add %bj, etc. to prompt strings.		10/27/89 mark
 *	v129: Add -+,-- commands; add set-option and	11/3/89  mark
 *	      unset-option to lesskey.
 *	v130: Generalize A_EXTRA to string, remove	11/6/89  mark
 *	      set-option, unset-option from lesskey.
 *	v131: Changed name of EDITPROTO to LESSEDIT.	11/7/89  mark
 *	v132: Allow editing of command prefix.		11/8/89  mark
 *	v133: Add -y option (thanks to Jeff Sullivan).	11/16/89 mark
 *	v134: Glob filenames in the -l command.		12/1/89  mark
 *	v135: Combined {}()[] commands into one, and	12/5/89  mark
 *	      added ESC-^F and ESC-^B commands.
 *	v136: Added -S, -R flags.  Added | command.	1/20/90  mark
 *	      Added warning for binary files. (thanks 
 *	      to Richard Brittain and J. Sullivan).
 *	v137: Rewrote horrible pappend code.		1/21/90  mark
 *	      Added * notation for hi-bit chars.
 *	v138: Fix magic cookie terminal handling.	1/24/90  mark
 *	      Get rid of "cleanup" loop in ch_get.
 *	v139: Added MSDOS support.  (many thanks	1/27/90  mark
 *	      to Richard Brittain).
 *	v140: Editing a new file adds it to the		2/7/90   mark
 *	      command line list.
 *	v141: Add edit_list for editing >1 file.	2/8/90   mark
 *	v142: Add :x command.				2/10/90  mark
 *	v143: Add * and @ modifies to search cmds.	2/11/90  mark
 *	      Change ESC-/ cmd from /@* to / *.
 *	v144: Messed around with ch_zero; 		3/1/90   mark
 *	      no real change.
 *	v145: Added -R and -v/-V for MSDOS;		3/2/90   mark
 *	      renamed FILENAME to avoid conflict.
 *	v146: Pull cmdbuf functions out of command.c	3/5/90   mark
 *	v147: Implement ?@; fix multi-file edit bugs.	3/7/90   mark
 *	v148: Fixed bug in :e<file> then :e#.		3/29/90  mark
 *	v149: Change error,ierror,query to use PARG.	4/3/90   mark
 *	v150: Add LESS_CHARSET, LESS_CHARDEF.		4/6/90   mark
 *	v151: Remove -g option; clean up ispipe.	4/13/90  mark
 *	v152: lsystem() closes input file, for		4/14/90  mark
 *	      editors which require exclusive open.
 *	v153: Fix bug if SHELL unset; 			4/18/90  mark
 *	      fix bug in overstrike control char.
 *	v154: Output to fd 2 via buffer.		4/25/90  mark
 *	v155: Ignore -i if uppercase in pattern		4/30/90  mark
 *	      (thanks to Michael Rendell.)
 *	v156: Remove scroll limits in forw() & back();	5/3/90   mark
 *	      causes problems with -c.
 *	v157: Forward search starts at next real line	5/4/90   mark
 *	      (not screen line) after jump target.
 *	v158: Added F command.				6/14/90  mark
 *	v159: Fix bug in exiting: output not flushed.	7/29/90  mark
 *	v160: Clear screen before initial output w/ -c.	7/29/90  mark
 *	v161: Add -T flag.				7/29/90  mark
 *	v162: Fix bug with +F on command line.		8/14/90  mark
 *	v163: Added LESSBINFMT variable.		8/21/90  mark
 *	v164: Added -p, LINES, COLUMNS and		9/5/90   mark
 *	      unset mark ' == BOF, for 1003.2 D5.
 *	v165: At EOF with -c set, don't display empty	9/6/90   mark
 *	      screen when try to page forward.
 *	v166: Fix G when final line in file wraps.	9/6/90   mark
 *	v167: Translate CR/LF -> LF for 1003.2.		9/11/90  mark
 *	v168: Return to curr file if "tag not found".	9/13/90  mark
 *	v169: G goes to EOF even if file has grown.	12/12/90 mark
 *	v170: Add optimization for BSD _setjmp;		1/17/91  mark
 *	      fix #include ioctl.h TERMIO problem.
 *	      (thanks to Paul Eggert)
 *		Posted to USENET.
 *	-----------------------------------------------------------------
 *	v171: Fix -? bug in get_filename.		3/6/91    mark
 *	v172: Fix G bug in empty file.			3/15/91   mark
 *	      Fix bug with ?\n and -i and uppercase
 *	      pattern at EOF!
 *	      (thanks to Paul Eggert)
 *	v173: Change N cmd to not permanently change	3/17/91   mark
 *	      direction. (thanks to Brian Matthews)
 *	v174: Fix bug with namelogfile not getting	3/18/91   mark
 *	      cleared when change files.
 *	v175: Fix bug with ++cmd on command line.	3/18/91   mark
 *	      (thanks to Jim Meyering)
 *	v176: Change | to not force current screen,	4/2/91    mark
 *	      include marked line, start/end from 
 *	      top of screen.  Improve search speed.
 *	      (thanks to Don Mears)
 *	v177: Add LESSHELP variable.			4/2/91    mark
 *	      Fix bug with F command with -e.
 *	      Try /dev/tty for input before using fd 2.
 */

char version[] = "@(#) less  version 177";
