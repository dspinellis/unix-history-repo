/* Copyright (c) 1991
 *      Juergen Weigert (jnweiger@immd4.informatik.uni-erlangen.de)
 *      Michael Schroeder (mlschroe@immd4.informatik.uni-erlangen.de)
 * Copyright (c) 1987 Oliver Laumann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program (see the file COPYING); if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Noteworthy contributors to screen's design and implementation:
 *	Wayne Davison (davison@borland.com)
 *	Patrick Wolfe (pat@kai.com, kailand!pat)
 *	Bart Schaefer (schaefer@cse.ogi.edu)
 *	Nathan Glasser (nathan@brokaw.lcs.mit.edu)
 *	Larry W. Virden (lwv27%cas.BITNET@CUNYVM.CUNY.Edu)
 *	Howard Chu (hyc@hanauma.jpl.nasa.gov)
 *	Tim MacKenzie (tym@dibbler.cs.monash.edu.au)
 *	Markku Jarvinen (mta@{cc,cs,ee}.tut.fi)
 *	Marc Boucher (marc@CAM.ORG)
 *
 ****************************************************************
 */

/****************************************************************
 * $Header: /src.stand/pub/utilities/screen/3.2/RCS/patchlevel.h,v 1.2 92/02/03 02:27:59 jnweiger Exp $ 
 *
 * patchlevel.h: Our life story.
 *   8.7.91 -- 3.00.01 -wipe and a 'setenv TERM dumb' bugfix.
 *  17.7.91 -- 3.00.02 another patchlevel by Wayne Davison
 *  31.7.91 -- 3.00.03 E0, S0, C0 for flexible semi-graphics, nonblocking 
 *                     window title input and 'C-a :' command input.
 *  10.8.91 -- 3.00.04 scrollback, markkeys and some bugfixes.
 *  13.8.91 -- 3.00.05 mark routine improved, ansi prototypes added.
 *  20.8.91 -- 3.00.06 screen -h, faster GotoPos in overlay, termcap %.
 *                     instead of %c
 *  28.8.91 -- 3.00.07 environment variable support. security. terminfo.
 *                     pyramid and ultrix support.
 *  07.9.91 -- 3.00.99 secopen(), MIPS support, SVR4 support.
 *  09.9.91 -- 3.01.00 backspace bug fixed.
 * 03.10.91 -- 3.01.01 ansi.c: null-ptr fixed, CLS now saves to scrollback.
 *                     Using setresuid on hpux. Memory leak fixed.
 *		       Better GotoPos(). Support for IC. Another resize bug.
 *                     Detach() w/o fore crashed. -T and -A(dapt) option.
 *                     GNU copyleft.
 * 19.12.91 -- 3.01.02 flow now really automatic (autoflow killed).
 *		       7 bit restriction removed from WriteString().
 * 09.01.92 -- 3.01.03 flow reattach bug fixed. VDISCARD bug fixed.
 * 13.01.92 -- 3.01.04 new flow concept: ^Af toggles now three states
 * 21.01.92 -- 3.01.05 '^A:screen 11' bug fixed. aflag in DoScreen().
 *                     Some code cleanup. attach_tty and display_tty[]
 *                     added.
 * 26.01.92 -- 3.01.06 apollo support, "hardcopy_append on", "bufferfile", 
 *                     SECURITY PROBLEM cleared..
 * 28.01.92 -- 3.01.07 screen after su allowed. Pid became part of 
 *                     SockName. sysvish 14 character restriction considered.
 * 31.01.92 -- 3.02.00 Ultrix port, Irix 3.3 SGI port, shadow pw support,
 *                     data loss on stdin overflow fixed. "refresh off".
 */

#define ORIGIN "FAU"
#define REV 3
#define VERS 2
#define PATCHLEVEL 0
#define DATE "01/31/92"
#define STATE ""
