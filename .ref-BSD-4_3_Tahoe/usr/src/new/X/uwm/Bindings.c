#ifndef lint
static char *rcsid_Bindings_c = "$Header: Bindings.c,v 10.4 86/11/19 16:23:21 jg Rel $";
#endif	lint

/*
 *			COPYRIGHT 1985, 1986
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITIBILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting documentation,
 * and that the name of Digital Equipment Corporation not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission.
 *
 */

 

/*
 * MODIFICATION HISTORY
 *
 * 000 -- M. Gancarz, DEC Ultrix Engineering Group
 */

#ifndef lint
static char *sccsid = "@(#)Bindings.c	3.8	1/24/86";
#endif

/*
 * Bindings.c 	Default bindings for the X window manager 'uwm'
 *
 * Note: Any changes to this file should also be added to the file
 * /usr/new/lib/X/uwm/default.uwmrc to keep users informed as to the bindings
 * contained herein.
 */

char *DefaultBindings[] = {
"autoselect;delta=25;freeze;grid;hiconpad=5;hmenupad=6;iconfont=oldengssx",
"menufont=timrom12b;resizefont=9x15;viconpad=5;vmenupad=3;volume=7",
"f.menu=m::l d:\"WINDOW OPS\"",
"f.menu=m::m d:\"EXTENDED WINDOW OPS\"",
"f.move=m:w|i:r d",
"f.circleup=m:r:r d",
"menu=\"WINDOW OPS\" {",
"\"(De)Iconify\":f.iconify",
"Move:f.move",
"Resize:f.resize",
"Lower:f.lower",
"Raise:f.raise",
"}",
"menu=\"EXTENDED WINDOW OPS\"{",
"Create Window:!\"xterm &\"",
"Iconify at New Position:f.newiconify",
"Focus Keyboard on Window:f.focus",
"Freeze All Windows:f.pause",
"Unfreeze All Windows:f.continue",
"Circulate Windows Up:f.circleup",
"Circulate Windows Down:f.circledown",
"}",
0	/* Must NOT be removed. */
};
