#ifndef lint
static char *rcsid_Bindings_c = "$Header: Bindings.c,v 10.3 86/02/01 16:22:39 tony Rel $";
#endif	lint

/************************************************************************
 *									*
 *			Copyright (c) 1986 by				*
 *		Digital Equipment Corporation, Maynard, MA		*
 *		         All Rights Reserved.				*
 *									*
 *	Permission to use, copy, modify, and distribute this software	*
 *	and its documentation is hereby granted only to licensees of 	*
 *	The Regents of the University of California pursuant to their	*
 *	license agreement for the Berkeley Software Distribution 	*
 *	provided that the following appears on all copies.		*
 *									*
 *            "LICENSED FROM DIGITAL EQUIPMENT CORPORATION		*
 *                      COPYRIGHT (C) 1986				*	
 *                 DIGITAL EQUIPMENT CORPORATION			*
 *                         MAYNARD, MA					*
 *                     ALL RIGHTS RESERVED.				*
 *									*
 *      THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT	* 
 *	NOTICE AND SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL	*
 *	EQUIPMENT CORPORATION.  DIGITAL MAKES NO REPRESENTATIONS	*
 *	ABOUT SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE. IT IS	*
 *	SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.		*
 *									*	
 * 	IF THE UNIVERSITY OF CALIFORNIA OR ITS LICENSEES MODIFY 	*	
 *	THE SOFTWARE IN A MANNER CREATING DERIVATIVE COPYRIGHT 		*	
 *	RIGHTS APPROPRIATE COPYRIGHT LEGENDS MAY BE PLACED ON THE	*
 *	DERIVATIVE WORK IN ADDITION TO THAT SET FORTH ABOVE."		*	
 *									*
 ************************************************************************/
 

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
