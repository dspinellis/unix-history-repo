/* $XConsortium: Sunkeysym.h,v 1.1 91/06/19 12:03:42 rws Exp $ */

/************************************************************
Copyright 1991 by Sun Microsystems, Inc. Mountain View, CA.

                    All Rights Reserved

Permission  to  use,  copy,  modify,  and  distribute   this
software  and  its documentation for any purpose and without
fee is hereby granted, provided that the above copyright no-
tice  appear  in all copies and that both that copyright no-
tice and this permission notice appear in  supporting  docu-
mentation,  and  that the names of Sun or MIT not be used in
advertising or publicity pertaining to distribution  of  the
software  without specific prior written permission. Sun and
MIT make  no  representations about the suitability  of this
software for any purpose. It is provided "as is" without any
express or implied warranty.

SUN DISCLAIMS ALL WARRANTIES WITH REGARD TO  THIS  SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FIT-
NESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SUN BE  LI-
ABLE  FOR  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,  DATA  OR
PROFITS,  WHETHER  IN  AN  ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.

***********************************************************/

/*
 * Floating Accent
 */

#define	SunXK_FA_Grave		0x1005FF00
#define	SunXK_FA_Circum		0x1005FF01
#define	SunXK_FA_Tilde		0x1005FF02

/*
 * Miscellaneous Functions
 */

#define	SunXK_F36		0x1005FF10	/* Labeled F11 */
#define	SunXK_F37		0x1005FF11	/* Labeled F12 */

#define SunXK_Sys_Req   	0x1005FF60
#define SunXK_Print_Screen	0x0000FF61	/* Same as XK_Print */

/*
 * International & Multi-Key Character Composition
 */

#define SunXK_Compose		0x0000FF20	/* Same as XK_Multi_key */
#define SunXK_AltGraph		0x0000FF7E	/* Same as XK_Mode_switch */

/*
 * Cursor Control
 */

#define SunXK_PageUp		0x0000FF55 	/* Same as XK_Prior */
#define SunXK_PageDown		0x0000FF56	/* Same as XK_Next */

/*
 * Open Look Functions
 */

#define SunXK_Undo		0x0000FF65	/* Same as XK_Undo */
#define SunXK_Again		0x0000FF66	/* Same as XK_Redo */
#define SunXK_Find		0x0000FF68	/* Same as XK_Find */
#define SunXK_Stop		0x0000FF69	/* Same as XK_Cancel */
#define SunXK_Props		0x1005FF70
#define SunXK_Front		0x1005FF71
#define SunXK_Copy		0x1005FF72
#define SunXK_Open		0x1005FF73
#define SunXK_Paste		0x1005FF74
#define SunXK_Cut		0x1005FF75
