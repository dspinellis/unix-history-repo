/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/

#include        "Xmd.h"
#define XK_KATAKANA
#include	"keysym.h"
/*  The following two lines wer added to let this compile
 *  in the pure MIT tree
 */
#include	"HPkeysym.h"
#include	"ap_keysym.h"

#include	"X.h"			/* MUST come after above includes */
#include	"input.h"

#include <XHPlib.h>		/* for keymap ids */

#define	MIN_KEYCODE	8
/* This file was composed from the X10 hil_keymap.h by
 * Jack Palevich, HP-Labs
 */


    /* A keymap filled with NoSymbol is all 2 columns.
     * This will be used when the keyboard is unknown and is not in a
     *   reconized family.
     * Notes:
     *   I know NoSymbol is 0 and won't ever change so I can sleeze and let
     *     the compiler initialize the table.
     */
static KeySym null_keymap[2 * 0x80];


#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
static KeySym USASCIIMap[4*0x82] = {
 /* code values in comments at line end are actual value reported on HIL.
    REMEMBER, there is an offset of MIN_KEYCODE+2 applied to this table!
    The PS2 keyboard table begins at offset 0, the 46021A table begins with
    the third row. *./
	/* Extend Char Right -- a.k.a. Kanji? */	
	XK_Control_R,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x00 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x01 */
	XK_Meta_R,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x02 */
	XK_Meta_L,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x03 */
	XK_Shift_R,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4 */
	XK_Shift_L,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x5 */
	XK_Control_L,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x6 */
	XK_Break,		XK_Reset,		NoSymbol,	NoSymbol,	/* 0x7 */
	XK_KP_4,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x8 */
	XK_KP_8,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x9 */
	XK_KP_5,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0xa */
	XK_KP_9,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0xb */
	XK_KP_6,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0xc */
	XK_KP_7,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0xd */
	XK_KP_Separator,	NoSymbol,		NoSymbol,	NoSymbol,	/* 0xe */
	XK_KP_Enter,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0xf */
	XK_KP_1,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x10 */
	XK_KP_Divide,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x11 */
	XK_KP_2,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x12 */
	XK_KP_Add,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x13 */
	XK_KP_3,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x14 */
	XK_KP_Multiply,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x15 */
	XK_KP_0,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x16 */
	XK_KP_Subtract,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x17 */
	XK_B,			NoSymbol,		XK_block,	NoSymbol,	/* 0x18 */
	XK_V,			NoSymbol,		XK_section,	NoSymbol,	/* 0x19 */
	XK_C,			NoSymbol,		XK_ccedilla,	XK_Ccedilla,	/* 0x1a */
	XK_X,			NoSymbol,		XK_scaron,	XK_Scaron,	/* 0x1b */
	XK_Z,			NoSymbol,		XK_paragraph,	NoSymbol,	/* 0x1c */
/* Was Kanji Left.... */	
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x1d */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x1e */
	XK_Escape,		XK_Delete,		NoSymbol,	NoSymbol,	/* 0x1f */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x20 */
	XK_F10,  		XK_KP_F2,		NoSymbol,	NoSymbol,	/* 0x21 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x22 */
	XK_F11,  		XK_KP_F3,		NoSymbol,	NoSymbol,	/* 0x23 */
	XK_KP_Decimal,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x24 */
	XK_F9,   		XK_KP_F1,		NoSymbol,	NoSymbol,	/* 0x25 */
	XK_KP_Tab,		XK_KP_BackTab,		NoSymbol,	NoSymbol,	/* 0x26 */
	XK_F12,  		XK_KP_F4,		NoSymbol,	NoSymbol,	/* 0x27 */
	XK_H,			NoSymbol,		XK_yen,		NoSymbol,	/* 0x28 */
	XK_G,			NoSymbol,		XK_currency,	NoSymbol,	/* 0x29 */
	XK_F,			NoSymbol,		XK_guilder,	NoSymbol,	/* 0x2a */
	XK_D,			NoSymbol,		XK_eth,		XK_Eth,		/* 0x2b */
	XK_S,			NoSymbol,		XK_ssharp,	NoSymbol,	/* 0x2c */
	XK_A,			NoSymbol,		XK_aring,	XK_Aring,	/* 0x2d */
	XK_Mode_switch,		NoSymbol,		NoSymbol,	XK_Mode_switch,	/* 0x2e */
	XK_Caps_Lock,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x2f */
	XK_U,			NoSymbol,		XK_mute_diaeresis,NoSymbol,	/* 0x30 */
	XK_Y,			NoSymbol,		XK_mute_asciicircum,NoSymbol,	/* 0x31 */
	XK_T,			NoSymbol,		XK_mute_grave,	NoSymbol,	/* 0x32 */
	XK_R,			NoSymbol,		XK_mute_acute,	NoSymbol,	/* 0x33 */
	XK_E,			NoSymbol,		XK_ae,		XK_AE,		/* 0x34 */
	XK_W,			NoSymbol,		XK_asciitilde,	NoSymbol,	/* 0x35 */
	XK_Q,			NoSymbol,		XK_periodcentered,		NoSymbol,	/* 0x36 */
	XK_Tab,			XK_BackTab,		NoSymbol,	NoSymbol,	/* 0x37 */
	XK_7,			XK_ampersand,		XK_backslash,	NoSymbol,	/* 0x38 */
	XK_6,			XK_asciicircum,		XK_asciicircum,	NoSymbol,	/* 0x39 */
	XK_5,			XK_percent,		XK_onehalf,	NoSymbol,	/* 0x3a */
	XK_4,			XK_dollar,		XK_onequarter,	XK_threequarters,	/* 0x3b */
	XK_3,			XK_numbersign,		XK_numbersign,	NoSymbol,	/* 0x3c */
	XK_2,			XK_at,			XK_at,		NoSymbol,	/* 0x3d */
	XK_1,			XK_exclam,		XK_exclamdown,	NoSymbol,	/* 0x3e */
	XK_quoteleft,		XK_asciitilde,		XK_guillemotleft,XK_guillemotright,/* 0x3f */
/* Was Mouse-L */	
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x40 */
/* Was Mouse-M */	
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x41 */
/* Was Mouse-R */	
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x42 */
/* Was 4 button puck */	
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x43 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x44 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x45 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x46 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x47 */
	XK_Menu,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x48 */
	XK_F4,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x49 */
	XK_F3,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4a */
	XK_F2,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4b */
	XK_F1,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4c */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4d */
/* Was 'Stop' */	
	XK_Cancel,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4e */
/* Was 'Enter' */	
	XK_Execute,		XK_Print,		NoSymbol,	NoSymbol,	/* 0x4f */
	XK_System,		XK_User,		NoSymbol,	NoSymbol,	/* 0x50 */
	XK_F5,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x51 */
	XK_F6,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x52 */
	XK_F7,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x53 */
	XK_F8,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x54 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x55 */
	XK_ClearLine,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x56 */
/* Was 'Clear Display' */	
	XK_Clear,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x57 */
	XK_8,			XK_asterisk,		XK_bracketleft,	XK_braceleft,	/* 0x58 */
	XK_9,			XK_parenleft,		XK_bracketright,XK_braceright,	/* 0x59 */
	XK_0,			XK_parenright,		XK_questiondown,NoSymbol,	/* 0x5a */
	XK_minus,		XK_underscore,		XK_longminus,	XK_macron,	/* 0x5b */
	XK_equal,		XK_plus,		XK_plusminus,	NoSymbol,	/* 0x5c */
	XK_BackSpace,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x5d */
	XK_InsertLine,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x5e */
	XK_DeleteLine,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x5f */
			
	XK_I,			NoSymbol,		XK_mute_asciitilde,NoSymbol,	/* 0x60 */
	XK_O,			NoSymbol,		XK_oslash,	XK_Ooblique,	/* 0x61 */
	XK_P,			NoSymbol,		XK_thorn,	XK_Thorn,	/* 0x62 */
	XK_bracketleft,		XK_braceleft,		XK_degree,	NoSymbol,	/* 0x63 */
	XK_bracketright,	XK_braceright,		XK_brokenbar,	NoSymbol,	/* 0x64 */
	XK_backslash,		XK_bar,			XK_mu,		NoSymbol,	/* 0x65 */
	
	/* HP special  might also be Insert */		
	XK_InsertChar,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x66 */
	XK_DeleteChar,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x67 */
	XK_J,			NoSymbol,		XK_dollar,	NoSymbol,	/* 0x68 */
	XK_K,			NoSymbol,		XK_cent,	NoSymbol,	/* 0x69 */
	XK_L,			NoSymbol,		XK_sterling,	NoSymbol,	/* 0x6a */
	XK_semicolon,		XK_colon,		XK_lira,	NoSymbol,	/* 0x6b */
	XK_quoteright,		XK_quotedbl,		XK_quoteleft,	XK_quoteright,	/* 0x6c */
	XK_Return,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x6d */
	XK_Home,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x6e */
	/* Prev */	
	XK_Prior,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x6f */
	
	XK_M,			NoSymbol,		XK_masculine,	NoSymbol,	/* 0x70 */
	XK_comma,		XK_less,		XK_less,	NoSymbol,	/* 0x71 */
	XK_period,		XK_greater,		XK_greater,	NoSymbol,	/* 0x72 */
	XK_slash,		XK_question,		XK_underscore,	NoSymbol,	/* 0x73 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x74 */
	XK_Select,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x75 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x76 */
	XK_Next,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x77 */
	XK_N,			NoSymbol,		XK_ordfeminine,	NoSymbol,	/* 0x78 */
	/* "Space  the final frontier..." */	
	XK_space,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x79 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7a */
	/* Kanji Right */	
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7b */
	
	XK_Left,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7c */
	XK_Down,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7d */
	XK_Up,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7e */
	XK_Right,		NoSymbol,		NoSymbol,	NoSymbol	/* 0x7f */
};



    /* This routine converts a hil keyboard id to the X keyboard id used to
     *   look up the keyboard in the /usr/lib/X11/XHPKeymaps file.
     * Notes:
     *   Modified from libXhp XHPGetHILandCvt().
     *   We used to just do some math to get the extended keyboard id but we
     *     got some signals crossed with the NLIO guys and some of the
     *     keyboards got misplaced in XHPKeymaps so we now have a look up
     *     table.  For the PS2 keyboards, we still use math.
     *   See the manual on using hp-hil devices with HP-UX for the keyboard
     *     nationality codes; they are the low order 6 bits of the device
     *     id; 0x1f is United States, so we'll subtract from 0x1f to give
     *     the U.S.  a keyId of zero; The PS2 keyboards have hil ids E0-FF.
     *   6 bits == a max of 64 different keyboards.  32 extended and 32 PS2.
     *   George says to use 7 bits:  HIL ids in the range A0-FF.
     *     A0-BF  Compressed keyboard.  Not used (yet).
     *     C0-DF  Extended (ITF) keyboard
     *     E0-FF  Standard keyboard.  We change to be the PS2.
     *   Map extended keyboards to key ids 0-31.  The unsupported keyboards
     *     are the ones with hard coded numbers.
     *   The apollo keyboards are 33-40 (I don't want to talk about it and
     *     hind sight is 20/20).
     *   Map PS2 keyboards to key ids 60-91.
     * WARNING
     *   ONLY call this for HIL keyed devices.  For other stuff (such as
     *     serial keyboards), use the named lookups.
     * Input:
     *   hil_id:
     * Returns:
     *   X keyboard id.
     *   If the hil_id is not a supported keyboard, return unsupported id.
     *   If the hil_id is not a keyboard, return KB_NULL.
     */
int hil_to_kbd_id(hil_id) int hil_id;
{
  int kbd_id;
  static short int key_tab[] = {
	31,			/* HIL=00h Undefined keyboard */
	30,			/* HIL=01h Undefined keyboard */
	KB_Japanese,		/* HIL=02h */
	KB_Swiss_French,	/* HIL=03h */
	29,			/* HIL=04h No keysym support for Portugues */
	28,			/* HIL=05h No keysym support for Arabic    */
	27,			/* HIL=06h No keysym support for Hebrew    */
	KB_Canada_English,	/* HIL=07h */
	26,			/* HIL=08h No keysym support for Turkish   */
	25,			/* HIL=09h No keysym support for Greek     */
	24,			/* HIL=0Ah No keysym support for Thai      */
	KB_Italian,		/* HIL=0Bh */
	KB_Korean,		/* HIL=0Ch */
	KB_Dutch,		/* HIL=0Dh */
	KB_Swedish,		/* HIL=0Eh */
	KB_German,		/* HIL=0Fh */
	KB_S_Chinese,		/* HIL=10h */
	KB_T_Chinese,		/* HIL=11h */
	KB_Swiss_French2,	/* HIL=12h */
	KB_Euro_Spanish,	/* HIL=13h */
	KB_Swiss_German2,	/* HIL=14h */
	KB_Belgian,		/* HIL=15h */
	KB_Finnish,		/* HIL=16h */
	KB_UK_English,		/* HIL=17h */
	KB_Canada_French,	/* HIL=18h */
	KB_Swiss_German,	/* HIL=19h */
	KB_Norwegian,		/* HIL=1Ah */
	KB_French,		/* HIL=1Bh */
	KB_Danish,		/* HIL=1Ch */
	KB_Katakana,		/* HIL=1Dh */
	KB_Latin_Spanish,	/* HIL=1Eh */
	KB_US_English,		/* HIL=1Fh */
    };

  if (hil_id == 0x30) return KB_BUTTON_BOX;

  if (hil_id == 0x5c) return KB_US_English;	/* Barcode reader */

  if (0xE0 <= hil_id && hil_id <= 0xFF)		/* PS2 keyboard: 0xE0 - 0xFF */
	return (91 - (hil_id - 0xE0));		/* 60 - 91 */

  if (0xC0 <= hil_id && hil_id <= 0xDF)		/* ITF keyboard: 0xC0 - 0xDF */
	return key_tab[hil_id & 0x1f];		/* 0 - 31 */

    /* Something unknown or not yet supported (such as a nonkbd device (like
     *   the ID module)).
     */
  return KB_NULL;
}

#endif	/* __hpux */

#if __apollo
#include "ap_keysym.h"		/* for the apXK_ keysyms */

static KeySym Apollo_NorthAmericanMap[] = {

 /* code values in comments at line end are actual value reported on HIL.
    REMEMBER, there is an offset of MIN_KEYCODE applied to this table! */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x00 */
	XK_Select,		XK_Insert,		NoSymbol,	NoSymbol,	/* 0x01 */
	apXK_LineDel,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x02 */
	apXK_CharDel,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x03 */
	XK_F10,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x04 */
	XK_F1,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x05 */
	XK_F2,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x06 */
	XK_F3,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x07 */
	XK_F4,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x08 */
	XK_F5,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x09 */
	XK_F6,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x0a */
	XK_F7,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x0b */
	XK_F8,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x0c */
	XK_F9,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x0d */
	XK_Redo,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x0e */
	apXK_Read,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x0f */
	apXK_Edit,		apXK_Save,		NoSymbol,	NoSymbol,	/* 0x10 */
	apXK_Exit,		XK_Cancel,		NoSymbol,	NoSymbol,	/* 0x11 */
	XK_Pause,		XK_Help,		NoSymbol,	NoSymbol,	/* 0x12 */
	apXK_Copy,		apXK_Cut,		NoSymbol,	NoSymbol,	/* 0x13 */
	apXK_Paste,		XK_Undo,		NoSymbol,	NoSymbol,	/* 0x14 */
	apXK_Grow,		apXK_Move,		NoSymbol,	NoSymbol,	/* 0x15 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x16 */
	XK_Escape,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x17 */
	XK_1,			XK_exclam,		NoSymbol,	NoSymbol,	/* 0x18 */
	XK_2,			XK_at,			NoSymbol,	NoSymbol,	/* 0x19 */
	XK_3,			XK_numbersign,		NoSymbol,	NoSymbol,	/* 0x1a */
	XK_4,			XK_dollar,		NoSymbol,	NoSymbol,	/* 0x1b */
	XK_5,			XK_percent,		NoSymbol,	NoSymbol,	/* 0x1c */
	XK_6,			XK_asciicircum,		NoSymbol,	NoSymbol,	/* 0x1d */
	XK_7,			XK_ampersand,		NoSymbol,	NoSymbol,	/* 0x1e */
	XK_8,			XK_asterisk,		NoSymbol,	NoSymbol,	/* 0x1f */
	XK_9,			XK_parenleft,		NoSymbol,	NoSymbol,	/* 0x20 */
	XK_0,			XK_parenright,		NoSymbol,	NoSymbol,	/* 0x21 */
	XK_minus,		XK_underscore,		NoSymbol,	NoSymbol,	/* 0x22 */
	XK_equal,		XK_plus,		NoSymbol,	NoSymbol,	/* 0x23 */
	XK_quoteleft,		XK_asciitilde,		NoSymbol,	NoSymbol,	/* 0x24 */
	XK_BackSpace,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x25 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x26 */
	apXK_LeftBar,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x27 */
	apXK_Cmd,		apXK_Shell,		NoSymbol,	NoSymbol,	/* 0x28 */
	apXK_RightBar,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x29 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x2a */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x2b */
	XK_Tab,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x2c */
	XK_Q,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x2d */
	XK_W,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x2e */
	XK_E,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x2f */
	XK_R,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x30 */
	XK_T,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x31 */
	XK_Y,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x32 */
	XK_U,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x33 */
	XK_I,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x34 */
	XK_O,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x35 */
	XK_P,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x36 */
	XK_bracketleft,		XK_braceleft,		NoSymbol,	NoSymbol,	/* 0x37 */
	XK_bracketright,	XK_braceright,		NoSymbol,	NoSymbol,	/* 0x38 */
	XK_Mode_switch,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x39 */
	XK_Delete,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x3a */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x3b */
	XK_KP_7,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x3c */
	XK_KP_8,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x3d */
	XK_KP_9,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x3e */
	XK_KP_Add,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x3f */
	apXK_LeftBox,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x40 */
	XK_Up,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x41 */
	apXK_RightBox,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x42 */
	XK_Control_L,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x43 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x44 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x45 */
	XK_A,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x46 */
	XK_S,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x47 */
	XK_D,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x48 */
	XK_F,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x49 */
	XK_G,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4a */
	XK_H,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4b */
	XK_J,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4c */
	XK_K,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4d */
	XK_L,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x4e */
	XK_semicolon,		XK_colon,		NoSymbol,	NoSymbol,	/* 0x4f */
	XK_quoteright,		XK_quotedbl,		NoSymbol,	NoSymbol,	/* 0x50 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x51 */
	XK_Return,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x52 */
	XK_backslash,		XK_bar,			NoSymbol,	NoSymbol,	/* 0x53 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x54 */
	XK_KP_4,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x55 */
	XK_KP_5,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x56 */
	XK_KP_6,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x57 */
	XK_KP_Subtract,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x58 */
	XK_Left,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x59 */
	XK_Next,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x5a */
	XK_Right,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x5b */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x5c */
	apXK_Repeat,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x5d */
	XK_Shift_L,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x5e */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x5f */
	XK_Z,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x60 */
	XK_X,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x61 */
	XK_C,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x62 */
	XK_V,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x63 */
	XK_B,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x64 */
	XK_N,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x65 */
	XK_M,			NoSymbol,		NoSymbol,	NoSymbol,	/* 0x66 */
	XK_comma,		XK_less,		NoSymbol,	NoSymbol,	/* 0x67 */
	XK_period,		XK_greater,		NoSymbol,	NoSymbol,	/* 0x68 */
	XK_slash,		XK_question,		NoSymbol,	NoSymbol,	/* 0x69 */
	XK_Shift_R,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x6a */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x6b */
	apXK_Pop,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x6c */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x6d */
	XK_KP_1,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x6e */
	XK_KP_2,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x6f */
	XK_KP_3,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x70 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x71 */
	apXK_UpBox,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x72 */
	XK_Down,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x73 */
	apXK_DownBox,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x74 */
	XK_Alt_L,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x75 */
	XK_space,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x76 */
	XK_Alt_R,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x77 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x78 */
	XK_KP_0,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x79 */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7a */
	XK_KP_Decimal,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7b */
	XK_KP_Enter,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7c */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7d */
	XK_Caps_Lock,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7e */
	NoSymbol,		NoSymbol,		NoSymbol,	NoSymbol,	/* 0x7f */
};
#endif	/* __apollo */

static KeySym LPFKMap[] = {
	XK_exclam,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x21 */
	XK_quotedbl,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x22 */
	XK_numbersign,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x23 */
	XK_dollar,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x24 */
	XK_percent,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x25 */
	XK_ampersand,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x26 */
	XK_quoteright,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x27 */
	XK_parenleft,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x28 */
	XK_parenright,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x29 */
	XK_asterisk,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x2a */
	XK_plus,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x2b */
	XK_comma,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x2c */
	XK_minus,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x2d */
	XK_period,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x2e */
	XK_slash,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x2f */
	XK_0,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x30 */
	XK_1,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x31 */
	XK_2,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x32 */
	XK_3,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x33 */
	XK_4,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x34 */
	XK_5,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x35 */
	XK_6,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x36 */
	XK_7,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x37 */
	XK_8,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x38 */
	XK_9,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x39 */
	XK_colon,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x3a */
	XK_semicolon,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x3b */
	XK_less,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x3c */
	XK_equal,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x3d */
	XK_greater,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x3e */
	XK_question,	NoSymbol,	NoSymbol,	NoSymbol,	/* 0x3f */
	XK_at,		NoSymbol,	NoSymbol,	NoSymbol,	/* 0x40 */
};

KeySymsRec LPFKKeySyms = {LPFKMap, 0x21, 0x40, 4};

   /* This structure has ONE default KeySymsRec per keyboard family per OS
    *   that is used in case we can't look up the correct one.
    * Notes:
    *    All key maps SHOULD start at MIN_KEYCODE (aka 0) except HP-UX,
    *      where the exteneded keyboard keymaps start at 2 (because the
    *      extended keyboards didn't generate keycodes 0 and 1).  The PS2
    *      keyboards generate keycodes starting at 0 so they need a
    *      different MIN_KEYCODE.
    *    These default keymaps are overwritten so they should contain at
    *      least enough entries so that the largest keytable will fit.  In
    *      most cases this means that last entry is 0x7f and the keymap size
    *      is max-entries x width == 0x80 x 4.
    */
static KeySymsRec DefaultKeySyms[] = {
    /*	map name 	minKeyCode		maxKC		      width */
#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
    &USASCIIMap[8],	(MIN_KEYCODE + 0x02),	(MIN_KEYCODE + 0x7F),	4,
    USASCIIMap,		(MIN_KEYCODE),		(MIN_KEYCODE + 0x7F),	4,
    null_keymap,	(MIN_KEYCODE),		(MIN_KEYCODE + 0x7F),	2,
#endif

#if __apollo
    Apollo_NorthAmericanMap,	(MIN_KEYCODE),	(MIN_KEYCODE + 0x7F),	4,
#endif
};

    /* Convert a X keyboard ID into a family.
     * Input: X keyboard ID
     * Returns:
     *   0 (ITF)
     *   1 (PS2)
     *   2 (null or unknown device)
     *   0 Apollo
     */
static int kbd_family(key_id)
{
  int n;

#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
  if ( 0 <= key_id && key_id <= 31)   n = 0;	/* ITF keyboard */
  else
    if (60 <= key_id && key_id <= 91) n = 1;	/* PS2 keyboard */
    else			      n = 2;	/* KB_NULL */
#endif

#if __apollo
  n = 0;
#endif

  return n;
}

    /* This routine is called to get a pointer to a KeySymRec so you can
     * overwrite the key table with the correct one for the keyboard
     * connected to the system.
     */
KeySymsRec *hpKeySyms(key_id)
{
  return &DefaultKeySyms[kbd_family(key_id)];
}

    /* This routine is called if the load-keytable-from-file routine fails
     * and you need a default KeySymRec.  I need this for HP-UX 'cause the
     * extended and PS2 keyboards have different min keycodes and I want to
     * use the US Ascii keymap as the default.
     */
KeySymsRec *hpDefaultKeySyms(key_id)
{
  return hpKeySyms(key_id);
}


/* ******************************************************************** */
/* ************************* Modifier Tables ************************** */
/* ******************************************************************** */

#define	cT	(ControlMask)
#define	sH	(ShiftMask)
#define	lK	(LockMask)
#define	mT	(Mod1Mask)

    /* A modmap with no modifiers.  To be used with null_keymap */
static CARD8 null_modmap[MAP_LENGTH];


    /* Shift table values up by 8.  This offset is necessary to reserve
     *   codes for mouse buttons.  Note last 8 entries of table are
     *   commented out to preserve length of table.
     * Note:  '#define MIN_KEYCODE 8' is above
     */

#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)

    /* This table is for the HP hil extended keyboards.
     * For the PS2 keyboards, the only difference is keycode 0.  I used to
     *   use the same table for both keyboard types (since keycode 0 can't
     *   be generated by the extended keyboard) but that caused problems
     *   with xmodmap.
     */
static CARD8 hil_modmap[MAP_LENGTH] = {
    0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  mT, mT, sH, sH, cT, 0,  0,  0,  0,  0,  0,  0,  0,  0, /* 00-0f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 10-1f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  mT, lK,/* 20-2f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 30-3f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 40-4f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 50-5f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 60-6f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 70-7f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 80-8f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 90-9f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* a0-af */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* b0-bf */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* c0-cf */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* d0-df */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* e0-ef */
    0,  0,  0,  0,  0,  0,  0,  0,/*0,  0,  0,  0,  0,  0,  0,  0, /* f0-ff */
};

    /* Same table as the hil_modmap but for PS2 keyboards.  Only
     *   difference is keycode 0 is a control key.
     */
static CARD8 PS2_modmap[MAP_LENGTH] = {
    0,  0,  0,  0,  0,  0,  0,  0,
    cT, 0,  mT, mT, sH, sH, cT, 0,  0,  0,  0,  0,  0,  0,  0,  0, /* 00-0f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 10-1f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  mT, lK,/* 20-2f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 30-3f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 40-4f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 50-5f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 60-6f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 70-7f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 80-8f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 90-9f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* a0-af */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* b0-bf */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* c0-cf */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* d0-df */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* e0-ef */
    0,  0,  0,  0,  0,  0,  0,  0,/*0,  0,  0,  0,  0,  0,  0,  0, /* f0-ff */
};

#endif

#ifdef __apollo

    /* This table is for the Apollo NA and MN keyboards. */
static CARD8 apollo_modmap[MAP_LENGTH] = {
    0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 00-0f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 10-1f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 20-2f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  mT, 0,  0,  0,  0,  0,  0, /* 30-3f */
    0,  0,  0,  cT, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 40-4f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  sH, 0, /* 50-5f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  sH, 0,  0,  0,  0,  0, /* 60-6f */
    0,  0,  0,  0,  0,  mT, 0, mT,  0,  0,  0,  0,  0,  0,  lK, 0, /* 70-7f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 80-8f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* 90-9f */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* a0-af */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* b0-bf */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* c0-cf */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* d0-df */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, /* e0-ef */
    0,  0,  0,  0,  0,  0,  0,  0,/*0,  0,  0,  0,  0,  0,  0,  0, /* f0-ff */
};
#endif

    /* This routine returns the modifier map to be used with the keymap that
     *   has keyboard/(X id) kbd_id.
     * Note:  This is easy because each OS we support now only has one or
     *   two mod maps for all keyboards it supports.
     */
CARD8 *hpModMap(kbd_id)
{
#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
  switch (kbd_family(kbd_id))
  {
    case 0: return hil_modmap;		/* Extended keyboard */
    case 1: return PS2_modmap;		/* PS2 keyboard */
    case 2: return null_modmap;		/* button box */
  }
#endif

#ifdef __apollo
  return apollo_modmap;
#endif
}


/* ******************************************************************** */
/* *********************************  ********************************* */
/* ******************************************************************** */

	/* Routines in getkeysym.c */
extern HPKKeyDeviceInfo *HPKlookup_kd_by_id(), *HPKlookup_kd_by_name();
extern HPKModMap *HPKlookup_modmap();


    /* Empty space that can be filled in when I read keyboard info from a
     *   file.  I don't think I can just overwrite (eg) the null_maps
     *   because if they change keyboards, recycle and the I can't find a
     *   map, the default won't work anymore.
     */
static KeySym tmp_keymap[8 * 256];		/* biggest keymap */
static CARD8  tmp_modmap[MAP_LENGTH];

static void maparoo(kptr, keysyms_rec, complain, modmap, default_modmap)
  KeySymsRec *keysyms_rec;
  HPKKeyDeviceInfo *kptr;
  CARD8 **modmap, *default_modmap;
{
  CARD8 *the_modmap;
  HPKModMap *mptr;

  keysyms_rec->map	  = tmp_keymap;
  keysyms_rec->minKeyCode = kptr->min_keycode;
  keysyms_rec->maxKeyCode = kptr->max_keycode;
  keysyms_rec->mapWidth   = kptr->columns;

  if (mptr = HPKlookup_modmap(kptr->modmap_name))
  {
    memcpy(tmp_modmap, mptr->modmap, MAP_LENGTH);
    the_modmap = tmp_modmap;
  }
  else
  {
    if (complain)
	ErrorF("Could not find modmap \"%s\"- using default (NULL map).\n",
	      kptr->modmap_name);
    the_modmap = default_modmap;
  }
  *modmap = the_modmap;
}

    /* 
     * Input:
     *   keydevice_id:  The keydevice ID.  On HP-UX, this is the output of
     *     hil_to_kbd_id().
     *   keysyms_rec:  Pointer to a KeySymsRec to fill in.
     *   modmap:       Pointer to a pointer to a modmap.
     * Output:
     *   keysyms_rec:  FIlled in.
     *   modmap:  points to a modmap to use with the keysyms_rec.
     * Returns:
     *   TRUE : everything went as expected
     *   FALSE: Something went screwie, using default maps.
     * Notes:
     *   Don't complain if can't load maps for the button box.
     */
int HPKget_maps_by_id(keydevice_id, keysyms_rec, modmap)
  KeySymsRec *keysyms_rec; CARD8 **modmap;
{
  CARD8 *the_modmap;
  HPKKeyDeviceInfo *kptr;
  HPKModMap *mptr;

  *keysyms_rec = *hpDefaultKeySyms(keydevice_id);
  *modmap = hpModMap(keydevice_id);

  if (keydevice_id == KB_NULL) return TRUE;

  if (!HPKsetup())
  {
  opps:
    if (keydevice_id != KB_BUTTON_BOX)
	ErrorF("Unable to load keytable - using defaults.\n");
  }
  else
  {
    if (!HPKread_keymap(keydevice_id, tmp_keymap)) goto opps;

    if (kptr = HPKlookup_kd_by_id(keydevice_id))
	maparoo(kptr, keysyms_rec, False, modmap, hpModMap(keydevice_id));
  }

  HPKclean_up();

  return TRUE;
}


    /* 
     * Input:
     *   keydevice_name:  
     *   keysyms_rec:  Pointer to a KeySymsRec to fill in.
     *   modmap:       Pointer to a pointer to a modmap.
     * Output:
     *   keysyms_rec:  FIlled in.
     *   modmap:  points to a modmap to use with the keysyms_rec.
     * Returns:
     *   TRUE : everything went as expected
     *   FALSE: Something went screwie, using null maps.
     */
int HPKget_kb_info_by_name(keydevice_name, keysyms_rec, modmap)
  char *keydevice_name; KeySymsRec *keysyms_rec; CARD8 **modmap;
{
  int all_OK = TRUE;

  if (!HPKsetup())
  {
  opps:
    ErrorF("Unable to load keytable \"%s\" - using defaults (NULL maps).\n",
	keydevice_name);
    *keysyms_rec = *hpDefaultKeySyms(KB_NULL);
    *modmap = hpModMap(KB_NULL);

    all_OK = FALSE;
  }
  else
  {
    HPKKeyDeviceInfo *kptr;

    if (!(kptr = HPKlookup_kd_by_name(keydevice_name))	||
        !HPKread_keymap(kptr->keydevice_id, tmp_keymap)) goto opps;

    maparoo(kptr, keysyms_rec, True, modmap, null_keymap);
  }

  HPKclean_up();

  return all_OK;
}
