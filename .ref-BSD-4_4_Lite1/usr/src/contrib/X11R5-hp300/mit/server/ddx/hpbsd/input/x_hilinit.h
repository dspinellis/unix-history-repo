#ifndef X_HILINIT_H
#define X_HILINIT_H
/* $Header: /host/kaukau/disk2/X11R5/R5-hp300/mit/server/ddx/hpbsd/input/RCS/x_hilinit.h,v 1.1 1992/09/30 03:14:10 root Exp $ */
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
/**************************************************************************
 *
 * file: x_hilinit.h
 *
 * contains key definitions and other static information used by x_hilinit.c
 *
 */
 
#define	STARTUP			0
#define	MAX_STRINGS		(sizeof (strings) / sizeof (struct str_table))
#define	MAX_DEV_TYPES		(sizeof (devices) / sizeof (struct dev_table))
#define	MAX_POINTER_FUNCS	(sizeof (pointerfunc) / sizeof (struct pointerfunc))
#define	QUAD_INDEX		8	/* index of quad entry in dev_table */
#define	NINE_KNOB_ID		0x61
#define	KEY			0
#define	UCHAR_NUMBER		1
#define	STRING			2
#define	MODIFIER		3
#define	USHORT_NUMBER		4
#define	UINT_NUMBER		5

struct	opendevs
    {
    int	type;
    int	pos;
    int	use;
    char path[MAXNAMLEN+1];
    char name[MAXNAMLEN+1];
    char entry[MAXNAMLEN+1];
    };

struct	dev_table
    {
    int			lowid;
    int			highid;
    int			dev_type;
    int			x_type;
    char		*name;
    unsigned char	num_keys;
    unsigned char	min_kcode;
    unsigned char	max_kcode;
    };

struct	dev_table  devices[] =
	{{0x00,0x1f,KEYPAD,KEYBOARD,"KEYPAD",0,0,0},
	 {0x2f,0x2f,APOLLO_LPFK,KEYBOARD,"BUTTONBOX",33,64,32},
	 {0x30,0x33,BUTTONBOX,KEYBOARD,"BUTTONBOX",32,10,41},
	 {0x34,0x34,ID_MODULE,XOTHER,"ID_MODULE",0,0,0},
	 {0x35,0x3f,BUTTONBOX,KEYBOARD,"BUTTONBOX",32,10,41},
	 {0x5c,0x5f,BARCODE,KEYBOARD,"BARCODE",109,10,135},
	 {0x60,0x60,ONE_KNOB,MOUSE,"ONE_KNOB",0,0,0},
	 {0x61,0x61,NINE_KNOB,MOUSE,"NINE_KNOB",0,0,0},
	 {0x62,0x67,QUADRATURE,MOUSE,"QUADRATURE",0,0,0},
	 {0x68,0x6b,MOUSE,MOUSE,"MOUSE",0,0,0},
	 {0x6c,0x6f,TRACKBALL,MOUSE,"TRACKBALL",0,0,0},
	 {0x70,0x70,VCD_8_DIALBOX,MOUSE,"KNOB_BOX",0,0,0},
	 {0x71,0x71,SS_SPACEBALL,MOUSE,"SPACEBALL",0,0,0},
	 {0x88,0x8b,TOUCHPAD,MOUSE,"TOUCHPAD",0,0,0},
	 {0x8c,0x8f,TOUCHSCREEN,MOUSE,"TOUCHSCREEN",0,0,0},
	 {0x90,0x97,TABLET,MOUSE,"TABLET",0,0,0},
	 {0x98,0x98,MMII_1812_TABLET,MOUSE,"TABLET",0,0,0},
	 {0x99,0x99,MMII_1201_TABLET,MOUSE,"TABLET",0,0,0},
	 {0xA0,0xBF,KEYBOARD,KEYBOARD,"KEYBOARD",93,10,135},
	 {0xC0,0xDF,KEYBOARD,KEYBOARD,"KEYBOARD",109,10,135},
	 {0xE0,0xFF,KEYBOARD,KEYBOARD,"KEYBOARD",87,10,135},
	 {0x00,0x00,NULL_DEVICE,NULL_DEVICE,"NULL",0,0,0}};

char	*position[] = 
    {
    "FIRST",
    "SECOND",
    "THIRD",
    "FOURTH",
    "FIFTH",
    "SIXTH",
    "SEVENTH"
    };

char	*suffix[] =
#if defined(__hp9000s300) || defined(__hp9000s700) || defined(__hp_osf) || defined(hp300)
    {"1","2","3","4","5","6","7"};
#else
    {"0.1","0.2","0.3","0.4","0.5","0.6","0.7",
     "1.1","1.2","1.3","1.4","1.5","1.6","1.7",
     "2.1","2.2","2.3","2.4","2.5","2.6","2.7",
     "3.1","3.2","3.3","3.4","3.5","3.6","3.7"};
#endif /* __hp9000s300 or __hp9000s700 */

extern	u_char	cursor_down;
extern	u_char	cursor_left;
extern	u_char	cursor_right;
extern	u_char	cursor_up;
extern	u_char	button_1;
extern	u_char	button_2;
extern	u_char	button_3;
extern	u_char	button_4;
extern	u_char	button_5;
extern	u_char	button_6;
extern	u_char	button_7;
extern	u_char	button_8;

extern	u_char	pointer_key_mods[3];
extern	u_char	pointer_amt_mods[3];
extern	u_char	reset_mods[3];
extern	u_char	borrow_mode_mods[3];
extern	u_short	pointer_move;
extern	u_short	pointer_mod1_amt;
extern	u_short	pointer_mod2_amt;
extern	u_short	pointer_mod3_amt;
extern	u_char	borrow_mode;
extern	u_char	reset;
extern	u_char	screen_change_amt;
extern	u_char	button_chording;
extern	u_char	button_latching;
extern	u_char	ptr_button_map[];
extern	u_char	isotropic_scaling;
extern	u_char	screen_orientation;
extern	u_char	screen_row_wrap;
extern	u_char	screen_col_wrap;
extern	u_int	tablet_xorigin;
extern	u_int	tablet_yorigin;
extern	u_int	tablet_width;
extern	u_int	tablet_height;
    
struct	pointerfunc
    {
    char	*name;
    u_char	*code;
    int		type;
    };

struct	pointerfunc pointerfunc [] =
	{{"POINTER_LEFT_KEY", &cursor_left, KEY},
	 {"POINTER_RIGHT_KEY", &cursor_right, KEY},
	 {"POINTER_UP_KEY", &cursor_up, KEY},
	 {"POINTER_DOWN_KEY", &cursor_down, KEY},
	 {"POINTER_KEY_MOD1", &pointer_key_mods[0], MODIFIER},
	 {"POINTER_KEY_MOD2", &pointer_key_mods[1], MODIFIER},
	 {"POINTER_KEY_MOD3", &pointer_key_mods[2], MODIFIER},
	 {"POINTER_BUTTON1_KEY", &button_1, KEY},
	 {"POINTER_BUTTON2_KEY", &button_2, KEY},
	 {"POINTER_BUTTON3_KEY", &button_3, KEY},
	 {"POINTER_BUTTON4_KEY", &button_4, KEY},
	 {"POINTER_BUTTON5_KEY", &button_5, KEY},
	 {"POINTER_BUTTON6_KEY", &button_6, KEY},
	 {"POINTER_BUTTON7_KEY", &button_7, KEY},
	 {"POINTER_BUTTON8_KEY", &button_8, KEY},
	 {"POINTER_MOVE", (u_char *) &pointer_move, USHORT_NUMBER},
	 {"POINTER_MOD1_AMT", (u_char *) &pointer_mod1_amt, USHORT_NUMBER},
	 {"POINTER_MOD2_AMT", (u_char *) &pointer_mod2_amt, USHORT_NUMBER},
	 {"POINTER_MOD3_AMT", (u_char *) &pointer_mod3_amt, USHORT_NUMBER},
#ifdef __apollo
	 {"BORROW_MODE_KEY", &borrow_mode, KEY},
	 {"BORROW_MODE_MOD1_KEY", &borrow_mode_mods[0], MODIFIER},
	 {"BORROW_MODE_MOD2_KEY", &borrow_mode_mods[1], MODIFIER},
#endif /* __apollo */
	 {"RESET", &reset, KEY},
	 {"RESET_MOD1", &reset_mods[0], MODIFIER},
	 {"RESET_MOD2", &reset_mods[1], MODIFIER},
	 {"RESET_MOD3", &reset_mods[2], MODIFIER},
	 {"POINTER_AMT_MOD1", &pointer_amt_mods[0], MODIFIER},
	 {"POINTER_AMT_MOD2", &pointer_amt_mods[1], MODIFIER},
	 {"POINTER_AMT_MOD3", &pointer_amt_mods[2], MODIFIER},
	 {"BUTTON_1_VALUE", &ptr_button_map[1], UCHAR_NUMBER},
	 {"BUTTON_2_VALUE", &ptr_button_map[2], UCHAR_NUMBER},
	 {"BUTTON_3_VALUE", &ptr_button_map[3], UCHAR_NUMBER},
	 {"BUTTON_4_VALUE", &ptr_button_map[4], UCHAR_NUMBER},
	 {"BUTTON_5_VALUE", &ptr_button_map[5], UCHAR_NUMBER},
	 {"BUTTON_6_VALUE", &ptr_button_map[6], UCHAR_NUMBER},
	 {"BUTTON_7_VALUE", &ptr_button_map[7], UCHAR_NUMBER},
	 {"BUTTON_8_VALUE", &ptr_button_map[8], UCHAR_NUMBER},
	 {"SCREEN_CHANGE_AMT", &screen_change_amt, UCHAR_NUMBER},
	 {"BUTTON_CHORDING", &button_chording, STRING},
	 {"BUTTON_LATCHING", &button_latching, STRING},
	 {"TABLET_SUBSET_XORIGIN", (u_char *) &tablet_xorigin, UINT_NUMBER},
	 {"TABLET_SUBSET_YORIGIN", (u_char *) &tablet_yorigin, UINT_NUMBER},
	 {"TABLET_SUBSET_WIDTH", (u_char *) &tablet_width, UINT_NUMBER},
	 {"TABLET_SUBSET_HEIGHT", (u_char *) &tablet_height, UINT_NUMBER},
	 {"ISOTROPIC_SCALING", &isotropic_scaling, STRING},
	 {"SCREEN_ORIENTATION", &screen_orientation, STRING},
	 {"SCREEN_ROW_WRAP", &screen_row_wrap, STRING},
	 {"SCREEN_COL_WRAP", &screen_col_wrap, STRING}};

struct str_table
    {
    char *string;
    u_char value;
    } strings [] = {
    {"OFF",CHORDING_OFF},
    {"ON",CHORDING_ON},
    {"DEFAULT",CHORDING_DEFAULT},
    {"WRAP",WRAP},
    {"NOWRAP",NOWRAP},
    {"SAMESCREEN",SAMESCREEN},
    {"CHANGE_BY_TWO",CHANGE_BY_TWO},
    {"VERTICAL",VERTICAL},
    {"HORIZONTAL",HORIZONTAL},
    {"MATRIX",MATRIX}};

char *keyset1[] = {
#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
    "RIGHT_CONTROL",
    "",
    "RIGHT_EXTEND",
    "LEFT_EXTEND",
    "RIGHT_SHIFT",
    "LEFT_SHIFT",
    "CONTROL",
    "BREAK",
    "KEYPAD_4",
    "KEYPAD_8",
    "KEYPAD_5",
    "KEYPAD_9",
    "KEYPAD_6",
    "KEYPAD_7",
    "KEYPAD_COMMA",
    "KEYPAD_ENTER",
    "KEYPAD_1",
    "KEYPAD_/",
    "KEYPAD_2",
    "KEYPAD_+",
    "KEYPAD_3",
    "KEYPAD_*",
    "KEYPAD_0",
    "KEYPAD_-",
    "B",
    "V",
    "C",
    "X",
    "Z",
    "",
    "",
    "ESCAPE",
    "",
    "BLANK_F10",
    "",
    "BLANK_F11",
    "KEYPAD_PERIOD",
    "BLANK_F9",
    "KEYPAD_TAB",
    "BLANK_F12",
    "H",
    "G",
    "F",
    "D",
    "S",
    "A",
    "",
    "CAPS_LOCK",
    "U",
    "Y",
    "T",
    "R",
    "E",
    "W",
    "Q",
    "TAB",
    "7",
    "6",
    "5",
    "4",
    "3",
    "2",
    "1",
    "`",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "MENU",
    "F4",
    "F3",
    "F2",
    "F1",
    "",
    "STOP",
    "ENTER",
    "SYSTEM",
    "F5",
    "F6",
    "F7",
    "F8",
    "",
    "CLEAR_LINE",
    "CLEAR_DISPLAY",
    "8",
    "9",
    "0",
    "-",
    "=",
    "BACKSPACE",
    "INSERT_LINE",
    "DELETE_LINE",
    "I",
    "O",
    "P",
    "[",
    "]",
    "\\",
    "INSERT_CHAR",
    "DELETE_CHAR",
    "J",
    "K",
    "L",
    ";",
    "'",
    "RETURN",
    "HOME_CURSOR",
    "PREV",
    "M",
    ",",
    ".",
    "/",
    "",
    "SELECT",
    "",
    "NEXT",
    "N",
    "SPACE_BAR",
    ".",
    "",
    "CURSOR_LEFT",
    "CURSOR_DOWN",
    "CURSOR_UP",
    "CURSOR_RIGHT",
#endif /* __hpux */

#ifdef __apollo
	"",
	"SELECT",
	"DREMOVE",
	"APCHARDEL",
	"F10",	
	"F1",
	"F2",
	"F3",
	"F4",
	"F5",
	"F6",
	"F7",
	"F8",
	"F9",
	"REDO",
	"APREAD",
	"APEDIT",
	"APEXIT",
	"PAUSE",	
	"APCOPY",
	"APPASTE",
	"APGROW",
	"",
	"ESCAPE",
	"1",	
	"2",	
	"3",
	"4",
	"5",
	"6",
	"7",
	"8",
	"9",
	"0",
	"MINUS",
	"EQUAL",
	"GRAVE",
	"BACKSPACE",
	"",	
	"APLEFTBAR",	
	"APCMD",	
	"APRIGHTBAR",
	"",
	"",
	"TAB",		
	"Q",	
	"W",
	"E",
	"R",
	"T",
	"Y",
	"U",
	"I",
	"O",
	"P",
	"BRACKETLEFT",
	"BRACKETRIGHT",
	"",	
	"DELETE",
	"",
	"KP_7",
	"KP_8",
	"KP_9",
	"KP_ADD",
	"APLEFTBOX",
	"UP",		
	"APRIGHTBOX",
	"CONTROL_L",
	"",
	"",
	"A",
	"S",
	"D",
	"F",
	"G",
	"H",
	"J",
	"K",
	"L",
	"SEMICOLON",
	"APOSTROPHE",
	"",
	"RETURN",
	"BACKSLASH",
	"",
	"KP_4",
	"KP_5",
	"KP_6",
	"KP_SUBTRACT",
	"LEFT",
	"NEXT",
	"RIGHT",
	"",
	"APREPEAT",
	"SHIFT_L",	
	"",
	"Z",		
	"X",		
	"C",		
	"V",		
	"B",		
	"N",		
	"M",	
	"COMMA",
	"PERIOD",
	"SLASH",	
	"SHIFT_R",	
	"",	
	"APPOP",	
	"",	
	"KP_1",	
	"KP_2",	
	"KP_3",
	"",
	"APUPBOX",
	"DOWN",	
	"APDOWNBOX",	
	"ALT_L",	
	"SPACE",	
	"ALT_R",	
	"",	
	"KP_0",	
	"",	
	"KP_DECIMAL",	
	"KP_ENTER",	
	"",	
	"CAPS_LOCK",
	"",	
#endif /* __apollo */
	};

#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
char *newkeyset1[] = {
    "CONTROL_R",
    "",
    "META_R",
    "META_L",
    "SHIFT_R",
    "SHIFT_L",
    "CONTROL_L",
    "BREAK",
    "KP_4",
    "KP_8",
    "KP_5",
    "KP_9",
    "KP_6",
    "KP_7",
    "KP_SEPARATOR",
    "KP_ENTER",
    "KP_1",
    "KP_DIVIDE",
    "KP_2",
    "KP_ADD",
    "KP_3",
    "KP_MULTIPLY",
    "KP_0",
    "KP_SUBTRACT",
    "B",
    "V",
    "C",
    "X",
    "Z",
    "",
    "",
    "ESCAPE",
    "",
    "F10",
    "",
    "F11",
    "KP_DECIMAL",
    "F9",
    "KP_TAB",
    "F12",
    "H",
    "G",
    "F",
    "D",
    "S",
    "A",
    "NOSYMBOL",
    "CAPS_LOCK",
    "U",
    "Y",
    "T",
    "R",
    "E",
    "W",
    "Q",
    "TAB",
    "7",
    "6",
    "5",
    "4",
    "3",
    "2",
    "1",
    "QUOTELEFT",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "MENU",
    "F4",
    "F3",
    "F2",
    "F1",
    "",
    "CANCEL",
    "EXECUTE",
    "SYSTEM",
    "F5",
    "F6",
    "F7",
    "F8",
    "",
    "CLEARLINE",
    "CLEAR",
    "8",
    "9",
    "0",
    "MINUS",
    "EQUAL",
    "BACKSPACE",
    "INSERTLINE",
    "DELETELINE",
    "I",
    "O",
    "P",
    "BRACKETLEFT",
    "BRACKETRIGHT",
    "BACKSLASH",
    "INSERTCHAR",
    "DELETECHAR",
    "J",
    "K",
    "L",
    "SEMICOLON",
    "QUOTERIGHT",
    "RETURN",
    "HOME",
    "PRIOR",
    "M",
    "COMMA",
    "PERIOD",
    "SLASH",
    "",
    "SELECT",
    "",
    "NEXT",
    "N",
    "SPACE",
    "",
    "",
    "LEFT",
    "DOWN",
    "UP",
    "RIGHT"};
#endif /* __hpux */

#endif /* X_HILINIT_H */
