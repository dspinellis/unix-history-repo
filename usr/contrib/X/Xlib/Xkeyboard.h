#include <X/mit-copyright.h>

/* $Header: Xkeyboard.h,v 10.4 86/02/01 15:41:50 tony Rel $ */
/* Copyright 1985, Massachusetts Institute of Technology */
/*
 * This file is useful for dealing with key events independent of any
 * ascii mapping.
 */

#define KC_F1	0126
#define KC_F2 	0127
#define KC_F3 	0130
#define KC_F4 	0131
#define KC_F5 	0132

#define KC_F6 	0144
#define KC_F7 	0145
#define KC_F8 	0146
#define KC_F9 	0147
#define KC_F10 	0150

#define KC_F11 	0161
#define KC_ESC  0161  /* escape: same as F11 */

#define KC_F12 	0162  
#define KC_BS   0162  /* backspace: same as F12 */

#define KC_F13  0163
#define KC_LF   0163  /* line feed: same as F13 */
#define KC_F14 	0164

#define KC_F15 	0174
#define KC_F16 	0175

#define KC_F17 	0200
#define KC_F18 	0201
#define KC_F19 	0202
#define KC_F20 	0203

#define KC_E1  	0212
#define KC_E2	0213
#define KC_E3	0214
#define KC_E4	0215
#define KC_E5	0216
#define KC_E6	0217

#define KC_KEYPAD_0 0222
#define KC_KEYPAD_PERIOD    0224
#define KC_ENTER    0225
#define KC_KEYPAD_1 0226
#define KC_KEYPAD_2 0227
#define KC_KEYPAD_3 0230
#define KC_KEYPAD_4 0231
#define KC_KEYPAD_5 0232
#define KC_KEYPAD_6 0233
#define KC_KEYPAD_COMMA	    0234
#define KC_KEYPAD_7 0235
#define KC_KEYPAD_8 0236
#define KC_KEYPAD_9 0237
#define KC_KEYPAD_MINUS	    0240

#define KC_PF1	0241
#define KC_PF2	0242
#define KC_PF3	0243
#define KC_PF4	0244

#define KC_SHIFT 0256
#define KC_CTRL	 0257
#define KC_LOCK  0260
#define KC_SYMBOL 0261
#define KC_META   0261  /* same as KC_SYMBOL */

#define KC_CURSOR_LEFT	0247
#define KC_CURSOR_RIGHT 0250
#define KC_CURSOR_DOWN	0251
#define KC_CURSOR_UP	0252

#define IsShiftKey(code) \
    (((unsigned)code)>=KC_SHIFT && ((unsigned)code)<=KC_META)

#define IsCursorKey(code) \
    (((unsigned)code)>=KC_CURSOR_LEFT && ((unsigned)code)<=KC_CURSOR_UP)

#define IsKeypadKey(code) \
    (((unsigned)code)>=KC_KEYPAD_0 && ((unsigned)code)<=KC_KEYPAD_MINUS)

#define IsFunctionKey(code) \
    (((unsigned)code)>=KC_F1 && ((unsigned)code)<=KC_E6)

#define IsPFKey(code) \
    (((unsigned)code)>=KC_PF1 && ((unsigned)code)<=KC_PF4)

#define IsTypewriterKey(code) \
    (((unsigned)code)>=0274 && ((unsigned)code)<=0373)
