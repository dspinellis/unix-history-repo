/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
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
 *
 *	@(#)video.h	3.3 (Berkeley) %G%
 */

/*
 * This is a header file describing the interface via int 10H to the
 * video subsystem.
 */

#define	BIOS_VIDEO	0x10

typedef enum {
    SetMode = 0,
    SetCursorType,
    SetCursorPosition,
    ReadCursorPosition,
    ReadLightPenPosition,
    SelectActiveDisplayPage,
    ScrollActivePageUp,
    ScrollActivePageDown,
    ReadAttribute_Character,
    WriteAttribute_Character,
    WriteCharacterOnly,
    SetColorPalette,
    WriteDot,
    ReadDot,
    WriteTeletypeToActivePage,
    CurrentVideoState,
    Reserved16,
    Reserved17,
    Reserved18,
    WriteString
} VideoOperationsType;

typedef enum {
    bw_40x25 = 0,
    color_40x25,
    bw_80x25,
    color_80x25,
    color_320x200,
    bw_320x200,
    bw_640x200,
    internal_bw_80x25
} VideoModeType;
