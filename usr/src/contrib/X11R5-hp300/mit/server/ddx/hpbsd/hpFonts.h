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
/***********************************************************************
 *  file: hpFonts.h
 *
 *  Header file for font and text routines
 *
 *		Hewlett Packard -- Corvallis Workstation Operation
 *		Project -- port of X11 to HP9000
 *		Harry Phinney -- MTS
 *
 *
 */

typedef struct _chunkcontents {
    int startChar;
    int endChar;
} hpCharRange;

typedef struct _hpFontRec {
    int NumChunks;		/* num of segments the font is broken into */
    hpCharRange *pRange;	/* start & end char in each chunk */
    hpChunk **ppChunk;		/* array of chunks font is stored in */
    hpChunk *stippleChunk;	/* space for stippling one glyph at a time */
    Bool fDefaultExists;	/* flag for existance of default char */
    int maxWidth;
    int maxHeight;
    int firstChar;
    int lastChar;
} hpFontRec;

/*
 * The following determine which characters get optimized, and what
 * size chunks are allocated in offscreen memory.
 * This info should probably be gotten from properties stored with the
 * font. These choices are a bit ethnocentric, but there you are...
 */
#define STARTCHAR 32
#define LASTCHAR 127
#define CHARSPERCHUNK 32
