/*
 * $XConsortium: XIMproto.h,v 1.4 91/06/05 08:24:13 rws Exp $
 */

/*
 * Copyright 1990, 1991 by OMRON Corporation
 * Copyright 1991 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of OMRON and MIT not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  OMRON and MIT make no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * OMRON AND MIT DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL OMRON OR MIT BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE. 
 *
 *	Author:	Seiji Kuwari	OMRON Corporation
 *				kuwa@omron.co.jp
 *				kuwa%omron.co.jp@uunet.uu.net
 */				

#ifndef XIMPROTO_H
#define XIMPROTO_H

#include <X11/Xmd.h>

/*
 * Define constants for the sizes of the network packets.  The sz_ prefix is
 * used instead of something more descriptive so that the symbols are no more
 * than 32 characters in length (which causes problems for some compilers).
 */

#define	XIM_MAJOR_VERSION	4
#define	XIM_MINOR_VERSION	0

#define	sz_ximRequestHeader	4
#define	sz_ximNormalReply	4

#define	sz_ximConnClient	4
#define	sz_ximGetIMReq		8
#define	sz_ximGetIMReply	8
#define sz_ximCreateICReq	4
#define sz_ximCreateICReply	8
#define sz_ximChangeICReq	8
#define sz_ximChangeICReply	sz_ximNormalReply
#define sz_ximGetICReq		12
#define sz_ximGetICReply	sz_ximNormalReply
#define sz_ximICValuesReq	28
#define	sz_ximICAttributesReq	48
#define	sz_ximDestroyICReq	8
#define	sz_ximICFocusReq	8
#define	sz_ximResetICReq	8

#define sz_ximEventReq		8
#define sz_ximEventReply	8
#define sz_ximReturnReply	8

/* For Callback */
#define sz_ximPreDrawReply	16
#define sz_ximPreCaretReply	8
#define sz_ximStatusDrawReply	12

#define	Window	CARD32
#define	Atom	CARD32
#define	Colormap	CARD32
#define	Pixmap	CARD32
#define	Cursor	CARD32

typedef struct {	/* 4 */
    CARD8	reqType;
    BYTE	pad;
    CARD16	length B16;
} ximRequestHeader;

typedef struct {	/* 4 */
    CARD16	state B16;
    CARD16	detail B16;
} ximNormalReply;

typedef struct {	/* 4 */
    CARD8	byteOrder;
    BYTE	pad;
    CARD16	length B16;
} ximConnClient;

typedef struct {	/* 8 */
    CARD8	reqType;
    BYTE	pad;
    CARD16	length B16;
    CARD32	mask B32;
} ximGetIMReq;

typedef struct {	/* 8 */
    CARD16	state B16;
    INT16	num_styles B16;
    INT16	nbytes B16;
    CARD16	pad B16;
} ximGetIMReply;

typedef struct {	/* 4 */
    CARD8	reqType;
    BYTE	pad;
    CARD16	length B16;
} ximCreateICReq;

typedef struct {	/* 8 */
    CARD16	state B16;
    CARD16	detail B16;
    CARD32	xic B32;
} ximCreateICReply;

typedef	struct {	/* 8 */
    CARD8	reqType;
    BYTE	pad;
    CARD16	length B16;
    CARD32	xic B32;
} ximChangeICReq;

typedef ximNormalReply	ximChangeICReply;

typedef struct {	/* 12 */
    CARD8	reqType;
    BYTE	pad;
    CARD16	length B16;
    CARD32	xic B32;
    CARD32	mask B32;
} ximGetICReq;

typedef ximNormalReply	ximGetICReply;
 
typedef struct {	/* 28 */
    CARD32	mask B32;
    INT32	input_style B32;
    Window	c_window B32;
    Window	focus_window B32;
    CARD32	filter_events B32;
    INT32	max_keycode B32;
    INT16	nbytes B16;
    INT16	nbytes2 B16;
} ximICValuesReq;

typedef struct {	/* 48 */
    INT16	area_x B16, area_y B16;
    CARD16	area_width B16, area_height B16;
    CARD16	areaneeded_width B16, areaneeded_height B16;
    INT16	spot_x B16, spot_y B16;
    Colormap	colormap B32;
    Atom	std_colormap B32;
    CARD32	foreground B32, background B32;
    Pixmap	pixmap B32;
    INT16	line_space B16;
    CARD16	pad1 B16;
    Cursor	cursor B32;
    CARD16	nfonts B16;
    INT16	nbytes B16;
} ximICAttributesReq;

typedef	struct {	/* 8 */
    CARD8	reqType;
    BYTE	pad;
    CARD16	length B16;
    CARD32	xic B32;
} ximDestroyICReq;
    
typedef struct {	/* 8 */
    CARD8	reqType;
    BYTE	pad;
    CARD16	length B16;
    CARD32	xic B32;
} ximICFocusReq;
    
typedef	struct {	/* 8 */
    CARD8	reqType;
    BYTE	pad;
    CARD16	length B16;
    CARD32	xic B32;
} ximResetICReq;

typedef	struct {	/* 8 */
    CARD8	reqType;
    BYTE	pad;
    CARD16	length B16;
    CARD32	xic B32;
} ximEventReq;

typedef struct {	/* 8 */
    CARD16	state B16;
    CARD16	detail B16;
    INT16	number B16;
    CARD16	pad B16;
} ximEventReply;

typedef struct {	/* 8 */
    CARD16	type B16;
    INT16	length B16;
    KeySym	keysym B32;
} ximReturnReply;

/* For Callback */
typedef struct {	/* 16 */
    INT16	caret B16;
    INT16	chg_first B16;
    INT16	chg_length B16;
    INT16	encoding_is_wchar B16;
    INT16	length B16;
    CARD16	pad B16;
    INT32	feedback B32;
} ximPreDrawReply;

typedef struct {	/* 8 */
    INT16	position B16;
    CARD16	direction B16;
    CARD16	style B16;
    CARD16	pad B16;
} ximPreCaretReply;

typedef struct {	/* 12 */
    CARD16	type B16;
    INT16	encoding_is_wchar B16;
    INT16	length B16;
    INT16	feedback B16;
    Pixmap	bitmap B32;
} ximStatusDrawReply;

#define	XIM_GetIM		1
#define	XIM_CreateIC		2
#define	XIM_ChangeIC		3
#define	XIM_GetIC		4
#define	XIM_DestroyIC		5
#define	XIM_SetICFocus		6
#define	XIM_UnsetICFocus	7
#define	XIM_ResetIC		8
#define	XIM_Event		9

#define	XIM_NOTHING		1
#define	XIM_NOFILTER		2
#define	XIM_RETURN		3
#define	XIM_CALLBACK		4
#define	XIM_IC			5
#ifdef	XML
#define XIM_CH_LOCALE		6
#endif	/* XML */


#define	XIM_STRING		(short)1
#define	XIM_KEYSYM		(short)2

/* For Callback */
#define	XIM_CB_PRE_START	1
#define	XIM_CB_PRE_DONE		2
#define	XIM_CB_PRE_DRAW		3
#define	XIM_CB_PRE_CARET	4
#define	XIM_CB_ST_START		5
#define	XIM_CB_ST_DONE		6
#define	XIM_CB_ST_DRAW		7

#define	XIM_CB_FW_CHAR		1
#define	XIM_CB_BW_CHAR		2
#define	XIM_CB_FW_WORD		3
#define	XIM_CB_BW_WORD		4
#define	XIM_CB_CARET_UP		5
#define	XIM_CB_CARET_DOWN	6
#define	XIM_CB_NEXT_LINE	7
#define	XIM_CB_PREV_LINE	8
#define	XIM_CB_LINE_START	9
#define	XIM_CB_LINE_END		10
#define	XIM_CB_ABS_POS		11
#define	XIM_CB_DONT_CHANGE	12

#define	XIM_ST_TEXT		1
#define	XIM_ST_BITMAP		2

#undef	Window
#undef	Atom
#undef	Colormap
#undef	Pixmap
#undef	Cursor	

#define _Read(fd, data, size)	read((fd), (data), (size))
#define _Write(fd, data, size)	write((fd), (data), (size))

#endif	/* XIMPROTO_H */
