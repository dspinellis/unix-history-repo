/*
 * $XConsortium: XIMlibint.h,v 1.13 92/07/29 13:55:45 rws Exp $
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

#ifndef	_XIMLIBINT_H_
#define	_XIMLIBINT_H_

#include "XIMproto.h"

#define	XIM_INPUTMETHOD		"_XIM_INPUTMETHOD"

#ifndef	ESC
#define	ESC			0x1b
#endif	/* ESC */
#define	ASCII_DESIGNATE		"\033\050\102"

#define hname_size		128
#define offset_of_portnumber	hname_size
#define portnumber_size		2
#define offset_of_version	(offset_of_portnumber + portnumber_size)
#define version_size		4
#define offset_of_minor_version	(offset_of_version + version_size)

#define ipIMofIC(ic) ((XipIM)ic->core.im)
#ifndef	NO_LOCAL_IM
#define ipLocalIMofIC(ic) ((XipLocalIM)ic->core.im)
#endif

extern short	_XipTypeOfNextICQueue();
extern KeySym	_XipKeySymOfNextICQueue();
extern unsigned int	_XipStateOfNextICQueue();
extern char *	_XipStringOfNextICQueue();
extern void	_XipFreeNextICQueue();
extern int	_XipPutICQueue();
extern void	_XipGetNextICQueue();
extern void	_XipFreeAllICQueue();
extern void	_XipSaveOverflowICQueue();
extern void	_XipGetOverflowICQueue();
extern int	_XipWriteToIM();
extern int	_XipReadFromIM();
extern int	_XipFlushToIM();
extern void	_XipSetCurSock();
extern void	_XipSetCurIM();
extern Bool	_XipConnectIM();
extern void	_XipDisconnectIM();
extern int	_XipCallCallbacks();
extern Bool	_XipBackEndFilter();
extern Status	_XipReceiveICValues();
extern int	_XipSendICValues();
extern Bool	_XipCreateDefIC();
extern char *	_XipICSetAttrValues();
extern char *	_XipICSetValues();
extern char *	_XipICGetValues();
#ifndef	NO_LOCAL_IM
extern Bool	_XipBackEndFilter();
#endif
#ifdef	XML
extern void	_XipChangeLocale();
#endif	/* XML */

#endif	/* _XIMLIBINT_H_ */
