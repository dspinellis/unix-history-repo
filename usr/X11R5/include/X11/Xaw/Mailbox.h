/*
 * $XConsortium: Mailbox.h,v 1.20 91/05/04 18:58:42 rws Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Jim Fulton, MIT X Consortium
 */

#ifndef _XawMailbox_h
#define _XawMailbox_h

/*
 * Mailbox widget; looks a lot like the clock widget, don't it...
 */

/* resource names used by mailbox widget that aren't defined in StringDefs.h */

#ifndef _XtStringDefs_h_
#define XtNupdate "update"
#endif

/* command to exec */
#define XtNcheckCommand "checkCommand"
#define XtNonceOnly "onceOnly"

/* Int: volume for bell */
#define XtNvolume "volume"
#define XtNfullPixmap "fullPixmap"
#define XtNfullPixmapMask "fullPixmapMask"
#define XtNemptyPixmap "emptyPixmap"
#define XtNemptyPixmapMask "emptyPixmapMask"
#define XtNflip "flip"
#define XtNshapeWindow "shapeWindow"

#define XtCCheckCommand "CheckCommand"
#define XtCVolume "Volume"
#define XtCPixmapMask "PixmapMask"
#define XtCFlip "Flip"
#define XtCShapeWindow "ShapeWindow"


/* structures */

typedef struct _MailboxRec *MailboxWidget;  /* see MailboxP.h */
typedef struct _MailboxClassRec *MailboxWidgetClass;  /* see MailboxP.h */


extern WidgetClass mailboxWidgetClass;

#endif /* _XawMailbox_h */
/* DON'T ADD STUFF AFTER THIS #endif */
