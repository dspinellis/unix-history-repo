#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/
#ifndef lint
static char *rcsid_dispatch_c = "$Header: dispatch.c,v 10.16 86/04/11 23:18:55 newman Rel $";
#endif

#include <dbm.h>
#undef NULL
#include "Xint.h"
#include <errno.h>
#include <sys/time.h>
#include <netinet/in.h>
#include "rgb.h"

extern int errno;
extern RESOURCE **Resources;
extern int MaxResource;
extern DEVICE device;
extern int havemask[];
extern int havestate;
extern int rr_counter;
extern int requestId[];
extern char *bufptr[];
extern int bufcnt[];
#ifdef DUALTCP
extern int swapped[];
#endif
extern WINDOW *rootwindow;
extern CURSOR *rootcursor;
extern CURSOR *cursor;
extern short blank_video;
extern struct timeval waittime;
extern struct timeval longtime;
extern int havergb;

char *Xalloc(), *Read_segment();
long Add_resource();
PIXMAP *Do_PixmapSave(), *StorePixmap(), *MakePixmap();
FONT *Get_font();
BITMAP *StoreBitmap(), *CharBitmap();
CURSOR *Store_cursor();
WINDOW *Unmap_window();

u_char Xstatus;		/* Holds error code */
short base_feep;	/* Base value for X_Feep */

/* The main read/dispatch loop */

Dispatcher()
{
	register XReq *req;
	XRep rep;
	int client;
	register RESOURCE *rptr, **res;
	register WINDOW *w;
	register int n;
	register FONT *font;
	char *data1, *data2;
	PIXMAP *pix1, *pix2;
	BITMAP *bit1, *bit2;
	WINDOW *icon, *sw;
	CURSOR *cur;
	vsCursor mcursor;
	int count;
	datum dbent;
	RGB color;
#define ILLEGAL(rval,typ) (((n = RESIDX(rval)) <= 0) || \
			   (n >= MaxResource) || \
			   (((rptr=res[n])->id) != rval) || \
			   (rptr->type != typ))
#define REPLY Write (client, (caddr_t) &rep, sizeof (XRep))

	rep.code = X_Reply;

	/* Just keep receiving a message and dispatching on it. */

	while (1) {
	    /* If not exactly one ready, or have events, go receive/schedule */
	    if (havestate != rr_counter ||
		device.queue->head != device.queue->tail)
		Receive ();

	    client = n = rr_counter;
	    req = (XReq *) bufptr[n];
	    bufptr[n] += sizeof (XReq);
	    if ((bufcnt[n] -= sizeof (XReq)) < sizeof (XReq)) {
		bitclear(havemask, n);
		if (havestate < 0)
		    havestate++;
		else
		    havestate = 0;
	    }
	    requestId[n]++;
#ifdef DUALTCP
	    if (swapped[client])
		Swap_request (req);
#endif
	    Xstatus = 0;
	    res = Resources;

	    /* Check that the window pointer points to a real window */
	    if (req->code < X_SetUp) {
		if ILLEGAL(req->windowId, RT_WINDOW) {
		    /* get segment size */
		    switch (req->code) {
			case X_StoreName:
			    if ((n = req->param.s[0]) > 0)
				n = BytePad(n);
			    break;
			case X_PixmapBitsPut:
			    n = Pix_size (req->param.s[4],
					  req->param.s[0], req->param.s[1]);
			    break;
			case X_BitmapBitsPut:
			    n = Bit_size (req->param.s[0], req->param.s[1]);
			    break;
			case X_Text:
			case X_TextMask:
			    if ((n = req->param.s[6]) > 0)
				n = BytePad(n);
			    break;
			case X_Draw:
			case X_DrawFilled:
			    if ((n = req->param.s[0] * sizeof (Vertex)) > 0)
				n = WordPad(n);
			    break;
			default:
			    n = 0;
		    }
		    if (n <= 0 || Read_segment (client, n)) {
			Xstatus = BadWindow;
			Oops (client, req);
		    }
		    continue;
		} else
		    w = (WINDOW *) rptr->value;
	    }
	    /* Here we verify and convert resource id's to real resources,
	     * then call the appropriate routine (or do it inline).
	     * This makes the routine huge, but it minimizes the spread of
	     * protocol-specifics to other modules.
	     */
	    switch (req->code) {
		case X_CreateWindow:
		    if (req->func == 0)
			pix1 = NULL;
		    else if ILLEGAL(req->param.l[2], RT_PIXMAP) {
			Xstatus = BadPixmap;
			break;
		    } else
			pix1 = (PIXMAP *) rptr->value;
		    if (req->param.l[3] == 0)
			pix2 = NULL;
		    else if ILLEGAL(req->param.l[3], RT_PIXMAP) {
			Xstatus = BadPixmap;
			break;
		    } else
			pix2 = (PIXMAP *) rptr->value;
		    rep.param.l[0] = Create_window (req->param.s[0],
					req->param.s[1], req->param.s[2],
					req->param.s[3], (int) req->func,
					pix1, pix2, IsOpaque, w, client);
		    if (Xstatus) break;
#ifdef DUALTCP
		    if (swapped[client]) {
			pswapl(&rep, 0);
		    }
#endif
		    REPLY;
		    break;

		case X_CreateTransparency:
		    rep.param.l[0] = Create_window (req->param.s[0],
					req->param.s[1], req->param.s[2],
					req->param.s[3], 0,
					(PIXMAP *) NULL, (PIXMAP *) NULL,
					IsTransparent, w, client);
		    if (Xstatus) break;
#ifdef DUALTCP
		    if (swapped[client]) {
			pswapl(&rep, 0);
		    }
#endif
		    REPLY;
		    break;

		case X_DestroyWindow:
		    if (w != rootwindow) {
			Free_resource (rptr);
			Deal_with_movement ();
		    }
		    break;

		case X_DestroySubwindows:
		    if (w->first_child) {
			Remove_subwindows (w, 1, 1);
			Deal_with_movement ();
		    }
		    break;

		case X_MapWindow:
		    if FALSE(w->mapped) {
			Map_window (w);
			Deal_with_movement ();
		    }
		    break;

		case X_MapSubwindows:
		    Map_subwindows (w, 0, 1);
		    Deal_with_movement ();
		    break;

		case X_UnmapWindow:
		    if (w != rootwindow) {
			if TRUE(w->mapped) {
			    Stash_simple (w, (long) UnmapWindow, 0);
			    Unmap_window (w, 1);
			    Deal_with_movement ();
			}
			w->should_be_mapped = 0;
		    }
		    break;

		case X_UnmapSubwindows:
		    Unmap_subwindows (w);
		    Deal_with_movement ();
		    break;

		case X_UnmapTransparent:
		    if (w != rootwindow) {
			if TRUE(w->mapped) {
			    Unmap_window (w, -1);
			    Deal_with_movement ();
			}
			w->should_be_mapped = 0;
		    }
		    break;

		case X_RaiseWindow:
		    if (w != rootwindow) {
			Raise_window (w);
			Deal_with_movement ();
		    }
		    break;

		case X_LowerWindow:
		    if (w != rootwindow) {
			Lower_window (w);
			Deal_with_movement ();
		    }
		    break;

		case X_CircWindowUp:
		    Circulate_window_up (w);
		    Deal_with_movement ();
		    break;

		case X_MoveWindow:
		    if (w != rootwindow) {
			Move_window (w, req->param.s[0], req->param.s[1]);
			Deal_with_movement ();
		    }
		    break;

		case X_ChangeWindow:
		    if (w == rootwindow) break;
		    Change_window (w, w->full.left - w->bwidth -
				      ((w->mapped) ? w->parent->full.left : 0),
				      w->full.top - w->bwidth -
				      ((w->mapped) ? w->parent->full.top : 0),
				      req->param.s[0], req->param.s[1]);
		    Deal_with_movement ();
		    break;

		case X_ConfigureWindow:
		    if (w == rootwindow) break;
		    Change_window (w, req->param.s[2], req->param.s[3],
				      req->param.s[0], req->param.s[1]);
		    Deal_with_movement ();
		    break;

		case X_ChangeBackground:
		    if (req->param.l[0] == 0)
			Change_background (w, (PIXMAP *) NULL);
		    else if ILLEGAL(req->param.l[0], RT_PIXMAP)
			Xstatus = BadPixmap;
		    else
			Change_background (w, (PIXMAP *) rptr->value);
		    break;

		case X_ChangeBorder:
		    if ILLEGAL(req->param.l[0], RT_PIXMAP)
			Xstatus = BadPixmap;
		    else
			Change_border (w, (PIXMAP *) rptr->value);
		    break;

		case X_TileMode:
		    if (req->func > 1)
			Xstatus = BadValue;
		    else if (w != rootwindow)
			w->tilemode = req->func;
		    break;

		case X_ClipMode:
		    if (req->func <= 1)
			w->clipmode = req->func;
		    else
			Xstatus = BadValue;
		    break;

		case X_QueryWindow:
		    rep.param.s[0] = w->full.bottom - w->full.top;
		    rep.param.s[1] = w->full.right - w->full.left;
		    rep.param.s[2] = w->full.left - w->bwidth;
		    rep.param.s[3] = w->full.top - w->bwidth;
		    if (TRUE(w->mapped) && w->parent) {
			rep.param.s[2] -= w->parent->full.left;
			rep.param.s[3] -= w->parent->full.top;
		    }
		    rep.param.s[4] = w->bwidth;
		    if (TRUE(w->should_be_mapped) && FALSE(w->mapped))
			rep.param.b[10] = IsInvisible;
		    else
			rep.param.b[10] = w->mapped;
		    rep.param.b[11] = w->kind;
		    if (w->icon)
			rep.param.l[3] = w->icon->rid;
		    else
			rep.param.l[3] = 0;
		    rep.param.l[4] = w->mask;
#ifdef DUALTCP
		    if (swapped[client]) {
			pswaps(&rep, 0);
			pswaps(&rep, 1);
			pswaps(&rep, 2);
			pswaps(&rep, 3);
			pswaps(&rep, 4);
			pswapl(&rep, 3);
			pswapl(&rep, 4);
		    }
#endif
		    REPLY;
		    break;

		case X_StoreName:
		    if ((n = req->param.s[0]) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, BytePad(n))) {
			if (w->name) {
			    free (w->name);
			    w->name = NULL;
			}
			w->name = Xalloc (n + 1);
			bcopy (data1, w->name, n);
			w->name[n] = '\0';
		    }
		    break;

		case X_FetchName:
		    rep.param.s[0] = count = w->name ? strlen (w->name) : 0;
#ifdef DUALTCP
		    if (swapped[client]) {
			pswaps(&rep, 0);
		    }
#endif
		    REPLY;
		    if (n = count)
			Write (client, w->name, BytePad(n));
		    break;

		case X_SetIconWindow:
		    if (w->kind != IsOpaque)
			Xstatus = BadMatch;
		    else if (req->param.l[0] == 0) {
			if (icon = w->icon) {
			    icon->kind = IsOpaque;
			    icon->icon = NULL;
			    w->icon = NULL;
			}
		    } else if ILLEGAL(req->param.l[0], RT_WINDOW)
			Xstatus = BadWindow;
		    else {
			icon = (WINDOW *) rptr->value;
			if (icon->kind != IsOpaque || icon == w ||
			    icon->parent != w->parent)
			    Xstatus = BadMatch;
			else {
			    icon->kind = IsIcon;
			    icon->icon = w;
			    w->icon = icon;
			}
		    }
		    break;

		case X_SetResizeHint:
		    if (req->param.s[0] < 0 || req->param.s[1] <= 0 ||
			req->param.s[2] < 0 || req->param.s[3] <= 0)
			Xstatus = BadValue;
		    else {
			w->height0 = req->param.s[0];
			w->heightinc = req->param.s[1];
			w->width0 = req->param.s[2];
			w->widthinc = req->param.s[3];
		    }
		    break;

		case X_GetResizeHint:
		    rep.param.s[0] = w->height0;
		    rep.param.s[1] = w->heightinc;
		    rep.param.s[2] = w->width0;
		    rep.param.s[3] = w->widthinc;
#ifdef DUALTCP
		    if (swapped[client]) {
			pswaps(&rep, 0);
			pswaps(&rep, 1);
			pswaps(&rep, 2);
			pswaps(&rep, 3);
		    }
#endif
		    REPLY;
		    break;

		case X_DefineCursor:
		    if (req->param.l[0] == 0) {
			if (w == rootwindow)
			    Register_cursor (w, rootcursor);
			else
			    Unregister_cursor (w);
		    } else if ILLEGAL(req->param.l[0], RT_CURSOR)
			Xstatus = BadCursor;
		    else
			Register_cursor (w, (CURSOR *) rptr->value);
		    break;

		case X_SelectInput:
		    Select_input (w, client, req->param.l[0]);
		    break;

		case X_GrabMouse:
		    if ILLEGAL(req->param.l[0], RT_CURSOR)
			Xstatus = BadCursor;
		    else {
			Grab_mouse (w, (CURSOR *) rptr->value,
				    req->param.l[1], client);
			if (Xstatus) break;
			REPLY;
		    }
		    break;

		case X_GrabButton:
		    if ILLEGAL(req->param.l[0], RT_CURSOR)
			Xstatus = BadCursor;
		    else {
			Grab_button (w, (CURSOR *) rptr->value, req->mask,
				     req->param.l[1], client);
			if (Xstatus) break;
			REPLY;
		    }
		    break;

		case X_QueryMouse:
		    mcursor = *device.mouse;
		    Interpret_locator (w, mcursor.x + cursor->xoff,
				       mcursor.y + cursor->yoff, &rep);
#ifdef DUALTCP
		    if (swapped[client]) {
			pswapl(&rep, 0);
			pswaps(&rep, 2);
			pswaps(&rep, 3);
			pswaps(&rep, 4);
		    }
#endif
		    REPLY;
		    break;

		case X_InterpretLocator:
		    Interpret_locator (w,
#ifdef vax
				       req->param.s[1], req->param.s[0],
#else
#ifdef mc68000
				       req->param.s[0], req->param.s[1],
#else
				       req->param.l[0] >> 16,
				       req->param.l[0] & 0xffff,
#endif
#endif
				       &rep);
#ifdef DUALTCP
		    if (swapped[client]) {
			pswapl(&rep, 0);
			pswaps(&rep, 2);
			pswaps(&rep, 3);
		    }
#endif
		    REPLY;
		    break;

		case X_WarpMouse:
		    if ILLEGAL(req->param.l[1], RT_WINDOW)
			Xstatus = BadWindow;
		    else
			Warp_mouse (w, req->param.s[0], req->param.s[1],
				    (WINDOW *) rptr->value,
				    (REGION *) &req->param.s[4]);
		    break;

		case X_FocusKeyboard:
		    Focus_keyboard (w);
		    break;

		case X_CircWindowDown:
		    Circulate_window_down (w);
		    Deal_with_movement ();
		    break;

		case X_QueryTree:
		    count = 0;
		    for (sw = w->first_child; sw; sw = sw->next_sib)
			count++;
		    rep.param.l[0] = w->parent ? w->parent->rid : 0;
		    rep.param.l[1] = count;
#ifdef DUALTCP
		    if (swapped[client]) {
			pswapl(&rep, 0);
			pswapl(&rep, 1);
		    }
#endif
		    REPLY;
		    if (n = count) {
			data1 = Xalloc (n << 2);
			n = 0;
			for (sw = w->first_child; sw; sw = sw->next_sib) {
			    ((Window *)data1)[n] = sw->rid;
			    n++;
			}
#ifdef DUALTCP
			if (swapped[client])
			    Swap_longs ((long *) data1, n);
#endif
			Write (client, data1, n << 2);
			free (data1);
		    }
		    break;

		case X_Clear:
		    Do_background (w, 1);
		    break;

		case X_PixFill:
		    if (req->param.l[3] == 0)
			bit1 = NULL;
		    else if ILLEGAL(req->param.l[3], RT_BITMAP) {
			Xstatus = BadBitmap;
			break;
		    } else
			bit1 = (BITMAP *) rptr->value;
		    Do_PixFill (w, req, bit1);
		    break;

		case X_TileFill:
		    if (req->param.l[3] == 0)
			bit1 = NULL;
		    else if ILLEGAL(req->param.l[3], RT_BITMAP) {
			Xstatus = BadBitmap;
			break;
		    } else
			bit1 = (BITMAP *) rptr->value;
		    if ILLEGAL(req->param.l[2], RT_PIXMAP)
			Xstatus = BadPixmap;
		    else
			Do_TileFill (w, req, (PIXMAP *) rptr->value, bit1);
		    break;

		case X_PixmapPut:
		    if ILLEGAL(req->param.l[2], RT_PIXMAP)
			Xstatus = BadPixmap;
		    else
			Do_PixmapPut (w, req, (PIXMAP *) rptr->value);
		    break;

		case X_PixmapBitsPut:
		    if ((n = Pix_size (req->param.s[4],
				       req->param.s[0], req->param.s[1])) == 0 ||
			(data1 = Read_segment (client, n)) == NULL)
			break;
#ifdef DUALTCP
		    if (swapped[client] &&
			(req->param.s[4] == XYFormat || device.planes > 8))
			Swap_shorts ((short *) data1, n >> 1);
#endif
		    if (req->param.l[3] == 0)
			bit1 = NULL;
		    else if ILLEGAL(req->param.l[3], RT_BITMAP) {
			Xstatus = BadBitmap;
			break;
		    } else
			bit1 = (BITMAP *) rptr->value;
		    Do_PixmapBitsPut (w, req, data1, bit1);
		    break;

		case X_BitmapBitsPut:
		    if ((n = Bit_size (req->param.s[0], req->param.s[1])) == 0 ||
			(data1 = Read_segment (client, n)) == NULL)
			break;
#ifdef DUALTCP
		    if (swapped[client])
			Swap_shorts ((short *) data1, n >> 1);
#endif
		    if (req->param.l[3] == 0)
			bit1 = NULL;
		    else if ILLEGAL(req->param.l[3], RT_BITMAP) {
			Xstatus = BadBitmap;
			break;
		    } else
			bit1 = (BITMAP *) rptr->value;
		    Do_BitmapBitsPut (w, req, data1, bit1);
		    break;

		case X_CopyArea:
		    Do_CopyArea (w, req);
		    break;

		case X_Text:
		    if ((n = req->param.s[6]) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, BytePad(n))) {
			if ILLEGAL(req->param.l[1], RT_FONT)
			    Xstatus = BadFont;
			else if (TRUE(w->unobscured) &&
				 w->kind != IsTransparent &&
				 TRUE(w->mapped) && req->func < funclim &&
				 req->param.s[6] > 0)
			    /* We special case this just to make it faster */
			    PrintText (data1, req->param.s[6],
				       (FONT *) rptr->value,
				       (int) req->param.u[4],
				       (int) req->param.u[5],
				       req->param.b[14], req->param.b[15],
				       req->param.s[0] + w->full.left,
				       req->param.s[1] + w->full.top,
				       &w->clip, 1,
				       (int) req->func, (int) req->mask);
			else
			    Do_Text (w, req, data1, (FONT *) rptr->value);
		    }
		    break;

		case X_TextMask:
		    if ((n = req->param.s[6]) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, BytePad(n))) {
			if ILLEGAL(req->param.l[1], RT_FONT)
			    Xstatus = BadFont;
			else
			    Do_TextMask (w, req, data1, (FONT *) rptr->value);
		    }
		    break;

		case X_Line:
		    Do_Line (w, req);
		    break;

		case X_Draw:
		    if ((n = (req->param.s[0] * sizeof (Vertex))) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, WordPad(n))) {
#ifdef DUALTCP
			if (swapped[client])
			    Swap_shorts ((short *) data1, n >> 1);
#endif
			Do_Draw (w, req, (Vertex *) data1);
		    }
		    break;

		case X_DrawFilled:
		    if ((n = (req->param.s[0] * sizeof (Vertex))) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, WordPad(n))) {
			if (req->param.l[1] == 0)
			    pix1 = NULL;
			else if ILLEGAL(req->param.l[1], RT_PIXMAP) {
			    Xstatus = BadPixmap;
			    break;
			} else
			    pix1 = (PIXMAP *) rptr->value;
#ifdef DUALTCP
			if (swapped[client])
			    Swap_shorts ((short *) data1,
					 req->param.s[0] * (sizeof (Vertex) /
							    sizeof (short)));
#endif
			Do_DrawFilled (w, req, (Vertex *) data1, pix1);
		    }
		    break;

		case X_PixmapSave:
		    pix1 = Do_PixmapSave (w, req);
		    if (Xstatus) break;
		    rep.param.l[0] = Add_resource (RT_PIXMAP, client,
						   (caddr_t) pix1);
#ifdef DUALTCP
		    if (swapped[client]) {
			pswapl(&rep, 0);
		    }
#endif
		    REPLY;
		    break;

		case X_PixmapGet:
		    Do_PixmapGet (w, req, client);
		    break;

		case X_StippleFill:
		    if ILLEGAL(req->param.l[3], RT_BITMAP) {
			Xstatus = BadBitmap;
			break;
		    } else
			bit1 = (BITMAP *) rptr->value;
		    Do_StippleFill (w, req, bit1);
		    break;

		case X_SetUp:
		    rep.param.l[0] = rootwindow->rid;
		    rep.param.s[2] = X_PROTOCOL;
		    rep.param.s[3] = device.id;
		    rep.param.s[4] = device.planes;
		    rep.param.u[5] = device.entries;
#ifdef DUALTCP
		    if (swapped[client]) {
			pswapl(&rep, 0);
			pswaps(&rep, 2);
			pswaps(&rep, 3);
			pswaps(&rep, 4);
			pswaps(&rep, 5);
		    }
#endif
		    REPLY;
		    break;

		case X_UngrabMouse:
		    Ungrab_mouse (client);
		    break;

		case X_UngrabButton:
		    Ungrab_button (req->mask, client);
		    break;

		case X_GetColor:
		    if (rep.param.u[0] = Get_color (client, req->param.u[0],
						    req->param.u[1], req->param.u[2])) {
#ifdef DUALTCP
			if (swapped[client]) {
			    pswaps(&rep, 0);
			}
#endif
			REPLY;
		    }
		    break;

		case X_GetColorCells:
		    if (req->func > 1 ||
			req->param.s[0] < 0 || req->param.s[1] < 0) {
			Xstatus = BadValue;
			break;
		    }
		    rep.param.u[0] = Get_cells (client, (int) req->func,
						req->param.s[0], req->param.s[1],
						(ushort **) &data1);
		    if (!Xstatus) {
#ifdef DUALTCP
			if (swapped[client]) {
			    pswaps(&rep, 0);
			    Swap_shorts ((short *) data1, req->param.s[0]);
			}
#endif
			REPLY;
			if (n = req->param.s[0]) {
			    Write (client, data1, WordPad(n << 1));
#ifdef DUALTCP
			    if (swapped[client])
				Swap_shorts ((short *) data1, n);
#endif
			}
		    }
		    break;

		case X_FreeColors:
		    if ((n = req->param.s[0]) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, WordPad(n << 1))) {
#ifdef DUALTCP
			if (swapped[client])
			    Swap_shorts ((short *) data1, n);
#endif
			Free_colors (client, n, (ushort *) data1, req->mask);
		    }
		    break;

		case X_StoreColors:
		    if ((n = req->param.s[0]) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, n * sizeof (ColorDef))) {
#ifdef DUALTCP
			if (swapped[client])
			    Swap_shorts ((short *) data1, n * (sizeof (ColorDef) / sizeof (short)));
#endif
			Store_colors (n, (ColorDef *) data1);
		    }
		    break;

		case X_QueryColor:
		    Query_color (req->param.u[0], rep.param.u+0,
				 rep.param.u+1, rep.param.u+2);
		    if (!Xstatus) {
#ifdef DUALTCP
			if (swapped[client]) {
			    pswaps(&rep, 0);
			    pswaps(&rep, 1);
			    pswaps(&rep, 2);
			}
#endif
			REPLY;
		    }
		    break;

		case X_GetFont:
		    if ((n = req->param.s[0]) <= 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, BytePad(n))) {
			data2 = Xalloc (n + 1);
			bcopy (data1, data2, n);
			data2[n] = '\0';
			font = Get_font (data2);
			free (data2);
			if (Xstatus) break;
			rep.param.l[0] = Add_resource (RT_FONT, client,
						       (caddr_t) font);
#ifdef DUALTCP
			if (swapped[client]) {
			    pswapl(&rep, 0);
			}
#endif
			REPLY;
		    }
		    break;

		case X_FreeFont:
		    if ILLEGAL(req->param.l[0], RT_FONT)
			Xstatus = BadFont;
		    else
			Free_resource (rptr);
		    break;

		case X_QueryFont:
		    if ILLEGAL(req->param.l[0], RT_FONT)
			Xstatus = BadFont;
		    else {
			font = (FONT *) rptr->value;
			rep.param.s[0] = font->height;
			rep.param.s[1] = font->avg_width;
			rep.param.s[2] = font->first;
			rep.param.s[3] = font->last;
			rep.param.s[4] = font->base;
			rep.param.s[5] = font->fixed;
#ifdef DUALTCP
			if (swapped[client]) {
			    pswaps(&rep, 0);
			    pswaps(&rep, 1);
			    pswaps(&rep, 2);
			    pswaps(&rep, 3);
			    pswaps(&rep, 4);
			    pswaps(&rep, 5);
			}
#endif
			REPLY;
		    }
		    break;

		case X_CharWidths:
		    if ((n = req->param.s[2]) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, BytePad(n))) {
			if ILLEGAL(req->param.l[0], RT_FONT)
			    Xstatus = BadFont;
			else {
			    font = (FONT *) rptr->value;
			    n = req->param.s[2];
			    data2 = Xalloc ((int) WordPad(count = n << 1));
			    while (--n >= 0)
				((short *)data2)[n] = CharWidth (((u_char *) data1)[n],
								 font);
			    rep.param.l[0] = count;
#ifdef DUALTCP
			    if (swapped[client]) {
				pswapl(&rep, 0);
				Swap_shorts ((short *) data2, req->param.s[2]);
			    }
#endif
			    REPLY;
			    Write (client, data2, WordPad(count));
			    free (data2);
			}
		    }
		    break;

		case X_StringWidth:
		    if ((n = req->param.s[2]) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, BytePad(n))) {
			if ILLEGAL(req->param.l[0], RT_FONT)
			    Xstatus = BadFont;
			else {
			    rep.param.s[0] = TextWidth (data1, req->param.s[2],
							0, (FONT *) rptr->value);
#ifdef DUALTCP
			    if (swapped[client]) {
				pswaps(&rep, 0);
			    }
#endif
			    REPLY;
			}
		    }
		    break;

		case X_FontWidths:
		    if ILLEGAL(req->param.l[0], RT_FONT)
			Xstatus = BadFont;
		    else {
			font = (FONT *) rptr->value;
			n = font->last - font->first + 1;
			data1 = Xalloc ((int) WordPad(count = n << 1));
			while (--n >= 0)
			    ((short *)data1)[n] = CharWidth ((unsigned) font->first + n,
							     font);
			rep.param.l[0] = count;
#ifdef DUALTCP
			if (swapped[client]) {
			    pswapl(&rep, 0);
			    Swap_shorts ((short *) data1, count >> 1);
			}
#endif
			REPLY;
			Write (client, data1, WordPad(count));
			free (data1);
		    }
		    break;

		case X_StoreBitmap:
		    if ((n = Bit_size (req->param.s[0], req->param.s[1])) > 0 &&
			(data1 = Read_segment (client, n))) {
#ifdef DUALTCP
		    if (swapped[client])
			Swap_shorts ((short *) data1, n >> 1);
#endif
			bit1 = StoreBitmap (req->param.s[1], req->param.s[0],
					    data1);
			if (bit1 == NULL)
			    Xstatus = BadAlloc;
			else {
			    rep.param.l[0] = Add_resource (RT_BITMAP, client,
							   (caddr_t) bit1);
#ifdef DUALTCP
			    if (swapped[client]) {
				pswapl(&rep, 0);
			    }
#endif
			    REPLY;
			}
		    }
		    break;

		case X_FreeBitmap:
		    if ILLEGAL(req->param.l[0], RT_BITMAP)
			Xstatus = BadBitmap;
		    else
			Free_resource (rptr);
		    break;

		case X_CharBitmap:
		    if ILLEGAL(req->param.l[0], RT_FONT)
			Xstatus = BadFont;
		    else {
			bit1 = CharBitmap ((unsigned) req->param.s[2],
					   (FONT *) rptr->value);
			if (bit1) {
			    rep.param.l[0] = Add_resource (RT_BITMAP, client,
							   (caddr_t) bit1);
#ifdef DUALTCP
			    if (swapped[client]) {
				pswapl(&rep, 0);
			    }
#endif
			    REPLY;
			} else if (errno == ENOMEM)
			    Xstatus = BadAlloc;
			else
			    Xstatus = BadValue;
		    }
		    break;

		case X_StorePixmap:
		    if ((n = Pix_size ((int) req->func,
				       req->param.s[0], req->param.s[1])) > 0 &&
			(data1 = Read_segment (client, n))) {
#ifdef DUALTCP
		    if (swapped[client] &&
			(req->func == XYFormat || device.planes > 8))
			Swap_shorts ((short *) data1, n >> 1);
#endif
			pix1 = StorePixmap (req->param.s[1], req->param.s[0],
					    (int) req->func, data1);
			if (pix1 == NULL)
			    Xstatus = BadAlloc;
			else {
			    rep.param.l[0] = Add_resource (RT_PIXMAP, client,
							   (caddr_t) pix1);
#ifdef DUALTCP
			    if (swapped[client]) {
				pswapl(&rep, 0);
			    }
#endif
			    REPLY;
			}
		    }
		    break;

		case X_FreePixmap:
		    if ILLEGAL(req->param.l[0], RT_PIXMAP)
			Xstatus = BadBitmap;
		    else
			Free_resource (rptr);
		    break;

		case X_MakePixmap:
		    if (req->param.l[0] == 0) {
			bit1 = NULL;
			if (req->param.u[2] > WhitePixel &&
			    req->param.u[2] >= device.entries) {
			    Xstatus = BadValue;
			    break;
			}
		    } else if ILLEGAL(req->param.l[0], RT_BITMAP) {
			Xstatus = BadBitmap;
			break;
		    } else if ((req->param.u[2] > WhitePixel &&
				req->param.u[2] >= device.entries) ||
			       (req->param.u[3] > WhitePixel &&
				req->param.u[3] >= device.entries)) {
			Xstatus = BadValue;
			break;
		    } else
			bit1 = (BITMAP *) rptr->value;
		    pix1 = MakePixmap (bit1, (int) req->param.u[2],
				       (int) req->param.u[3]);
		    if (pix1 == NULL)
			Xstatus = BadAlloc;
		    else {
			rep.param.l[0] = Add_resource (RT_PIXMAP, client,
						       (caddr_t) pix1);
#ifdef DUALTCP
			if (swapped[client]) {
			    pswapl(&rep, 0);
			}
#endif
			REPLY;
		    }
		    break;

		case X_QueryShape:
		    if (req->func > 2 ||
			req->param.s[0] <= 0 || req->param.s[1] <= 0)
			Xstatus = BadValue;
		    else {
			QueryShape ((int) req->func,
				    req->param.s, req->param.s+1);
			rep.param.s[0] = req->param.s[0];
			rep.param.s[1] = req->param.s[1];
#ifdef DUALTCP
			if (swapped[client]) {
			    pswaps(&rep, 0);
			    pswaps(&rep, 1);
			}
#endif
			REPLY;
		    }
		    break;

		case X_StoreCursor:
		    if ILLEGAL(req->param.l[0], RT_BITMAP) {
			Xstatus = BadBitmap;
			break;
		    }
		    bit1 = (BITMAP *) rptr->value;
		    if (req->param.l[2] == 0)
			bit2 = NULL;
		    else if ILLEGAL(req->param.l[2], RT_BITMAP) {
			Xstatus = BadBitmap;
			break;
		    } else
			bit2 = (BITMAP *) rptr->value;
		    cur = Store_cursor (bit1, bit2, (int) req->param.u[2],
					(int) req->param.u[3],
					req->param.s[6], req->param.s[7],
					req->func);
		    if (Xstatus) break;
		    rep.param.l[0] = Add_resource (RT_CURSOR, client,
						   (caddr_t) cur);
#ifdef DUALTCP
		    if (swapped[client]) {
			pswapl(&rep, 0);
		    }
#endif
		    REPLY;
		    break;

		case X_FreeCursor:
		    if ILLEGAL(req->param.l[0], RT_CURSOR)
			Xstatus = BadCursor;
		    else
			Free_resource (rptr);
		    break;

		case X_MouseControl:
		    if (req->param.s[0] <= 0 || req->param.s[1] < 0)
			Xstatus = BadValue;
		    else
			SetMouseCharacteristics (req->param.s[1],
						 req->param.s[0]);
		    break;

		case X_FeepControl:
		    if (req->func > 7)
			Xstatus = BadValue;
		    else
			base_feep = req->func;
		    break;

		case X_Feep:
		    n = req->param.s[0];
		    if (n < -7 || n > 7)
			Xstatus = BadValue;
		    else {
			n += base_feep;
			if (n < 0)
			    n = 0;
			else if (n > 7)
			    n = 7;
			SoundBell (n);
		    }
		    break;

		case X_ShiftLock:
		    if (req->func > 1)
			Xstatus = BadValue;
		    else
			Set_shiftlock ((int) req->func);
		    break;

		case X_KeyClick:
		    if (req->func > 8)
			Xstatus = BadValue;
		    else
			SetKeyClick ((int) req->func);
		    break;

		case X_AutoRepeat:
		    if (req->func > 1)
			Xstatus = BadValue;
		    else
			SetAutoRepeat ((int) req->func);
		    break;

		case X_ScreenSaver:
		    if (req->func > 1 ||
			req->param.s[0] <= 0 || req->param.s[1] <= 0)
			Xstatus = BadValue;
		    else {
			blank_video = 1 - req->func;
			waittime.tv_sec = 60 * req->param.s[0];
			longtime.tv_sec = 60 * req->param.s[1];
		    }
		    break;

		case X_StoreBytes:
		    if ((n = req->param.s[0]) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, BytePad(n)))
			Store_cut (req->func, data1, n);
		    break;
#ifdef X_AppendBytes
		case X_AppendBytes:
		    if ((n = req->param.s[0]) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, BytePad(n)))
			Append_to_cut (req->func, data1, n);
		    break;
#endif X_AppendBytes
		case X_FetchBytes:
		    if ((count = Fetch_cut (req->func, &data1)) >= 0) {
			rep.param.s[0] = count;
#ifdef DUALTCP
			if (swapped[client]) {
			    pswaps(&rep, 0);
			}
#endif
			REPLY;
			if (n = count)
			    Write (client, data1, BytePad(n));
		    }
		    break;

		case X_RotateCuts:
		    Rotate_cuts (req->func);
		    break;

		case X_AddHost:
		    Add_host (client, (short) req->func, req->param.b);
		    break;

		case X_RemoveHost:
		    Remove_host (client, (short) req->func, req->param.b);
		    break;

		case X_GetHosts:
		    if ((count = Get_hosts ((short) req->func, &data1)) < 0)
			break;
		    rep.param.l[0] = count;
#ifdef DUALTCP
		    if (swapped[client]) {
			pswapl(&rep, 0);
		    }
#endif
		    REPLY;
		    if (count) {
			Write (client, data1, count);
			free (data1);
		    }
		    break;

		case X_GrabServer:
		    Grab_server (client);
		    break;

		case X_UngrabServer:
		    Ungrab_server ();
		    break;

		case X_LookupColor:
		    if ((n = req->param.s[0]) < 0)
			Xstatus = BadValue;
		    else if (data1 = Read_segment (client, BytePad(n))) {
			if TRUE(havergb) {
			    dbent.dptr = data1;
			    dbent.dsize = n;
			    dbent = fetch (dbent);
			    if (dbent.dptr) {
				bcopy(dbent.dptr, (caddr_t)&color, sizeof (RGB));
				rep.param.u[0] = color.red;
				rep.param.u[1] = color.green;
				rep.param.u[2] = color.blue;
				rep.param.u[3] = rep.param.u[0];
				rep.param.u[4] = rep.param.u[1];
				rep.param.u[5] = rep.param.u[2];
				ResolveColor (rep.param.u+3, rep.param.u+4,
					      rep.param.u+5);
#ifdef DUALTCP
				if (swapped[client]) {
				    pswaps(&rep, 0);
				    pswaps(&rep, 1);
				    pswaps(&rep, 2);
				    pswaps(&rep, 3);
				    pswaps(&rep, 4);
				    pswaps(&rep, 5);
				}
#endif
				REPLY;
			    } else
				Xstatus = BadColor;
			} else
			    Xstatus = BadColor;
		    }
		    break;

		default:
		    Xstatus = BadRequest;
		    break;
	    }

	    if (Xstatus) Oops (client, req);
	}
}

#ifdef DUALTCP
/* Byte swap a request */

Swap_request (req)
	register XReq *req;
{
	register swaptype n;

	switch (req->code) {
	    case X_CreateWindow:
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswaps(req, 2);
		pswaps(req, 3);
		pswapl(req, 2);
		pswapl(req, 3);
		break;

	    case X_CreateTransparency:
	    case X_ConfigureWindow:
	    case X_SetResizeHint:
	    case X_PixmapSave:
	    case X_PixmapGet:
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswaps(req, 2);
		pswaps(req, 3);
		break;

	    case X_DestroyWindow:
	    case X_DestroySubwindows:
	    case X_MapWindow:
	    case X_MapSubwindows:
	    case X_UnmapWindow:
	    case X_UnmapSubwindows:
	    case X_UnmapTransparent:
	    case X_RaiseWindow:
	    case X_LowerWindow:
	    case X_CircWindowUp:
	    case X_TileMode:
	    case X_ClipMode:
	    case X_QueryWindow:
	    case X_FetchName:
	    case X_GetResizeHint:
	    case X_QueryMouse:
	    case X_FocusKeyboard:
	    case X_CircWindowDown:
	    case X_QueryTree:
	    case X_Clear:
		swapl(&req->windowId);
		break;

	    case X_MoveWindow:
	    case X_ChangeWindow:
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		break;

	    case X_ChangeBackground:
	    case X_ChangeBorder:
	    case X_SetIconWindow:
	    case X_DefineCursor:
	    case X_SelectInput:
	    case X_InterpretLocator:
		swapl(&req->windowId);
		pswapl(req, 0);
		break;

	    case X_StoreName:
		swapl(&req->windowId);
		pswaps(req, 0);
		break;

	    case X_GrabMouse:
		swapl(&req->windowId);
		pswapl(req, 0);
		pswapl(req, 1);
		break;

	    case X_GrabButton:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswapl(req, 0);
		pswapl(req, 1);
		break;

	    case X_WarpMouse:
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswapl(req, 1);
		pswaps(req, 4);
		pswaps(req, 5);
		pswaps(req, 6);
		pswaps(req, 7);
		break;

	    case X_PixFill:
	    case X_PixmapBitsPut:
	    case X_StippleFill:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswaps(req, 2);
		pswaps(req, 3);
		pswaps(req, 4);
		pswapl(req, 3);
		break;

	    case X_TileFill:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswaps(req, 2);
		pswaps(req, 3);
		pswapl(req, 2);
		pswapl(req, 3);
		break;

	    case X_PixmapPut:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswaps(req, 2);
		pswaps(req, 3);
		pswapl(req, 2);
		pswaps(req, 6);
		pswaps(req, 7);
		break;

	    case X_BitmapBitsPut:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswaps(req, 2);
		pswaps(req, 3);
		pswaps(req, 4);
		pswaps(req, 5);
		pswapl(req, 3);
		break;

	    case X_CopyArea:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswaps(req, 2);
		pswaps(req, 3);
		pswaps(req, 6);
		pswaps(req, 7);
		break;

	    case X_Text:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswapl(req, 1);
		pswaps(req, 4);
		pswaps(req, 5);
		pswaps(req, 6);
		break;

	    case X_TextMask:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswapl(req, 1);
		pswaps(req, 4);
		pswaps(req, 6);
		break;

	    case X_Line:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswaps(req, 2);
		pswaps(req, 3);
		pswaps(req, 4);
		break;

	    case X_Draw:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswaps(req, 3);
		pswaps(req, 4);
		pswaps(req, 5);
		pswaps(req, 6);
		pswaps(req, 7);
		break;

	    case X_DrawFilled:
		swaps(&req->mask);
		swapl(&req->windowId);
		pswaps(req, 0);
		pswaps(req, 1);
		pswapl(req, 1);
		break;

	    case X_UngrabButton:
		swaps(&req->mask);
		break;

	    case X_GetColor:
		pswaps(req, 0);
		pswaps(req, 1);
		pswaps(req, 2);
		break;

	    case X_GetColorCells:
	    case X_StoreBitmap:
	    case X_StorePixmap:
	    case X_QueryShape:
	    case X_MouseControl:
	    case X_ScreenSaver:
		pswaps(req, 0);
		pswaps(req, 1);
		break;

	    case X_FreeColors:
		swaps(&req->mask);
		pswaps(req, 0);
		break;

	    case X_StoreColors:
	    case X_QueryColor:
	    case X_GetFont:
	    case X_Feep:
	    case X_StoreBytes:
	    case X_LookupColor:
		pswaps(req, 0);
		break;

	    case X_FreeFont:
	    case X_QueryFont:
	    case X_FontWidths:
	    case X_FreeBitmap:
	    case X_FreePixmap:
	    case X_FreeCursor:
		pswapl(req, 0);
		break;

	    case X_CharWidths:
	    case X_StringWidth:
	    case X_CharBitmap:
		pswapl(req, 0);
		pswaps(req, 2);
		break;

	    case X_MakePixmap:
		pswapl(req, 0);
		pswaps(req, 2);
		pswaps(req, 3);
		break;

	    case X_StoreCursor:
		pswapl(req, 0);
		pswaps(req, 2);
		pswaps(req, 3);
		pswapl(req, 2);
		pswaps(req, 6);
		pswaps(req, 7);
		break;
    }
}

/* Byte swap a list of longs */

Swap_longs (list, count)
	register long *list;
	register int count;
{
	register swaptype n;

	while (count >= 8) {
	    swapl(list+0);
	    swapl(list+1);
	    swapl(list+2);
	    swapl(list+3);
	    swapl(list+4);
	    swapl(list+5);
	    swapl(list+6);
	    swapl(list+7);
	    list += 8;
	    count -= 8;
	}
	while (--count >= 0) {
	    swapl(list);
	    list++;
	}
}
#endif

/* Byte swap a list of shorts */

Swap_shorts (list, count)
	register short *list;
	register int count;
{
	register swaptype n;

	while (count >= 16) {
	    swaps(list+0);
	    swaps(list+1);
	    swaps(list+2);
	    swaps(list+3);
	    swaps(list+4);
	    swaps(list+5);
	    swaps(list+6);
	    swaps(list+7);
	    swaps(list+8);
	    swaps(list+9);
	    swaps(list+10);
	    swaps(list+11);
	    swaps(list+12);
	    swaps(list+13);
	    swaps(list+14);
	    swaps(list+15);
	    list += 16;
	    count -= 16;
	}
	while (--count >= 0) {
	    swaps(list);
	    list++;
	}
}

/* Send an Error back to the client. */

Oops (client, req)
	int client;
	register XReq *req;
{
	XRep rep;
#ifdef DUALTCP
	register swaptype n;
#endif

	rep.code = X_Error;
	rep.param.l[0] = requestId[client];
	rep.param.b[4] = Xstatus;
	rep.param.b[5] = req->code;
	rep.param.b[6] = req->func;
	rep.param.l[2] = req->windowId;
#ifdef DUALTCP
	if (swapped[client]) {
	    pswapl(&rep, 0);
	    pswapl(&rep, 2);
	}
#endif
	Write (client, (caddr_t) &rep, sizeof (XRep));
}
