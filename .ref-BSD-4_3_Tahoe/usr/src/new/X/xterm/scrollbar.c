/*
 *	@(#)scrollbar.c	1.11 (Berkeley/CSRG) 9/18/87
 *	$Source: /u1/X/xterm/RCS/scrollbar.c,v $
 *	$Header: scrollbar.c,v 10.100 86/12/01 14:45:27 jg Rel $
 */

#include <stdio.h>
#include <sys/time.h>
#include <X/Xlib.h>
#include <setjmp.h>
#include "scrollbar.h"
#include "ptyx.h"
#include "data.h"
#include "error.h"
#ifdef MODEMENU
#include "menu.h"
#endif MODEMENU

#include "button.ic"
#include "dark.ic"
#include "light.ic"
#include "upline.ic"
#include "downline.ic"
#include "uppage.ic"
#include "downpage.ic"
#include "top.ic"
#include "bottom.ic"
#include "saveoff.ic"
#include "saveon.ic"
#include "sbcursor.cursor"
#include "sbcursor_mask.cursor"
#ifndef lint
static char csrg_id[] = "@(#)scrollbar.c	1.11\t(Berkeley/CSRG)\t9/18/87";
static char sccs_id[] = "@(#)scrollbar.c\tX10/6.6B\t12/26/86";
#endif	lint

static struct timeval stepspeed;

ScrollBar *
CreateScrollBar(w, x, y, height, fg, bg, bordertile, val, valregion,
 topval, botval, arrow)
	Window w;
	int x, y, height, fg, bg, val, valregion, topval, botval;
	Pixmap bordertile;
	Cursor arrow;
{
	register ScrollBar *sb;
	register int i;
	Pixmap btile, bgnd;
	extern char *calloc();
	static Window Make_tiled_window();
	extern Pixmap Make_tile();

	if(!w || height < MINSCROLLBARHEIGHT ||
	 (sb = (ScrollBar *)calloc(1, sizeof(ScrollBar))) == NULL)
		return(NULL);
	btile = bordertile;
	if(bg == BlackPixel && fg == WhitePixel) {
		bg = WhitePixel;
		fg = BlackPixel;
		if(btile == WhitePixmap)
			btile = BlackPixmap;
	}
	sb->fg = fg;
	sb->bg = bg;
	sb->cursor = XCreateCursor(sbcursor_width, sbcursor_height,
				   sbcursor_bits, sbcursor_mask_bits,
				   sbcursor_x_hot, sbcursor_y_hot,
				   fg, bg, GXcopy);
	if((sb->bar = Make_tiled_window(light_width, light_height, light_bits,
	 fg, bg, &bgnd, w, x, y, (SCROLLBARWIDTH - 1), height, 1, bordertile))
	 == NULL)
		goto failed_bar;
	if((sb->button = XCreateWindow(sb->bar, -1, -1, (SCROLLBARWIDTH - 1),
	 BUTTONHEIGHT - 1, 1, btile, bgnd)) == NULL)
		goto failed_button;
	if((sb->save = XCreateWindow(sb->bar, -1, BUTTONHEIGHT - 1,
	 (SCROLLBARWIDTH - 1), BUTTONHEIGHT - 1, 1, btile, bgnd)) == NULL)
		goto failed_save;
	if((sb->region = Make_tiled_window(dark_width, dark_height, dark_bits,
	 fg, bg, &bgnd, sb->bar, 0, 0, (SCROLLBARWIDTH - 1), 10, 0,
	 (Pixmap)NULL)) == NULL) {
		XDestroyWindow(sb->save);
failed_save:
		XDestroyWindow(sb->button);
failed_button:
		XDestroyWindow(sb->bar);
failed_bar:
		free((char *)sb);
		return(NULL);
	}
	sb->savebits[SAVE_OFF] = saveoff_bits;
	sb->savebits[SAVE_ON] = saveon_bits;
	sb->buttonbits[BUTTON_UPLINE / 2] = upline_bits;
	sb->buttonbits[BUTTON_DOWNLINE / 2] = downline_bits;
	sb->buttonbits[BUTTON_UPPAGE / 2] = uppage_bits;
	sb->buttonbits[BUTTON_DOWNPAGE / 2] = downpage_bits;
	sb->buttonbits[BUTTON_TOP / 2] = top_bits;
	sb->buttonbits[BUTTON_BOTTOM / 2] = bottom_bits;
	sb->buttonbits[BUTTON_NORMAL / 2] = button_bits;
	XDefineCursor(sb->bar, sb->cursor);
	XSelectInput(sb->bar, ButtonPressed | ButtonReleased | ExposeWindow |
	 EnterWindow | LeaveWindow | UnmapWindow);
	XSelectInput(sb->button, EnterWindow | LeaveWindow);
	XMapWindow(sb->button);	/* will really map when bar is mapped */
	XMapWindow(sb->save);	/* will really map when bar is mapped */
	sb->buttonstate = sb->buttonset = BUTTON_NORMAL;
	sb->savestate = sb->saveset = SAVE_ON;
	sb->set.value = val;
	sb->set.regionheight = valregion;
	sb->set.topvalue = topval;
	sb->set.bottomvalue = botval;
	sb->set.height = height - BARSTART;
	return(sb);
}

ShowScrollBar(sb)
	register ScrollBar *sb;
{
	if(sb->visible)
		return;
	sb->visible = 1;
	if(sb->regionvisible) {
		XUnmapWindow(sb->region);
		sb->regionvisible = 0;
	}
	XMapWindow(sb->bar);
	DrawScrollRegion(sb);
	sb->action = SHOW;
}

HideScrollBar(sb)
	register ScrollBar *sb;
{
	if(!sb->visible)
		return;
	sb->visible = 0;
	XUnmapWindow(sb->bar);
}

DrawScrollRegion(sb)
	register ScrollBar *sb;
{
	register int region, temp;

	if(sb->set.regionheight <= 0)
		sb->set.regionheight = 0;
	if((region = sb->set.topvalue - sb->set.bottomvalue) >= 0) {
		if(sb->set.value > sb->set.topvalue)
			sb->set.value = sb->set.topvalue;
		else if(sb->set.value < sb->set.bottomvalue)
			sb->set.value = sb->set.bottomvalue;
	} else {
		region = -region;
		if(sb->set.value < sb->set.topvalue)
			sb->set.value = sb->set.topvalue;
		else if(sb->set.value > sb->set.bottomvalue)
			sb->set.value = sb->set.bottomvalue;
	}
	if(sb->set.value == sb->set.topvalue) {
		sb->set.pixelheight = (region == 0) ? sb->set.height :
		 (sb->set.height - 1) * sb->set.regionheight /
		 (sb->set.regionheight + region);
		sb->set.y = BARSTART;
	} else if(sb->set.value == sb->set.bottomvalue) {
		sb->set.pixelheight = (sb->set.height - 1) *
		 sb->set.regionheight / (sb->set.regionheight + region);
		sb->set.y = BARSTART + sb->set.height - sb->set.pixelheight;
	} else {
		if(sb->set.topvalue >= sb->set.bottomvalue) {
			temp = sb->set.topvalue - 1;
			region = temp - (sb->set.bottomvalue + 1);
			sb->set.y = temp - sb->set.value;
		} else {
			temp = sb->set.topvalue + 1;
			region = (sb->set.bottomvalue - 1) - temp;
			sb->set.y = sb->set.value - temp;
		}
		sb->set.y = (BARSTART + 1) + sb->set.y * (sb->set.height - 2) /
		 (temp = sb->set.regionheight + region);
		sb->set.pixelheight = (sb->set.height - 2) *
		 sb->set.regionheight / temp;
	}
	if(sb->set.pixelheight <= 0)
		sb->set.pixelheight = 1;
	if(sb->set.regionheight == 0) {
		sb->state = sb->set;
		if(sb->regionvisible) {
			XUnmapWindow(sb->region);
			sb->regionvisible = 0;
		}
		return;
	}
	if(!sb->visible || sb->regionvisible
	 && sb->state.y == sb->set.y
	 && sb->state.pixelheight == sb->set.pixelheight) {
		sb->state = sb->set;
		return;
	}
	sb->state = sb->set;
	XConfigureWindow(sb->region, 0, sb->state.y, (SCROLLBARWIDTH - 1),
	 sb->state.pixelheight);
	if(!sb->regionvisible) {
		XMapWindow(sb->region);
		sb->regionvisible = 1;
	}
}

DrawButton(sb)
	register ScrollBar *sb;
{
	register int fg, bg;

	if(sb->visible && sb->buttonstate != sb->buttonset) {
		if((sb->buttonstate = sb->buttonset) & HILITED) {
			fg = sb->bg;
			bg = sb->fg;
		} else {
			fg = sb->fg;
			bg = sb->bg;
		}
		XBitmapBitsPut(sb->button, 0, 0, SCROLLBARWIDTH - 1,
		 BUTTONHEIGHT - 1, sb->buttonbits[sb->buttonstate / 2],
		 fg, bg, (Bitmap)0, GXcopy, AllPlanes);
	}
}

DrawSave(sb)
	register ScrollBar *sb;
{
	if(sb->visible && sb->savestate != sb->saveset)
		XBitmapBitsPut(sb->save, 0, 0, SCROLLBARWIDTH - 1,
		 BUTTONHEIGHT - 1, sb->savebits[sb->savestate = sb->saveset],
		 sb->fg, sb->bg, (Bitmap)0, GXcopy, AllPlanes);
}

ResizeScrollBar(sb, x, y, height, region)
	register ScrollBar *sb;
	int x, y, height, region;
{
	register int act;

	act = sb->action;
	sb->action = NONE;
	switch(act) {
	 case SHOW:
		return;
	 case HIDE:
		HideScrollBar(sb);
		return;
	}
	if(!sb->visible)
		return;
	if(sb->regionvisible) {
		XUnmapWindow(sb->region);
		sb->regionvisible = 0;
	}
	XConfigureWindow(sb->bar, x, y, (SCROLLBARWIDTH - 1), height);
	sb->set.height = height - BARSTART;
	sb->set.regionheight = region;
	DrawScrollRegion(sb);
}

PositionRegion(sb, y)
	register ScrollBar *sb;
	register int y;
{
	if(y <= BARSTART)
		sb->set.value = sb->set.topvalue;
	else if(y >= BARSTART + sb->set.height *
	 (sb->set.bottomvalue - sb->set.topvalue) /
	 (sb->set.bottomvalue + sb->set.regionheight - sb->set.topvalue))
		sb->set.value = sb->set.bottomvalue;
	else
		sb->set.value = sb->set.topvalue + (y - BARSTART) *
		 (sb->set.bottomvalue + sb->set.regionheight - sb->set.topvalue)
		 / sb->set.height;
	DrawScrollRegion(sb);
	return(sb->state.value);
}

ButtonRegion(sb)
	register ScrollBar *sb;
{
	register int reverse, pagesize;

	if(!(sb->buttonset & HILITED))
		return(sb->set.value);
	reverse = (sb->set.bottomvalue > sb->set.topvalue);
	pagesize = sb->set.regionheight - 1;
	switch(sb->buttonset) {
	 case BUTTON_UPLINEHI:
		if(reverse)
			sb->set.value--;
		else
			sb->set.value++;
		break;
	 case BUTTON_DOWNLINEHI:
		if(reverse)
			sb->set.value++;
		else
			sb->set.value--;
		break;
	 case BUTTON_UPPAGEHI:
		if(reverse)
			sb->set.value -= pagesize;
		else
			sb->set.value += pagesize;
		break;
	 case BUTTON_DOWNPAGEHI:
		if(reverse)
			sb->set.value += pagesize;
		else
			sb->set.value -= pagesize;
		break;
	 case BUTTON_TOPHI:
		sb->set.value = sb->set.topvalue;
		break;
	 case BUTTON_BOTTOMHI:
		sb->set.value = sb->set.bottomvalue;
	}
	DrawScrollRegion(sb);
	return(sb->set.value);
}

DownButtonDown(term, reply, pty)
	Terminal *term;
	register XKeyOrButtonEvent *reply;
	int pty;			/* file descriptor of pty */
{
	register Screen *screen = &term->screen;
	register ScrollBar *sb = screen->sb;
	register Window window = reply->subwindow;

	if(!window || window == sb->region) {
		register int page = sb->set.regionheight;
		register int height = sb->set.height + BARSTART;

		if (reply->detail & ShiftMask)
			sb->set.value += page * (height - reply->y) / height;
		else
			sb->set.value += page * reply->y / height;
		DrawScrollRegion(sb);
		WindowScroll(screen, sb->set.value);
		sb->buttonset = BUTTON_DOWNLINEHI;
		stepspeed.tv_usec = PAUSETIME;
		screen->timeout = &stepspeed;
		return;
	}
	if(window == sb->save) {
		SetSaveState(sb, !GetSaveState(sb));
		return;
	}
	if(window != sb->button) {
		Bell();
		return;
	}
	if(reply->detail & ControlMask)
		sb->buttonset = BUTTON_BOTTOMHI;
	else if(reply->detail & ShiftMask)
		sb->buttonset = BUTTON_DOWNPAGEHI;
	else {
		sb->buttonset = BUTTON_DOWNLINEHI;
		stepspeed.tv_usec = PAUSETIME;
		screen->timeout = &stepspeed;
		WindowScroll(screen, ButtonRegion(screen->sb));
	}
	DrawButton(sb);
}

UpButtonDown(term, reply, pty)
	Terminal *term;
	register XKeyOrButtonEvent *reply;
	int pty;			/* file descriptor of pty */
{
	register Screen *screen = &term->screen;
	register ScrollBar *sb = screen->sb;
	register Window window = reply->subwindow;

	if(window == sb->save) {
		SetSaveState(sb, !GetSaveState(sb));
		return;
	}
	if(!window || window == sb->region) {
		register int page = sb->set.regionheight;
		register int height = sb->set.height + BARSTART;

		if (reply->detail & ShiftMask)
			sb->set.value -= page * (height - reply->y) / height;
		else
			sb->set.value -= page * reply->y / height;
		DrawScrollRegion(sb);
		WindowScroll(screen, sb->set.value);
		sb->buttonset = BUTTON_UPLINEHI;
		stepspeed.tv_usec = PAUSETIME;
		screen->timeout = &stepspeed;
		return;
	}
	if(window != sb->button) {
		Bell();
		return;
	}
	if(reply->detail & ControlMask)
		sb->buttonset = BUTTON_TOPHI;
	else if(reply->detail & ShiftMask)
		sb->buttonset = BUTTON_UPPAGEHI;
	else {
		sb->buttonset = BUTTON_UPLINEHI;
		stepspeed.tv_usec = PAUSETIME;
		screen->timeout = &stepspeed;
		WindowScroll(screen, ButtonRegion(screen->sb));
	}
	DrawButton(sb);
}

ButtonUp(term, reply, pty)
	Terminal *term;
	XKeyOrButtonEvent *reply;
	int pty;			/* file descriptor of pty */
{
	register Screen *screen = &term->screen;
	register ScrollBar *sb = screen->sb;
	register int state;

	if((state = GetButtonState(sb)) == BUTTON_NORMAL)
		return;
	/* don't scroll further on line mode */
	if(state > BUTTON_DOWNLINEHI)
		WindowScroll(screen, ButtonRegion(sb));
	sb->buttonset = BUTTON_NORMAL;
	DrawButton(sb);
	screen->timeout = NULL;
	XUngrabMouse();
}

WindowScroll(screen, top)
	register Screen *screen;
	int top;
{
	register int i, lines;
	register int scrolltop, scrollheight, refreshtop;

	if((i = screen->topline - top) == 0)
		return;
	if(screen->cursor_state)
		HideCursor();
	lines = i > 0 ? i : -i;
	if(lines > screen->max_row + 1)
		lines = screen->max_row + 1;
	scrollheight = screen->max_row - lines + 1;
	if(i > 0)
		refreshtop = scrolltop = 0;
	else {
		scrolltop = lines;
		refreshtop = scrollheight;
	}
	if(scrollheight > 0) {
		if (screen->multiscroll && scrollheight == 1 &&
		 screen->topline == 0 && screen->top_marg == 0 &&
		 screen->bot_marg == screen->max_row) {
			if (screen->incopy < 0 && screen->scrolls == 0)
				CopyWait (screen);
			screen->scrolls++;
		} else {
			if (screen->incopy)
				CopyWait (screen);
			screen->incopy = -1;
		}
		XMoveArea(VWindow(screen), screen->border, scrolltop *
		 FontHeight(screen) + screen->border + Titlebar(screen),
		 screen->border, (scrolltop + i) * FontHeight(screen) +
		 screen->border + Titlebar(screen), Width(screen),
		 scrollheight * FontHeight(screen));
	}
	screen->topline = top;
	XTileSet(VWindow(screen), screen->border, refreshtop * FontHeight(screen) +
	 screen->border + Titlebar(screen), Width(screen), lines *
	 FontHeight(screen), screen->bgndtile);
	ScrnRefresh(screen, refreshtop, 0, lines, screen->max_col + 1);
}

ScrollBarOn(screen, show, init)
	register Screen *screen;
	int show, init;
{
	register int border = 2 * screen->border;
	register int i;
	char *realloc(), *calloc();

	if(screen->scrollbar)
		return;
	if(!screen->sb) {
		if((screen->sb = CreateScrollBar(VWindow(screen),
		 Width(screen) + border, Titlebar(screen) - 1,
		 Height(screen) + border, screen->foreground,
		 screen->background, screen->bordertile, 0,
		 screen->max_row + 1, 0, 0, screen->arrow)) == NULL) {
			Bell();
			return;
		}
		if((screen->allbuf = (ScrnBuf) realloc(screen->buf,
		 2*(screen->max_row + 2 + screen->savelines) * sizeof(char *)))
		 == NULL)
			Error (ERROR_SBRALLOC);
		screen->buf = &screen->allbuf[2 * screen->savelines];
		bcopy ((char *)screen->allbuf, (char *)screen->buf,
		 2 * (screen->max_row + 2) * sizeof (char *));
		for(i = 2 * screen->savelines - 1 ; i >= 0 ; i--)
			if((screen->allbuf[i] =
			 calloc(screen->max_col + 1, sizeof(char))) == NULL)
				Error (ERROR_SBRALLOC2);
		screen->sb->saveset = !screen->alternate;
	} else {
		XConfigureWindow(screen->sb->bar, FullWidth(screen),
		 Titlebar(screen) - 1, (SCROLLBARWIDTH - 1),
		 i = FullHeight(screen) - Titlebar(screen));
		screen->sb->set.height = i - BARSTART;
		screen->sb->set.regionheight = screen->max_row + 1;
	}
	if(show) {
		screen->scrollbar = SCROLLBARWIDTH;
		ShowScrollBar(screen->sb);
		if(!init) {
			XSetResizeHint(VWindow(screen), border + SCROLLBARWIDTH,
			 border + Titlebar(screen) + screen->statusheight,
			 FontWidth(screen), FontHeight(screen));
			XChangeWindow(VWindow(screen), (screen->max_col + 1) *
			 FontWidth(screen) + border + SCROLLBARWIDTH,
			 FontHeight(screen) * (screen->max_row + 1) +
			 screen->statusheight + border + Titlebar(screen));
		}
	}
}

ScrollBarOff(screen)
	register Screen *screen;
{
	register int border = 2 * screen->border;

	if(!screen->scrollbar)
		return;
	screen->sb->action = HIDE;
	screen->scrollbar = 0;
	XSetResizeHint(VWindow(screen), border, border + Titlebar(screen) +
	 screen->statusheight, FontWidth(screen), FontHeight(screen));
	XChangeWindow(VWindow(screen), (screen->max_col + 1) * FontWidth(screen) +
	 border, FontHeight(screen) * (screen->max_row + 1) + screen->statusheight
	 + border + Titlebar(screen));
}

ClearLinesOffTop(screen)
	register Screen *screen;
{
	if(!screen->sb)
		return;
	if(screen->topline)
		WindowScroll(screen, 0);
	SetScrollBarTop(screen->sb, 0);
	DrawScrollRegion(screen->sb);
}

SetSaveState(sb, state)
	register ScrollBar *sb;
	int state;
{
	extern Terminal term;
	register Screen *screen = &term.screen;

	if(screen->alternate)
		return;
	if(screen->scroll_amt)
		FlushScroll(screen);
	sb->saveset = state;
	DrawSave(sb);
}

SetButtonState(sb, state)
	register ScrollBar *sb;
	int state;
{
	sb->buttonset = state;
	DrawButton(sb);
}

static Window
Make_tiled_window(bitmap_width, bitmap_height, bitmap_bits, foreground,
 background, bgnd, parent, x, y, width, height, borderwidth, bordertile)
	int bitmap_width, bitmap_height, foreground, background, x, y, width,
	 height, borderwidth;
	short *bitmap_bits;
	Window parent;
	Pixmap *bgnd, bordertile;
{
	register Pixmap pix;
	register Window w;
	extern Pixmap Make_tile();

	if((pix = Make_tile(bitmap_width, bitmap_height, bitmap_bits,
	 foreground, background)) == NULL)
		return(NULL);
	w = XCreateWindow(parent, x, y, width, height, borderwidth, bordertile,
	 pix);
	*bgnd = pix;
	return(w);
}

Pixmap
Make_tile(bitmap_width, bitmap_height, bitmap_bits, foreground, background)
	int bitmap_width, bitmap_height, foreground, background;
	short *bitmap_bits;
{
	register Bitmap bm;
	register Pixmap pix;

	if((bm = XStoreBitmap(bitmap_width, bitmap_height, bitmap_bits))
	 == NULL)
		return(NULL);
	pix = XMakePixmap(bm, foreground, background);
	XFreeBitmap(bm);
	return(pix);
}

ScrollToBottom(sb)
register ScrollBar *sb;
{
	SetScrollBarValue(sb, GetScrollBarBottom(sb));
	DrawScrollRegion(sb);
	WindowScroll(&term.screen, GetScrollBarValue(sb));
}

#ifdef MODEMENU
#define	SMENU_SCROLLKEY	0
#define	SMENU_SCROLLINPUT (SMENU_SCROLLKEY+1)
#define	SMENU_LINESTOP	(SMENU_SCROLLINPUT+1)
#define	SMENU_LINE	(SMENU_LINESTOP+1)
#define	SMENU_CLEARTOP	(SMENU_LINE+1)
#define	SMENU_HIDE	(SMENU_CLEARTOP+1)

static char *stext[] = {
	"Scroll to Bottom on Key",
	"Scroll to Bottom on Input",
	"Lines Off Top Saved",
	"-",
	"Clear Lines Off Top",
	"Hide Scrollbar",
	0,
};


static int salternate;
static int slinestop;
static int sscrollinput;
static int sscrollkey;

Menu *ssetupmenu(menu)
register Menu **menu;
{
	register Screen *screen = &term.screen;
	register char **cp;

	if (*menu == NULL) {
		if ((*menu = NewMenu("Scrollbar", re_verse)) == NULL)
			return(NULL);
		for(cp = stext ; *cp ; cp++)
			AddMenuItem(*menu, *cp);
		if(sscrollkey = screen->scrollkey)
			CheckItem(*menu, SMENU_SCROLLKEY);
		if(sscrollinput = screen->scrollinput)
			CheckItem(*menu, SMENU_SCROLLINPUT);
		if(slinestop = (screen->sb && GetSaveState(screen->sb)))
			CheckItem(*menu, SMENU_LINESTOP);
		if(salternate = screen->alternate)
			DisableItem(*menu, SMENU_LINESTOP);
		DisableItem(*menu, SMENU_LINE);
		return(*menu);
	}
	if(sscrollkey != screen->scrollkey)
		SetItemCheck(*menu, SMENU_SCROLLKEY, (sscrollkey =
		 screen->scrollkey));
	if(sscrollinput != screen->scrollinput)
		SetItemCheck(*menu, SMENU_SCROLLINPUT, (sscrollinput =
		 screen->scrollinput));
	if(screen->sb && slinestop != GetSaveState(screen->sb))
		SetItemCheck(*menu, SMENU_LINESTOP, (slinestop =
		 GetSaveState(screen->sb)));
	if(salternate != screen->alternate)
		SetItemDisable(*menu, SMENU_LINESTOP, (salternate =
		 screen->alternate));
	return(*menu);
}

sdomenufunc(item)
int item;
{
	register Screen *screen = &term.screen;

	switch (item) {
	 case SMENU_SCROLLKEY:
		screen->scrollkey = !screen->scrollkey;
		break;

	 case SMENU_SCROLLINPUT:
		screen->scrollinput = !screen->scrollinput;
		break;

	 case SMENU_LINESTOP:
		SetSaveState(screen->sb, !GetSaveState(screen->sb));
		break;

	 case SMENU_CLEARTOP:
		ClearLinesOffTop(screen);
		break;

	 case SMENU_HIDE:
		ScrollBarOff(screen);
		break;
	}
}
#endif MODEMENU
