/*
 *	$Source: /u1/X/xterm/RCS/misc.c,v $
 *	$Header: misc.c,v 10.101 86/12/02 08:49:20 swick Exp $
 */

#include <stdio.h>
#include <setjmp.h>
#include <signal.h>
#include <ctype.h>
#include <pwd.h>
#include <sys/time.h>
#include <sys/file.h>
#include <X/Xlib.h>
#include "scrollbar.h"
#include "ptyx.h"
#include "data.h"
#include "error.h"
#include "gray.ic"
#include "hilite.ic"
#include "icon.ic"
#include "tek_icon.ic"
#include "wait.ic"
#include "waitmask.ic"
#include "../cursors/left_ptr.cursor"
#include "../cursors/left_ptr_mask.cursor"
#include "../cursors/tcross.cursor"
#include "../cursors/tcross_mask.cursor"
#include "../cursors/xterm.cursor"
#include "../cursors/xterm_mask.cursor"

#ifndef lint
static char csrg_id[] = "@(#)misc.c	1.8\t(Berkeley/CSRG)\t9/18/87";
static char sccs_id[] = "@(#)misc.c\tX10/6.6B\t1/9/87";
#endif	lint

xevents()
{
	XEvent reply;
	register XEvent *rep = & reply;
	register Screen *screen = &term.screen;

	if(screen->scroll_amt)
		FlushScroll(screen);
	XPending ();
	do {
		XNextEvent (&reply);
		xeventpass(&reply);
	} while (QLength() > 0);
}

xeventpass(rep)
register XEvent *rep;
{
	register Screen *screen = &term.screen;
	register Window window = rep->window;
	register Window w;
	register int i;

	switch ((int)rep->type) {
	 case KeyPressed:
		Input (&term.keyboard, &term.screen,
		 (XKeyPressedEvent *)rep);
		break;

	 case ExposeWindow:
		if(screen->sb && window == screen->sb->bar) {
#ifdef DEBUG
			if(debug)
				fputs("ExposeWindow scrollbar\n", stderr);
#endif DEBUG
			if(((XExposeEvent *)rep)->subwindow ==
			 screen->sb->button) {
				screen->sb->buttonstate = -1;
				DrawButton(screen->sb);
			} else if(((XExposeEvent *)rep)->subwindow ==
			 screen->sb->save) {
				screen->sb->savestate = -1;
				DrawSave(screen->sb);
			}
			break;
		}
		if (screen->active_icon) {
#ifdef DEBUG
		    fputs( "ExposeWindow icon\n", stderr );
#endif DEBUG
		    if (window == screen->iconVwin.window) {
		        if (!screen->icon_show) {
			    screen->mappedVwin = &screen->iconVwin;
			    screen->icon_show = TRUE;
			    screen->show = FALSE;
			    screen->timer = 0;
			    screen->holdoff = FALSE;
			    if(screen->fullTwin.window)
				moveiconwindow( screen->fullTwin.window,
						screen->fullVwin.window );
			}
			if (screen->TekEmu)
			    VTUnselect();
			if (term.flags & ICONINPUT)
			    reselectwindow(screen);
			else
			    unselectwindow(screen, INWINDOW);
			screen->iconinput = FALSE;
			VTExpose(rep);
		    } else if (window == screen->iconTwin.window) {
			if (!screen->icon_show) {
			    screen->mappedTwin = &screen->iconTwin;
			    screen->icon_show = TRUE;
			    screen->Tshow = FALSE;
			    screen->timer = 0;
			    screen->holdoff = FALSE;
			    if (screen->fullVwin.window)
				moveiconwindow( screen->fullVwin.window,
						screen->fullTwin.window );
			}
			if (!screen->TekEmu)
			    TekUnselect();
			if (term.flags & ICONINPUT)
			    reselectwindow(screen);
			else
			    unselectwindow(screen, INWINDOW);
			screen->iconinput = FALSE;
			TekExpose(rep);
			}
		} else if (window == screen->iconVwin.window ||
			   window == screen->iconTwin.window) {
#ifdef DEBUG
			if(debug)
				fprintf(stderr, "ExposeWindow %s\n", window ==
				 screen->iconVwin.window ? "icon" : "Ticon");
#endif DEBUG
			RefreshIcon(screen, window);
			break;
		}
		if(Titlebar(screen) || screen->icon_show) {
		/* icon_show is a kludge as the titlebar exposure event
		 * frequently arrives before the full window exposure event */
			if(window == screen->title.tbar) {
#ifdef DEBUG
			if(debug)
				fputs("ExposeWindow title\n", stderr);
#endif DEBUG
				VTTitleExpose((XExposeWindowEvent *)rep);
				break;
			} else if(window == screen->Ttitle.tbar) {
#ifdef DEBUG
			if(debug)
				fputs("ExposeWindow Ttitle\n", stderr);
#endif DEBUG
				TekTitleExpose((XExposeWindowEvent *)rep);
				break;
			}
		}
		if(window == screen->fullVwin.window) {
#ifdef DEBUG
			if(debug)
				fputs("ExposeWindow VT\n", stderr);
#endif DEBUG
			if(!screen->show) {
				screen->mappedVwin = &screen->fullVwin;
				if(screen->Ticonunmap) {
					screen->Ticonunmap = FALSE;
					XMapWindow( screen->fullTwin.window );
					screen->Tshow = TRUE;
					if(!screen->TekEmu) {
					    screen->holdoff = TRUE;
					    XUnmapTransparent(VWindow(screen));
					    XMapWindow(VWindow(screen));
					    break;
					}
				} else {
					screen->timer = 0;
					screen->holdoff = FALSE;
				}
				screen->show = TRUE;
				reselectwindow(screen);
				if(screen->icon_show && screen->deiconwarp)
				    DeiconWarp(screen);

				screen->icon_show = FALSE;
				screen->iconinput = FALSE;
			}
			VTExpose(rep);
		} else if(window == screen->fullTwin.window) {
#ifdef DEBUG
			if(debug)
				fputs("ExposeWindow Tek\n", stderr);
#endif DEBUG
			if(!screen->Tshow) {
				screen->mappedTwin = &screen->fullTwin;
				if(screen->iconunmap) {
					screen->iconunmap = FALSE;
					XMapWindow( screen->fullVwin.window );
					screen->show = TRUE;
					if(screen->TekEmu) {
					    screen->holdoff = TRUE;
					    XUnmapTransparent(TWindow(screen));
					    XMapWindow(TWindow(screen));
					    break;
					}
				} else {
					screen->timer = 0;
					screen->holdoff = FALSE;
				}
				screen->Tshow = TRUE;
				reselectwindow(screen);
				if(screen->icon_show && screen->deiconwarp)
					DeiconWarp(screen);
				screen->icon_show = FALSE;
				screen->iconinput = FALSE;
			}
			TekExpose(rep);
		}
		break;

	 case ExposeRegion:
		if (!screen->active_icon && window == screen->iconVwin.window ||
			   window == screen->iconTwin.window) {
#ifdef DEBUG
			if(debug)
				fprintf(stderr, "ExposeRegion %s\n", window ==
				 screen->iconVwin.window ? "icon" : "Ticon");
#endif DEBUG
			RefreshIcon(screen, window);
			break;
		}
#ifdef DEBUG
		if(debug)
			fputs("ExposeRegion\n", stderr);
#endif DEBUG
		if (((XExposeWindowEvent *)rep)->detail == ExposeCopy &&
		    screen->incopy <= 0) {
			screen->incopy = 1;
			if (screen->scrolls > 0)
				screen->scrolls--;
		}
		if (HandleExposure (screen, rep))
			screen->cursor_state = OFF;
		break;

	 case ExposeCopy:
#ifdef DEBUG
			if(debug)
				fputs("ExposeCopy\n", stderr);
#endif DEBUG
		if (screen->incopy <= 0 && screen->scrolls > 0)
			screen->scrolls--;
		if (screen->scrolls)
			screen->incopy = -1;
		else
			screen->incopy = 0;
		break;

	 case ButtonPressed:
		if (screen->incopy)
			CopyWait (screen);
		if(window == screen->iconVwin.window) {
#ifdef DEBUG
			if(debug)
				fputs("ButtonPressed icon\n", stderr);
#endif DEBUG
			XUnmapWindow(window);
			if(screen->Ticonunmap && !screen->TekEmu) {
				screen->Ticonunmap = FALSE;
				XMapWindow(TWindow(screen));
				screen->Tshow = TRUE;
				screen->holdoff = TRUE;
			}
			XMapWindow(screen->fullVwin.window);
			break;
		} else if(window == screen->iconTwin.window) {
#ifdef DEBUG
			if(debug)
				fputs("ButtonPressed Ticon\n", stderr);
#endif DEBUG
			XUnmapWindow(window);
			if(screen->iconunmap && screen->TekEmu) {
				screen->iconunmap = FALSE;
				XMapWindow(VWindow(screen));
				screen->show = TRUE;
				screen->holdoff = TRUE;
			}
			XMapWindow(screen->fullTwin.window);
			break;
		}
		/* drop through */
	 case ButtonReleased:
#ifdef DEBUG
			if(debug)
				fputs("ButtonPressed or ButtonReleased\n",
				 stderr);
#endif DEBUG
		HandleButtons(&term, rep, screen->respond);
		break;

	 case UnmapWindow:		/* full windows */
		if (window == screen->fullVwin.window) {
#ifdef DEBUG
			if(debug)
				fputs("UnmapWindow VT\n", stderr);
#endif DEBUG
			if (screen->fullVwin.titlebar)
				VTTitleUnhilite();
			screen->show = FALSE;
			if(screen->Tshow) {
				screen->Ticonunmap = TRUE;
				screen->Tshow = FALSE;
				XUnmapWindow( screen->fullTwin.window );
				SyncUnmap( screen->fullTwin.window,
					   TWINDOWEVENTS );
			}
		} else if(window == screen->fullTwin.window) {
#ifdef DEBUG
			if(debug)
				fputs("UnmapWindow Tek\n", stderr);
#endif DEBUG
			if (screen->fullTwin.titlebar)
				TekTitleUnhilite();
			screen->Tshow = FALSE;
			if(screen->show) {
				screen->iconunmap = TRUE;
				screen->show = FALSE;
				XUnmapWindow( screen->fullVwin.window );
				SyncUnmap( screen->fullVwin.window,
					   WINDOWEVENTS );
			}
		}
		reselectwindow(screen);
		screen->timer = 0;
		break;
	 case EnterWindow:
		if(screen->sb) {
			if(window == screen->sb->button) {
				if((i = GetButtonState(screen->sb)) !=
				 BUTTON_NORMAL)
					SetButtonState(screen->sb, i | HILITED);
				break;
			} else if(window == screen->sb->bar)
				break;
		}
		if((window == VWindow(screen) || window == TWindow(screen)) &&
		 (((XEnterWindowEvent *)rep)->detail & 0xff) !=
		 IntoOrFromSubwindow) {
#ifdef DEBUG
			if(debug)
				fprintf(stderr, "EnterWindow %s\n", window ==
				 VWindow(screen) ? "VT" : "Tek");
#endif DEBUG
			screen->autowindow = window;
			DoEnterLeave(screen, EnterWindow);
		}
		break;
	 case LeaveWindow:
		if(screen->sb) {
			if(window == screen->sb->button) {
				if((i = GetButtonState(screen->sb)) !=
				 BUTTON_NORMAL)
					SetButtonState(screen->sb, i& ~HILITED);
				break;
			} else if(window == screen->sb->bar)
				break;
		}
		if((window == VWindow(screen) || window == TWindow(screen)) &&
		 (((XEnterWindowEvent *)rep)->detail & 0xff) !=
		 IntoOrFromSubwindow)
#ifdef DEBUG
		{
			if(debug)
				fprintf(stderr, "LeaveWindow %s\n", window ==
				 VWindow(screen) ? "VT" : "Tek");
#endif DEBUG
			DoEnterLeave(screen, LeaveWindow);
#ifdef DEBUG
		}
#endif DEBUG
		break;
	 case FocusChange:
		if(((XFocusChangeEvent *)rep)->detail == EnterWindow)
			selectwindow(screen, FOCUS);
		else
			unselectwindow(screen, FOCUS);
		break;
	 default:
		break;
	}
}

selectwindow(screen, flag)
register Screen *screen;
register int flag;
{
	if(screen->TekEmu) {
		TekSelect();
		if(!Ttoggled)
			TCursorToggle(TOGGLE);
		if(screen->cellsused) {
			screen->colorcells[2].pixel =
			 screen->Tcursorcolor;
			XStoreColor(&screen->colorcells[2]);
		}
		screen->select |= flag;
		if(!Ttoggled)
			TCursorToggle(TOGGLE);
		return;
	} else {
		VTSelect();
		if(screen->cursor_state &&
		   (screen->cursor_col != screen->cur_col ||
		    screen->cursor_row != screen->cur_row))
		    HideCursor();
		screen->select |= flag;
		if(screen->cursor_state)
			ShowCursor();
		return;
	}
}

unselectwindow(screen, flag)
register Screen *screen;
register int flag;
{
	register int i;

	screen->select &= ~flag;
	if(!screen->select) {
		if(screen->TekEmu) {
			TekUnselect();
			if(!Ttoggled)
				TCursorToggle(TOGGLE);
			if(screen->cellsused) {
				i = (term.flags & REVERSE_VIDEO) == 0;
				screen->colorcells[i].pixel =
				 screen->Tcursorcolor;
				XStoreColor(
				 &screen->colorcells[i]);
			}
			if(!Ttoggled)
				TCursorToggle(TOGGLE);
			return;
		} else {
			VTUnselect();
			if(screen->cursor_state &&
			 (screen->cursor_col != screen->cur_col ||
			 screen->cursor_row != screen->cur_row))
				HideCursor();
			if(screen->cursor_state)
				ShowCursor();
			return;
		}
	}
}

reselectwindow(screen)
register Screen *screen;
{
	Window win;
	int x, y;

	if(XQueryMouse(RootWindow, &x, &y, &win)) {
		if(win && (win == VWindow(screen) || win == TWindow(screen))
		       && (!screen->icon_show
			   || (screen->active_icon && term.flags & ICONINPUT)))
			selectwindow(screen, INWINDOW);
		else	unselectwindow(screen, INWINDOW);
	}
}

DeiconWarp(screen)
register Screen *screen;
{
	if(screen->TekEmu)
		XWarpMouse(TWindow(screen), TFullWidth(screen) / 2,
		 TFullHeight(screen) / 2);
	else
		XWarpMouse(VWindow(screen), FullWidth(screen) / 2,
		 FullHeight(screen) / 2);
}

#define	ENTERLEAVE	500000L

DoEnterLeave(screen, type)
register Screen *screen;
int type;
{
	if (!screen->autoraise && !screen->holdoff) {
	    if (type == EnterWindow)
		    selectwindow( screen, INWINDOW );
	    else    unselectwindow( screen, INWINDOW );
	    return;
	}

	screen->timer = type;
	if(!screen->holdoff)
		Timer(ENTERLEAVE);
}

Timer(val)
long val;
{
	struct itimerval it;

	bzero(&it, sizeof(it));
	it.it_value.tv_usec = val;
	setitimer(ITIMER_REAL, &it, (struct itimerval *)0);
}

/*
 * Handle window select/unselect.
 *
 * We enter from a SIGALRM; X I/O may be in progress,
 * so just set a flag which is polled in in_put().
 */
onalarm()
{
	register Screen *screen = &term.screen;

	if(screen->timer == 0 || screen->holdoff) {	/* extraneous alarm */
		Timer(0L);
		return;
	}
	doonalarmcode++;
}

onalarmcode()
{
	register Screen *screen = &term.screen;

	if(screen->timer == EnterWindow) {
#ifdef DEBUG
		if(debug)
			fprintf(stderr, "onalarm: EnterWindow %s\n",
			 screen->autoraise ? (screen->autowindow ==
			 VWindow(screen) ? "VT" : "Tek") : "");
#endif DEBUG
		selectwindow(screen, INWINDOW);
		if(screen->autoraise)
			XRaiseWindow(screen->autowindow);
	} else	/* LeaveWindow */
#ifdef DEBUG
	{
		if(debug)
			fputs("onalarm: LeaveWindow\n", stderr);
#endif DEBUG
		unselectwindow(screen, INWINDOW);
#ifdef DEBUG
	}
#endif DEBUG
	screen->timer = 0;
	doonalarmcode = 0;
}

Pixmap make_hilite(fg, bg)
int fg, bg;
{
	extern Pixmap Make_tile();

	return(Make_tile(hilite_width, hilite_height, hilite_bits, fg,
	 bg));
}

Pixmap make_gray()
{
	extern Pixmap Make_tile();

	return(Make_tile(gray_width, gray_height, gray_bits, BlackPixel,
	 WhitePixel));
}

Cursor make_tcross(fg, bg, func)
int fg, bg, func;
{
	return(XCreateCursor(tcross_width, tcross_height, tcross_bits,
	 tcross_mask_bits, tcross_x_hot, tcross_y_hot, fg, bg, func));
}

Cursor make_xterm(fg, bg, func)
int fg, bg, func;
{
	return(XCreateCursor(xterm_width, xterm_height, xterm_bits,
	 xterm_mask_bits, xterm_x_hot, xterm_y_hot, fg, bg, func));
}

Cursor make_wait(fg, bg, func)
int fg, bg, func;
{
	return(XCreateCursor(wait_width, wait_height, wait_bits, waitmask_bits,
	 wait_x_hot, wait_y_hot, fg, bg, func));
}

Cursor make_arrow(fg, bg, func)
int fg, bg, func;
{
	return(XCreateCursor(left_ptr_width, left_ptr_height, left_ptr_bits,
	 left_ptr_mask_bits, left_ptr_x_hot, left_ptr_y_hot, fg, bg, func));
}

char *uniquesuffix(name)
char *name;
{
	register int *np, *fp, i;
	register Window *cp;
	register int temp, j, k, exact, *number;
	char *wname;
	Window *children, parent;
	int nchildren;
	static char *suffix, sufbuf[10];
	char *malloc();

	if(suffix)
		return(suffix);
	suffix = sufbuf;
	if(!XQueryTree(RootWindow, &parent, &nchildren, &children) ||
	 nchildren < 1 || (number = (int *)malloc(nchildren * sizeof(int)))
	 == NULL)
		return(suffix);
	exact = FALSE;
	i = strlen(name);
	for(np = number, cp = children, j = nchildren ; j > 0 ; cp++, j--) {
		if(!XFetchName(*cp, &wname) || wname == NULL)
			continue;
		if(strncmp(name, wname, i) == 0) {
			if(wname[i] == 0 || strcmp(&wname[i], " (Tek)") == 0)
				exact = TRUE;
			else if(strncmp(&wname[i], " #", 2) == 0)
				*np++ = atoi(&wname[i + 2]);
		}
		free(wname);
	}
	free((char *)children);
	if(exact) {
		if(np <= number)
			strcpy(suffix, " #2");
		else {
			exact = np - number;
			np = number;
			/* shell sort */
			for(i = exact / 2 ; i > 0 ; i /= 2)
				for(k = i ; k < exact ; k++)
					for(j = k - i ; j >= 0 &&
					 np[j] > np[j + i] ; j -= i) {
						temp = np[j];
						np[j] = np[j + i];
						np[j + i] = temp;
					}
			/* make numbers unique */
			for(fp = np + 1, i = exact - 1 ; i > 0 ; fp++, i--)
				if(*fp != *np)
					*++np = *fp;
			/* find least unique number */
			for(i = 2, fp = number ; fp <= np ; fp++) {
				if(i < *fp)
					break;
				if(i == *fp)
					i++;
			}
			sprintf(suffix, " #%d", i);
		}
	}
	free((char *)number);
	return(suffix);
}

Bell()
{
	extern Terminal term;
	register Screen *screen = &term.screen;
	register Window w;
	register int width, height, xorPixel;

	if (screen->audiblebell)
		XFeep(0);

	if (screen->visualbell) {
		if (screen->TekEmu) {
			if (screen->icon_show && !screen->active_icon) {
				w = screen->iconTwin.window;
				width = screen->iconTwin.width;
				height = screen->iconTwin.height;
			} else {
				w = TWindow(screen);
				width = TFullWidth(screen);
				height = TFullHeight(screen);
			}
		} else {
			if (screen->icon_show && !screen->active_icon) {
				w = screen->iconVwin.window;
				width = screen->iconVwin.width;
				height = screen->iconVwin.height;
			} else {
				w = VWindow(screen);
				width = FullWidth(screen);
				height = FullHeight(screen);
			}
		}
		xorPixel = screen->foreground ^ screen->background;
		XPixFill(w, 0, 0, width, height, xorPixel, 0, GXxor, AllPlanes);
		XFlush();
		usleep(screen->visbelldelay * 1000);
		XPixFill(w, 0, 0, width, height, xorPixel, 0, GXxor, AllPlanes);
	}
}

Redraw()
{
	extern Terminal term;
	register Screen *screen = &term.screen;

	if(VWindow(screen) && screen->show) {
		VTExpose(NULL);
		if(screen->scrollbar) {
			XClear(screen->sb->bar);
			XClear(screen->sb->region);
			screen->sb->buttonstate = -1;
			DrawButton(screen->sb);
			screen->sb->savestate = -1;
			DrawSave(screen->sb);
		}
		if(Titlebar(screen)) {
			XClear(screen->title.tbar);
			XClear(screen->title.left);
			XClear(screen->title.right);
			VTTitleExpose(NULL);
		}
	}
	if(TWindow(screen) && screen->Tshow) {
		TekExpose(NULL);
		if(Titlebar(screen)) {
			XClear(screen->Ttitle.tbar);
			XClear(screen->Ttitle.left);
			XClear(screen->Ttitle.right);
			TekTitleExpose(NULL);
		}
	}
}

IconInit(screen, bm, Tbm)
register Screen *screen;
char *bm, *Tbm;
{
	register int w;

again:
	if(!bm && !Tbm) {	/* use default bitmaps */
		screen->iconbitmap.bits = icon_bits;
		screen->Ticonbitmap.bits = tek_icon_bits;
		screen->bitmapwidth = screen->iconbitmap.width =
		 screen->Ticonbitmap.width = icon_width;
		screen->bitmapheight = screen->iconbitmap.height =
		 screen->Ticonbitmap.height = icon_height;
	} else if(bm && !*bm && Tbm && !*Tbm)	/* both empty means no bitmap */
		screen->bitmapwidth = screen->bitmapheight = 0;
	else {			/* user defined bitmap(s) */
		if(bm && *bm) {
			if((w = XReadBitmapFile(bm, &screen->iconbitmap.width,
			 &screen->iconbitmap.height, &screen->iconbitmap.bits,
			 NULL, NULL)) == 0) {
				/*
				 * Pretend there wasn't an icon specified
				 * and try for the default icon.
				 */
				fprintf(stderr, "%s: Can't open %s\n",
				 xterm_name, bm);
				bm = 0;
				goto again;
			} else if(w < 0) {
syntaxerror:
				fprintf(stderr, "%s: Syntax error in %s\n",
				 xterm_name, bm);
				Exit(ERROR_SYNTAXBITMAP);
			}
			screen->bitmapwidth = screen->iconbitmap.width;
			screen->bitmapheight = screen->iconbitmap.height;
		}
		if(Tbm && *Tbm) {
			if((w = XReadBitmapFile(Tbm, &screen->Ticonbitmap.width,
			 &screen->Ticonbitmap.height, &screen->Ticonbitmap.bits,
			 NULL, NULL)) == 0) {
				/*
				 * Pretend there wasn't an icon specified
				 * and try for the default icon.
				 */
				fprintf(stderr, "%s: Can't open %s\n",
				    xterm_name, Tbm);
				Tbm = 0;
				goto again;
			} else if (w < 0)
				goto syntaxerror;
			if(screen->bitmapwidth < screen->Ticonbitmap.width)
				screen->bitmapwidth = screen->Ticonbitmap.width;
			if(screen->bitmapheight < screen->Ticonbitmap.height)
				screen->bitmapheight =
				 screen->Ticonbitmap.height;
		}
		if(!screen->iconbitmap.bits) {
			if(!screen->Ticonbitmap.bits) {
				screen->Ticonbitmap.bits = tek_icon_bits;
				screen->bitmapwidth = screen->Ticonbitmap.width
				 = icon_width;
				screen->bitmapheight =
				 screen->Ticonbitmap.height = icon_height;
			}
			screen->iconbitmap.bits = screen->Ticonbitmap.bits;
			screen->iconbitmap.width = screen->Ticonbitmap.width;
			screen->iconbitmap.height = screen->Ticonbitmap.height;
		} else if(!screen->Ticonbitmap.bits) {
			if(!screen->iconbitmap.bits) {
				screen->iconbitmap.bits = icon_bits;
				screen->bitmapwidth = screen->iconbitmap.width
				 = icon_width;
				screen->bitmapheight =
				 screen->iconbitmap.height = icon_height;
			}
			screen->Ticonbitmap.bits = screen->iconbitmap.bits;
			screen->Ticonbitmap.width = screen->iconbitmap.width;
			screen->Ticonbitmap.height = screen->iconbitmap.height;
		}
	}
	if((screen->winname = malloc(strlen(win_name) + 10)) == NULL)
		Error(ERROR_WINNAME);
	strcpy(screen->winname, win_name);
	if (douniquesuffix)
		strcat(screen->winname, uniquesuffix(win_name));
	screen->winnamelen = strlen(screen->winname);
	IconRecalc(screen);
}

IconRecalc(screen)
register Screen *screen;
{
	register int i, w;

	if (screen->active_icon) return;

	w = XQueryWidth(screen->winname, screen->titlefont->id);
	if(screen->bitmapwidth > 0) {
		if(screen->textundericon) {
			screen->icon_text_x = TITLEPAD;
			screen->icon_text_y = screen->bitmapheight +
			 2 * TITLEPAD;
			screen->iconVwin.height = screen->bitmapheight +
			  screen->titlefont->height + 3 * TITLEPAD;
			screen->iconbitmap.x = screen->Ticonbitmap.x =
			 screen->iconbitmap.y = screen->Ticonbitmap.y =
			 TITLEPAD;
			if((i = screen->bitmapwidth - w) >= 0) {
				screen->iconVwin.width = screen->bitmapwidth +
				 2 * TITLEPAD;
				screen->icon_text_x += i / 2;
			} else {
				screen->iconVwin.width = w + 2 * TITLEPAD;
				i = (-i) / 2;
				screen->iconbitmap.x += i;
				screen->Ticonbitmap.x += i;
			}
		} else {
			screen->icon_text_x = screen->bitmapwidth +
			 2 * TITLEPAD;
			screen->icon_text_y = TITLEPAD;
			screen->iconVwin.width = w + screen->bitmapwidth +
			 3 * TITLEPAD;
			screen->iconbitmap.x = screen->Ticonbitmap.x =
			 screen->iconbitmap.y = screen->Ticonbitmap.y =
			 TITLEPAD;
			if((i = screen->bitmapheight -
			 screen->titlefont->height) >= 0) {
				screen->iconVwin.height = screen->bitmapheight +
				 2 * TITLEPAD;
				screen->icon_text_y += i / 2;
			} else {
				screen->iconVwin.height = screen->titlefont->height
				 + 2 * TITLEPAD;
				i = (-i) / 2;
				screen->iconbitmap.y += i;
				screen->Ticonbitmap.y += i;
			}
		}
		if((i = screen->iconbitmap.width - screen->Ticonbitmap.width)
		 >= 0)
			screen->Ticonbitmap.x += i / 2;
		else
			screen->iconbitmap.x += (-i) / 2;
		if((i = screen->iconbitmap.height - screen->Ticonbitmap.height)
		 >= 0)
			screen->Ticonbitmap.y += i / 2;
		else
			screen->iconbitmap.y += (-i) / 2;
	} else {
		screen->icon_text_x = TITLEPAD;
		screen->iconVwin.width = w + 2 * TITLEPAD;
		screen->iconVwin.height = screen->titlefont->height + 2 * TITLEPAD;
	}

	if (screen->iconVwin.window)
	    XChangeWindow( screen->iconVwin.window, screen->iconVwin.width,
			   screen->iconVwin.height );

	if (screen->iconTwin.window) {
	    screen->iconTwin.width = screen->iconVwin.width;
	    screen->iconTwin.height = screen->iconVwin.height;
	    XChangeWindow( screen->iconTwin.window, screen->iconTwin.width,
			   screen->iconTwin.height );
	}

	icon_box[0].x = screen->icon_text_x - 2;
	icon_box[0].y = screen->icon_text_y - 2;
	icon_box[3].x = -(icon_box[1].x = w + 3);
	icon_box[4].y = -(icon_box[2].y = screen->titlefont->height + 3);
}

RefreshIcon(screen, window)
register Screen *screen;
Window window;
{
	register BitmapBits *bb;
	register int fg, bg;

	bb = screen->TekEmu ? &screen->Ticonbitmap : &screen->iconbitmap;
	fg = screen->foreground;
	bg = screen->background;
	if(screen->bitmapwidth > 0)
		XBitmapBitsPut(window, bb->x, bb->y, bb->width, bb->height,
		 bb->bits, fg, bg, (Bitmap)0, GXcopy, AllPlanes);
	XText(window, screen->icon_text_x, screen->icon_text_y,
	 screen->winname, screen->winnamelen, screen->titlefont->id, fg, bg);
	screen->icon_show = TRUE;
	if(screen->iconinput)
		IconBox(screen);
}

IconBox(screen)
register Screen *screen;
{
	if (screen->active_icon) return;

	XDraw(screen->TekEmu ? screen->iconTwin.window : screen->iconVwin.window,
	 icon_box, NBOX, 1, 1, screen->foreground, GXcopy, AllPlanes);
}

/*
 * Move win1's icon window to where win2's icon window is.
 */
moveiconwindow(win1, win2)
register Window win1, win2;
{
	WindowInfo wininfo1, wininfo2;

	XQueryWindow(win1, &wininfo1);
	XQueryWindow(win2, &wininfo2);
	if(wininfo1.assoc_wind && wininfo2.assoc_wind) {
		XQueryWindow(wininfo2.assoc_wind, &wininfo2);
		XMoveWindow(wininfo1.assoc_wind, wininfo2.x, wininfo2.y);
	}
}

IconGeometry(screen, ix, iy)
register Screen *screen;
register int *ix, *iy;
{
	register int i;
	int w, h;

	if(icon_geom) {
		i = XParseGeometry(icon_geom, ix, iy, &w, &h);
		/*
		 * XParseGeometry returns negative values in addition to
		 * setting the bitmask.
		 */
		if((i & XValue) && (i & XNegative))
			*ix = DisplayWidth() + *ix - screen->iconVwin.width - 2 *
			 screen->borderwidth;
		if((i & YValue) && (i & YNegative))
			*iy = DisplayHeight() + *iy - screen->iconVwin.height - 2 *
			 screen->borderwidth;
	}
}

InTitle(screen, window, x)
register Screen *screen;
Window window;
int x;
{
	register int i, j;

	if(window == screen->title.tbar) {
		i = (j = FullWidth(screen) / 2) - (screen->title.x -
		 screen->title_n_size);
		j = x - j;
		if(j < 0)
			j = -j;
		if(j < i) {
			XUnmapWindow(VWindow(screen));
			XMapWindow(screen->iconVwin.window);
			return(TRUE);
		}
	} else {
		i = (j = TFullWidth(screen) / 2) - (screen->Ttitle.x -
		 screen->title_n_size);
		j = x - j;
		if(j < 0)
			j = -j;
		if(j < i) {
			XUnmapWindow(TWindow(screen));
			XMapWindow(screen->iconTwin.window);
			return(TRUE);
		}
	}
	return(FALSE);
}

SyncUnmap(win, mask)
register Window win;
register int mask;
{
	XEvent ev;
	register XEvent *rep = &ev;

	do { /* ignore events through unmap */
		XWindowEvent(win, mask, rep);
	} while(rep->type != UnmapWindow);
}

StartLog(screen)
register Screen *screen;
{
	register char *cp;
	register int i;
	static char *log_default;
	char *malloc(), *rindex();
	extern logpipe();

	if(screen->logging || (screen->inhibit & I_LOG))
		return;
	if(screen->logfile == NULL || *screen->logfile == 0) {
		if(screen->logfile)
			free(screen->logfile);
		if(log_default == NULL)
			mktemp(log_default = log_def_name);
		if((screen->logfile = malloc(strlen(log_default) + 1)) == NULL)
			return;
		strcpy(screen->logfile, log_default);
	}
	if(*screen->logfile == '|') {	/* exec command */
		int p[2];
		static char *shell;

		if(pipe(p) < 0 || (i = fork()) < 0)
			return;
		if(i == 0) {	/* child */
			close(p[1]);
			dup2(p[0], 0);
			close(p[0]);
			dup2(fileno(stderr), 1);
			dup2(fileno(stderr), 2);
			close(fileno(stderr));
			fileno(stderr) = 2;
			close(screen->display->fd);
			close(screen->respond);
			if(!shell) {
				register struct passwd *pw;
				char *getenv(), *malloc();
				struct passwd *getpwuid();

				if(((cp = getenv("SHELL")) == NULL || *cp == 0)
				 && ((pw = getpwuid(screen->uid)) == NULL ||
				 *(cp = pw->pw_shell) == 0) ||
				 (shell = malloc(strlen(cp) + 1)) == NULL)
					shell = "/bin/sh";
				else
					strcpy(shell, cp);
			}
			signal(SIGHUP, SIG_DFL);
			signal(SIGCHLD, SIG_DFL);
			setgid(screen->gid);
			setuid(screen->uid);
			execl(shell, shell, "-c", &screen->logfile[1], 0);
			fprintf(stderr, "%s: Can't exec `%s'\n", xterm_name,
			 &screen->logfile[1]);
			exit(ERROR_LOGEXEC);
		}
		close(p[0]);
		screen->logfd = p[1];
		signal(SIGPIPE, logpipe);
	} else {
		if(access(screen->logfile, F_OK) == 0) {
			if(access(screen->logfile, W_OK) < 0)
				return;
		} else if(cp = rindex(screen->logfile, '/')) {
			*cp = 0;
			i = access(screen->logfile, W_OK);
			*cp = '/';
			if(i < 0)
				return;
		} else if(access(".", W_OK) < 0)
			return;
		if((screen->logfd = open(screen->logfile, O_WRONLY | O_APPEND |
		 O_CREAT, 0644)) < 0)
			return;
		chown(screen->logfile, screen->uid, screen->gid);

	}
	screen->logstart = screen->TekEmu ? Tbptr : bptr;
	screen->logging = TRUE;
}

CloseLog(screen)
register Screen *screen;
{
	if(!screen->logging || (screen->inhibit & I_LOG))
		return;
	FlushLog(screen);
	close(screen->logfd);
	screen->logging = FALSE;
}

FlushLog(screen)
register Screen *screen;
{
	register char *cp;
	register int i;

	cp = screen->TekEmu ? Tbptr : bptr;
	if((i = cp - screen->logstart) > 0)
		write(screen->logfd, screen->logstart, i);
	screen->logstart = screen->TekEmu ? Tbuffer : buffer;
}

logpipe()
{
	register Screen *screen = &term.screen;

	if(screen->logging)
		CloseLog(screen);
}

do_osc(func)
int (*func)();
{
	register Screen *screen = &term.screen;
	register int mode, c;
	register char *cp;
	char buf[512];
	extern char *malloc();

	mode = 0;
	while(isdigit(c = (*func)()))
		mode = 10 * mode + (c - '0');
	cp = buf;
	while(isprint(c = (*func)()))
		*cp++ = c;
	*cp = 0;
	switch(mode) {
	 case 0:	/* new title */
		Retitle(buf);
		break;
	 case 46:	/* new log file */
		if((cp = malloc(strlen(buf) + 1)) == NULL)
			break;
		strcpy(cp, buf);
		if(screen->logfile)
			free(screen->logfile);
		screen->logfile = cp;
		break;
#ifdef CHANGEFONT
	 case 47:	/* new screen font */
		if (VTChangeFont(screen, buf)) {
			XChangeWindow (VWindow(screen),
			 FontWidth(screen) * (screen->max_col + 1) +
			 2 * screen->border + screen->scrollbar,
			 FontHeight(screen) * (screen->max_row + 1) +
			 screen->statusheight + Titlebar(screen)
			 + 2 * screen->border);
			XSync(FALSE);	/* synchronize */
			if(QLength() > 0)
				xevents();
		}
		break;
	}
#endif
}

Retitle(name)
register char *name;
{
	register Screen *screen = &term.screen;
	register int w, i, j;
	char icon[512];

	free(screen->winname);
	if((screen->winname = malloc((screen->winnamelen = strlen(name)) + 1))
	 == NULL)
		Error(ERROR_RTMALLOC1);
	strcpy(screen->winname, name);
	strcpy(icon, name);
	strcat(icon, " (icon)");
	IconRecalc(screen);
	if(screen->fullVwin.window) {
		XStoreName(screen->fullVwin.window, name);
		XStoreName(screen->iconVwin.window, icon);
		XChangeWindow(screen->iconVwin.window, screen->iconVwin.width,
		 screen->iconVwin.height);
		if(screen->title.tbar) {
			w = FullWidth(screen);
			screen->title.fullwidth = XQueryWidth(name,
			 screen->titlefont->id);
			if((screen->title.width = i = screen->title.fullwidth)
			 > (j = w - 2 * (MINHILITE + screen->title_n_size + 1)))
				screen->title.width = (i = j) +
				 screen->title_n_size;
			j = w - i - 2 * (screen->title_n_size + 1);
			i = j / 2;
			j -= i;
			screen->title.x = i + 1 + screen->title_n_size;
			screen->title.y = TITLEPAD;
			XClear(screen->title.tbar);
			XChangeWindow(screen->title.left, i,
			 screen->titlefont->height);
			XConfigureWindow(screen->title.right, w - j - 1,
			 TITLEPAD, j, screen->titlefont->height);
			VTTitleExpose((XExposeWindowEvent *)NULL);
		}
	}
	if(screen->fullTwin.window) {
		free(screen->Twinname);
		if((screen->Twinname = malloc((screen->Twinnamelen =
		 screen->winnamelen + 6) + 1)) == NULL)
			Error(ERROR_RTMALLOC2);
		strcpy(screen->Twinname, name);
		strcat(screen->Twinname, " (Tek)");
		XStoreName(screen->fullTwin.window, screen->Twinname);
		XStoreName(screen->iconTwin.window, icon);
		XChangeWindow(screen->iconTwin.window, screen->iconTwin.width,
		 screen->iconTwin.height);
		if(screen->Ttitle.tbar) {
			w = TFullWidth(screen);
			screen->Ttitle.fullwidth = XQueryWidth(screen->Twinname,
			 screen->titlefont->id);
			if((screen->Ttitle.width = i = screen->Ttitle.fullwidth)
			 > (j = w - 2 * (MINHILITE + screen->title_n_size + 1)))
				screen->Ttitle.width = (i = j) +
				 screen->title_n_size;
			j = w - i - 2 * (screen->title_n_size + 1);
			i = j / 2;
			j -= i;
			screen->Ttitle.x = i + 1 + screen->title_n_size;
			screen->Ttitle.y = TITLEPAD;
			XClear(screen->Ttitle.tbar);
			XChangeWindow(screen->Ttitle.left, i,
			 screen->titlefont->height);
			XConfigureWindow(screen->Ttitle.right, w - j - 1,
			 TITLEPAD, j, screen->titlefont->height);
			TekTitleExpose((XExposeWindowEvent *)NULL);
		}
	}
}

Panic(s, a)
char	*s;
int a;
{
#ifdef DEBUG
	if(debug) {
		fprintf(stderr, "%s: PANIC!	", xterm_name);
		fprintf(stderr, s, a);
		fputs("\r\n", stderr);
		fflush(stderr);
	}
#endif DEBUG
}

SysError (i)
int i;
{
	fprintf (stderr, "%s: Error %d, errno %d:", xterm_name, i, errno);
	perror ("");
	Cleanup(i);
}

Error (i)
int i;
{
	fprintf (stderr, "%s: Error %d\n", xterm_name, i);
	Cleanup(i);
}

/*
 * cleanup by sending SIGHUP to client processes
 */
Cleanup (code)
int code;
{
#ifdef notdef
	extern Terminal term;
	register Screen *screen;

	screen = &term.screen;
	if (screen->pid > 1)
		killpg(getpgrp(screen->pid), SIGHUP);
#endif
	Exit (code);
}

/*
 * sets the value of var to be arg in the Unix 4.2 BSD environment env.
 * Var should end with '=' (bindings are of the form "var=value").
 * This procedure assumes the memory for the first level of environ
 * was allocated using calloc, with enough extra room at the end so not
 * to have to do a realloc().
 */
Setenv (var, value)
register char *var, *value;
{
	extern char **environ;
	register int index = 0;
	register int len = strlen(var);

	while (environ [index] != NULL) {
	    if (strncmp (environ [index], var, len) == 0) {
		/* found it */
		environ[index] = (char *)malloc (len + strlen (value) + 1);
		strcpy (environ [index], var);
		strcat (environ [index], value);
		return;
	    }
	    index ++;
	}

#ifdef DEBUG
	if (debug) fputs ("expanding env\n", stderr);
#endif DEBUG

	environ [index] = (char *) malloc (len + strlen (value) + 1);
	strcpy (environ [index], var);
	strcat (environ [index], value);
	environ [++index] = NULL;
}

/*
 * returns a pointer to the first occurrence of s2 in s1,
 * or NULL if there are none.
 */
char *strindex (s1, s2)
register char	*s1, *s2;
{
	register char	*s3;
	char		*index();

	while ((s3=index(s1, *s2)) != NULL) {
		if (strncmp(s3, s2, strlen(s2)) == 0)
			return (s3);
		s1 = ++s3;
	}
	return (NULL);
}

xerror(d, ev)
Display *d;
register XErrorEvent *ev;
{
	fprintf(stderr, "%s: %s\n", xterm_name,
	 XErrDescrip(ev->error_code));
	fprintf(stderr, "Request code %d, func %d, serial #%ld, window %ld\n",
	 ev->request_code, ev->func, ev->serial, (long)ev->window);
	Exit(ERROR_XERROR);
}

xioerror(d)
Display *d;
{
	perror(xterm_name);
	Exit(ERROR_XIOERROR);
}
