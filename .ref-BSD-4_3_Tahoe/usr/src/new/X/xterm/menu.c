/*
 *	$Source: /u1/X/xterm/RCS/menu.c,v $
 *	$Header: menu.c,v 10.101 86/12/01 17:52:43 swick Rel $
 */

#ifdef MODEMENU
#include "X/Xlib.h"
#include "menu.h"

#ifndef lint
static char sccs_id[] = "@(#)menu.c\tX10/6.6B\t12/26/86";
#endif	lint

#define	FALSE			0
#define	TRUE			1
#define	InvertPlane		1
#define	SetStateFlags(item)	item->itemFlags = (item->itemFlags &\
				 ~(itemStateMask | itemChanged)) |\
				 ((item->itemFlags & itemSetMask) >>\
				 itemSetMaskShift)


static short Check_MarkBits[] = {
   0x0100, 0x0180, 0x00c0, 0x0060,
   0x0031, 0x001b, 0x000e, 0x0004
};
static short Check_GrayBits[] = {
   0x0100, 0x0080, 0x0040, 0x0020,
   0x0011, 0x000a, 0x0004, 0x0000
};
static short Default_CursorBits[] = {
   0x0000, 0x0002, 0x0006, 0x000e,
   0x001e, 0x003e, 0x007e, 0x00fe,
   0x01fe, 0x003e, 0x0036, 0x0062,
   0x0060, 0x00c0, 0x00c0, 0x0000
};
static short Default_GrayBits[] = {
   0xaaaa, 0x5555, 0xaaaa, 0x5555,
   0xaaaa, 0x5555, 0xaaaa, 0x5555,
   0xaaaa, 0x5555, 0xaaaa, 0x5555,
   0xaaaa, 0x5555, 0xaaaa, 0x5555,
};
static short Default_MaskBits[] = {
   0x0003, 0x0007, 0x000f, 0x001f,
   0x003f, 0x007f, 0x00ff, 0x01ff,
   0x03ff, 0x07ff, 0x007f, 0x00f7,
   0x00f3, 0x01e1, 0x01e0, 0x01c0
};
static char def_menu_font[] = "vtsingle";

Pixmap Gray_Tile;
Menu Menu_Default;
Cursor Menu_DefaultCursor;
char *Menu_DefaultFont;
FontInfo *Menu_DefaultFontInfo;

/*
 * AddMenuItem() adds a menu item to an existing menu, at the end of the
 * list, which are number sequentially from zero.  The menuitem index is
 * return, or -1 if failed.
 */

AddMenuItem(menu, text)
register Menu *menu;
register char *text;
{
	register MenuItem *menuitem, **next;
	register int i;
	extern char *malloc();

	if(!menu || !text || (menuitem = (MenuItem *)malloc(sizeof(MenuItem)))
	 == (MenuItem *)0)
		return(-1);
	bzero((char *)menuitem, sizeof(MenuItem));
	menuitem->itemText = text;
	menuitem->itemTextLength = strlen(text);
	for(i = 0, next = &menu->menuItems ; *next ; i++)
		next = &(*next)->nextItem;
	*next = menuitem;
	menu->menuFlags |= menuChanged;
	return(i);
}

/*
 * DisposeItem() releases the memory allocated for the given indexed
 * menuitem.  Nonzero is returned if an item was actual disposed of.
 */
DisposeItem(menu, i)
register Menu *menu;
register int i;
{
	register MenuItem **next, **last, *menuitem;

	if(!menu || i < 0)
		return(0);
	next = &menu->menuItems;
	do {
		if(!*next)
			return(0);
		last = next;
		next = &(*next)->nextItem;
	} while(i-- > 0);
	menuitem = *last;
	*last = *next;
	free(menuitem);
	return(1);
}

/*
 * DisposeMenu() releases the memory allocated for the given menu.
 */
DisposeMenu(menu)
register Menu *menu;
{
	static Unmap_Menu();

	if(!menu)
		return;
	if(menu->menuFlags & menuMapped)
		Unmap_Menu(menu);
	while(DisposeItem(menu, 0));
	if(menu->menuWindow)
		XDestroyWindow(menu->menuWindow);
	if(menu->menuSaved)
		XFreePixmap(menu->menuSaved);
	free(menu);
}

InitMenu(name)
register char *name;
{
	register char *cp;
	register Bitmap bit;

	/*
	 * If the gray tile hasn't been set up, do it now.
	 */
	if(!Gray_Tile) {
		if(!(bit = XStoreBitmap(grayWidth, grayHeight,
		 Default_GrayBits)))
			return;
		Gray_Tile = XMakePixmap(bit, WhitePixel, BlackPixel);
		XFreeBitmap(bit);
	}
	Menu_Default.menuFlags = menuChanged;
	if((cp = XGetDefault(name, "MenuFreeze")) && strcmp(cp, "on") == 0)
		Menu_Default.menuFlags |= menuFreeze;
	if((cp = XGetDefault(name, "MenuSave")) && strcmp(cp, "on") == 0)
		Menu_Default.menuFlags |= menuSaveMenu;
	Menu_Default.menuInitialItem = -1;
	Menu_Default.menuBorderWidth = (cp = XGetDefault(name, "MenuBorder")) ?
	 atoi(cp) : 2;
	Menu_Default.menuItemPad = (cp = XGetDefault(name, "MenuPad")) ?
	 atoi(cp) : 3;
	Menu_DefaultFont = (cp = XGetDefault(name, "MenuFont")) ? cp :
	 def_menu_font;
};

/*
 * ItemFlags returns the state of item "n" of the menu.
 */
ItemFlags(menu, n)
register Menu *menu;
register int n;
{
	register MenuItem *item;

	if(!menu || !menu->menuItems || n < 0)
		return(-1);
	for(item = menu->menuItems ; n > 0 ; n--)
		if(!(item = item->nextItem))
			return(0);
	return((item->itemFlags & itemSetMask) >> itemSetMaskShift);
}

/*
 * ItemText changes the text of item "n" of the menu.
 */
ItemText(menu, n, text)
register Menu *menu;
register int n;
char *text;
{
	register MenuItem *item;

	if(!menu || !menu->menuItems || n < 0 || !text)
		return(0);
	for(item = menu->menuItems ; n > 0 ; n--)
		if(!(item = item->nextItem))
			return(0);
	item->itemText = text;
	menu->menuFlags |= menuChanged;
	return(1);
}

/*
 * NewMenu() returns a pointer to an initialized new Menu structure, or NULL
 * if failed.
 *
 * The Menu structure _menuDefault contains the default menu settings.
 */
Menu *NewMenu(name, reverse)
char *name;
int reverse;
{
	register Menu *menu;
	register int fg, bg;
	extern char *malloc();

	/*
	 * If the GrayTile hasn't been defined, InitMenu() was never
	 * run, so exit.
	 */
	if(!Gray_Tile)
		return((Menu *)0);
	/*
	 * Allocate the memory for the menu structure.
	 */
	if((menu = (Menu *)malloc(sizeof(Menu))) == (Menu *)0)
		return((Menu *)0);
	/*
	 * Initialize to default values.
	 */
	*menu = Menu_Default;
	/*
	 * If the menu font hasn't yet been gotten, go get it.
	 */
	if(!menu->menuFontInfo) {
		if(!Menu_DefaultFontInfo && !(Menu_DefaultFontInfo =
		 XOpenFont(Menu_DefaultFont)))
			return((Menu *)0);
		menu->menuFontInfo = Menu_DefaultFontInfo;
	}
	/*
	 * If the menu cursor hasn't been given, make a default one.
	 */
	if(!menu->menuCursor) {
		if(!Menu_DefaultCursor) {
			if(reverse) {
				fg = WhitePixel;
				bg = BlackPixel;
			} else {
				fg = BlackPixel;
				bg = WhitePixel;
			}
			if(!(Menu_DefaultCursor =
			 XCreateCursor(defaultCursorWidth, defaultCursorHeight,
			  Default_CursorBits, Default_MaskBits, defaultCursorX,
			  defaultCursorY, fg, bg, GXcopy)))
				return((Menu *)0);
		}
		menu->menuCursor = Menu_DefaultCursor;
	}
	/*
	 * Initialze the default background and border pixmaps and foreground
	 * and background colors (black and white).
	 */
	if(reverse) {
		menu->menuBgTile = BlackPixmap;
		menu->menuFgColor = WhitePixel;
		menu->menuBgColor = BlackPixel;
	} else {
		menu->menuBgTile = WhitePixmap;
		menu->menuFgColor = BlackPixel;
		menu->menuBgColor = WhitePixel;
	}
	/*
	 * Set the menu title.  If name is NULL or is an empty string, no
	 * title will be displayed.
	 */
	if(name && *name) {
		menu->menuTitleLength = strlen(menu->menuTitle = name);
		menu->menuTitleWidth = XStringWidth(name, menu->menuFontInfo,
		 0, 0);
		menu->menuItemTop = menu->menuFontInfo->height + 2 *
		 menu->menuItemPad + 1;
	} else
		menu->menuTitleLength = menu->menuTitleWidth =
		 menu->menuItemTop = 0;
	return(menu);
}

/*
 * SetItemCheck sets the check state of item "n" of the menu to "state".
 */
SetItemCheck(menu, n, state)
register Menu *menu;
register int n;
int state;
{
	register MenuItem *item;

	if(!menu || !menu->menuItems || n < 0)
		return(0);
	for(item = menu->menuItems ; n > 0 ; n--)
		if(!(item = item->nextItem))
			return(0);
	if(state)
		item->itemFlags |= itemSetChecked;
	else
		item->itemFlags &= ~itemSetChecked;
	if(((item->itemFlags & itemSetMask) >> itemSetMaskShift) !=
	 (item->itemFlags & itemStateMask)) {
		item->itemFlags |= itemChanged;
		menu->menuFlags |= menuItemChanged;
	} else
		item->itemFlags &= ~itemChanged;
	return(1);
}

/*
 * SetItemDisable sets the disable state of item "n" of the menu to "state".
 */
SetItemDisable(menu, n, state)
register Menu *menu;
register int n;
int state;
{
	register MenuItem *item;

	if(!menu || !menu->menuItems || n < 0)
		return(0);
	for(item = menu->menuItems ; n > 0 ; n--)
		if(!(item = item->nextItem))
			return(0);
	if(state)
		item->itemFlags |= itemSetDisabled;
	else
		item->itemFlags &= ~itemSetDisabled;
	if(((item->itemFlags & itemSetMask) >> itemSetMaskShift) !=
	 (item->itemFlags & itemStateMask)) {
		item->itemFlags |= itemChanged;
		menu->menuFlags |= menuItemChanged;
	} else
		item->itemFlags &= ~itemChanged;
	return(1);
}

/*
 * TrackMenu does most of the work of displaying the menu and tracking the
 * mouse.
 */
TrackMenu(menu, event)
register Menu *menu;
register XButtonPressedEvent *event;
{
	register MenuItem *item;
	register int i, button;
	register MenuItem *hilited_item = (MenuItem *)0;
	register int drawn;
	XButtonReleasedEvent ev;
	register int changed;
	int y, n, hilited_y, hilited_n, in_window;
	static MenuItem *Mouse_InItem(), *Y_InItem();
	static Unmap_Menu();

	/*
	 * Check that things are reasonable.
	 */
	if(!menu || !event || !menu->menuItems || event->type != ButtonPressed)
		return(-1);
	/*
	 * Set the changed flag and clear the menu changed flags.
	 */
	changed = menu->menuFlags & (menuChanged | menuItemChanged);
	/*
	 * If the entire menu has changed, throw away any saved pixmap and
	 * then call RecalcMenu().
	 */
	if(changed & menuChanged) {
		if(menu->menuSaved)
			XFreePixmap(menu->menuSaved);
		menu->menuSaved = (Pixmap)0;
		if(!Recalc_Menu(menu))
			return(-1);
		changed &= ~menuItemChanged;
	}
	/*
	 * Now if the window was never created, go ahead and make it.  Otherwise
	 * if the menu has changed, resize the window.
	 */
	if(!menu->menuWindow) {
		if((menu->menuWindow = XCreateWindow(RootWindow, 0, 0,
		 menu->menuWidth, menu->menuHeight, menu->menuBorderWidth,
		 Gray_Tile, menu->menuBgTile)) == (Window)0)
			return(-1);
		XDefineCursor(menu->menuWindow, menu->menuCursor);
		XSelectInput(menu->menuWindow, ExposeWindow | EnterWindow |
		 LeaveWindow | MouseMoved | ButtonReleased);
	} else if(changed & menuChanged)
	 	XChangeWindow(menu->menuWindow, menu->menuWidth,
		 menu->menuHeight);
	/*
	 * Figure out where the menu is supposed to go, from the initial button
	 * press, and move the window there.  Then map the menu.
	 */
	if(!Move_Menu(menu, event) || !Map_Menu(menu))
		return(-1);
	/*
	 * Try to grab the mouse, over a period of 10 seconds.
	 */
	for(i = 10 ; ; ) {
		if(XGrabMouse(menu->menuWindow, menu->menuCursor,
		 ButtonReleased | EnterWindow | LeaveWindow | MouseMoved))
			break;
		if(--i <= 0) {
			Unmap_Menu(menu);
			return(-1);
		}
		sleep(1);
	}
	/*
	 * Save away the button that was pressed and use it to match a
	 * corresponding ButtonReleased event.
	 */
	button = event->detail & 03;
	/*
	 * Now process events for the menu window.
	 */
	drawn = 0;
	for( ; ; ) {
		XNextEvent(&ev);
		if(ev.type != ButtonReleased && ev.window != menu->menuWindow) {
			if(menu->menuEventHandler)
				(*menu->menuEventHandler)(&ev);
			continue;
		}
		switch(ev.type) {
		 case ExposeWindow:
			/*
			 * If we have a saved pixmap, display it.  Otherwise
			 * redraw the menu and save it away.
			 */
			if(menu->menuSaved) {
				XPixmapPut(menu->menuWindow, 0, 0, 0, 0,
				 menu->menuWidth, menu->menuHeight,
				 menu->menuSaved, GXcopy, AllPlanes);
				/*
				 * If the menuItemChanged flag is still set,
				 * then we need to redraw certain menu items.
				 * ("i" is the vertical position of the top
				 * of the current item.)
				 */
				if(changed & menuItemChanged) {
					i = menu->menuItemTop;
					for(item = menu->menuItems ; item ;
					 item = item->nextItem) {
						if(item->itemFlags &
						 itemChanged)
							Modify_Item(menu, item,
							 i);
						i += item->itemHeight;
					}
				}
			} else
				Draw_Menu(menu);
			/*
			 * If the menu has changed in any way and we want to
			 * save the menu, throw away any existing save menu
			 * image and make a new one.
			 */
			XFlush();
			if(changed && (menu->menuFlags & menuSaveMenu)) {
				if(menu->menuSaved)
					XFreePixmap(menu->menuSaved);
				menu->menuSaved = XPixmapSave(menu->menuWindow,
				 0, 0, menu->menuWidth, menu->menuHeight);
			}
			/*
			 * See which item the cursor may currently be in.  If
			 * it is in a non-disabled item, hilite it.
			 */
			if(hilited_item = Mouse_InItem(menu, &hilited_y,
			 &hilited_n, &in_window))
				XPixFill(menu->menuWindow, 0, hilited_y,
				 menu->menuWidth, hilited_item->itemHeight,
				 BlackPixmap, (Bitmap)0, GXinvert, InvertPlane);
			drawn++;
			break;
		 case EnterWindow:
			in_window = TRUE;
			/* drop through */
		 case MouseMoved:
			if(!drawn || !in_window)
				break;
			/*
			 * See which item the cursor may currently be in.  If
			 * the item has changed, unhilite the old one and
			 * then hilited the new one.
			 */
			y = ((XEnterWindowEvent *)&ev)->y;
			if((item = Y_InItem(menu, &y, &n)) != hilited_item) {
				if(hilited_item)
					XPixFill(menu->menuWindow, 0,
					 hilited_y, menu->menuWidth,
					 hilited_item->itemHeight, BlackPixmap,
					 (Bitmap)0, GXinvert, InvertPlane);
				if(hilited_item = item) {
					XPixFill(menu->menuWindow, 0,
					 hilited_y = y, menu->menuWidth,
					 item->itemHeight, BlackPixmap,
					 (Bitmap)0, GXinvert, InvertPlane);
					hilited_n = n;
				}
			}
			break;
		 case LeaveWindow:
			if(!drawn)
				break;
			/*
			 * Unhilite any window that is currently hilited.
			 */
			if(hilited_item) {
				XPixFill(menu->menuWindow, 0, hilited_y,
				 menu->menuWidth, hilited_item->itemHeight,
				 BlackPixmap, (Bitmap)0, GXinvert, InvertPlane);
				hilited_item = (MenuItem *)0;
			}
			in_window = FALSE;
			break;
		 case ButtonReleased:
			/*
			 * If the correct button was released, ungrab the mouse
			 * and return the index number of any selected menu
			 * item.
			 */
			if((ev.detail & 0x3) == button) {
				if(in_window) {
					y = ((XButtonReleasedEvent *)&ev)->y;
					if((item = Y_InItem(menu, &y, &n)) !=
					 hilited_item) {
					    if(hilited_item)
						XPixFill(menu->menuWindow, 0,
						 hilited_y, menu->menuWidth,
						 hilited_item->itemHeight,
						 BlackPixmap, (Bitmap)0,
						 GXinvert, InvertPlane);
					    if(hilited_item = item) {
						XPixFill(menu->menuWindow, 0,
						 hilited_y = y, menu->menuWidth,
						 hilited_item->itemHeight,
						 BlackPixmap, (Bitmap)0,
						 GXinvert, InvertPlane);
						hilited_n = n;
					    }
					}
				}
				XUngrabMouse();
				menu->menuFlags &= ~(menuChanged |
				 menuItemChanged);
				Unmap_Menu(menu);
				XFlush();
				if(hilited_item)
					return(menu->menuInitialItem =
					 hilited_n);
				return(-1);
			}
			break;
		}
	}
}

/*
 * Recalculate all of the various menu and item variables.
 */
static Recalc_Menu(menu)
register Menu *menu;
{
	register MenuItem *item;
	register int max, i, height, fontheight;

	/*
	 * We must have already gotten the menu font.
	 */
	if(!menu->menuFontInfo)
		return(0);
	/*
	 * Initialize the various max width variables.
	 */
	fontheight = menu->menuFontInfo->height;
	height = menu->menuItemTop;
	menu->menuMaxTextWidth = menu->menuTitleWidth;
	/*
	 * The item height is the maximum of the font height and the
	 * checkbox height.
	 */
	max = fontheight;
	if(checkMarkHeight > max)
		max = checkMarkHeight;
	/*
	 * Go through the menu item list.
	 */
	for(item = menu->menuItems ; item ; item = item->nextItem) {
		/*
		 * If the item text is a single dash, we assume this is
		 * a line separator and treat it special.
		 */
		if(strcmp(item->itemText, "-") == 0)
			height += (item->itemHeight = lineSeparatorHeight);
		else {
			height += (item->itemHeight = max);
			/*
			 * Check the text width with the max value stored in
			 * menu.
			 */
			if((item->itemTextWidth = XStringWidth(item->itemText,
			 menu->menuFontInfo, 0, 0)) > menu->menuMaxTextWidth)
				menu->menuMaxTextWidth = item->itemTextWidth;
		}
		/*
		 * If the itemChanged flag is set, set the state bits.
		 */
		if(item->itemFlags & itemChanged) {
			item->itemFlags = (item->itemFlags & ~itemStateMask) |
			 ((item->itemFlags & itemSetMask) >> itemSetMaskShift);
			item->itemFlags &= ~itemChanged;
		}
	}
	/*
	 * Set the menu height and then set the menu width.
	 */
	menu->menuHeight = height;
	menu->menuWidth = 3 * menu->menuItemPad + menu->menuMaxTextWidth +
	 checkMarkWidth;
	return(1);
}

/*
 * Figure out where to popup the menu, relative to the where the button was
 * pressed.
 */
static Move_Menu(menu, ev)
register Menu *menu;
XButtonPressedEvent *ev;
{
	register MenuItem *item;
	register int n, x, y;
	int ev_x, ev_y;
	int total_width;
	Window subw;
	extern int dropmenu;	/* XXX */

	/*
	 * Get the coordinates of the mouse when the button was pressed.
	 */
	XInterpretLocator(RootWindow, &ev_x, &ev_y, &subw, ev->location);
	/*
	 * Try to popup the menu so that the cursor is centered within the
	 * width of the menu, but compensate if that would run it outside
	 * the display area.
	 */
	total_width = menu->menuWidth + 2 * menu->menuBorderWidth;
	if((x = ev_x - total_width / 2) < 0)
		x = 0;
	else if(x + total_width > DisplayWidth())
		x = DisplayWidth() - total_width;
	if (dropmenu)
		y = 0;
	else if(menu->menuInitialItem >= 0) {
		/*
		 * If we have an inital item, try to popup the menu centered
		 * vertically within this item.
		 *
		 * Look through the item list. "y" is the vertical position
		 * of the top of the current item and "n" is the item number.
		 */
		y = menu->menuItemTop + menu->menuBorderWidth;
		for(n = 0, item = menu->menuItems ; ; n++) {
			/*
			 * On finding the intial item, center within this item.
			 */
			if(n == menu->menuInitialItem) {
				y += item->itemHeight / 2;
				break;
			}
			y += item->itemHeight;
			/*
			 * If we run out of items, turn off the initial item
			 * and treat this as if no initial item.
			 */
			if(!(item = item->nextItem)) {
				menu->menuInitialItem = -1;
				goto noInitial;
			}
		}
	/*
	 * If no initial item, try to popup the menu centered in the item
	 * nearest the center of the menu.
	 */
	} else {
noInitial:
		/*
		 * Look through the item list. "y" is the vertical position
		 * of the top of the current item and "n" is the vertical
		 * position of the center of the menu.
		 */
		y = menu->menuItemTop + menu->menuBorderWidth;
		for(n = menu->menuHeight / 2, item = menu->menuItems ; item ;
		 item = item->nextItem)
			/*
			 * If the center of the menu is in this item, we
			 * center within this item.
			 */
			if((y += item->itemHeight) > n) {
				y -= item->itemHeight / 2;
				break;
			}
	}
	/*
	 * If the menu extends above outside of the display, warp
	 * the mouse vertically so the menu will all show up.
	 */
	if((y = ev_y - y) < 0) {
		XWarpMouse(RootWindow, ev_x, ev_y - y);
		y = 0;
	} else if((n = y + menu->menuHeight + 2 * menu->menuBorderWidth
		       - DisplayHeight()) > 0) {
		XWarpMouse(RootWindow, ev_x, ev_y - n);
		y -= n;
	}
	XMoveWindow(menu->menuWindow, x, y);
	/*
	 * If we are in freeze mode, save what will be the coordinates of
	 * the save image.
	 */
	if(menu->menuFlags & menuFreeze) {
		menu->menuSavedImageX = x;
		menu->menuSavedImageY = y;
	}
	return(1);
}

/*
 * Map the menu window.
 */
static Map_Menu(menu)
register Menu *menu;
{
	register int i;

	/*
	 * If we are in freeze mode, save the pixmap underneath where the menu
	 * will be (including the border).
	 */
	if(menu->menuFlags & menuFreeze) {
		XGrabServer();
		i = 2 * menu->menuBorderWidth;
		if((menu->menuSavedImage = XPixmapSave(RootWindow,
		 menu->menuSavedImageX, menu->menuSavedImageY, menu->menuWidth
		 + i, menu->menuHeight + i)) == (Pixmap)0)
			return(0);
	}
	/*
	 * Actually map the window.
	 */
	XMapWindow(menu->menuWindow);
	menu->menuFlags |= menuMapped;
	return(1);
}

/*
 * Draw the entire menu in the blank window.
 */
static Draw_Menu(menu)
register Menu *menu;
{
	register MenuItem *item;
	register int top = menu->menuItemTop;
	register int x = menu->menuItemPad;
	register int y, dim;

	/*
	 * If we have a menu title, draw it first, centered and hilited.
	 */
	if(menu->menuTitleLength) {
		XPixSet(menu->menuWindow, 0, 0, menu->menuWidth,
		 top - 1, menu->menuFgColor);
		XText(menu->menuWindow, (menu->menuWidth -
		 menu->menuTitleWidth) / 2, menu->menuItemPad, menu->menuTitle,
		 menu->menuTitleLength, menu->menuFontInfo->id,
		 menu->menuBgColor, menu->menuFgColor);
	}
	/*
	 * For each item in the list, first draw any check mark and then
	 * draw the rest of it.
	 */
	for(item = menu->menuItems ; item ; item = item->nextItem) {
		SetStateFlags(item);
		dim = (item->itemFlags & itemDisabled);
		/*
		 * Draw the check mark, possibly dimmed, wherever is necessary.
		 */
		if(item->itemFlags & itemChecked) {
			XBitmapBitsPut(menu->menuWindow, x, y = top +
			 (item->itemHeight - checkMarkHeight) / 2,
			 checkMarkWidth, checkMarkHeight, dim ? Check_GrayBits :
			 Check_MarkBits, menu->menuFgColor, menu->menuBgColor,
			 (Bitmap)0, GXcopy, AllPlanes);
		}
		/*
		 * Draw the item, possibly dimmed.
		 */
		Draw_Item(menu, item, top, dim);
		top += item->itemHeight;
	}
}

/*
 * Modify the item at vertical position y.  This routine is table driven and
 * the state and set bits are each 2 bits long, contiguous, the least
 * significant bits in the flag word and with the state bits in bits 0 & 1.
 */

#define	drawCheck	0x10
#define	removeCheck	0x08
#define	dimCheck	0x04
#define	drawItem	0x02
#define	dimItem		0x01

static char Modify_Table[] = {
	0x00, 0x02, 0x08, 0x0a, 0x01, 0x00, 0x09, 0x08,
	0x10, 0x12, 0x00, 0x12, 0x15, 0x14, 0x05, 0x00
};
	
static Modify_Item(menu, item, top)
register Menu *menu;
register MenuItem *item;
int top;
{
	register int x = menu->menuItemPad;
	register int y;
	register int center = top + item->itemHeight / 2;
	register int func = Modify_Table[item->itemFlags &
	 (itemStateMask | itemSetMask)];

	/*
	 * If we really won't be making a change, return.
	 */
	if(func == 0)
		return;
	/*
	 * Draw the check mark if needed, possibly dimmed.
	 */
	y = center - (checkMarkHeight / 2);
	if(func & (drawCheck | dimCheck))
		XBitmapBitsPut(menu->menuWindow, x, y, checkMarkWidth,
		 checkMarkHeight, (func & dimCheck) ? Check_GrayBits :
		 Check_MarkBits, menu->menuFgColor, menu->menuBgColor,
		 (Bitmap)0, GXcopy, AllPlanes);
	/*
	 * Remove the check mark if needed.
	 */
	if(func & removeCheck)
		XTileSet(menu->menuWindow, x, y, checkMarkWidth,
		 checkMarkHeight, menu->menuBgTile);
	/*
	 * Call Draw_Item if we need to draw or dim the item.
	 */
	if((x = func & dimItem) || (func & drawItem))
		Draw_Item(menu, item, top, x);
	/*
	 * Update state flags.
	 */
	SetStateFlags(item);
}

/*
 * Draw the item (less check mark) at vertical position y.
 * Dim the item if "dim" is set.
 */
static Draw_Item(menu, item, y, dim)
register Menu *menu;
register MenuItem *item;
register int y;
int  dim;
{
	register int x = 2 * menu->menuItemPad + checkMarkWidth;
	register int center = y + item->itemHeight / 2;

	/*
	 * If the item text is a single dash, draw a separating line.
	 */
	if(strcmp(item->itemText, "-") == 0) {
		XLine(menu->menuWindow, 0, center, menu->menuWidth, center,
		 1, 1, menu->menuFgColor, GXcopy, AllPlanes);
		return;
	}
	/*
	 * Draw and/or dim the text, centered vertically.
	 */
	y = center - (menu->menuFontInfo->height / 2);
	if(dim) {
		XTileSet(menu->menuWindow, x, y, item->itemTextWidth,
		 menu->menuFontInfo->height, Gray_Tile);
		XTextPad(menu->menuWindow, x, y, item->itemText,
		 item->itemTextLength, menu->menuFontInfo->id, 0, 0,
		 menu->menuFgColor, menu->menuBgColor, menu->menuFgColor ?
		 GXand : GXor, AllPlanes);
	} else
		XText(menu->menuWindow, x, y, item->itemText,
		 item->itemTextLength, menu->menuFontInfo->id,
		 menu->menuFgColor, menu->menuBgColor);
}

/*
 * Determine which enabled menu item the mouse is currently in.  Return the
 * top position of this item and its item number.  Set inwindow to whether
 * we are or not.
 */
static MenuItem *Mouse_InItem(menu, top, n, inwindow)
register Menu *menu;
int *top, *n, *inwindow;
{
	int x, y;
	Window subw;
	static MenuItem *Y_InItem();

	/*
	 * Find out where the mouse is.  If its not in the menu window,
	 * return NULL.
	 */
	XQueryMouse(RootWindow, &x, &y, &subw);
	if(subw != menu->menuWindow) {
		*inwindow = FALSE;
		return((MenuItem *)0);
	}
	*inwindow = TRUE;
	/*
	 * Now get the coordinates relative to the menu window.
	 */
	XInterpretLocator(menu->menuWindow, &x, &y, &subw, (x << 16) | y);
	/*
	 * Call Y_InItem().
	 */
	*top = y;
	return(Y_InItem(menu, top, n));
}

/*
 * Return which enabled item the locator is in.  Also return the
 * top position of this item and its item number.  Initial y passed
 * in top.
 */
static MenuItem *Y_InItem(menu, top, n)
register Menu *menu;
int *top, *n;
{
	register MenuItem *item;
	register int t, i;
	register int y = *top;
	Window subw;

	/*
	 * Go through the item list.  "t" is the vertical position of the
	 * current item and "i" is its item number.
	 */
	t = menu->menuItemTop;
	/*
	 * If the mouse is before the first item, return.
	 */
	if(y < t)
		return((MenuItem *)0);
	for(i = 0, item = menu->menuItems ; item ; i++, item = item->nextItem) {
		/*
		 * If the y coordinate is within this menu item, then return.
		 * But don't return disable items.
		 */
		if(t + item->itemHeight > y) {
			if(item->itemFlags & itemDisabled)
				return((MenuItem *)0);
			*top = t;
			*n = i;
			return(item);
		}
		t += item->itemHeight;
	}
	/*
	 * Should never get here.
	 */
	return((MenuItem *)0);
}

/*
 * Unmap_Menu() unmaps a menu, if it is currently mapped.
 */
static Unmap_Menu(menu)
register Menu *menu;
{
	register int i;

	if(!menu || !(menu->menuFlags & menuMapped))
		return;
	if(menu->menuFlags & menuFreeze) {
		XUnmapTransparent(menu->menuWindow);
		i = 2 * menu->menuBorderWidth;
		XPixmapPut(RootWindow, 0, 0, menu->menuSavedImageX,
		 menu->menuSavedImageY, menu->menuWidth + i,
		 menu->menuHeight + i, menu->menuSavedImage,
		 GXcopy, AllPlanes);
		XFreePixmap(menu->menuSavedImage);
		XUngrabServer();
	} else
		XUnmapWindow(menu->menuWindow);
	menu->menuFlags &= ~menuMapped;
}
#endif MODEMENU
