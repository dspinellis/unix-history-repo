#ifndef lint
static char *rcsid_Menu_c = "$Header: Menu.c,v 10.4 86/11/19 16:23:55 jg Rel $";
#endif	lint

/*
 *			COPYRIGHT 1985, 1986
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITIBILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting documentation,
 * and that the name of Digital Equipment Corporation not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission.
 *
 */


/*
 * MODIFICATION HISTORY
 *
 * 000 -- M. Gancarz, DEC Ultrix Engineering Group
 */

#ifndef lint
static char *sccsid = "@(#)Menu.c	3.8	1/24/86";
#endif

#include "uwm.h"

#define DisplayLine(w, pane, width, height, str, fg, bg) \
         XPixSet(w, 0, pane, width, height, bg); \
         XTextMask(w, HMenuPad, pane + VMenuPad, str, strlen(str), MFont, fg);

#define NVERTS	5			/* Number of vertices for hi-liter. */

static Vertex vlist[NVERTS];		/* Vertex list for hi-liter. */

Bool Menu(window, mask, button, x, y, menu)
Window window;				/* Event window. */
int mask;				/* Button/key mask. */
short button;				/* Button event detail. */
int x, y;				/* Event mouse position. */
MenuInfo *menu;
{
    XButtonEvent button_event;		/* Button event packet. */
    Bool func_stat;			/* Function status return. */
    int cur_x, cur_y;			/* Current mouse position. */
    Window sub_window;			/* Current subwindow. */
    int cur_item = 0;			/* Current menu item. */
    int hi_lite = 0;			/* Current highlighted item. */
    int i;				/* Iteration counter. */
    short hlfg, hlbg;			/* Hi-liter pixels. */
    MenuLine *ml;			/* Menu lines pointer. */
    char *hlname;			/* Pointer to hi-liter name. */
    char *strbuf;			/* String buffer for IsTextNL. */
    char *malloc();

    /*
     * Change the cursor.
     */
    status = XGrabButton(RootWindow, MenuCursor, mask, EVENTMASK);
    if (status == FAILURE)
        Error("Menu -> Unable to grab button and change cursor.");

    /*
     * Map the menu.
     */
    MapMenu(menu, x, y);

    /*
     * Main loop.
     */
    while (TRUE) {

        /*
         * If no button event, check the current mouse position.
         */
        status = XUpdateMouse(menu->w, &cur_x, &cur_y, &sub_window);
        if (status == FAILURE) continue;

        /*
         * If the mouse has moved out of the menu sideways, abort
         * the menu operation. Reset the cursor and unmap the menu.
         */
        if (cur_x < 0 || cur_x > menu->width) {
            UnmapMenu(menu, mask);
            return(FALSE);
        }

        /*
         * If the mouse has moved below or above the menu, but is still
         * within the same vertical plane, then simply adjust the values
         * so the user doesn't fall off the edge.
         */
        if (cur_y >= menu->height) cur_y = menu->height - 1;
        else if (cur_y < 0) cur_y = 0;

        /*
         * If the mouse has moved to another item in the menu,
         * highlight the new item.
         */
        cur_item = cur_y / menu->iheight;
        if (cur_item != hi_lite) {

            /*
             * Remove highlighting on old item.
             */
            if (hi_lite) {
                DisplayLine(menu->w, hi_lite * menu->iheight,
                            menu->width, menu->iheight, hlname,
                            hlfg, hlbg);
            }

            /*
             * Highlight new item.
             */
            if (cur_item) {
                for(i = 1, ml = menu->line; ml; i++, ml = ml->next) {
                    if (i == cur_item) break;
                }
                DisplayLine(menu->w, cur_item * menu->iheight,
                            menu->width, menu->iheight, ml->name,
                            menu->hlfg.pixel, menu->hlbg.pixel);
                vlist[0].y = cur_item * menu->iheight + 1;
                XDraw(menu->w, vlist, NVERTS, 1, 1,
                      menu->hlfg.pixel, GXcopy, AllPlanes);
            }
            hi_lite = cur_item;
            hlfg = ml->fg.pixel;
            hlbg = ml->bg.pixel;
            hlname = ml->name;
        }

        /*
         * Check to see if we have a change in the mouse buttons.
         * This means the user has selected an item or aborted the
         * operation.
         */
        if (XPending() && GetButton(&button_event)) {

            /*
             * Was button released?
             */
            if ((button_event.type == ButtonReleased) &&
                ((button_event.detail & ValueMask) == button)) {
                break;
            } else {

                /*
                 * Some other button event occurred, so abort the menu
                 * operation.
                 */
                UnmapMenu(menu, mask);
                return(TRUE);
            }
        }
    }

    /*
     * If no item was selected, simply close the menu and return.
     */
    if (!cur_item) {
        UnmapMenu(menu, mask);
        return(TRUE);
    }

    /*
     * Get a pointer to the menu line selected.
     */
    --cur_item;
    for(i = 0, ml = menu->line; ml; i++, ml = ml->next) {
        if (i == cur_item) break;
    }

    /*
     * Perform the selected menu line action.
     */
    switch (ml->type) {

        case IsShellCommand:
            UnmapMenu(menu, mask);
            system(ml->text);
            break;

        case IsText:
            UnmapMenu(menu, mask);
            XStoreBytes(ml->text, strlen(ml->text));
            break;

        case IsTextNL:
            UnmapMenu(menu, mask);
            strbuf = (char *)malloc(strlen(ml->text) + 2);
            strcpy(strbuf, ml->text);
            strcat(strbuf, "\n");
            XStoreBytes(strbuf, strlen(strbuf));
            free(strbuf);
            break;

        case IsUwmFunction:
            GetContext(&sub_window, &cur_x, &cur_y);
            UnmapMenu(menu, mask);
            if (sub_window != menu->w)
                (*ml->func) (sub_window, mask, button, cur_x, cur_y);
            break;

        case IsImmFunction:
            UnmapMenu(menu, mask);
            (*ml->func) (sub_window, mask, button, cur_x, cur_y);
            break;

        case IsMenuFunction:
            while (TRUE) {
                if (!GetButton(&button_event)) continue;
                if (button_event.type != ButtonPressed) continue;
                if ((KeyMask(button_event.detail) != KeyMask(mask)) ||
                    ((button_event.detail & ButtonMods) != button)) {
                    UnmapMenu(menu, mask);
                    return(TRUE);
                }
                break;
            }
            UnmapMenu(menu, mask);
            func_stat = Menu(menu->w, mask, button, x, y, ml->menu);
            return(func_stat);
            break;

        default:
            Error("Menu -> Internal type error.");
    }
    return(TRUE);
}

/*
 * Create the menu windows for later use.
 */
CreateMenus()
{
    MenuLink *ptr;

    /*
     * If MaxColors isn't set, then jam it to an impossibly high
     * number.
     */
    if (MaxColors == 0)
        MaxColors = 25000;

    for(ptr = Menus; ptr; ptr = ptr->next)
        InitMenu(ptr->menu);
}

/*
 * Initialize a menu.
 */
InitMenu(menu)
MenuInfo *menu;
{
    MenuLine *ml;		/* Menu lines pointer. */
    int width;			/* Width of an item name. */
    int maxwidth;		/* Maximum width of item names. */
    int len;			/* Length of an item name. */
    int count = 1;		/* Number of items + 1 for name. */

    /*
     * Determine the name of the longest menu item.
     */
    maxwidth = XQueryWidth(menu->name, MFont);
    if (maxwidth == 0)
        Error("InitMenu -> Couldn't get length of menu name");

    for(ml = menu->line; ml; ml = ml->next) {
        if ((len = strlen(ml->name)) == 0)
            break;
        width = XQueryWidth(ml->name, MFont);
        if (width == 0) Error("InitMenu -> Couldn't get length of menu item name");
        if (width > maxwidth) maxwidth = width;
        count++;
    }

    /*
     * Get the color cells for the menu items.
     */
    GetMenuColors(menu);

    /*
     * Stash the menu parameters in the menu info structure.
     */
    menu->iheight = MFontInfo.height + (VMenuPad << 1);
    menu->height = menu->iheight * count;
    menu->width = maxwidth + (HMenuPad << 1);
    menu->image = NULL;

    /*
     * Create the menu window.
     */
    menu->w = XCreateWindow(RootWindow,
                            0, 0,
                            menu->width,
                            menu->height,
                            MBorderWidth,
                            MBorder, MBackground);
    if (menu->w == NULL) Error("InitMenu -> Couldn't create menu window");

    /*
     * Store the window name.
     */
    XStoreName(menu->w, menu->name);

    /*
     * Define a cursor for the window.
     */
    XDefineCursor(menu->w, MenuCursor);
}

/*
 * Map a menu.
 */
MapMenu(menu, x, y)
MenuInfo *menu;
int x, y;
{
    int item;
    Window w;
    MenuLine *ml;

    w = menu->w;

    /*
     * Move the menu into place, normalizing the coordinates, if necessary;
     * then map it.
     */
    x -= (menu->width >> 1);
    if (x < 0) x = 0;
    else if (x + menu->width >= ScreenWidth)
        x = ScreenWidth - menu->width - (MBorderWidth << 1);
    if (y < 0) y = 0;
    else if (y + menu->height >= ScreenHeight)
        y = ScreenHeight - menu->height - (MBorderWidth << 1);
    XMoveWindow(w, x, y);

    /*
     * Map the window and draw the text items.
     */
    XMapWindow(w);
    DisplayLine(w, 0, menu->width, menu->iheight, menu->name,
                menu->bg.pixel, menu->fg.pixel);

    SetUpVlist(menu);
    vlist[0].x = 1;
    vlist[0].y = 1;
    XDraw(menu->w, vlist, NVERTS, 1, 1, menu->bg.pixel, GXcopy, AllPlanes);
    item = menu->iheight;
    for(ml = menu->line; ml; ml = ml->next) {
        DisplayLine(w, item, menu->width, menu->iheight, ml->name,
                    ml->fg.pixel, ml->bg.pixel);
        item += menu->iheight;
    }

    /*
     * Position the mouse cursor in the menu header (or in the first item
     * if "autoselect" is set).
     */
    if (Autoselect)
        XWarpMouse(w, (menu->width >> 2) * 3, (menu->iheight >> 1) * 3);
    else XWarpMouse(w, (menu->width >> 2) * 3, menu->iheight >> 1);

    XFlush();
}

/*
 * Unmap a menu, restoring the contents of the screen underneath
 * if necessary. (Restore portion is a future.)
 */
UnmapMenu(menu, mask)
MenuInfo *menu;
int mask;
{
    /*
     * Restore the main cursor.
     */
    Grab((short)mask);

    /*
     * Unmap and flush.
     */
    XUnmapWindow(menu->w);
    XFlush();
}

/*
 * Get the context for invoking a window manager function.
 */
GetContext(w, x, y)
Window *w;
int *x, *y;
{
    XButtonEvent button_event;  /* Button input event. */

    while (TRUE) {

        /*
         * Get the next mouse button event.  Spin our wheels until
         * a button event is returned (ie. GetButton == TRUE).
         * Note that mouse events within an icon window are handled
         * in the "GetButton" function or by the icon's owner if
         * it is not uwm.
         */
        if (!GetButton(&button_event)) continue;

        /*
         * If the button event received is not a ButtonPressed event
         * then continue until we find one.
         */
        if (button_event.type != ButtonPressed) continue;

        /*
         * Okay, determine the event window and mouse coordinates.
         */
        status = XInterpretLocator(RootWindow,
                                    x, y,
                                    w,
                                    button_event.location);

        if (status == FAILURE) continue;

        if (*w == 0)
            *w = RootWindow;

        return;
    }
}

/*
 * Get the color cells for a menu.  This function is slightly brain-damaged
 * in that once MaxColors <= 1, then it refuses to even try to allocate any
 * more colors, even though the colors may have already been allocated.  It
 * probably ought to be done right someday.
 */
GetMenuColors(menu)
MenuInfo *menu;
{
    register MenuLine *ml;		/* Menu lines pointer. */

    /*
     * If we have more than 2 colors available, then attempt to get
     * the color map entries requested by the user.
     * Otherwise, default to standard black and white.
     */
    if (DisplayCells() > 2) {

        /*
         * Get the menu header colors first.
         */
        if (!(menu->foreground && menu->background && MaxColors > 1 &&
              XParseColor(menu->foreground, &menu->fg) &&
              XGetHardwareColor(&menu->fg) &&
              XParseColor(menu->background, &menu->bg) &&
              XGetHardwareColor(&menu->bg))) {
            menu->fg.pixel = MTextForground;
            menu->bg.pixel = MTextBackground;
        } else {
            AdjustMaxColors(menu->fg.pixel);
            AdjustMaxColors(menu->bg.pixel);
        }

        /*
         * Get the menu highlight colors.
         */
        if (!(menu->fghighlight && menu->bghighlight && MaxColors > 1 &&
              XParseColor(menu->fghighlight, &menu->hlfg) &&
              XGetHardwareColor(&menu->hlfg) &&
              XParseColor(menu->bghighlight, &menu->hlbg) &&
              XGetHardwareColor(&menu->hlbg))) {
            menu->hlfg.pixel = MTextBackground;
            menu->hlbg.pixel = MTextForground;
        } else {
            AdjustMaxColors(menu->hlfg.pixel);
            AdjustMaxColors(menu->hlbg.pixel);
        }

        /*
         * Get the menu item colors.
         */
        for(ml = menu->line; ml; ml = ml->next) {
            if (!(ml->foreground && ml->background && MaxColors > 1 &&
                  XParseColor(ml->foreground, &ml->fg) &&
                  XGetHardwareColor(&ml->fg) &&
                  XParseColor(ml->background, &ml->bg) &&
                  XGetHardwareColor(&ml->bg))) {
                ml->fg.pixel = MTextForground;
                ml->bg.pixel = MTextBackground;
            } else {
                AdjustMaxColors(ml->fg.pixel);
                AdjustMaxColors(ml->bg.pixel);
            }
        }

    } else {

        /*
         * Only 2 colors available, so default to standard black and white.
         */
        menu->fg.pixel = MTextForground;
        menu->bg.pixel = MTextBackground;
        menu->hlfg.pixel = MTextBackground;
        menu->hlbg.pixel = MTextForground;
        for(ml = menu->line; ml; ml = ml->next) {
            ml->fg.pixel = MTextForground;
            ml->bg.pixel = MTextBackground;
        }
    }
}

/*
 * Decrement "MaxColors" if this pixel value has never been used in a
 * menu before.
 */
AdjustMaxColors(pixel)
int pixel;
{
    register MenuLink *mptr;
    register MenuLine *lptr;
    int count = 0;

    for(mptr = Menus; mptr; mptr = mptr->next) {
        if (mptr->menu->fg.pixel == pixel) ++count;
        if (mptr->menu->bg.pixel == pixel) ++count;
        if (mptr->menu->hlfg.pixel == pixel) ++count;
        if (mptr->menu->hlbg.pixel == pixel) ++count;
        for(lptr = mptr->menu->line; lptr; lptr = lptr->next) {
            if (lptr->fg.pixel == pixel) ++count;
            if (lptr->bg.pixel == pixel) ++count;
        }
        if (count > 1) return;
    }
    --MaxColors;
}

/*
 * Set up the vertex list for the hi-liter.
 */
SetUpVlist(menu)
MenuInfo *menu;
{
    vlist[1].x = menu->width - 3;
    vlist[1].y = 0;
    vlist[2].x = 0;
    vlist[2].y = menu->iheight - 3;
    vlist[3].x = (short)(0 - menu->width + 3);
    vlist[3].y = 0;
    vlist[4].x = 0;
    vlist[4].y = (short)(0 - menu->iheight + 3);
    vlist[1].flags = vlist[2].flags = vlist[3].flags =
    vlist[4].flags = VertexRelative;
}
