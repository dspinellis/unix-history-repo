#ifndef lint
static char *rcsid_GetButton_c = "$Header: GetButton.c,v 10.5 86/11/19 16:23:39 jg Rel $";
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
static char *sccsid = "@(#)GetButton.c	3.8	1/24/86";
#endif
/*
 *	GetButton - This subroutine is used by the Ultrix Window Manager (uwm)
 *	to acquire button events.  It waits for a button event to occur
 *	and handles all event traffic in the interim.
 *
 *	File:		GetButton.c
 */

#include "uwm.h"

Bool GetButton(button_event)
    XButtonEvent *button_event;	/* Button event packet. */
{
    XKeyPressedEvent *kp_event;	/* Key pressed event. */
    char *icon_str;		/* Icon's name string. */
    register int icon_str_len;	/* Icon name string lenght.  */
    register int key_char;	/* Key press character code. */
    register int icon_x;	/* Icon window X coordinate. */
    register int icon_y;	/* Icon window Y coordinate. */
    register int icon_w;	/* Icon window width. */
    register int icon_h;	/* Icon window height. */    
    int status;			/* Routine call return status. */
    Window icon;		/* Icon window. */
    WindowInfo icon_info;	/* Icon window info structure. */
    char *kbd_str;              /* Keyboard string. */
    int nbytes;                 /* Keyboard string length. */
    int i;                      /* Iteration counter. */


    /*
     * Get next event from input queue and store it in the event packet
     * passed to GetButton.
     */
    XNextEvent(button_event);

    /*
     * The event occured on the root window, it must be a mouse
     * button event. 
     */
    if (button_event->window == RootWindow) {
	return(TRUE);
    }

    /*
     * Ok, if the event is not on the root window it must be an event on
     * one of the icons owned by uwm.
     */
    icon = button_event->window;

    /*
     * Find out current information about the icon window.
     */
    status = XQueryWindow(icon, &icon_info);
    if (status == FAILURE) return(FALSE);

    /*
     * If the icon's normal window is gone, then
     * destroy the icon window and return FALSE.
     */
    if (icon_info.assoc_wind == 0) {
        XDestroyWindow(icon);
        return(FALSE);
    }

    /*
     * If the event is an UnmapWindow event,
     * then return FALSE.
     */
    if (button_event->type == UnmapWindow)
        return(FALSE);

    /*
     * Initialize the icon position variables.
     */
    icon_x = icon_info.x;
    icon_y = icon_info.y;

    /*
     * Get the name of the window associated with the icon and
     * determine its length.
     */
    status = XFetchName(icon_info.assoc_wind, &icon_str);
    if (status == FAILURE) return(FALSE);
    icon_str_len = icon_str ? strlen(icon_str) : 0;

    /*
     * If the event is a window exposure event and the icon's name string
     * is not of zero length, simply repaint the text in the icon window
     * and return FALSE.
     */
    if (button_event->type == ExposeWindow && Frozen == 0) {
        XClear(icon);
        if (icon_str_len != 0) {
            XTextPad(icon,
                     HIconPad, VIconPad,
                     icon_str, icon_str_len,
	             IFont, 0, 0,
                     ITextForground, ITextBackground,
                     GXcopy, AllPlanes);
	    /*
	     * Remember to free the icon name string.
	     */
	    free(icon_str);
        }
	return(FALSE);
    }

    /*
     * If we have gotten this far event can only be a key pressed event.
     */
    kp_event = (XKeyPressedEvent *) button_event;

    /* 
     * We convert the key pressed event to ascii.
     */
    kbd_str = XLookupMapping(kp_event, &nbytes);

    /*
     * If kbd_str is a "non-string", then don't do anything.
     */
    if (nbytes == 0) {
        if (icon_str) free(icon_str);
        return(FALSE);
    }
    for (i = 0; i < nbytes; i++) {
        key_char = kbd_str[i];
        /*
         * If the key was <DELETE>, then delete a character from the end of
         * the name, return FALSE.
         *
         * If the key was <CTRL-U>, then wipe out the entire window name
         * and return FALSE.
         *
         * All other ctrl keys are squashed and we return FALSE.
         *
         * All printable characters are appended to the window's name, which
         * may have to be grown to allow for the extra length.
         */
        if (key_char == '\177') {
            /*
             * <DELETE>
             */
            if (icon_str_len > 0) {
		icon_str_len--;
		icon_str[icon_str_len] = '\0';
	    }
        }
        else if (key_char == '\025') {
            /*
             * <CTRL-U>
             */
            if (icon_str_len > 0) {
		icon_str_len = 0;
		icon_str = '\0';
	    }
        }
        else if (key_char < IFontInfo.firstchar ||
                 key_char > IFontInfo.lastchar) {
            /*
             * Any other random (non-printable) key; ignore it.
             */
	    /* do nothing */ ;
        }
        else {
            /*
             * ASCII Alphanumerics.
             */
	    if (icon_str == NULL)
	    	icon_str = (char *) malloc (icon_str_len + 2);
	    else
	    	icon_str = (char *)realloc(icon_str, (icon_str_len + 2));
            if (icon_str == NULL) {
                errno = ENOMEM;
                Error("GetButton -> Realloc of window name string memory failed.");
            }
            icon_str[icon_str_len] = key_char;
            icon_str[icon_str_len + 1] = '\0';
            icon_str_len += 1;
        }
    }

    /*
     * Now that we have changed the size of the icon we have to reconfigure
     * it so that everything looks good.  Oh yes, don't forget to move the
     * mouse so that it stays in the window!
     */

    /*
     * Set the window name to the new string.
     */
    XStoreName(icon_info.assoc_wind, icon_str);

    /*
     * Determine the new icon window configuration.
     */
    icon_h = IFontInfo.height + (VIconPad << 1);
    icon_w = XQueryWidth(icon_str, IFont);
    if (icon_w == 0) {
        icon_w = icon_h;
    }
    else {
	icon_w += (HIconPad << 1);
    }

    if (icon_x < 0) icon_x = 0;
    if (icon_y < 0) icon_y = 0;
    if (icon_x - 1 + icon_w + (IBorderWidth << 1) > ScreenWidth) {
	icon_x = ScreenWidth - icon_w - (IBorderWidth << 1) + 1;
    }
    if (icon_y - 1 + icon_h + (IBorderWidth << 1) > ScreenHeight) {
	icon_y = ScreenHeight - icon_h - (IBorderWidth << 1) + 1;
    }

    XConfigureWindow(icon, icon_x, icon_y, icon_w, icon_h);
    XWarpMouse(icon, (icon_w >> 1), (icon_h >> 1));

    /* 
     * Free the local storage and return FALSE.
     */
    if (icon_str) free(icon_str);
    return(FALSE);
}
