#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 *	GetButton - This subroutine is used by the X Window Manager (xwm)
 *	to acquire button events.  It waits for a button event to occur
 *	and handles all event traffic in the interim.
 *
 *	File:		GetButton.c
 */
#ifndef lint
static char *rcsid_GetButton_c = "$Header: GetButton.c,v 10.5 86/02/01 16:09:26 tony Rel $";
#endif

#include "xwm.h"

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
    char *kbd_str;		/* Keyboard string	*/
    int nbytes;			/* Keyboard string length	*/
    int i;			/* loop over the string		*/
    Bool name_changed = FALSE;  /* if TRUE, must redisplay */

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
     * one of the icons owned by xwm.
     */
    icon = button_event->window;

    /*
     * If the event is an UnmapWindow event then delete the icon window
     * and return FALSE.
     */
    if (button_event->type == UnmapWindow) {
    	XDestroyWindow(icon);
	return(FALSE);
    }

    /*
     * If it is not an UnmapWindow event then it must be either an 
     * ExposeWindow event or a KeyPressed event.
     */
     
    /*
     * Find out current information about the icon window.
     */
    status = XQueryWindow(icon, &icon_info);
    if (status == FAILURE) return(FALSE);

    /*
     * Initialize the icon position variables.
     */
    icon_x = icon_info.x;
    icon_y = icon_info.y;

    /*
     * Get the name of the window associated with the icon and
     * determine its lenght.
     */
    status = XFetchName(icon_info.assoc_wind, &icon_str);
    if (status == FAILURE) return(FALSE);
    icon_str_len = icon_str ? strlen(icon_str) : 0;

    /*
     * If the event is a window exposure event and the icon's name string
     * is not of zero length, simply repaint the text in the icon window
     * and return FALSE.
     */
    if (button_event->type == ExposeWindow) {
	XClear(icon);
	if (icon_str_len != 0) {
	    XTextPad (
		icon,
		IPadding, IPadding,
		icon_str, icon_str_len,
		IFont, 0, 0, 
		ITextForground, ITextBackground,
		GXcopy, AllPlanes
	    );
	    /*
	     * Remember to free the icon name string.  (Oh Bother! Said Poo.)
	     */
	    free (icon_str);
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
    for (i = 0; i < nbytes; i++) {
	    key_char = kbd_str[i];
	    /*
	     * If the key was <DELETE>, then delete a character from the end of
	     * the name.
	     *
	     * If the key was <CTRL-U>, then wipe out the entire window name.
	     *
	     * All other ctrl keys are squashed.
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
		    name_changed = TRUE;
		    }
	        }
	    else if (key_char == '\025') {
		/*
		 * <CTRL-U>
		 */
		if (icon_str_len > 0) {
		    icon_str_len = 0;
		    *icon_str = '\0';
		    name_changed = TRUE;
		    }
	        }
	    else if (key_char < IFontInfo.firstchar 
		  || key_char > IFontInfo.lastchar)
		/*
		 * Any other random (non-printable) key: ignore it.
		 */
	        /* do nothing */ ;
	    else {
		/*
		 * ASCII Alphanumerics.
		 */
		if (icon_str == NULL)
		    icon_str = (char *)malloc(icon_str_len + 2);
		else
		    icon_str = (char *)realloc(icon_str, (icon_str_len + 2));
		if (icon_str == NULL) {
		    errno = ENOMEM;
		    Error("GetButton -> Realloc of window name string memory failed.");
		}
		icon_str[icon_str_len] = key_char;
		icon_str[icon_str_len + 1] = '\0';
		icon_str_len += 1;
		name_changed = TRUE;
    	}
    }
    
    if (name_changed) {
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
	icon_h = IFontInfo.height + (IPadding << 1);
	icon_w = XQueryWidth(icon_str, IFont);
	if (icon_w == 0) {
	    icon_w = icon_h;
	}
	else {
	    icon_w += (IPadding << 1);
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
    }

    /* 
     * Free the local storage and return FALSE.
     */
    if (icon_str) free(icon_str);
    return(FALSE);
}
