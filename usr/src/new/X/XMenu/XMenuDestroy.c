#include <X/mit-copyright.h>

/* $Header: XMenuDestroy.c,v 10.7 86/05/15 16:26:40 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuDestroy - Free all resources associated with and XMenu.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInternal.h"

XMenuDestroy(menu)
    register XMenu *menu;	/* Menu object to destroy. */
{
    register XMPane *p_ptr;	/* Pointer to the current pane. */
    register XMPane *p_next;	/* Pointer to the next pane. */
    register XMSelect *s_ptr;	/* Pointer to the current selection. */
    register XMSelect *s_next;  /* Pointer to the next selection. */

    /*
     * Destroy the selection and pane X windows and free
     * their corresponding XMWindows.
     */
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_next
    ) {
	for (
	    s_ptr = p_ptr->s_list->next;
	    s_ptr != p_ptr->s_list;
	    s_ptr = s_next
	) {
	    s_next = s_ptr->next;
	    free(s_ptr);
	}
	if (p_ptr->window) {
	    XDestroySubwindows(p_ptr->window);
	    XDestroyWindow(p_ptr->window);
	}
	p_next = p_ptr->next;
	free(p_ptr);
    }

    /*
     * Destroy the association table.
     */
    XDestroyAssocTable(menu->assoc_tab);

    /*
     * Free the mouse cursor.
     */
    XFreeCursor(menu->mouse_cursor);

    /*
     * Free the fonts.
     */
    XFreeFont(menu->p_fnt_info->id);
    XFreeFont(menu->s_fnt_info->id);

    /*
     * Free the pixmaps.
     */
    XFreePixmap(menu->p_bdr_pixmap);
    XFreePixmap(menu->s_bdr_pixmap);
    XFreePixmap(menu->p_frg_pixmap);
    XFreePixmap(menu->s_frg_pixmap);
    XFreePixmap(menu->bkgnd_pixmap);
    XFreePixmap(menu->inact_pixmap);

    /*
     * Free the color cells.
     */
    if ((menu->p_bdr_color != BlackPixel) && (menu->p_bdr_color != WhitePixel))
	XFreeColors(&menu->p_bdr_color, 1, 0);
    if ((menu->s_bdr_color != BlackPixel) && (menu->s_bdr_color != WhitePixel))
	XFreeColors(&menu->s_bdr_color, 1, 0);
    if ((menu->p_frg_color != BlackPixel) && (menu->p_frg_color != WhitePixel))
	XFreeColors(&menu->p_frg_color, 1, 0);
    if ((menu->s_frg_color != BlackPixel) && (menu->s_frg_color != WhitePixel))
	XFreeColors(&menu->s_frg_color, 1, 0);
    if ((menu->bkgnd_color != BlackPixel) && (menu->bkgnd_color != WhitePixel))
	XFreeColors(&menu->bkgnd_color, 1, 0);

    /*
     * Free the XMenu.
     */
    free(menu);
}
