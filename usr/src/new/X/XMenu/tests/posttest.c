#include <X/mit-copyright.h>

/* $Header: posttest.c,v 10.8 86/11/30 17:00:10 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window System menu package.
 *
 *	posttest.c	Posttest is the XMenu posting and testing utility.
 *
 *	Author:		Tony Della Fera, DEC
 *			September, 1985
 */
 
#include <X/Xlib.h>
#include "../XMenu.h"
#include <stdio.h>

main(argc, argv)
    int argc;		/* Argument count. */
    char **argv;	/* Argument vector. */
{
    register int i;		/* Counter. */
    register int j;		/* Counter. */
    register int stat;		/* Return status. */
    register XMenu *menu;	/* Menu structure. */
    char *data;			/* Test data. */

    int p_num = 2;		/* Pane number. */
    int s_num = 2;		/* Selection number. */
    int x, y;			/* Mouse X and Y postition. */
    
    Window twin;		/* Temporary window. */

    /*
     * Open the display. 
     */
    if (XOpenDisplay(NULL) == NULL) {
	printf("posttest: Error opening display.\n");
	exit(0);
    }

    /*
     * Create the XMenu.
     */
    menu = (XMenu *)XMenuCreate(RootWindow, argv[0]);
    if (menu == NULL) MenuError();

    /*
     * Assemble the panes and selections.
     */
    printf("Posttest assembling panes: zero... ");
    stat = XMenuAddPane(menu, "Pane Zero", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)1, "Exit posttest.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)2, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)2, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)2, "Selection three.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("one... ");
    stat = XMenuAddPane(menu, "Pane One", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)2, "Selection zero.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)2, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)2, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("two... ");
    stat = XMenuAddPane(menu, "Pane Two", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)2, "Selection zero.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)2, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)2, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)2, "Selection three.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)2, "Selection four.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("three... ");
    stat = XMenuAddPane(menu, "Pane Three", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 3, (char *)2, "Selection zero.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("four... ");
    stat = XMenuAddPane(menu, "Pane Four", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 4, (char *)2, "Selection zero.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 4, (char *)2, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    printf("done.\n");

    /*
     * Recompute menu.
     */
    printf("Posttest recomputing menu dependencies.\n");
    stat = XMenuRecompute(menu);
    if (stat == XM_FAILURE) MenuError();
    
    /*
     * Post the menu.
     */
    while (1) {
	data = NULL;
	printf(
	    "Posttest posting menu: pane = %d, selection = %d.\n",
	    p_num, s_num
	);
	XQueryMouse(RootWindow, &x, &y, &twin);
	stat = XMenuActivate(
	    menu,
	    &p_num, &s_num,
	    x, y,
	    ButtonPressed,
	    &data
	);
	printf("Postest results:  data = %d, pane = %d, selection = %d\n",
	    (int)data, p_num, s_num);
	if (stat == XM_FAILURE) MenuError();
	if (stat == XM_NO_SELECT) {
	    printf("Posttest reports no selection made.\n");
	    s_num = -1;
	    continue;
	}
	if (stat == XM_IA_SELECT) {
	    printf("Posttest reports no selection active.\n");
	    continue;
	}
	if ((int)data == 1) break;
    }

    /*
     * Destroy XMenu.
     */
    printf("Posttest destroying menu.\n");
    XMenuDestroy(menu);
}

/*
 * Print the XMenu error message.
 */
MenuError()
{
    printf("\nPosttest reports XMenu error: %s.\n\n", XMenuError());
    exit(0);
}
