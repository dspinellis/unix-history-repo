#include <X/mit-copyright.h>

/* $Header: rstest.c,v 10.6 86/02/12 16:17:54 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window System menu package.
 *
 *	rstest.c		rstest is an XMenu random selection
 *				addition testing utility.
 *
 *	Author:			Tony Della Fera, DEC
 *				September, 1985
 */
 
#include <X/Xlib.h>
#include "../XMenu.h"
#include <stdio.h>

main(argc, argv)
    int argc;			/* Argument count. */
    char **argv;		/* Argument vector. */
{
    register int i;		/* Counter. */
    register int j;		/* Counter. */
    register int stat;		/* Return status. */
    register XMenu *menu;	/* Menu structure. */
    char *data;			/* Test data. */

    int p_num = 0;		/* Pane number. */
    int s_num = 0;		/* Selection number. */
    int x, y;			/* Mouse X and Y position. */
    
    Window twin;		/* Temporary window. */

    /*
     * Open the display. 
     */
    if (XOpenDisplay(NULL) == NULL) {
	printf("rstest: Error opening display.\n");
	exit(0);
    }

    /*
     * Create the XMenu.
     */
    menu = (XMenu *)XMenuCreate(RootWindow, argv[0]);
    if (menu == NULL) MenuError();

    /*
     * Assemble the panes.
     */
    printf("Rstest assembling panes: zero... ");
    stat = XMenuAddPane(menu, "Pane Zero", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("one... ");
    stat = XMenuAddPane(menu, "Pane One", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("two... ");
    stat = XMenuAddPane(menu, "Pane Two", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("three... ");
    stat = XMenuAddPane(menu, "Pane Three", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("four... ");
    stat = XMenuAddPane(menu, "Pane Four", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("done.\n");

    /*
     * Add selections.
     */
    printf("Adding selections... ");
    stat = XMenuAddSelection(menu, 0, (char *)1, "Exit rstest.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)2, "Selection zero.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)2, "Selection zero.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)2, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)2, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)2, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)2, "Selection three.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)2, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 4, (char *)2, "Selection zero.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)2, "Selection four.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 4, (char *)2, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 3, (char *)2, "Selection zero.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)2, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)2, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)2, "Selection three.", 1);
    if (stat == XM_FAILURE) MenuError();
    printf("done.\n");

    /*
     * Recompute menu.
     */
    printf("Rstest recomputing menu dependencies.\n");
    stat = XMenuRecompute(menu);
    if (stat == XM_FAILURE) MenuError();
    
    /*
     * Post the menu.
     */
    while (1) {
	data = NULL;
	printf(
	    "Rstest posting menu: pane = %d, selection = %d.\n",
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
	printf("Rstest results:  data = %d, pane = %d, selection = %d\n",
	    (int)data, p_num, s_num);
	if (stat == XM_FAILURE) MenuError();
	if (stat == XM_NO_SELECT) {
	    printf("Rstest reports no selection made.\n");
	    s_num = 0;
	    continue;
	}
	if (stat == XM_IA_SELECT) {
	    printf("Rstest reports no selection active.\n");
	    continue;
	}
	if ((int)data == 1) break;
    }

    /*
     * Destroy XMenu.
     */
    printf("Rstest destroying menu.\n");
    XMenuDestroy(menu);
}

/*
 * Print the XMenu error message.
 */
MenuError()
{
    printf("\nRstest reports XMenu error: %s.\n\n", XMenuError());
    exit(0);
}
