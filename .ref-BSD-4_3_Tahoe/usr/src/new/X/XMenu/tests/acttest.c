#include <X/mit-copyright.h>

/* $Header: acttest.c,v 10.6 86/02/12 16:17:31 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window System menu package.
 *
 *	acttest.c		Acttest is an XMenu selection / pane
 *				activation testing utility.
 *
 *	Author:			Tony Della Fera, DEC
 *				September, 1985
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

    int p_num = 0;		/* Pane number. */
    int s_num = 0;		/* Selection number. */
    int x, y;			/* Mouse X and Y position. */
    
    Window twin;		/* Temporary window. */

    /*
     * Open the display. 
     */
    if (XOpenDisplay(NULL) == NULL) {
	printf("acttest: Error opening display.\n");
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
    printf("Acttest assembling panes: zero... ");
    stat = XMenuAddPane(menu, "Pane Zero", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)1, "Exit acctest.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)2, "Reactivate all panes.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)3, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)3, "Selection three.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("one... ");
    stat = XMenuAddPane(menu, "Pane One", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)4, "Deactivate pane.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)3, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)3, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("two... ");
    stat = XMenuAddPane(menu, "Pane Two", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)4, "Deactivate pane.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)3, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)3, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)3, "Selection three.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)3, "Selection four.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("three... ");
    stat = XMenuAddPane(menu, "Pane Three", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 3, (char *)4, "Deactivate pane.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("four... ");
    stat = XMenuAddPane(menu, "Pane Four", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 4, (char *)4, "Deactivate pane.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 4, (char *)3, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    printf("done.\n");

    /*
     * Recompute menu.
     */
    printf("Acttest recomputing menu dependencies.\n");
    stat = XMenuRecompute(menu);
    if (stat == XM_FAILURE) MenuError();
    
    /*
     * Post the menu.
     */
    while (1) {
	data = NULL;
	printf(
	    "Acttest posting menu: pane = %d, selection = %d.\n",
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
	printf("Acttest results:  data = %d, pane = %d, selection = %d\n",
	    (int)data, p_num, s_num);
	if (stat == XM_FAILURE) MenuError();
	if (stat == XM_NO_SELECT) {
	    printf("Acttest reports no selection made.\n");
	    s_num = 0;
	    continue;
	}
	if (stat == XM_IA_SELECT) {
	    printf("Acttest reports no selection active.\n");
	    continue;
	}
	if ((int)data == 1) break;
	if ((int)data == 2) {
	    printf("Acttest reactivating all selections.\n");
	    stat = XMenuSetPane(menu, 0, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 0, 0, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 0, 1, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 0, 2, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 0, 3, 1);
	    if (stat == XM_FAILURE) MenuError();

	    stat = XMenuSetPane(menu, 1, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 1, 0, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 1, 1, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 1, 2, 1);
	    if (stat == XM_FAILURE) MenuError();
	    
	    stat = XMenuSetPane(menu, 2, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 2, 0, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 2, 1, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 2, 2, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 2, 3, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 2, 4, 1);
	    if (stat == XM_FAILURE) MenuError();

	    stat = XMenuSetPane(menu, 3, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 3, 0, 1);
	    if (stat == XM_FAILURE) MenuError();

	    stat = XMenuSetPane(menu, 4, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 4, 0, 1);
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuSetSelection(menu, 4, 1, 1);
	    if (stat == XM_FAILURE) MenuError();
	    continue;
	}
	if ((int)data == 3) {
	    printf(
		"Acttest deactivating selection %d in pane %d.\n",
		s_num, p_num
	    );
	    stat = XMenuSetSelection(menu, p_num, s_num, 0);
	    if (stat == XM_FAILURE) MenuError();
	}
	if ((int)data == 4) {
	    printf("Acttest deactivating pane %d.\n", p_num);
	    stat = XMenuSetPane(menu, p_num, 0);
	    if (stat == XM_FAILURE) MenuError();
	}
    }

    /*
     * Destroy XMenu.
     */
    printf("Acttest destroying menu.\n");
    XMenuDestroy(menu);
}

/*
 * Print the XMenu error message.
 */
MenuError()
{
    printf("\nActtest reports XMenu error: %s.\n\n", XMenuError());
    exit(0);
}
