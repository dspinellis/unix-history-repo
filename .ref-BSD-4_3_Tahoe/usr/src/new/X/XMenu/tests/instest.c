#include <X/mit-copyright.h>

/* $Header: instest.c,v 10.7 86/02/12 16:17:41 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window System menu package.
 *
 *	instest.c		Instest is an XMenu selection / pane
 *				insertion testing utility.
 * 
 *	Author:			Tony Della Fera, DEC
 *				20-Nov-85
 */
 
#include <X/Xlib.h>
#include "../XMenu.h"
#include <stdio.h>

main(argc, argv)
    int argc;		/* Argument count. */
    char **argv;	/* Argument vector. */
{
    register int i;	/* Counter. */
    register int j;	/* Counter. */
    register int stat;	/* Return status. */
    int p_num = 0;	/* Pane number. */
    int s_num = 0;	/* Selection number. */
    int x, y;		/* Mouse X and Y position. */
    
    Window twin;	/* Temporary window. */
    XMenu *menu;	/* Menu structure. */
    char *data;		/* Test data. */

    /*
     * Open the display. 
     */
    if (XOpenDisplay(NULL) == NULL) {
	printf("deltest: Error opening display.\n");
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
    printf("Instest assembling panes: zero... ");
    stat = XMenuAddPane(menu, "Pane Zero", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)1, "Exit instest.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)3, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)3, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 0, (char *)3, "Selection three.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("one... ");
    stat = XMenuAddPane(menu, "Pane One", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)4, "Insert pane.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)3, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 1, (char *)3, "Selection two.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("two... ");
    stat = XMenuAddPane(menu, "Pane Two", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 2, (char *)4, "Insert pane.", 1);
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
    stat = XMenuAddSelection(menu, 3, (char *)4, "Insert pane.", 1);
    if (stat == XM_FAILURE) MenuError();

    printf("four... ");
    stat = XMenuAddPane(menu, "Pane Four", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 4, (char *)4, "Insert pane.", 1);
    if (stat == XM_FAILURE) MenuError();
    stat = XMenuAddSelection(menu, 4, (char *)3, "Selection one.", 1);
    if (stat == XM_FAILURE) MenuError();
    printf("done.\n");

    /*
     * Recompute menu.
     */
    printf("Instest recomputing menu dependencies.\n");
    stat = XMenuRecompute(menu);
    if (stat == XM_FAILURE) MenuError();
    
    /*
     * Post the menu.
     */
    while (1) {
	data = NULL;
	printf(
	    "Instest posting menu: pane = %d, selection = %d.\n",
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
	printf("Instest results:  data = %d, pane = %d, selection = %d\n",
	    (int)data, p_num, s_num);
	if (stat == XM_FAILURE) MenuError();
	if (stat == XM_NO_SELECT) {
	    printf("Instest reports no selection made.\n");
	    s_num = 0;
	    continue;
	}
	if (stat == XM_IA_SELECT) {
	    printf("Instest reports no selection active.\n");
	    continue;
	}
	if ((int)data == 1) break;
	if ((int)data == 3) {
	    printf(
		"Instest inserting selection %d in pane %d.\n",
		(s_num + 1), p_num
	    );
	    stat = XMenuInsertSelection(
		menu,
		p_num, (s_num + 1),
		(char *)3,
		"Inserted selection.",
		1
	    );
	    if (stat == XM_FAILURE) MenuError();
	    printf("Instest recomputing menu dependencies.\n");
	    stat = XMenuRecompute(menu);
	    if (stat == XM_FAILURE) MenuError();
	}
	if ((int)data == 4) {
	    printf("Instest inserting pane %d.\n", (p_num + 1));
	    stat = XMenuInsertPane(
		menu,
		(p_num + 1),
		"Inserted Pane.",
		1
	    );
	    if (stat == XM_FAILURE) MenuError();

	    stat = XMenuAddSelection(
		menu,
		(p_num + 1),
		(char *)4,
		"Insert pane.",
		1
	    );
	    if (stat == XM_FAILURE) MenuError();
	    stat = XMenuAddSelection(
		menu,
		(p_num + 1),
		(char *)3,
		"Selection one.",
		1
	    );
	    if (stat == XM_FAILURE) MenuError();
	    printf("Instest recomputing menu dependencies.\n");
	    stat = XMenuRecompute(menu);
	    if (stat == XM_FAILURE) MenuError();
	}
    }

    /*
     * Destroy XMenu.
     */
    printf("Instest destroying menu.\n");
    XMenuDestroy(menu);
}

/*
 * Print the XMenu error message.
 */
MenuError()
{
    printf("\nInstest reports XMenu error: %s.\n\n", XMenuError());
    exit(0);
}
