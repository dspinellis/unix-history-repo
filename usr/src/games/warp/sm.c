/* $Header: sm.c,v 7.0 86/10/08 15:13:35 lwall Exp $ */

/* $Log:	sm.c,v $
 * Revision 7.0  86/10/08  15:13:35  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include <stdio.h>
#include <ctype.h>
#include "config.h"

main()
{
    char screen[23][90], buf[10];
    Reg1 int y;
    Reg2 int x;
    int tmpy, tmpx;

    for (x=0; x<79; x++)
	screen[0][x] = ' ';
    screen[0][79] = '\0';
    
    fgets(screen[0],90,stdin);
    if (isdigit(screen[0][0])) {
	int numstars = atoi(screen[0]);

	for (y=0; y<23; y++) {
	    for (x=0; x<79; x++)
		screen[y][x] = ' ';
	    screen[y][79] = '\0';
	}
	
	for ( ; numstars; numstars--) {
	    scanf("%d %d\n",&tmpy,&tmpx);
	    y = tmpy;
	    x = tmpx;
	    screen[y][x+x] = '*';
	}

	for (y=0; y<23; y++) {
	    printf("%s\n",screen[y]);
	}
    }
    else {
	Reg3 int numstars = 0;

	for (y=1; y<23; y++) {
	    for (x=0; x<79; x++)
		screen[y][x] = ' ';
	    screen[y][79] = '\0';
	}
	
	for (y=1; y<23; y++) {
	    fgets(screen[y],90,stdin);
	}

	for (y=0; y<23; y++) {
	    for (x=0; x<80; x += 2) {
		if (screen[y][x] == '*') {
		    numstars++;
		}
		else if (screen[y][x] == '\t' || screen[y][x+1] == '\t') {
		    fprintf(stderr,"Cannot have tabs in starmap--please expand.\n");
		    exit(1);
		}
	    }
	}

	printf("%d\n",numstars);

	for (y=0; y<23; y++) {
	    for (x=0; x<80; x += 2) {
		if (screen[y][x] == '*') {
		    printf("%d %d\n",y,x/2);
		}
	    }
	}
    }
    exit(0);
}
