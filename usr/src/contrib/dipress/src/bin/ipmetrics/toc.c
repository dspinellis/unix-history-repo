/*
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * Print out a table of contents (toc)
 *
 *  HISTORY
 * 15-Apr-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Created from troff.c
 *
 *	Nov, 1985	Lee Moore, Xerox Webster Research Center
 *		Created.
 */

#include <stdio.h>
#include <math.h>
#include "stack.h"
#include "token.h"
#include "config.h"
#include "ipmetrics.h"

#define TRUE	1
#define FALSE	0


#define public
#define private	static

public char *malloc();

public char *DeviceName,
	    *LibraryDirectory;

public
InitTOC() {
	putchar('\n');
}


/*
 * called once per font on the stack
 */

public
PerTOCFont(configChain, fontDescVec)
    struct FontConfig *configChain;
    unsigned char *fontDescVec; {
	unsigned char *charMetricsProperty,
		      *metricsProperty,
		      *width,
		      *charMetric;
	char iSender[MAXTOKENSIZE],
	     iCharName[MAXTOKENSIZE],
	     fileType[MAXTOKENSIZE],
	    *fontName[3],
	     iCharSet[MAXTOKENSIZE],
	     iCharCode[MAXTOKENSIZE];
	FILE *descFile,
	     *modelFile;
	struct FontConfig *p;
	struct TokenState *ts;
	int charSet,
	    charNumber,
	    charIndex,
	    xWidth;

	if( !GetFontNameProperty(fontDescVec, fontName) ) {
		fprintf(stderr, "ipmetrics: can't get font name\n");
		return;
	}

	printf("font name is: %s/%s/%s\n", fontName[0], fontName[1], fontName[2]);


	if( (charMetricsProperty = GetStringProp("characterMetrics", fontDescVec))
			== NULL ) {
		printf("ipmetrics: can't find 'characterMetrics' property\n");
		return;	}

	if( (metricsProperty = GetStringProp("metrics", fontDescVec))
			!= NULL ) {
		unsigned char *easyProperty;

		if( (easyProperty = GetStringProp("easy", metricsProperty))
				!= NULL ) 
			printf("\thas easy metrics\n");
	}
}
