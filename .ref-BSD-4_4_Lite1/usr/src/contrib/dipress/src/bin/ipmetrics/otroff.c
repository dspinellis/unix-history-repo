/*
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * Handle old C/A/T troff metrics here.
 *
 * note that Troff uses "fat points" of which there are exactly 72 per inch.
 *
 *  HISTORY
 * 15-Apr-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Now prints out the number of special character names in the
 *	DESC file.
 *
 * Nov, 1985	Lee Moore, Xerox Webster Research Center
 *	Created.
 */

#include <stdio.h>
#include <math.h>
#include "stack.h"
#include "token.h"
#include "config.h"
#include "ipmetrics.h"
#include "troff.h"
#include "strings.h"

#define TRUE	1
#define FALSE	0


#define public
#define private	static

public char *malloc();

public char *DeviceName;

public
CleanUpOTroff(configChain)
    struct FontConfig *configChain; {
	struct FontConfig *p;

	WriteInstallFile(configChain);
	WriteCleanUpFile(configChain);

	for( p = configChain; p != NULL; p = p->Next )
		if( !p->SeenFlag )
			printf("couldn't find: %s/%s/%s\n",
				p->FontPt1, p->FontPt2, p->FontPt3);
}

private
WriteInstallFile(configChain)
    struct FontConfig *configChain; {
	FILE *installFile;
	struct FontConfig *p;

	if( (installFile = fopen(INSTALLNAME, "w")) == NULL ) {
	    fprintf(stderr, "can't open the file 'install' for writing\n");
	    return; }

	fprintf(installFile, "#! /bin/sh\n");

	for( p = configChain; p != NULL; p = p->Next )
		if( p->SeenFlag )
	    		fprintf(installFile, "cp %s /usr/lib/fonts\n",
				p->TargetName);

	(void) fclose(installFile);
	(void) chmod(INSTALLNAME, 0755); }


/*
 * write a file that rm's all the files created by this program
 */

private
WriteCleanUpFile(configChain)
    struct FontConfig *configChain; {
	FILE *cleanupFile;
	struct FontConfig *p;

	if( (cleanupFile = fopen(CLEANUPNAME, "w")) == NULL ) {
	    fprintf(stderr, "can't open the file 'cleanup' for writing\n");
	    return; }

	fprintf(cleanupFile, "#! /bin/sh\n");

	for( p = configChain; p != NULL; p = p->Next )
		if( p->SeenFlag )
	    		fprintf(cleanupFile, "rm %s\n", p->TargetName);

	fprintf(cleanupFile, "rm %s\n", INSTALLNAME);
	fprintf(cleanupFile, "rm %s\n", CLEANUPNAME);
	(void) fclose(cleanupFile);
	(void) chmod(CLEANUPNAME, 0755); }

/*
 * called once per font on the stack
 */

public
PerOTroffFont(configChain, fontDescVec)
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

	if( (charMetricsProperty = GetStringProp("characterMetrics", fontDescVec))
			== NULL ) {
		printf("ipmetrics: can't find 'characterMetrics' property\n");
		return;	}

	for( p = configChain; p != NULL; p = p->Next ) {
		if( !(strcmp(p->FontPt1,  fontName[0]) == 0 &&
			    strcmp(p->FontPt2,  fontName[1]) == 0 &&
			    strcmp(p->FontPt3,  fontName[2]) == 0) )
			continue;

		if( (descFile = fopen(p->TargetName , "w")) == NULL ) {
			printf("ipmetrics: can't open %s for writing\n", p->TargetName);
			return;}
	
		fprintf(stderr, "writing %s\n", p->TargetName);

		if( (modelFile = fopen(p->MapFile, "r")) == NULL ) {
			printf("ipmetrics: can't open %s for reading\n", p->MapFile);
			return;}

		p->SeenFlag = TRUE;
	
		ts = InitTokenStream(modelFile);
	
		fprintf(descFile, "#\n");
		fprintf(descFile, "# %s/%s/%s for Interpress device %s\n", p->FontPt1, p->FontPt2, p->FontPt3, DeviceName);
		fprintf(descFile, "name %s\n", p->TargetName);
	
		GetToken(ts, fileType, MAXTOKENSIZE);
	
		while( !EndOfFile(ts) ) {
			GetToken(ts, iCharSet, MAXTOKENSIZE);

			if( sscanf(iCharSet, "%o", &charSet) != 1 )
			    printf("ipmetrics: couldn't convert iCharSet number.  Token was: %s\n", iCharSet);

			if( EndOfLine(ts) ) {
			    printf("ipmetrics: premature end of line in map file: %s!\n", p->MapFile);
			    printf("\tlast token was iCharSet: `%s'\n", iCharSet);
			    continue; }

			GetToken(ts, iCharCode, MAXTOKENSIZE);

			if( sscanf(iCharCode, "%o", &charNumber) != 1 )
			    printf("ipmetrics: couldn't convert iCharCode.  Token was: %s\n", iCharCode);

			if( EndOfLine(ts) ) {
			    printf("ipmetrics: premature end of line in map file: %s!\n", p->MapFile);
			    printf("\tlast token was iCharCode: `%s'\n", iCharCode);
			    continue; }

			GetToken(ts, iSender, MAXTOKENSIZE);

			if( EndOfLine(ts) ) {
			    printf("ipmetrics: premature end of line in map file: %s!\n", p->MapFile);
			    printf("\tlast token was iSender: `%s'\n", iSender);
			    continue; }
	
			GetToken(ts, iCharName, MAXTOKENSIZE);
			charIndex = Make16BitChar(charSet, charNumber);

			/* skip the rest of this loop if the character is 0 */
			if( charIndex == 0 ) {
				fprintf(descFile, "\t  0,\t\t/*(unused)*/\n");
				EatRestOfLine(ts);
				continue; }
	
			/* skip the rest of this loop if it's not in this font */
			if( (charMetric =
			    GetIntegerProp(charIndex, charMetricsProperty)) == NULL ) {
				fprintf(descFile, "\t  0,\t\t/*(not in file) %s */\n", iCharName);
				EatRestOfLine(ts);
				continue; }
	
			if( (width = GetStringProp("widthX", charMetric)) == NULL ){
				printf("ipmetrics: can't find widthX property of %d\n",
					charIndex);
				EatRestOfLine(ts);
				continue;}
	
			if( gettype(width) != type_number ) {
				printf("ipmetrics: width not of type number for %d\n",
					charIndex);
				EatRestOfLine(ts);
				continue;}
	
			if( getsubtype(width) != subtype_rational ) {
				printf("ipmetrics: width not of subtype number for %d\n",
					charIndex);
				EatRestOfLine(ts);
				continue;}
	
			xWidth = (getnumerator(width)*36)/
				getdenominator(width) + .5;
	
			fprintf(descFile, "\t%3d + 0%s00,\t/* %s ", 
				xWidth, iSender, iCharName);
	
			while( !EndOfLine(ts) ) {
				GetToken(ts, iCharName, MAXTOKENSIZE);
				fprintf(descFile, "%s ", iCharName); }

			fprintf(descFile, "*/\n"); }
	
		CloseTokenStream(ts);
		(void) fclose(descFile);
		(void) fclose(modelFile);
	}
}
