/*
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 *  create TeX font files
 *
 * HISTORY
 *	Dec, 1985	Lee Moore, Xerox Webster Research Center
 *		Created.
 *
 */

#include <stdio.h>
#include <math.h>
#include "stack.h"
#include "token.h"
#include "config.h"
#include "ipmetrics.h"
#include "tex.h"

#define TRUE	1
#define FALSE	0

#define public
#define private	static

public char *malloc();

public char *DeviceName,
	    *LibraryDirectory;

public
CleanUpTeX(configChain)
    struct FontConfig *configChain; {
	struct FontConfig *p;

	WriteTeXInstallFile(configChain);
	WriteTeXCleanUpFile(configChain);

	for( p = configChain; p != NULL; p = p->Next )
		if( !p->SeenFlag )
			printf("couldn't find: %s/%s/%s\n",
				p->FontPt1, p->FontPt2, p->FontPt3);
}


private
WriteTeXInstallFile(configChain)
    struct FontConfig *configChain; {
	int i;
	FILE *installFile;
	struct FontConfig *p;

	if( (installFile = fopen(INSTALLNAME, "w")) == NULL ) {
	    fprintf(stderr, "can't open the file 'install' for writing\n");
	    return; }

	fprintf(installFile, "#! /bin/sh\n");
	fprintf(installFile, "if test ! -d %s/fonts/%s\n", LibraryDirectory, DeviceName);
	fprintf(installFile, "  then\n");
	fprintf(installFile, "    mkdir %s/fonts/%s\n", LibraryDirectory, DeviceName);
	fprintf(installFile, "  fi\n");
	fprintf(installFile, "if test ! -d %s/fonts/%s/tex\n", LibraryDirectory, DeviceName);
	fprintf(installFile, "  then\n");
	fprintf(installFile, "    mkdir %s/fonts/%s/tex\n", LibraryDirectory, DeviceName);
	fprintf(installFile, "  fi\n");


	for( p = configChain; p != NULL; p = p->Next )
		if( p->SeenFlag ) {
	    		fprintf(installFile, "cp %s.pl %s/fonts/%s/tex\n",
				p->TroffName, LibraryDirectory, DeviceName);
			fprintf(installFile, "pltotf %s.pl %s.tfm\n",
				p->TroffName, p->TroffName); }

	fprintf(installFile, "cd %s/fonts/%s/tex\n", LibraryDirectory,
			DeviceName);
	fclose(installFile);
	chmod(INSTALLNAME, 0755); }


/*
 * write a file that rm's all the files created by this program
 */

private
WriteTeXCleanUpFile(configChain)
    struct FontConfig *configChain; {
	int i;
	FILE *cleanupFile;
	struct FontConfig *p;

	if( (cleanupFile = fopen(CLEANUPNAME, "w")) == NULL ) {
	    fprintf(stderr, "can't open the file 'cleanup' for writing\n");
	    return; }

	fprintf(cleanupFile, "#! /bin/sh\n");

	for( p = configChain; p != NULL; p = p->Next )
		if( p->SeenFlag )
	    		fprintf(cleanupFile, "rm %s.pl\n", p->TroffName);

	fprintf(cleanupFile, "rm %s\n", CLEANUPNAME);
	fprintf(cleanupFile, "rm %s\n", INSTALLNAME);
	fclose(cleanupFile);
	chmod(CLEANUPNAME, 0755); }


PerTeXFont(configChain, fontDescVec)
struct FontConfig *configChain;
unsigned char *fontDescVec; {
	unsigned char *charMetricsProperty,
		      *metricsProperty,
		      *width,
		      *charMetric;
	char iSender[MAXTOKENSIZE],
	     iCharName[MAXTOKENSIZE],
	     fileType[MAXTOKENSIZE],
	    *fontName[40],
	     metricFileName[40],
	     iCharSet[MAXTOKENSIZE],
	     iCharCode[MAXTOKENSIZE];
	FILE *descFile,
	     *modelFile;
	struct FontConfig *p;
	struct TokenState *ts;
	int charSet,
	    charNumber,
	    charIndex;
	double xWidth;

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

		sprintf(metricFileName, "%s.pl", p->TroffName);

		if( (descFile = fopen(metricFileName, "w")) == NULL ) {
			printf("ipmetrics: can't open %s for writing\n", metricFileName);
			return;}
	
		if( (modelFile = fopen(p->MapFile, "r")) == NULL ) {
			printf("ipmetrics: can't open %s for reading\n", p->MapFile);
			return;}

		p->SeenFlag = TRUE;
/*		(void) strcpy(malloc((unsigned) 40), p->TroffName);	*/
	
		ts = InitTokenStream(modelFile);

		fprintf(descFile, "(COMMENT %s/%s/%s for Interpress device %s\n", p->FontPt1, p->FontPt2, p->FontPt3, DeviceName);

		fprintf(descFile, " for interpress device '%s')\n", DeviceName);
		fprintf(descFile, "(FAMILY %s)\n", p->TroffName);
		fprintf(descFile, "(DESIGNSIZE D 10)\n");
		fprintf(descFile, "(DESIGNUNITS D 1)\n");

		GetToken(ts, fileType, MAXTOKENSIZE);
		/* file type doesn't mean much in this case... */

		while( !EndOfFile(ts) ) {
			GetToken(ts, iCharSet, MAXTOKENSIZE);
			sscanf(iCharSet, "%o", &charSet);
			GetToken(ts, iCharCode, MAXTOKENSIZE);
			sscanf(iCharCode, "%o", &charNumber);
			GetToken(ts, iSender, MAXTOKENSIZE); /* ignored... */
			GetToken(ts, iCharName, MAXTOKENSIZE);
			charIndex = Make16BitChar(charSet, charNumber);

			/* skip the rest of this loop if it's not in this font */
			if( (charMetric =
			    GetIntegerProp(charIndex, charMetricsProperty)) == NULL )
				continue;

			if( (width = GetStringProp("widthX", charMetric)) == NULL ){
				printf("ipmetrics: can't find widthX property of %d\n",
					charIndex);
				continue;}

			if( gettype(width) != type_number ) {
				printf("ipmetrics: width not of type number for %d\n",
					charIndex);
				continue;}
	
			if( getsubtype(width) != subtype_rational ) {
				printf("ipmetrics: width not of subtype number for %d\n",
					charIndex);
				continue;}
	
			xWidth = ((float) getnumerator(width)) / ((float) getdenominator(width));
	
			fprintf(descFile, "(CHARACTER O %3o", charNumber);
			fprintf(descFile, " (CHARWD R %6.4f))", xWidth);

			if( charSet == 0 )
				fprintf(descFile, "\n");
			else
				fprintf(descFile, "\t(COMMENT in charset 0%o)\n", charSet);

			while( !EndOfLine(ts) ) {
				GetToken(ts, iCharName, MAXTOKENSIZE);} }

		CloseTokenStream(ts);
		fclose(descFile);
		fclose(modelFile); }
}
