/*
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 *  create Generic font files
 *
 * HISTORY
 * 03-Sep-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Created from tex.c .
 *
 * 11-Feb-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added the creation of an extra text file which contains information
 *	on how to access characters in other character-sets than zero (0).
 *
 * 15-Dec-85  Lee Moore (lee) at Xerox Webster Research Center
 *	Created.
 *
 */

#include <stdio.h>
#include <math.h>
#include "stack.h"
#include "token.h"
#include "config.h"
#include "ipmetrics.h"
#include "generic.h"

#define TRUE	1
#define FALSE	0

#define public
#define private	static

extern unsigned char **getvector();

public char *malloc();

public char *DeviceName,
	    *LibraryDirectory;

public
CleanUpGeneric(configChain)
    struct FontConfig *configChain; {
	struct FontConfig *p;

	WriteGenericInstallFile(configChain);
	WriteGenericCleanUpFile(configChain);

	for( p = configChain; p != NULL; p = p->Next )
		if( !p->SeenFlag )
			printf("couldn't find: %s/%s/%s\n",
				p->FontPt1, p->FontPt2, p->FontPt3);
}


private
WriteGenericInstallFile(configChain)
    struct FontConfig *configChain; {
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
	fprintf(installFile, "if test ! -d %s/fonts/%s/generic\n", LibraryDirectory, DeviceName);
	fprintf(installFile, "  then\n");
	fprintf(installFile, "    mkdir %s/fonts/%s/generic\n", LibraryDirectory, DeviceName);
	fprintf(installFile, "  fi\n");


	for( p = configChain; p != NULL; p = p->Next )
		if( p->SeenFlag )
	    		fprintf(installFile, "cp %s.generic %s/fonts/%s/generic\n",
				p->TargetName, LibraryDirectory, DeviceName);

	fprintf(installFile, "cd %s/fonts/%s/generic\n", LibraryDirectory,
			DeviceName);
	(void) fclose(installFile);
	(void) chmod(INSTALLNAME, 0755); }


/*
 * write a file that rm's all the files created by this program
 */

private
WriteGenericCleanUpFile(configChain)
    struct FontConfig *configChain; {
	FILE *cleanupFile;
	struct FontConfig *p;

	if( (cleanupFile = fopen(CLEANUPNAME, "w")) == NULL ) {
	    fprintf(stderr, "can't open the file 'cleanup' for writing\n");
	    return; }

	fprintf(cleanupFile, "#! /bin/sh\n");

	for( p = configChain; p != NULL; p = p->Next )
		if( p->SeenFlag )
	    		fprintf(cleanupFile, "rm %s.generic\n", p->TargetName);

	fprintf(cleanupFile, "rm %s\n", CLEANUPNAME);
	fprintf(cleanupFile, "rm %s\n", INSTALLNAME);
	(void) fclose(cleanupFile);
	(void) chmod(CLEANUPNAME, 0755); }


PerGenericFont(configChain, fontDescVec)
struct FontConfig *configChain;
unsigned char *fontDescVec; {
	unsigned char *charMetricsProperty,
		      *width,
		      **array;
	char *fontName[40],
	     metricFileName[40];
	FILE *descFile;
	struct FontConfig *p;
	int n,
	    depth;
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

		(void) sprintf(metricFileName, "%s.generic", p->TargetName);

		if( (descFile = fopen(metricFileName, "w")) == NULL ) {
			printf("ipmetrics: can't open %s for writing\n", metricFileName);
			return;}
	
		p->SeenFlag = TRUE;
	
		fprintf(descFile, "# %s/%s/%s for Interpress device %s\n", p->FontPt1, p->FontPt2, p->FontPt3, DeviceName);

		fprintf(descFile, "# for interpress device '%s'\n", DeviceName);
		fprintf(descFile, "#\n");
		fprintf(descFile, "# character\n");
		fprintf(descFile, "#   index\twidth\n");
		fprintf(descFile, "# (decimal)\t(ems)\n");

		if( gettype(charMetricsProperty) != type_vector ) {
			printf("ipmetrics: characterMetrics not a vector\n");
			return;}

		if( getsubtype(charMetricsProperty) != subtype_general ) {
			printf("ipmetrics: characterMetrics subtype != general\n");
			return;}

		if( (depth = getdepth(charMetricsProperty)) & 01 ) {
			printf("ipmetrics: characterMetrics vector is odd length\n");
			return;}

		array = getvector(charMetricsProperty);

		for (n = 0; n < depth; n += 2 ) {
			if( ! checktype(array[n], type_number, subtype_integer)) {
				printf("ipmetrics: property of incorrect type\n");
				return;}

			if( (width = GetStringProp("widthX", array[n+1])) == NULL ){
				printf("ipmetrics: can't find widthX property");
				continue;}

			if( gettype(width) != type_number ) {
				printf("width not of type number\n");
				continue;}

			if( getsubtype(width) != subtype_rational ) {
				printf("ipmetrics: width not of type rational\n");
				continue;}

			xWidth = ((float) getnumerator(width)) / ((float) getdenominator(width));
			fprintf(descFile, "%8d\t%7.5f\n", getint(array[n]), xWidth);
		}

		free((char *) array);


		(void) fclose(descFile); }
}
