/*
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * Handle troff metrics here.
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


/* the following value should be choosen so that none of the widths
 * is greater than 256
 */
#define UNITWIDTH	5	/* was 10 */


#define MAXSPECIALNAMES	221	/* maximum number of special characters.
				 * this constant is fixed in Troff
 				 */

#define public
#define private	static

public char *malloc();

public char *DeviceName,
	    *LibraryDirectory;

struct CharElement {
	struct CharElement *Next;
	char CharName[3]; };

private struct CharElement *CharSet = NULL;

private int SetOfPointSizes[MAXPOINTSIZE],
	    FontCount = 0;


/*
 * Clean up our state
 */
public
CleanUpTroff(configChain)
    struct FontConfig *configChain; {
	struct FontConfig *p;

	WriteDescFile(configChain);
	WriteFontMapFile(configChain);
	WriteInstallFile(configChain);
	WriteCleanUpFile(configChain);

	/* if we haven't seen a font, say so! */
	for( p = configChain; p != NULL; p = p->Next )
		if( !p->SeenFlag )
			printf("couldn't find: %s/%s/%s\n",
				p->FontPt1, p->FontPt2, p->FontPt3);
}


/*
 * Write the DESC file
 */
private
WriteDescFile(configChain)
    struct FontConfig *configChain; {
	int i;
	FILE *deviceFile;
	struct FontConfig *p;

	if( (deviceFile = fopen("DESC", "w")) == NULL ) {
	    printf("can't open 'DESC' for writing\n");
	    exit(1); }

	/* output boiler plate */
	fprintf(deviceFile, "# describe the '%s' interpress device\n", DeviceName);
	fprintf(deviceFile, "res %d\n", MICAS_PER_INCH);
	fprintf(deviceFile, "hor 1\n");
	fprintf(deviceFile, "vert 1\n");
	fprintf(deviceFile, "unitwidth %d\n", UNITWIDTH);
	fprintf(deviceFile, "paperwidth %d\n", PAGE_WIDTH_IN_MICAS);
	fprintf(deviceFile, "paperlength %d\n", PAGE_HEIGHT_IN_MICAS);

	/* output sizes */
	fprintf(deviceFile, "sizes ");

	for( i = 1; i < MAXPOINTSIZE; i++ )
	    if( SetOfPointSizes[i] )
		fprintf(deviceFile," %d", i);

	fprintf(deviceFile, " 0\n");

	/* output fonts */
	fprintf(deviceFile, "fonts %d ", FontCount);

	for( p = configChain; p != NULL; p = p->Next )
		if( p->SeenFlag )
			fprintf(deviceFile, "%s ", p->TargetName);

	fprintf(deviceFile, "\n");
	PrintCharSet(deviceFile);
	(void) fclose(deviceFile); }



/*
 * Write the file that shows the relationship between Troff names and
 * Interpress names
 */
private
WriteFontMapFile(configChain)
    struct FontConfig *configChain; {
	FILE *fontMapFile;
	struct FontConfig *p;

	if( (fontMapFile = fopen(FONTMAPFILENAME, "w")) == NULL ) {
	    fprintf(stderr, "can't open the file '%s' for writing\n", FONTMAPFILENAME);
	    return; }

	for( p = configChain; p != NULL; p = p->Next )
		if( p->SeenFlag )
	    		fprintf(fontMapFile, "%s %s/%s/%s\n", p->TargetName,
				p->FontPt1, p->FontPt2, p->FontPt3);

	(void) fclose(fontMapFile);
	(void) chmod(FONTMAPFILENAME, 0755); }


/*
 * Create a shell script that will install all these things
 */
private
WriteInstallFile(configChain)
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
	fprintf(installFile, "if test ! -d %s/fonts/%s/devipress\n", LibraryDirectory, DeviceName);
	fprintf(installFile, "  then\n");
	fprintf(installFile, "    mkdir %s/fonts/%s/devipress\n", LibraryDirectory, DeviceName);
	fprintf(installFile, "  fi\n");


	for( p = configChain; p != NULL; p = p->Next )
		if( p->SeenFlag )
	    		fprintf(installFile, "cp %s %s/fonts/%s/devipress\n",
				p->TargetName, LibraryDirectory, DeviceName);

	fprintf(installFile, "cp DESC %s/fonts/%s/devipress\n", LibraryDirectory,
			DeviceName);
	fprintf(installFile, "cp %s %s/fonts/%s/devipress\n", FONTMAPFILENAME,
			LibraryDirectory, DeviceName);
	fprintf(installFile, "cd %s/fonts/%s/devipress\n", LibraryDirectory,
			DeviceName);
	(void) fprintf(installFile, "makedev DESC\n");
	(void) fprintf(installFile, "makextdev DESC\n");
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

	fprintf(cleanupFile, "rm DESC\n");
	fprintf(cleanupFile, "rm %s\n", INSTALLNAME);
	fprintf(cleanupFile, "rm %s\n", FONTMAPFILENAME);
	fprintf(cleanupFile, "rm %s\n", CLEANUPNAME);
	(void) fclose(cleanupFile);
	(void) chmod(CLEANUPNAME, 0755); }

/*
 * called once per font on the stack
 */

public
PerTroffFont(configChain, fontDescVec)
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
	
		printf("\t%s\n", p->TargetName);

		if( (modelFile = fopen(p->MapFile, "r")) == NULL ) {
			printf("ipmetrics: can't open %s for reading\n", p->MapFile);
			return;}

		p->SeenFlag = TRUE;
		FontCount++;
	
		ts = InitTokenStream(modelFile);
	
		fprintf(descFile, "#\n");
		fprintf(descFile, "# %s/%s/%s for Interpress device %s\n", p->FontPt1, p->FontPt2, p->FontPt3, DeviceName);
		fprintf(descFile, "name %s\n", p->TargetName);
		fprintf(descFile, "internalname %d\n", FontCount);
	
		GetToken(ts, fileType, MAXTOKENSIZE);
	
		if( strcmp(fileType, "special") == 0 )
			fprintf(descFile, "special\n");
		else
			ProcessTroffLigatures(charMetricsProperty, descFile);
	
		fprintf(descFile, "charset\n");
	
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
	
			/* skip the rest of this loop if it's not in this font */
			if( (charMetric =
			    GetIntegerProp(charIndex, charMetricsProperty)) == NULL ) {
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
				printf("ipmetrics: width not of subtype rational for %d\n",
					charIndex);
				EatRestOfLine(ts);
				continue;}
	
			xWidth = (getnumerator(width)*UNITWIDTH*MICAS_PER_INCH +
				 (getdenominator(width) * POINTS_PER_INCH)/2)/
				 (getdenominator(width) * POINTS_PER_INCH);
	
			if( xWidth >= 256 )
				printf("ipmetrics: warning width >= 256\n");
	
			fprintf(descFile, "%s\t%d\t%s\t", iCharName, xWidth,
				iSender);
	
			if( charIndex < 0377 )
				fprintf(descFile, "%d\n", charIndex);
			else
				fprintf(descFile, "0377\t0%o\n", charIndex);
	
			CheckForSpecialness(iCharName);
	
			while( !EndOfLine(ts) ) {
				GetToken(ts, iCharName, MAXTOKENSIZE);
				fprintf(descFile, "%s\t\"\n", iCharName);
				CheckForSpecialness(iCharName);}}
	
		CloseTokenStream(ts);
		(void) fclose(descFile);
		(void) fclose(modelFile);
	}

	if( (metricsProperty = GetStringProp("metrics", fontDescVec))
			!= NULL ) {
		unsigned char *easyProperty;

		if( (easyProperty = GetStringProp("easy", metricsProperty))
				!= NULL ) 
			ProcessEasy(easyProperty);
	}
}


/*
 * read the rest of the tokens off the current line
 */
EatRestOfLine(ts)
struct TokenState *ts; {
	char byteBucket[MAXTOKENSIZE];

	while( !EndOfLine(ts) ) 
		GetToken(ts, byteBucket, MAXTOKENSIZE); }

/*
 * assume that the font is in XC1-1-1 standard and find the ligatures
 * that troff wants
 */

private
ProcessTroffLigatures(charMetricsVec, descFile)
    unsigned char *charMetricsVec;
    FILE *descFile; {
	char ligatureNames[21];

	(void) strcpy(ligatureNames, "");

	if( GetIntegerProp(Make16BitChar(0360, 044), charMetricsVec) != NULL )
	    (void) strcat(ligatureNames, " fi");

	if( GetIntegerProp(Make16BitChar(0360, 045), charMetricsVec) != NULL )
	    (void) strcat(ligatureNames, " fl");

	if( GetIntegerProp(Make16BitChar(0360, 041), charMetricsVec) != NULL )
	    (void) strcat(ligatureNames, " ff");

	if( GetIntegerProp(Make16BitChar(0360, 042), charMetricsVec) != NULL )
	    (void) strcat(ligatureNames, " ffi");

	if( GetIntegerProp(Make16BitChar(0360, 043), charMetricsVec) != NULL )
	    (void) strcat(ligatureNames, " ffl");

	if( strcmp(ligatureNames, "") != 0 )
		fprintf(descFile, "ligatures %s 0\n", ligatureNames);}

/*
 * Check to see if a character is special and add it to the "charset"
 * in the DESC file if it is.
 */

private
CheckForSpecialness(s)
    char *s; {
	/* right now, if it two characters long, then it must be special */
	if( strlen(s) == 2 )
	    AddToCharSet(s); }

/*
 * add a special character to the set of special characters.  The set
 * is implemented as a linked list.
 */

private
AddToCharSet(s)
    char *s; {
	struct CharElement **p,
			    *q;

	p = &CharSet;

	while( *p != NULL ) {
	    if( strcmp(s, (*p)->CharName) == 0 )
		return;

	    p = &(*p)->Next; }

	q = (struct CharElement *) malloc((unsigned) sizeof(struct CharElement));
	(void) strcpy(q->CharName, s);
	q->Next = NULL;
	*p = q; }

/*
 * print out the list of special characters to the DESC file
 */
	
private
PrintCharSet(file)
    FILE *file; {
	int itemsPerLine,
	    total;		/* total number of special chars */
	struct CharElement *p;

	/* test to see if there is a char. set.  ditroff requires this!
         * you just can't have a null charset! */
	if( CharSet == NULL )
	    return;

	fprintf(file, "charset\n");
	itemsPerLine = 0;
	total = 0;

	for( p = CharSet; p != NULL; p = p->Next ) {
	    fprintf(file, " %s", p->CharName);
	    total++;

	    if( itemsPerLine++ > 20 ) {
		(void) fputc('\n', file);
		itemsPerLine = 0; } }

	if( itemsPerLine != 0 )
	    (void) fputc('\n', file);

	printf("\tthere were %d special characters (out of a max of 221)\n", total); }

/*
 * Process the "easy" property of the "metrics" property.  This
 * will tell us what (viewing) sizes the font is available in.
 */

private
ProcessEasy(easyProperty)
unsigned char *easyProperty; {
	int	depth,
		i;
	unsigned char **array;
	
	if( gettype(easyProperty) != type_vector  ||
	    getsubtype(easyProperty) != subtype_general ) {
		printf("ipmetrics: wrong vector type in 'easy'\n");
		return; }

	depth = getdepth(easyProperty);
	array = getvector(easyProperty);

	for( i = 0; i < depth; i++ ) {
		double *transform,
			fPointSize;
		int iPointSize;

		if( gettype(array[i]) != type_transformation ) {
			printf("ipmetrics: transforms not found in 'easy'\n");
			return; }

		transform = gettransformation(array[i]);

		if( transform[0] != transform[4] ) {
			printf("ipmetrics: only square transforms in 'easy'\n");
			return; }

		if( transform[1] != 0  ||  transform[2] != 0  ||
		    transform[3] != 0  ||  transform[5] != 0  ) {
			printf("ipmetrics: troff doesn't support rotations\n");
			return; }

		fPointSize = transform[0]*72*100000/2540;
		iPointSize = fPointSize + 0.5;

		if( fabs(fPointSize - iPointSize) > .25 ) {
			printf("ipmetrics: troff doesn't support fractional points: %f6.2\n", fPointSize);
			return; }

		SetOfPointSizes[iPointSize] = 1;
		free((char *) transform);
	}

	free((char *) array);
}
