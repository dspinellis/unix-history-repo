/*
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 * define a font configuration record
 */

#define MAXTOKENSIZE 40

struct FontConfig {
	char FontPt1[MAXTOKENSIZE];
	char FontPt2[MAXTOKENSIZE];
	char FontPt3[MAXTOKENSIZE];
	char TargetName[MAXTOKENSIZE];
	char MapFile[MAXTOKENSIZE];
	struct FontConfig *Next;
	int SeenFlag;			/* was this font actually there? */
};
