/*
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * Owner: Lee Moore
 *
 * Description:
 *    This program reads an Interpress file, executes it and produces a
 *    set of Troff printer description files.  
 *
 *    The metrics file will be read from the file "name.ip",
 *    where name is read from the command line.  The ".ip"
 *    extension will not be added if it is already present in the
 *    name.
 *
 * HISTORY
 * 18-Aug-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Moved typesetter specific routine calls to the file "conf.c".
 *
 * 15-Apr-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added table of contents mode (toc).
 *
 * 04-Feb-86  lee at Xerox, WRC
 *	Added the ability to execute several masters instead of just one.
 *
 * 06-Jan-86  Lee Moore(lee) at Xerox, WRC
 *	Converted for use with ipmetrics.
 */

#include <stdio.h>
#include <strings.h>
#include "stack.h"
#include "token.h"
#include "config.h"
#include "ipmetrics.h"
#include "conf.h"

#define TRUE	1
#define FALSE	0

#define SAME	0

extern struct CompositionSwitch CompositionSwitch[];

extern char *getstring();
extern char *malloc();

extern unsigned char **getvector();

struct FontConfig *ReadConfigFile();

char *LibraryDirectory = "../lib",
      LibraryDirStorage[1024];


#define err0 "ipmetrics: No input ip file name!\n"
#define err1 "ipmetrics: ip file could not be found, %s!\n"

FILE *fpin;
int fdout;
char DeviceName1[MAXTOKENSIZE];
char *DeviceName = DeviceName1;


main(argc, argv)
  int argc;
  char *argv[];
  {
	int system;
	struct FontConfig *configChain;
	unsigned char *fontVec;
  	struct CompositionSwitch *csp;
  
	DebugLevel = 0;
	system = getargs(argc, argv);

	csp = &CompositionSwitch[system];

	if( csp->cs_readConfigFile )
	  	configChain = ReadConfigFile();

	fprintf(stderr, "output initialization...\n");
	(* csp->cs_initializationProc)(configChain);

	/* call the metrics vector processing routine for each font vector */
	while (!stackempty()) {
   		fontVec = pop(0, 0);
		(* csp->cs_fontProc)(configChain, fontVec);
    		free((char *) fontVec); }   /* should do a recursive free? */

	fprintf(stderr, "cleaning-up...\n");
	(* csp->cs_cleanUpProc)(configChain);
	fprintf(stderr, "done.\n");
  	exit(0);
  }



/*
 * process the command line arguments
 */

getargs(argc, argv)
  int argc;
  char *argv[];
  {
  char *filename;
  extern int optind;
  extern char *optarg;
  int system,
	c;
  char systemName[30];
  struct CompositionSwitch *csp;

  (void) strcpy(systemName, "troff");

  while ((c = getopt(argc, argv, "c:d:")) != EOF)
	switch (c) {
	    case 'd':
		(void) strncpy(LibraryDirStorage, optarg, sizeof(LibraryDirStorage));
		LibraryDirectory = LibraryDirStorage;
		break;

	    case 'c':
		(void) strncpy(systemName, optarg, sizeof(systemName));
		break;

	    default:
		printf("ipmetrics: option '%c' not allowed\n");
	}

  /* look-up system name */
  for(csp = CompositionSwitch, system = 0; csp->cs_systemName != NULL; csp++, system++)
  	if( strncmp(csp->cs_systemName, systemName, sizeof(systemName)) == SAME)
	    break;

  if( csp->cs_systemName == NULL) {
	printf("unknown system: %s\n", systemName);
	exit(1); }

  if (argc == optind) {	/* at least one argument */
	error(err0);
	exit(1);
  }
  printf("executing...\n");

  for( ; optind < argc ; optind++ ) {
	/* Open input IP file. */
	fdout = 1;
	filename = (char *) malloc((unsigned) strlen(argv[optind])+1+strlen(".ip"));
	(void) strcpy(filename, argv[optind]);

	if (strcmp(".ip", rindex(filename, '.')) != 0)
		(void) strcat(filename, ".ip");

	fpin = fopen(filename, "r");

	if (fpin == NULL) {
		fprintf(stderr, err1, filename);
		exit(2);
	}

	printf("\t%s\n", filename); (void) fflush(stdout);
  	parse(fpin);
  }

  return system;
  }



/*
 * read the font configuration file
 */
struct FontConfig *
ReadConfigFile() {
	char token[MAXTOKENSIZE];
	struct TokenState *ts;
	struct FontConfig **last,
			   *p,
			   *configChain;

	printf("reading configuration file\n");
	last = &configChain;
	ts = InitTokenStream(stdin);
	GetToken(ts, token, MAXTOKENSIZE);

	if( strcmp(token, "device") != 0 ) {
		fprintf(stderr, "first token is %s, not 'device'\n", token);
		exit(1); }

	GetToken(ts, DeviceName, MAXTOKENSIZE);
	printf("\tdevice is %s\n", DeviceName);

	while( !EndOfFile(ts) ) {
		p = (struct FontConfig *)
			malloc((unsigned) sizeof(struct FontConfig));

		GetToken(ts, p->FontPt1, MAXTOKENSIZE);

		if( EndOfLine(ts) ) {
			fprintf(stderr,
				"lines ends prematurely; last token was `%s'\n",
				p->FontPt1);
			exit(2);}

		GetToken(ts, p->FontPt2, MAXTOKENSIZE);

		if( EndOfLine(ts) ) {
			fprintf(stderr,
				"lines ends prematurely; last token was `%s'\n",
				p->FontPt2);
			exit(2);}

		GetToken(ts, p->FontPt3, MAXTOKENSIZE);

		if( EndOfLine(ts) ) {
			fprintf(stderr,
				"lines ends prematurely; last token was `%s'\n",
				p->FontPt3);
			exit(2);}

		GetToken(ts, p->TargetName, MAXTOKENSIZE);

		if( !EndOfLine(ts) )
			GetToken(ts, p->MapFile, MAXTOKENSIZE);
		else
			p->MapFile[0] = '\0';

		p->Next = NULL;
		*last = p;
		last = &p->Next;
	}

	return configChain;
  }




/*
 * get the font name from a font vector
 */

GetFontNameProperty(fontDescVec, CName)
   unsigned char *fontDescVec;
   char *CName[3]; {
	unsigned char *nameProperty,
		     **nameVec;
	int i;

	if( (nameProperty = GetStringProp("name", fontDescVec)) == NULL ) {
		printf("ipmetrics: can't find 'name' property\n");
		return FALSE;
	}

	nameVec = getvector(nameProperty);

	/* loop for each part of the three part name */
	for( i = 0; i < 3; i++ ) {
		if( gettype(nameVec[i]) != type_string ) {
			printf("name vector not of type string\n");
			free((char *) nameVec);
			return FALSE;
		}

		if( getsubtype(nameVec[i]) != subtype_identifier ) {
			printf("name subtype not an identifier\n");
			free((char *) nameVec);
			return FALSE;
		}

		CName[i] = getstring(nameVec[i], subtype_identifier);
	}

	free((char *) nameVec);
	return TRUE;
   }


/*
 * get a property off the property-vector whose type is "string"
 */

unsigned char *
GetStringProp(propName, list)
     char *propName;
     unsigned char *list; {
	int i,
	    vecLength;
	char *candidate;
	unsigned char **listArray,
			*result;

	if( gettype(list) != type_vector ) {
		printf("ipmetric: non-vector found in stack!\n");
		return NULL;}

	if( getsubtype(list) != subtype_general ) {
		printf("ipmetric: vector sub-type is not 'general'\n");
		return NULL; }

	if( (vecLength = getdepth(list)) & 01 ) {
		printf("ipmetrics: property vector is of odd length\n");
		return NULL;}

	listArray = getvector(list);

	/* cdr down the list */
	for( i = 0; i < vecLength; i += 2 ) {
		if( ! checktype(listArray[i], type_string, subtype_identifier) ) {
			printf("ipmetrics: property of incorrect type\n");
			free((char *) listArray);
			return NULL;}

		candidate = getstring(listArray[i], subtype_identifier);

	        if( strcmp(propName, candidate) == 0 ) {
			result = listArray[i+1];
			free((char *) listArray);
			return result;
		}
	}

	free((char *) listArray);
	return NULL;
     }


/*
 * get a property from the property-vector that is type "integer"
 */

unsigned char *
GetIntegerProp(property, list)
     int property;
     unsigned char *list; {
	int i,
	    vecLength;
	int candidate;
	unsigned char **listArray,
			*result;

	if( gettype(list) != type_vector ) {
		printf("ipmetric: non-vector found in stack!\n");
		return NULL;}

	if( getsubtype(list) != subtype_general ) {
		printf("ipmetric: vector sub-type is not 'general'\n");
		return NULL; }

	if( (vecLength = getdepth(list)) & 01 ) {
		printf("ipmetrics: property vector is of odd length\n");
		return NULL;}

	listArray = getvector(list);

	for( i = 0; i < vecLength; i += 2 ) {
		if( ! checktype(listArray[i], type_number, subtype_integer) ) {
			printf("ipmetrics: property of incorrect type\n");
			free((char *) listArray);
			return NULL;}

		candidate = getint(listArray[i]);

	        if( property == candidate ) {
			result = listArray[i+1];
			free((char *) listArray);
			return result;
		}
	}

	free((char *) listArray);
	return NULL;
     }
