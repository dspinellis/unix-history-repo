/*
 *
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 * Module: ipmetrics
 * Owner: Moore
 * args:
 *   name         (name of the input metrics file)
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
 * 04-Feb-86  lee at Xerox, WRC
 *	Added the ability to execute several masters instead of just one.
 *
 * 06-Jan-86  Lee Moore(lee) at Xerox, WRC
 *	Converted for use with ipmetrics.
 *
 *
 * K. Knox,   28-Mar-85 15:04:13, Created first version.
 *
 */

#include <stdio.h>
#include <strings.h>
#include "stack.h"
#include "token.h"
#include "config.h"
#include "ipmetrics.h"

#define TRUE	1
#define FALSE	0

enum TypesettingSystems { none, Troff, TeX};

extern char *getstring();
extern unsigned char *malloc();

extern unsigned char **getvector();

struct FontConfig *ReadConfigFile();
enum TypesettingSystems getargs();

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
	enum TypesettingSystems system;
	struct FontConfig *configChain;
  
	system = getargs(argc, argv);
	printf("reading description file... "); fflush(stdout);
  	configChain = ReadConfigFile();
  	printf("writing font files... "); fflush(stdout);
	InitFontFiles(system, configChain);
 	WriteEachFont(system, configChain);
	CleanUpFontFiles(system, configChain);
  	printf("done.\n");
  	exit(0);
  }

InitFontFiles(system, configChain)
    enum TypesettingSystems system;
    struct FontConfig *configChain; {
	switch( system ) {
		case Troff:
			InitTroff();
			break;

		case TeX:
			break;

		default:
			printf("ipmetrics: internal error\n");
	}
}

CleanUpFontFiles(system, configChain)
    enum TypesettingSystems system;
    struct FontConfig *configChain; {
	switch( system ) {
		case Troff:
			CleanUpTroff(configChain);
			break;

		case TeX:
			CleanUpTeX(configChain);
			break;

		default:
			printf("ipmetrics: internal error\n");
	}
}


enum TypesettingSystems
getargs(argc, argv)
  int argc;
  char *argv[];
  {
  char *filename,
        c;
  extern int optind;
  extern char *optarg;
  enum TypesettingSystems system = Troff;

   while ((c = getopt(argc, argv, "d:tT")) != EOF)
	switch (c) {
	    case 'd':
		strncpy(LibraryDirStorage, optarg, sizeof(LibraryDirStorage));
		LibraryDirectory = LibraryDirStorage;
		break;

	    case 't':
		system = Troff;
		break;

	    case 'T':
		system = TeX;
		break;

	    default:
		printf("ipmetrics: option '%c' not allowed\n");
	}

  if (argc == optind) {	/* at least one argument */
	error(err0);
	exit(1);
  }

  for( ; optind < argc ; optind++ ) {
	/* Open input IP file. */
	fdout = 1;
	filename = (char *) malloc((unsigned) strlen(argv[optind])+1+strlen(".ip"));
	strcpy(filename, argv[optind]);

	if (strcmp(".ip", rindex(filename, '.')) != 0)
		strcat(filename, ".ip");

	fpin = fopen(filename, "r");

	if (fpin == NULL)
		error(err1, filename);

	printf("executing %s... ", filename); fflush(stdout);
  	parse(fpin);
  }

  return system;
  }

/*
 * read the font configuration file
 */
struct FontConfig *
ReadConfigFile()  {
  int n;
	char token[MAXTOKENSIZE];
	struct TokenState *ts;
	struct FontConfig **last,
			   *p,
			   *configChain;

	last = &configChain;
	ts = InitTokenStream(stdin);
	GetToken(ts, token, MAXTOKENSIZE);

	if( strcmp(token, "device") != 0 ) {
		fprintf(stderr, "first token in %s, not 'device'\n", token);
		exit(1); }

	GetToken(ts, DeviceName, MAXTOKENSIZE);
	printf(" device is %s\n", DeviceName);

	while( !EndOfFile(ts) ) {
		p = (struct FontConfig *) malloc((unsigned) sizeof(struct FontConfig));

		GetToken(ts, p->FontPt1, MAXTOKENSIZE);
		GetToken(ts, p->FontPt2, MAXTOKENSIZE);
		GetToken(ts, p->FontPt3, MAXTOKENSIZE);
		GetToken(ts, p->TroffName, MAXTOKENSIZE);

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

WriteEachFont(system, configChain)
    enum TypesettingSystems system;
    struct FontConfig *configChain;
{
	unsigned char *fontVec;
	int tableIndex;

	tableIndex = 0;

	while (!stackempty()) {
   		fontVec = pop(0, 0);

		switch( system ) {
			case Troff:
				PerTroffFont(configChain, fontVec);
				tableIndex++; /* necessary? */
				break;

			case TeX:
				PerTeXFont(configChain, fontVec);
				break;

			default:
				printf("ipmetrics: internal error\n");
		}

    		free((char *) fontVec); }
}


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


unsigned char *
GetStringProp(propName, list)
     char *propName;
     unsigned char *list; {
	int i,
	    vecLength;
	char *candidate;
	unsigned char **listArray;

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
		if( ! checktype(listArray[i], type_string, subtype_identifier) ) {
			printf("ipmetrics: property of incorrect type\n");
			return NULL;}

		candidate = getstring(listArray[i], subtype_identifier);

	        if( strcmp(propName, candidate) == 0 )
			return listArray[i+1];
	}
	free((char *) listArray);
	return NULL;
     }


unsigned char *
GetIntegerProp(property, list)
     int property;
     unsigned char *list; {
	int i,
	    vecLength;
	int candidate;
	unsigned char **listArray;

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
			return NULL;}

		candidate = getint(listArray[i], subtype_identifier);

	        if( property == candidate )
			return listArray[i+1];
	}

	free((char *) listArray);
	return NULL;
     }



printitem(ptr, element)
  unsigned char *ptr;
  int element;
  {
  printf("Element: %d\n", element);
  printf("Length: %d\n", getlength(ptr));
  switch (gettype(ptr))
    {
    case type_number:          printnumber(ptr);           break;
    case type_string:          printstring(ptr);           break;
    case type_vector:          printvector(ptr);           break;
    case type_operator:        printoperator(ptr);         break;
    case type_pixelarray:      printpixelarray(ptr);       break;
    case type_transformation:  printtransformation(ptr);   break;
    case type_integers:        printintegers(ptr);         break;
    default:                   printf("Type: unknown\n");  break;
    }
  }

printnumber(ptr)
  unsigned char *ptr;
  {
  printf("Type: number\n");
  switch (getsubtype(ptr))
    {
    case subtype_integer:    printinteger(ptr);            break;
    case subtype_rational:   printrational(ptr);           break;
    default:                 printf("Subtype: unknown\n"); break;
    }
  }

printinteger(ptr)
  unsigned char *ptr;
  {
  printf("Subtype: integer\n");
  printf("Value: %d\n", getint(ptr));
  }

printrational(ptr)
  unsigned char *ptr;
  {
  printf("Subtype: rational\n");
  printf("Value: %f/%f\n", getnumerator(ptr), getdenominator(ptr));
  }

printstring(ptr)
  unsigned char *ptr;
  {
  printf("Type: string\n");
  switch (getsubtype(ptr))
    {
    case subtype_identifier:  printidentifier(ptr);          break;
    case subtype_string:      printsubstring(ptr);           break;
    default:                  printf("Subtype: unknown\n");  break;
    }
  }

printidentifier(ptr)
  unsigned char *ptr;
  {
  printf("Subtype: identifier\n");
  printf("Identifier: %s\n", getstring(ptr, subtype_identifier));
  }

printsubstring(ptr)
  unsigned char *ptr;
  {
  printf("Subtype: string\n");
  printf("String: %s\n", getstring(ptr, subtype_string));
  }

printvector(ptr)
  unsigned char *ptr;
  {
  printf("Type: vector\n");
  switch (getsubtype(ptr))
    {
    case subtype_general:   printvec(ptr, "general");        break;
    case subtype_integers:  printvec(ptr, "integers");       break;
    case subtype_samples:   printvec(ptr, "samples");        break;
    default:                printf("Subtype: unknown\n");    break;
    }
  }

printvec(ptr, string)
  unsigned char *ptr;
  char *string;
  {
  int n, depth;
  unsigned char **array;

  depth = getdepth(ptr);
  printf("Subtype: %s\n", string);
  printf("Depth: %d\n", depth);
  array = getvector(ptr);

  for (n=0; n < depth; n++) printitem(array[n], n);

  free((char *) array);
  }

printoperator(ptr)
  unsigned char *ptr;
  {
  printf("Type: operator\n");
  switch (getsubtype(ptr))
    {
    case subtype_decompressop:   printop(ptr, "decompressop");     break;
    case subtype_colorop:        printop(ptr, "colorop");          break;
    case subtype_colormodelop:   printop(ptr, "colormodelop");     break;
    default:                     printf("Subtype: unknown\n");     break;
    }
  }

printop(ptr, string)
  unsigned char *ptr;
  char *string;
  {
  int n, depth;
  unsigned char **array;

  depth = getdepth(ptr);
  printf("Subtype: %s\n", string);
  printf("Depth: %d\n", depth);
  array = getoperator(ptr);

  for (n=0; n < depth; n++) printitem(array[n], n);

  free((char *) array);
  }

printpixelarray(ptr)
  unsigned char *ptr;
  {
  }

printtransformation(ptr)
  unsigned char *ptr;
  {
  double *array;

  array = gettransformation(ptr);
  printf("Type: transformation\n");
  printf("A: %f\n", array[0]);
  printf("B: %f\n", array[1]);
  printf("C: %f\n", array[2]);
  printf("D: %f\n", array[3]);
  printf("E: %f\n", array[4]);
  printf("F: %f\n", array[5]);
  free((char *) array);
  }

printintegers(ptr)
  unsigned char *ptr;
  {
  printf("Type: integers\n");
  printf("Bytes/Integer: %d\n", getbytesPerInteger(ptr));
  printf("Bytepos: %ld\n", getbytepos(ptr));
  printf("ByteLength: %ld\n", getbytelength(ptr));
  }
