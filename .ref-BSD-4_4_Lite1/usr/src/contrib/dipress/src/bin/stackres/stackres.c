/* stackres.c
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * Module: stackres
 * Owner: knox
 * stdout: text description
 * args:
 *   name         (name of the input res file)
 *
 * Description:
 *    This program reads an RES file, executes it and produces a
 *    text description file.  The name of the RES file is the
 *    the first argument of the command line.  The text description
 *    written to the standard output describes the results left
 *    on the stack after the image has been executed.
 *
 *    The image raster will be read from the file "name.res",
 *    where name is read from the command line.  The ".res"
 *    extension will not be added if it is already present in the
 *    name.
 *
 * HISTORY
 *
 *
 */

#include "stack.h"
#include <stdio.h>
#include <strings.h>

extern char *malloc();

#define err0 "readres: No input RES file name!\n"
#define err1 "readres: RES file could not be found, %s!\n"

FILE *fpin;
int fdout;

main(argc, argv)
  int argc;
  char *argv[];
  {
  getargs(argc, argv);
  parse(fpin);
  writedata();
  exit(0);
  }

getargs(argc, argv)
  int argc;
  char *argv[];
  {
  char *filename;

  if (argc < 2) {
	fprintf(stderr, err0);
	exit(2);
  }
 
  /* Open input RES file. */
  fdout = 1;
  filename = (char *) malloc((unsigned) strlen(argv[1])+1+strlen(".res"));
  (void) strcpy(filename, argv[1]);

  if (strcmp(".res", rindex(filename, '.')) != 0)
	(void) strcat(filename, ".res");

  fpin = fopen(filename, "r");

  if (fpin == NULL) {
	fprintf(stderr, err1, filename);
	exit(2);
  }
}



writedata()
  {
  /* remove everything from the stack. */
  int n;
  unsigned char *ptr;
  n = 0;
  while (!stackempty())
    {
    n++;
    ptr = pop(0);
    printitem(ptr, n);
    printf("\n");
    free((char *) ptr);
    }
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
    case type_color:           printcolor(ptr);            break;
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

  for (n=0; n < depth; n++)
	printitem(array[n], n);

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

  for (n=0; n < depth; n++)
	printitem(array[n], n);

  free((char *) array);
  }

printcolor(ptr)
  unsigned char *ptr;
  {
  printf("Type: color\n");
  switch (getsubtype(ptr))
    {
    case subtype_value:      printcol(ptr, "value");         break;
    case subtype_name:       printcol(ptr, "name");          break;
    case subtype_operator:   printcol(ptr, "operator");      break;
    default:                 printf("Subtype: unknown\n");   break;
    }
  }

printcol(ptr, string)
  unsigned char *ptr;
  char *string;
  {
  int n, depth;
  unsigned char **array;
  depth = getdepth(ptr);
  printf("Subtype: %s\n", string);
  printf("Depth: %d\n", depth);
  array = getcolor(ptr);
  for (n=0; n < depth; n++) printitem(array[n], n);
  free((char *) array);
  }

printpixelarray(ptr)
  unsigned char *ptr;
  {
  int n, depth;
  unsigned char **array;
  depth = getdepth(ptr);
  printf("Type: pixelarray\n");
  printf("Depth: %d\n", depth);
  array = getcolor(ptr);

  for (n=0; n < depth; n++)
	printitem(array[n], n);

  free((char *) array);
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

/* Change Log
 *
 * K. Knox,   28-Mar-85 15:04:13, Created first version.
 *
 *
 *
 */
 
 
 
 
