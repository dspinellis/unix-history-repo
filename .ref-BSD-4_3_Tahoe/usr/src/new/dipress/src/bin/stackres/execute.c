/* execute.c
 *
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 *  Define the functions used in parse.c.
 *
 * Execute the RES file and leave the correct parameters on the stack.
 *
 */

#include <math.h>
#include <stdio.h>
#include <iptokens.h>
#include "stack.h"

#define err0 "(%d) execute: Bad RES header, got %s\n"
#define err1 "(%d) execute: roll moveFirst > depth, %d > %d!\n"
#define err2 "(%d) execute: unknown operator subtype, got %d!\n"
#define err3 "(%d) execute: unknown operator, type=%d!\n"
#define err4 "(%d) execute: unknown sequence, type=%d, length=%d!\n"
#define err5 "(%d) execute: sequenceInsertFile not implemented!\n"


/* Defined elsewhere. */
extern unsigned char **malloc();
extern long filepos;
extern FILE *fp;

/* Defined in this module. */
extern unsigned char *decompressID();
extern unsigned char *decompressOP();
extern unsigned char *imagedata();
extern unsigned char *popidentifier();
extern double popdouble();


/*
 * Public procedures defined for "parse" module.
 *
 */

header(string)
char *string;
  {
  fprintf(stderr, "strings are '%s', '%s'\n", string, RES_Header);
  if (strcmp(string, RES_Header) != 0) error(err0, filepos, string);
  }

op_makevec()
  {
  int n, depth;
  unsigned char *ptr, **array;
  depth = popint();
  array = malloc((depth+1)*sizeof(unsigned char *));   /* null terminated array */
  for (n=0; n < depth; n++) array[depth-n-1] = pop(0);
  array[depth] = (unsigned char *) 0;
  ptr = makevector(array, type_vector, subtype_general);
  for (n=0; n < depth; n++) free(array[n]);
  free(array);
  push(ptr);
  }

op_pop()
  {
  free(pop(0));
  }

op_copy()
  {
  int n, depth;
  unsigned char *ptr, **temp1, **temp2;
  depth = popint();
  temp1 = malloc(depth*sizeof(unsigned char *));
  temp2 = malloc(depth*sizeof(unsigned char *));
  for (n=0; n < depth; n++) temp1[depth-n-1] = pop(0);
  for (n=0; n < depth; n++) temp2[n] = duplicate(temp1[n]);
  for (n=0; n < depth; n++) push(temp1[n]);
  for (n=0; n < depth; n++) push(temp2[n]);
  free(temp1);
  free(temp2);
  }

op_dup()
  {
  unsigned char *ptr, *newptr;
  ptr = pop(0);
  newptr = duplicate(ptr);
  push(ptr);
  push(newptr);
  }

op_roll()
  {
  int n, depth, moveFirst;
  unsigned char *ptr, **temp;
  moveFirst = popint();
  depth = popint();
  if (moveFirst > depth) error(err1, filepos, moveFirst, depth);
  temp = malloc(depth*sizeof(unsigned char *));
  for (n=0; n < depth; n++) temp[depth-n-1] = pop(0);
  for (n=moveFirst; n < depth; n++) push(temp[n]);
  for (n=0; n < moveFirst; n++) push(temp[n]);
  free(temp);
  }

op_exch()
  {
  unsigned char *temp1, *temp2;
  temp1 = pop(0);
  temp2 = pop(0);
  push(temp1);
  push(temp2);
  }

op_nop()
  {
  }

op_translate()
  {
  double x, y;
  unsigned char *ptr;
  y = popdouble();
  x = popdouble();
  ptr = maketransformation(1.0, 0.0, x, 0.0, 1.0, y);
  push(ptr);
  }

op_rotate()
  {
  double angle, cosA, sinA, pi;
  unsigned char *ptr;
  angle = popdouble();
  angle = 3.1415926*angle/180.;
  cosA = cos(angle);
  sinA = sin(angle);
  ptr = maketransformation(cosA, -sinA, 0.0, sinA, cosA, 0.0);
  push(ptr);
  }

op_scale()
  {
  unsigned char *ptr;
  double s;
  s = popdouble();
  ptr = maketransformation(s, 0.0, 0.0, 0.0, s, 0.0);
  push(ptr);
  }

op_scale2()
  {
  unsigned char *ptr;
  double sx, sy;
  sy = popdouble();
  sx = popdouble();
  ptr = maketransformation(sx, 0.0, 0.0, 0.0, sy, 0.0);
  push(ptr);
  }

op_concat()
  {
  double a, b, c, d, e, f;
  unsigned char *ptr, *nptr, *mptr;
  double *m, *n;
  nptr = pop(type_transformation, 0);
  mptr = pop(type_transformation, 0);
  n = gettransformation(nptr);
  m = gettransformation(mptr);
  a = m[0]*n[0]+m[3]*n[1];
  b = m[1]*n[0]+m[4]*n[1];
  c = m[2]*n[0]+m[5]*n[1]+n[2];
  d = m[0]*n[3]+m[3]*n[4];
  e = m[1]*n[3]+m[4]*n[4];
  f = m[2]*n[3]+m[5]*n[4]+n[5];
  ptr = maketransformation(a, b, c, d, e, f);
  free(mptr);
  free(nptr);
  free(m);
  free(n);
  push(ptr);
  }

op_makepixelarray()
  {
  int n;
  unsigned char *ptr, **array;
  array = malloc(9*sizeof(unsigned char *));
  array[6] = pop(type_vector, 0);                 /* samples */
  array[5] = pop(type_transformation, 0);         /* m */
  array[4] = pop(type_number, 0);                 /* samplesInterleaved */
  array[3] = pop(type_number | type_vector, 0);   /* maxSampleValue */
  array[2] = pop(type_number, 0);                 /* samplesPerPixel */
  array[1] = pop(type_number, 0);                 /* yPixels */
  array[0] = pop(type_number, 0);                 /* xPixels */
  array[7] = makeselect(getint(array[2]), ~0);    /* select == all samples */
  array[8] = (unsigned char *) 0;                 /* null terminated list */
  ptr = makepixelarray(array);
  for (n=0; n < 9; n++) free(array[n]);
  free(array);
  push(ptr);
  }


op_extractpixelarray()
  {
  int depth;
  unsigned char *oldptr, *newptr, *select, **array;
  select = pop(type_vector, 0);
  oldptr = pop(type_pixelarray, 0);
  array = getpixelarray(oldptr);
  array[7] = select;
  newptr = makepixelarray(array);
  free(select);
  free(oldptr);
  free(array);
  push(newptr);
  }

op_do()
  {
  unsigned char *ptr, **array;
  int type, subtype;
  array = malloc(3*sizeof(unsigned char *));
  array[0] = pop(type_operator, 0);           /* operator to do */
  array[1] = pop(type_vector, 0);             /* vector argument */
  array[2] = (unsigned char *) 0;
  switch (getsubtype(array[0]))
    {
    case subtype_decompressop:
      type = type_vector;
      subtype = subtype_samples;
      break;
    case subtype_colorop:
      type = type_color;
      subtype = subtype_operator;
      break;
    case subtype_colormodelop:
      type = type_operator;
      subtype = subtype_colorop;
      break;
    default:
      error(err2, filepos, getsubtype(array[0]));
    }
  ptr = makevector(array, type, subtype);
  free(array[0]);
  free(array[1]);
  free(array);
  push(ptr);
  }

op_finddecompressor()
  {
  unsigned char *ptr, **array;
  array = malloc(2*sizeof(unsigned char *));
  array[0] = popidentifier("decompressionOps/");
  array[1] = (unsigned char *) 0;
  ptr = makeoperator(array, subtype_decompressop);
  free(array[0]);
  free(array);
  push(ptr);
  }

op_makegray()
  {
  unsigned char *ptr, **array;
  array = malloc(2*sizeof(unsigned char *));
  array[0] = pop(type_number);
  array[1] = (unsigned char *) 0;
  ptr = makecolor(array, subtype_value);
  push(ptr);
  }

op_findcolor()
  {
  unsigned char *ptr, **array;
  array = malloc(2*sizeof(unsigned char *));
  array[0] = popidentifier("colors/");
  array[1] = (unsigned char *) 0;
  ptr = makecolor(array, subtype_name);
  free(array[0]);
  free(array);
  push(ptr);
  }

op_findcoloroperator()
  {
  unsigned char *ptr, **array;
  array = malloc(2*sizeof(unsigned char *));
  array[0] = popidentifier("colorOps/");
  array[1] = (unsigned char *) 0;
  ptr = makeoperator(array, subtype_colorop);
  free(array[0]);
  free(array);
  push(ptr);
  }

op_findcolormodeloperator()
  {
  unsigned char *ptr, **array;
  array = malloc(2*sizeof(unsigned char *));
  array[0] = popidentifier("colorModelOps/");
  array[1] = (unsigned char *) 0;
  ptr = makeoperator(array, subtype_colormodelop);
  free(array[0]);
  free(array);
  push(ptr);
  }

op_beginblock()
  {
  }

op_endblock()
  {
  }

op_unknown(op)
  int op;
  {
  error(err3, filepos, op);
  }

seq_comment(nbytes)
  int nbytes;
  {
  fseek(fp, (long) nbytes, 1);
  }

seq_continued(nbytes, last)
  int nbytes, last;
  {
  switch (last)
    {
    case sequenceAdaptivePixelVector:
    case sequenceCompressedPixelVector:
    case sequencePackedPixelVector:
    case sequenceLargeVector:            extendpixel(nbytes);             break;
    case sequenceComment:                fseek(fp, (long) nbytes, 1);     break;
    case sequenceInteger:
    case sequenceRational:               extendnumber(nbytes);            break;
    case sequenceString:
    case sequenceIdentifier:             extendstring(nbytes);            break;
    default:                             error(err4, filepos, last, nbytes);
    }
  }

seq_insertfile(nbytes)
  int nbytes;
  {
  error(err5, filepos);
  }

seq_largevector(nbytes)
  int nbytes;
  {
  int b;
  long bytepos, bytelength;
  unsigned char *ptr, **array;
  b = getc(fp) & 0377;    /* read the number of bytes/integer. */
  bytepos = ftell(fp);
  bytelength = nbytes-1;
  array = malloc(2*sizeof(unsigned char *));
  array[0] = makeintegers(b, bytepos, bytelength);
  array[1] = (unsigned char *) 0;
  ptr = makevector(array, type_vector, subtype_integers);
  fseek(fp, bytelength, 1);
  free(array[0]);
  free(array);
  push(ptr);
  }


seq_adaptivepixel(nbytes)
  int nbytes;
  {
  pushpixel(nbytes, "adaptive");
  }

seq_compressedpixel(nbytes)
  int nbytes;
  {
  pushpixel(nbytes, "compressed");
  }

seq_packedpixel(nbytes)
  int nbytes;
  {
  pushpixel(nbytes, "packed");
  }

seq_identifier(nbytes)
  int nbytes;
  {
  pushstring(nbytes, subtype_identifier);
  }

seq_string(nbytes)
  int nbytes;
  {
  pushstring(nbytes, subtype_string);
  }

seq_unknown(type, nbytes)
  int type, nbytes;
  {
  error(err4, filepos, type, nbytes);
  }
 
seq_integer(nbytes)
  int nbytes;
  {
  pushinteger(nbytes, subtype_integer);
  }

seq_rational(nbytes)
  int nbytes;
  {
  pushinteger(nbytes, subtype_rational);
  }

shortnum(number)
  int number;
  {
  unsigned char value[2];
  unsigned char *ptr;
  value[0] = (number >> 8) & 0377;
  value[1] = number & 0377;
  ptr = makenumber(2, value, subtype_integer);
  push(ptr);
  }


/*
 * Private procedures to this module.
 *
 */

static pushinteger(nbytes, subtype)
  int nbytes, subtype;
  {
  int n;
  unsigned char *ptr;
  unsigned char *array;
  array = (unsigned char *) malloc(nbytes);
  for (n=0; n < nbytes; n++) array[n] = getc(fp) & 0377;
  ptr = makenumber(nbytes, array, subtype);
  free(array);
  push(ptr);
  }

static pushstring(nbytes, subtype)
  int nbytes, subtype;
  {
  int n;
  unsigned char *ptr;
  char *string;
  string = (char *) malloc(nbytes+1);
  for (n=0; n < nbytes; n++) string[n] = getc(fp) & 0377;
  string[nbytes] = (char) 0;
  ptr = makestring(string, subtype);
  free(string);
  push(ptr);
  }

static pushpixel(nbytes, compression)
  int nbytes;
  char *compression;
  {
  extern unsigned char *decompressID();
  extern unsigned char *decompressOP();
  extern unsigned char *imagedata();
  unsigned char *ptr, *idvec, **array;
  idvec = decompressID(compression);
  array = malloc(3*sizeof(unsigned char *));
  array[0] = decompressOP(idvec);
  array[1] = imagedata(nbytes);
  array[2] = (unsigned char *) 0;
  ptr = makevector(array, type_vector, subtype_samples);
  free(idvec);
  free(array[0]);
  free(array[1]);
  free(array);
  push(ptr);
  }

static unsigned char *decompressID(compression)
  char *compression;
  {
  unsigned char *ptr, **array;
  array = malloc(3*sizeof(unsigned char *));
  array[0] = makestring("xerox", subtype_identifier);
  array[1] = makestring(compression, subtype_identifier);
  array[2] = (unsigned char *) 0;
  ptr = makevector(array, type_vector, subtype_general);
  free(array[0]);
  free(array[1]);
  free(array);
  return(ptr);
  }

static unsigned char *decompressOP(idvec)
  unsigned char *idvec;
  {
  unsigned char *ptr, **array;
  array = malloc(2*sizeof(unsigned char *));
  array[0] = makeidentifier(idvec, "decompressionOps/");
  array[1] = (unsigned char *) 0;
  ptr = makeoperator(array, subtype_decompressop);
  free(array[0]);
  free(array);
  return(ptr);
  }

static unsigned char *imagedata(nbytes)
  int nbytes;
  {
  long bytepos, bytelength;
  unsigned char *ptr, **array;
  bytepos = ftell(fp);
  bytelength = nbytes;
  array = malloc(2*sizeof(unsigned char *));
  array[0] = makeintegers(2, bytepos, bytelength);
  array[1] = (unsigned char *) 0;
  ptr = makevector(array, type_vector, subtype_integers);
  fseek(fp, bytelength, 1);
  free(array[0]);
  free(array);
  return(ptr);
  }

static popint()
  {
  int result;
  unsigned char *ptr;
  ptr = pop(type_number, 0);
  result = getint(ptr);
  free(ptr);
  return(result);
  }
 
static double popdouble()
  {
  double result;
  unsigned char *ptr;
  ptr = pop(type_number, 0);
  result = getdouble(ptr);
  free(ptr);
  return(result);
  }
 
static unsigned char *popidentifier(prefix)
  char *prefix;   /* should end with '/' character */
  {
  unsigned char *ptr, *composite;
  ptr = pop(type_vector, subtype_general);
  composite = makeidentifier(ptr, prefix);
  free(ptr);
  return(composite);
  }

static extendpixel(nbytes)
  int nbytes;
  {
  int n, depth;
  long bytepos, bytelength;
  unsigned char *ptr, **samplesarray, **integersarray;
  unsigned char *newptr, **newarray;
  ptr = pop(type_vector, subtype_samples);
  samplesarray = getvector(ptr);
  depth = getdepth(samplesarray[1]);
  integersarray = getvector(samplesarray[1]);
  newarray = malloc((depth+2)*sizeof(unsigned char *));
  for (n=0; n < depth; n++) newarray[n] = integersarray[n];
  bytepos = ftell(fp);
  bytelength = nbytes;
  newarray[depth] = makeintegers(2, bytepos, bytelength);
  newarray[depth+1] = (unsigned char *) 0;
  samplesarray[1] = makevector(newarray, type_vector, subtype_integers);
  newptr = makevector(samplesarray, type_vector, subtype_samples);
  fseek(fp, bytelength, 1);
  free(ptr);
  free(newarray[depth]);
  free(samplesarray[1]);
  free(samplesarray);
  free(integersarray);
  free(newarray);
  push(newptr);
  }

static extendnumber(nbytes)
  int nbytes;
  {
  int n, len;
  unsigned char *number, *newnumber;
  unsigned char *ptr, *newptr;
  ptr = pop(type_number, subtype_integer | subtype_rational);
  number = getnumber(ptr);
  len = getnumlen(ptr);
  newnumber = (unsigned char *) malloc(len+nbytes);
  for (n=0; n < len; n++) newnumber[n] = number[n];
  for (n=0; n < nbytes; n++) newnumber[n+len] = getc(fp) & 0377;
  newptr = makenumber(len+nbytes, newnumber, getsubtype(ptr));
  free(ptr);
  free(newnumber);
  push(newptr);
  }

static extendstring(nbytes)
  int nbytes;
  {
  int n, len;
  char *string, *newstring;
  unsigned char *ptr, *newptr;
  ptr = pop(type_string, subtype_identifier | subtype_string);
  string = getstring(ptr, 0);
  len = strlen(string);
  newstring = (char *) malloc(len+nbytes+1);
  strcpy(newstring, string);
  for (n=0; n < nbytes; n++) newstring[n+len] = getc(fp) & 0377;
  newstring[len+nbytes] = (char) 0;
  newptr = makestring(newstring, getsubtype(ptr));
  free(ptr);
  free(newstring);
  push(newptr);
  }





/* Change Log
 *
 * K. Knox,  1-Apr-85 23:45:46, Created first version.
 * K. Knox, 13-May-85 10:25:25, Fixed bugs in calls to maketransformation.
 * K. Knox, 13-May-85 10:25:25, Fixed bug in angle calculation in op_rotate.
 *
 *
 *
 */




