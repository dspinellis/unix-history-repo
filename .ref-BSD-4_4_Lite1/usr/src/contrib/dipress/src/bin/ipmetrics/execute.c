/* execute.c
 *
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 *  Define the functions used in parse.c.
 *
 * Execute the RES file and leave the correct parameters on the stack.
 *
 * HISTORY
 *
 * K. Knox,  1-Apr-85 23:45:46, Created first version.
 * K. Knox, 13-May-85 10:25:25, Fixed bugs in calls to maketransformation.
 * K. Knox, 13-May-85 10:25:25, Fixed bug in angle calculation in op_rotate.
 *
 */

#include <math.h>
#include <stdio.h>
#include <iptokens.h>
#include "stack.h"

#define RES_header "Interpress/Xerox/2.1/RasterEncoding/1.0 "
#define len_RES_header 40

#define err0 "(%d) execute: Bad IP header, got %s\n"
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
extern unsigned char *popidentifier();
extern double popdouble();


/*
 * Public procedures defined for "parse" module.
 *
 */

header(string)
  {
  if (strncmp(string, RES_header, 10) != 0) error(err0, filepos, string);
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

op_makeveclu()
  {
  int n, depth,
      upper,
      lower;
  unsigned char *ptr, **array;

  upper = popint();
  lower = popint();
  depth = upper - lower + 1;
  array = malloc((depth+1)*sizeof(unsigned char *));   /* null terminated array */
  for (n=0; n < depth; n++)
	array[depth-n-1] = pop(0);

  array[depth] = (unsigned char *) 0;
  ptr = makevector(array, type_vector, subtype_general);
  /* set uppper and lower bounds */

  for (n=0; n < depth; n++)
	free(array[n]);

  free(array);
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

op_beginblock()
  {
  }

op_endblock()
  {
  }

op_beginbody()
  {
  }

op_endbody()
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


