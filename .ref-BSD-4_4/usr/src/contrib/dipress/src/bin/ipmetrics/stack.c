/* stack.c
 *
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 * This module manipulates the RES stack.
 *
 */

#include "stack.h"

extern long filepos;
extern unsigned char **malloc();

/* Private procedures defined in this module. */
extern unsigned char *makeitem();
extern double readdouble();

struct item
  {
  int length;                 /* total byte length of this stack item. */
  struct item *next;          /* next element on the stack. */
  int type;                   /* stack element type. */
  int subtype;                /* subtype within this type. */
  };

struct number
  {
  int length;
  struct item *next;
  int type;
  int subtype;
  unsigned char nbytes;
  unsigned char num[1];
  };

struct string
  {
  int length;
  struct item *next;
  int type;
  int subtype;
  char s[1];
  /* the string follows here, above char gives room for terminating null */
  };

struct vector
  {
  int length;
  struct item *next;
  int type;
  int subtype;
  int depth;
  };

struct integers
  {
  int length;
  struct item *next;
  int type;
  int subtype;
  int bytesPerInteger;
  long bytepos;
  long bytelength;
  };


struct transformation
  {
  int length;
  struct item *next;
  int type;
  int subtype;
  double a;
  double b;
  double c;
  double d;
  double e;
  double f;
  };

static struct item *stack = (struct item *) 0;

#define err0 "(%d) stack: pop called on an empty stack!\n"
#define err1 "(%d) stack: expecting type (%s, %s), got (%d, %d)!\n"
#define err2 "(%d) stack: should be 8 elements in pixelarray, got %d!\n"


/*
 *  Items on the stack.
 *
 */

stackempty()
  {
  return(stack == (struct item *) 0);
  }

unsigned char *pop(type, subtype)
  int type, subtype;
  {
  unsigned char *ptr;
  struct item *pitem;

  pitem = stack;

  if (pitem == (struct item *) 0)
	error(err0, filepos);

  stack = pitem->next;
  ptr = (unsigned char *) pitem;
  checktype(ptr, type, subtype);
  return (ptr);
  }

push(pitem)
  struct item *pitem;
  {
  pitem->next = stack;
  stack = pitem;
  }

unsigned char *duplicate(from)
  unsigned char *from;
  {
  int n;
  unsigned char *to;
  to = makeitem(getlength(from), gettype(from), getsubtype(from));
  for (n=0; n < getlength(from); n++) to[n] = from[n];
  return (to);
  }

gettype(pitem)
  struct item *pitem;
  {
  return(pitem->type);
  }

getsubtype(pitem)
  struct item *pitem;
  {
  return(pitem->subtype);
  }

getlength(pitem)
  struct item *pitem;
  {
  return(pitem->length);
  }

checktype(ptr, type, subtype)
  unsigned char *ptr;
  int type, subtype;
  {
  int intype, insubtype, problem;
  char *typename, *subtypename;
  intype = gettype(ptr);
  insubtype = getsubtype(ptr);
  problem = 0;
  if (type)
    {
    if ((type & intype) == 0) problem = 1;
    if ((subtype != 0) && ((subtype & insubtype) == 0)) problem = 1;
    }
  if (problem)
    {
    typename = gettypename(type);
    subtypename = getsubtypename(subtype);
    error(err1, filepos, typename, subtypename, intype, insubtype);
    }
  }

char *gettypename(type)
  int type;
  {
  switch (type)
    {
    case 0: return("undefined");
    case 1: return("number");
    case 2: return("string");
    case 4: return("vector");
    case 8: return("operator");
    case 16: return("color");
    case 32: return("pixelarray");
    case 64: return("transformation");
    case 128: return("integers");
    default: return("unknown");
    };
  }

char *getsubtypename(subtype)
  int subtype;
  {
  switch (subtype)
    {
    case 0: return("undefined");
    case 1: return("integer");
    case 2: return("rational");
    case 4: return("identifier");
    case 8: return("string");
    case 16: return("general");
    case 32: return("integers");
    case 64: return("samples");
    case 128: return("decompressop");
    case 256: return("colorop");
    case 512: return("colormodelop");
    case 1024: return("value");
    case 2048: return("name");
    case 4096: return("operator");
    default: return("unknown");
    };
  }


/*
 *  Numbers
 *
 */

unsigned char *makenumber(nbytes, array, subtype)
  int nbytes;
  unsigned char *array;
  {
  int n;
  unsigned char *ptr;
  struct number *pnumber;
  ptr = makeitem(sizeof(struct number)+nbytes, type_number, subtype);
  pnumber = (struct number *) ptr;
  pnumber->nbytes = nbytes;
  for (n=0; n < nbytes; n++) pnumber->num[n] = array[n];
  return(ptr);
  }

getnumlen(pnumber)
  struct number *pnumber;
  {
  checktype(pnumber, type_number, 0);
  return(pnumber->nbytes);
  }

unsigned char *getnumber(pnumber)
  struct number *pnumber;
  {
  checktype(pnumber, type_number, 0);
  return(pnumber->num);
  }

getint(ptr)
  unsigned char *ptr;
  {
  int result;
  result = getdouble(ptr);
  return (result);
  }
 
double getdouble(pnumber)
  struct number *pnumber;
  {
  int nbytes;
  double result, numerator, denominator;
  checktype(pnumber, type_number, subtype_integer | subtype_rational);
  switch (getsubtype(pnumber))
    {
    case subtype_integer:
      result = readdouble(pnumber->nbytes, pnumber->num);
      break;
    case subtype_rational:
      nbytes = pnumber->nbytes/2;
      numerator = readdouble(nbytes, pnumber->num);
      denominator = readdouble(nbytes, pnumber->num+nbytes);
      result = numerator/denominator;
      break;
    }
  return (result);
  }

double getnumerator(pnumber)
  unsigned char *pnumber;
  {
  int nbytes;
  double numerator;
  checktype(pnumber, type_number, subtype_rational);
  nbytes = getnumlen(pnumber)/2;
  numerator = readdouble(nbytes, getnumber(pnumber));
  return (numerator);
  }

double getdenominator(pnumber)
  unsigned char *pnumber;
  {
  int nbytes;
  double denominator;
  checktype(pnumber, type_number, subtype_rational);
  nbytes = getnumlen(pnumber)/2;
  denominator = readdouble(nbytes, getnumber(pnumber)+nbytes);
  return (denominator);
  }


/*
 *  Strings
 *
 */

unsigned char *makestring(string, subtype)
  char *string;
  int subtype;
  {
  unsigned char *ptr;
  struct string *pstring;
  ptr = makeitem(sizeof(struct string)+strlen(string), type_string, subtype);
  pstring = (struct string *) ptr;
  strcpy(pstring->s, string);
  return(ptr);
  }

unsigned char *makeidentifier(ptr, prefix)
  unsigned char *ptr;
  char *prefix;
  {
  char *string;
  int n, len, depth;
  unsigned char *composite, **array;
  depth = getdepth(ptr);
  array = getvector(ptr);
  len = strlen(prefix);
  for (n=0; n < depth; n++)
    {
    string = getstring(array[n], subtype_identifier);
    len += strlen(string)+1;    /* add one for '/' character */
    }                           /* added one too many, gives extra space */
  string = (char *) malloc(len+1);   /* add one for null character */
  strcpy(string, prefix);
  for (n=0; n < depth; n++)
    {
    if (n) strcat(string, "/");
    strcat(string, getstring(array[n], subtype_identifier));
    }
  free(array);
  composite = makestring(string, subtype_identifier);
  free(string);
  return(composite);
  }

char *getstring(pstring, subtype)
  struct string *pstring;
  {
  checktype(pstring, type_string, subtype);
  return(pstring->s);
  }


/*
 *  Vectors
 *
 */

unsigned char *makevector(array, type, subtype)
  unsigned char **array;
  int type, subtype;
  {
  int n, m, len, depth;
  unsigned char *ptr, *to, *from;
  struct vector *pvector;
  len = sizeof(struct vector);
  for (depth=0; array[depth] != (unsigned char *) 0; depth++)
    len += getlength(array[depth]);
  ptr = makeitem(len, type, subtype);
  pvector = (struct vector *) ptr;
  pvector->depth = depth;
  to = ptr;
  to += sizeof(struct vector);
  for (n=0; n < depth; n++)
    {
    from = array[n];
    len = getlength(from);
    for (m=0; m < len; m++) to[m] = from[m];
    to += len;
    }
  return(ptr);
  }

unsigned char **getvector(from)
  unsigned char *from;
  {
  int n, m, depth, len;
  unsigned char *to, **array;

  depth = getdepth(from);
  array = malloc((depth+1)*sizeof(unsigned char *));
  from += sizeof(struct vector);

  for (n=0; n < depth; n++) {
    array[n] = from;
    len = getlength(from);
    from += len;
  }

  array[depth] = (unsigned char *) 0;    /* null terminated list */
  return(array);
  }
 
getdepth(pvector)
  struct vector *pvector;
  {
  return(pvector->depth);
  }



/*
 *  Operators
 *
 */

unsigned char *makeoperator(array, subtype)
  unsigned char **array;
  int subtype;
  {
  unsigned char *ptr;
  ptr = makevector(array, type_operator, subtype);
  return(ptr);
  }

unsigned char **getoperator(ptr)
  unsigned char *ptr;
  {
  unsigned char **array;
  array = getvector(ptr);
  return(array);
  }




/*
 *  Pixel array
 *
 */


unsigned char *makepixelarray(array)
  unsigned char **array;
  {
  int depth;
  unsigned char *ptr;
  for (depth=0; array[depth] != (unsigned char *) 0; depth++) ;
  if (depth != 8) error(err2, filepos, depth);
  ptr = makevector(array, type_pixelarray, 0);
  return(ptr);
  }

unsigned char *makeselect(max, samples)
  int max, samples;
  {
  int n, mask, depth;
  unsigned char *ptr, **array, value;
  array = malloc((max+1)*sizeof(unsigned char *));
  depth = 0;
  for (n=0; n < max; n++)
    {
    value = n;
    mask = 1 << n;
    if (samples & mask)
      {
      array[depth] = makenumber(1, &value, subtype_integer);
      depth++;
      }
    }
  array[depth] = (unsigned char *) 0;    /* null terminated list */
  ptr = makevector(array, type_vector, subtype_general);
  for (n=0; n < depth; n++) free(array[n]);
  free(array);
  return(ptr);
  }

unsigned char **getpixelarray(from)
  unsigned char *from;
  {
  if (getdepth(from) != 8) error(err2, filepos, getdepth(from));
  return(getvector(from));
  }


/*
 *  Transformations
 *
 */

unsigned char *maketransformation(a, b, c, d, e, f)
  double a, b, c, d, e, f;
  {
  unsigned char *ptr;
  struct transformation *ptransformation;
  ptr = makeitem(sizeof(struct transformation), type_transformation, 0);
  ptransformation = (struct transformation *) ptr;
  ptransformation->a = a;
  ptransformation->b = b;
  ptransformation->c = c;
  ptransformation->d = d;
  ptransformation->e = e;
  ptransformation->f = f;
  return(ptr);
  }

double *gettransformation(ptransformation)
  struct transformation *ptransformation;
  {
  double *array;
  checktype(ptransformation, type_transformation, 0);
  array = (double *) malloc(6*sizeof(double));
  array[0] = ptransformation->a;
  array[1] = ptransformation->b;
  array[2] = ptransformation->c;
  array[3] = ptransformation->d;
  array[4] = ptransformation->e;
  array[5] = ptransformation->f;
  return(array);
  }


/*
 *  Integers
 *
 */

unsigned char *makeintegers(bytesPerInteger, bytepos, bytelength)
  int bytesPerInteger;
  long bytepos, bytelength;
  {
  unsigned char *ptr;
  struct integers *pintegers;
  ptr = makeitem(sizeof(struct integers), type_integers, 0);
  pintegers = (struct integers *) ptr;
  pintegers->bytesPerInteger = bytesPerInteger;
  pintegers->bytepos = bytepos;
  pintegers->bytelength = bytelength;
  return(ptr);
  }

getbytesPerInteger(pintegers)
  struct integers *pintegers;
  {
  checktype(pintegers, type_integers, 0);
  return(pintegers->bytesPerInteger);
  }

long getbytepos(pintegers)
  struct integers *pintegers;
  {
  checktype(pintegers, type_integers, 0);
  return(pintegers->bytepos);
  }

long getbytelength(pintegers)
  struct integers *pintegers;
  {
  checktype(pintegers, type_integers, 0);
  return(pintegers->bytelength);
  }



/*
 *  Private procedures to this module.
 *
 */

static unsigned char *makeitem(length, type, subtype)
  int length, type, subtype;
  {
  unsigned char *ptr;
  struct item *pitem;
  length = (length+sizeof(int)-1)/sizeof(int);
  length *= sizeof(int);
  ptr = (unsigned char *) malloc(length);
  pitem = (struct item *) ptr;
  pitem->length = length;
  pitem->type = type;
  pitem->subtype = subtype;
  return(ptr);
  }

static double readdouble(nbytes, array)
  int nbytes;
  unsigned char *array;
  {
  int n, negative, temp;
  double result;
  negative = 0;
  if (array[0] > 127)
    {
    negative = 1;
    for (n=0; n < nbytes; n++) array[n] = ~array[n];
    }
  result = 0;
  for (n=0; n < nbytes; n++)
    {
    temp = array[n];
    result = 256.*result+(double)temp;
    }
  if (negative) result = -result-1;
  return(result);
  }



/* Change Log
 *
 * K. Knox, 29-Mar-85 18:20:18, Created first version.
 * K. Knox, 13-May-85  9:50:52, Fixed negative number bug in readdouble().
 *
 *
 *
 */
 
 
 
 
