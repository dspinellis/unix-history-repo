/*
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 * stack.h -- elements of the RES stack.
 */


/* stack element types */
#define type_number              1
#define type_string              2
#define type_vector              4
#define type_operator            8
#define type_color               16
#define type_pixelarray          32
#define type_transformation      64
#define type_integers            128

/* number sub types */
#define subtype_integer          1
#define subtype_rational         2

/* string sub types */
#define subtype_identifier       4
#define subtype_string           8

/* vector sub types */
#define subtype_general          16
#define subtype_integers         32
#define subtype_samples          64

/* operator sub types */
#define subtype_decompressop     128
#define subtype_colorop          256
#define subtype_colormodelop     512

/* color sub types */
#define subtype_value            1024
#define subtype_name             2048
#define subtype_operator         4096

extern stackempty();
extern unsigned char *pop();
extern push();
extern unsigned char *duplicate();
extern gettype();
extern getsubtype();
extern getlength();
extern checktype();
extern char *gettypename();
extern char *getsubtypename();
extern unsigned char *makenumber();
extern getnumlen();
extern unsigned char *getnumber();
extern getint();
extern double getdouble();
extern double getnumerator();
extern double getdenominator();
extern unsigned char *makestring();
extern unsigned char *makeidentifier();
extern char *getstring();
extern unsigned char *makevector();
extern unsigned char **getvector();
extern getdepth();
extern unsigned char *makeoperator();
extern unsigned char **getoperator();
extern unsigned char *makecolor();
extern unsigned char **getcolor();
extern unsigned char *makepixelarray();
extern unsigned char *makeselect();
extern unsigned char **getpixelarray();
extern unsigned char *maketransformation();
extern double *gettransformation();
extern unsigned char *makeintegers();
extern getbytesPerInteger();
extern long getbytepos();
extern long getbytelength();



/* Change Log
 *
 * K. Knox,          25-Mar-85 15:48:54, Created first version.
 *
 *
 *
 */
 
