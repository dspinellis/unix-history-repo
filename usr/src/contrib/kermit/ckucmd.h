/*  C K U C M D . H  --  Header file for Unix cmd package  */

/*
 Author: Frank da Cruz (SY.FDC@CU20B),
 Columbia University Center for Computing Activities, January 1985.
 Copyright (C) 1985, Trustees of Columbia University in the City of New York.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as it is not sold for profit, provided this
 copyright notice is retained. 
*/

/* Special getchars... */

#ifdef vax11c
#define getchar()   vms_getchar()
#endif

#ifdef aegis
#define getchar()   coninc(0)
#endif

/* Sizes of things */

#define HLPLW  78			/* Width of ?-help line */
#define HLPCW  19			/* Width of ?-help column */
#define CMDBL  200			/* Command buffer length */
#define HLPBL  100			/* Help string buffer length */
#define ATMBL  100			/* Command atom buffer length*/

/* Special characters */

#ifndef NUL
#define NUL  '\0'			/* Null */
#endif
#define HT   '\t'			/* Horizontal Tab */
#define NL   '\n'			/* Newline */
#ifndef CR
#define CR   '\r'
#endif
#define FF   0014			/* Formfeed    (^L) */
#define RDIS 0022			/* Redisplay   (^R) */
#define LDEL 0025			/* Delete line (^U) */
#define WDEL 0027			/* Delete word (^W) */
#define ESC  0033			/* Escape */
#define RUB  0177			/* Rubout */

#ifndef BEL
#define BEL  0007			/* Bell */
#endif

#ifndef BS
#define BS   0010			/* Backspace */
#endif

#ifndef SP
#define SP   0040			/* Space */
#endif

/* Keyword table flags */

#define CM_INV 1			/* Invisible keyword */

/* Keyword Table Template */

struct keytab {				/* Keyword table */
    char *kwd;				/* Pointer to keyword string */
    int val;				/* Associated value */
    int flgs;				/* Flags (as defined above) */
};
