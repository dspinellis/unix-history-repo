/* $Header: /f/osi/others/quipu/uips/pod/RCS/defs.h,v 7.1 91/02/22 09:31:28 mrose Interim $ */

#ifndef PODDEFS
#define PODDEFS

#ifndef NULLCP
#define NULLCP (char *) 0
#endif

typedef short bool;

typedef enum {rfc822, greybook} mailtype;

typedef enum {
  Okay, 
  timelimit, 
  timelimit_w_partial,
  listsizelimit,
  adminlimit,
  adminlimit_w_partial,
  nothingfound, 
  localdsaerror,
  remotedsaerror,
  duaerror,
  attributerror,
  namerror,
  security,
  updaterror,
  serviceerror
  } dsEnqError;

typedef struct dsErrorStruct {
  dsEnqError error;
  char *err_mess;
} dsErrorStruct;

#define RESBUF 10000
#define MAXARGS 20
#define STRINGLEN 1000
#define SMALLSTRING 255
#define MAXTYPES  255

#endif
