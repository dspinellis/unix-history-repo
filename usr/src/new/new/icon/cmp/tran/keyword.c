#include "../h/config.h"
#include "keyword.h"
#include "sym.h"

/*
 * keyword table
 */

struct keyent keytab[] = {
   "ascii",            K_ASCII,
   "clock",            K_CLOCK,
   "cset",             K_CSET,
   "date",             K_DATE,
   "dateline",         K_DATELINE,
   "errout",           K_ERROUT,
   "fail",             K_FAIL,
   "host",             K_HOST,
   "input",            K_INPUT,
   "lcase",            K_LCASE,
   "level",            K_LEVEL,
   "main",             K_MAIN,
   "null",             K_NULL,
   "output",           K_OUTPUT,
   "pos",              K_POS,
   "random",           K_RANDOM,
   "source",           K_SOURCE,
   "subject",          K_SUBJECT,
   "time",             K_TIME,
   "trace",            K_TRACE,
   "ucase",            K_UCASE,
   "version",          K_VERSION,
   "",                 -1
   };
