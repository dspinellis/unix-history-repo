#include "ulink.h"

char *btable[] = {
   "abs",          "any",          "bal",          "center",
   "close",        "copy",         "cset",         "display",
   "exit",         "find",         "get",          "image",
   "integer",      "left",         "list",
   "many",         "map",          "match",        "move",
   "numeric",      "open",         "pop",          "pos",
   "pull",         "push",         "put",          "read",
   "reads",        "real",         "repl",         "reverse",
   "right",
#ifdef EXT
   "seq",
#endif EXT
   "sort",         "stop",         "string",
   "system",       "tab",          "table",        "trim",
   "type",         "upto",         "write",        "writes"
   };

#define NBUILTIN (sizeof(btable)/sizeof(char *))

/*
 * blocate - binary search for a builtin function.
 * If found, returns pointer to entry.
 */

blocate(s)
register char *s;
   {
   register int test, cmp;
   int low, high;

   low = 0;
   high = NBUILTIN;
   do {
      test = (low + high) / 2;
      cmp = strcmp(btable[test], s);
      if (cmp < 0)
	 low = test + 1;
      else if (cmp > 0)
	 high = test;
      else
	 return (test+1);
      } while (low < high);
   return (0);
   }
