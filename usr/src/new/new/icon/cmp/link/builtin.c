#include "ulink.h"

struct bentry btable[] = {
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

#define NBUILTIN (sizeof(btable)/sizeof(struct bentry))

/*
 * blocate - binary search for a builtin function.
 * If found, returns pointer to entry.
 */

struct bentry *blocate(s)
register char *s;
   {
   register int test;
   register struct bentry *p;
   int low, high, cmp;

   low = 0;
   high = NBUILTIN;
   do {
      test = (low + high) / 2;
      p = &btable[test];
      if ((cmp = strcmp(p->b_name, s)) < 0)
	 low = test + 1;
      else if (cmp > 0)
	 high = test;
      else
	 return (p);
      } while (low < high);
   return (NULL);
   }
