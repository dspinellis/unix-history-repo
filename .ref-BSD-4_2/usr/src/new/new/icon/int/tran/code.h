#

/*
 * code generator parameters
 */

#define LOOPDEPTH   20		/* max. depth of nested loops */
#define CASEDEPTH   10		/* max. depth of nested case statements */
#define CREATDEPTH  10		/* max. depth of nested create statements */

/*
 * code generator structures
 */

struct loopstk {
   int nextlab;
   int breaklab;
   int markcount;
   int ltype;
   };

struct casestk {
   int endlab;
   nodeptr deftree;
   };

struct creatstk {
   int nextlab;
   int breaklab;
   };
