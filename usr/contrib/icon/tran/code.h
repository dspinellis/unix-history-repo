/*
 * Code generator parameters.
 */

#define LOOPDEPTH   20		/* max. depth of nested loops */
#define CASEDEPTH   10		/* max. depth of nested case statements */
#define CREATDEPTH  10		/* max. depth of nested create statements */

/*
 * loopstk structures hold information about nested loops.
 */
struct loopstk {
   int nextlab;			/* label for next exit */
   int breaklab;		/* label for break exit */
   int markcount;		/* number of marks */
   int ltype;			/* loop type */
   };

/*
 * casestk structure hold information about case statements.
 */
struct casestk {
   int endlab;			/* label for exit from case statement */
   nodeptr deftree;		/* pointer to tree for default clause */
   };

/*
 * creatstk structures hold information about create statements.
 */
struct creatstk {
   int nextlab;			/* previous value of nextlab */
   int breaklab;		/* previous value of breaklab */
   };
