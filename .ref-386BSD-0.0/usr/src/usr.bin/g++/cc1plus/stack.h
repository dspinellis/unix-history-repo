/* Stack of data placed on obstacks.  */
   
struct stack_level
{
  /* Pointer back to previous such level.  */
  struct stack_level *prev;

  /* Point to obstack we should return to.  */
  struct obstack *obstack;

  /* First place we start putting data.  */
  tree *first;

  /* Number of entries we can have from `first'.
     Right now we are dumb: if we overflow, abort.  */
  int limit;
};

struct stack_level *push_stack_level ();
struct stack_level *pop_stack_level ();

