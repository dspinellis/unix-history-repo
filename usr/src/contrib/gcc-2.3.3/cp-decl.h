/* In grokdeclarator, distinguish syntactic contexts of declarators.  */
enum decl_context
{ NORMAL,			/* Ordinary declaration */
  FUNCDEF,			/* Function definition */
  PARM,				/* Declaration of parm before function body */
  FIELD,			/* Declaration inside struct or union */
  BITFIELD,			/* Likewise but with specified width */
  TYPENAME,			/* Typename (inside cast or sizeof)  */
  MEMFUNCDEF			/* Member function definition */
};

/* C++: Keep these around to reduce calls to `get_identifier'.
   Identifiers for `this' in member functions and the auto-delete
   parameter for destructors.  */
extern tree this_identifier, in_charge_identifier;

/* Parsing a function declarator leaves a list of parameter names
   or a chain or parameter decls here.  */
extern tree last_function_parms;

/* A list of static class variables.  This is needed, because a
   static class variable can be declared inside the class without
   an initializer, and then initialized, staticly, outside the class.  */
extern tree pending_statics;

/* A list of objects which have constructors or destructors
   which reside in the global scope.  The decl is stored in
   the TREE_VALUE slot and the initializer is stored
   in the TREE_PURPOSE slot.  */
extern tree static_aggregates;

/* A list of functions which were declared inline, but later had their
   address taken.  Used only for non-virtual member functions, since we can
   find other functions easily enough.  */
extern tree pending_addressable_inlines;

#ifdef SOS
/* SOS extensions.  */
extern tree zlink_type, zret_type;
extern tree zlink, zret;
#endif

#ifdef DEBUG_CP_BINDING_LEVELS
/* Purely for debugging purposes.  */
extern int debug_bindings_indentation;
#endif
