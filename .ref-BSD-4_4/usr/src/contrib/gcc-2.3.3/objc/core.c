/* Method dispatcher and class-object creator for Objective C.
   Copyright (C) 1992 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include "tconfig.h"
#include "assert.h"
#include <ctype.h>
#include "gstdarg.h"
#include <stdio.h>
#include "gstddef.h"

#include "hash.h"
#include "objc.h"
#include "objc-proto.h"


#define MODULE_HASH_SIZE 32   /* Initial module hash table size.
				 Value doesn't really matter.  */

#define CLASS_HASH_SIZE 32    /* Initial number of buckets size of
				 class hash table.  Value doesn't
				 really matter.  */


/* Forward declare some functions.  */
id            objc_object_create (Class_t),
              objc_object_dispose (id),
              objc_object_realloc (id, unsigned int),
              objc_object_copy (id);
void          objc_error (id object, const char *fmt, va_list ap);
static id     nil_method (id, SEL, ...);
static id     return_error_static (id, SEL, ...);
static IMP    handle_runtime_error (id, SEL);
static void   initialize_dispatch_tables (void);
static SEL    record_selector (const char*);
static void   record_methods_from_class (Class_t);
static void   record_methods_from_list (MethodList_t);
static void   initialize_class (const char*);
/*
 * This is a hash table of Class_t structures. 
 *
 * At initialization the executable is searched for all Class_t structures. 
 * Since these structures are created by the compiler they are therefore
 * located in the TEXT segment.  
 *
 * A identical structure is allocated from the free store and initialized from
 * its TEXT counterpart and placed in the hash table using the TEXT part as
 * its key. 
 *
 * Since the free store structure is now writable, additional initialization
 * takes place such as its "info" variable, method cache allocation, and
 * linking of all of its method and ivar lists from multiple implementations. 
 */
cache_ptr	class_hash_table = NULL;

/*
 * This variable is a flag used within the messaging routines.  If a
 * application sets it to !0 then the messager will print messages sent to
 * objects. 
 */
BOOL  objc_trace = NO;

/* This mutex provides a course lock for method dispatch.  */
MUTEX	runtimeMutex;

/*
 * This hash table is used by the initialization routines.  When the
 * constructor function (__objc_execClass) is called it is passed a pointer
 * to a module structure.  That pointer is stored in this table and its
 * contents are processed in __objcInit. 
 */
cache_ptr    module_hash_table = NULL;

/*
 * This hash table is used in the constructor subroutine to hold pointers 
 * to categories that have not been attached to a class.  Constructors are
 * received in a random order.  Files may contain category implementation
 * of objects whose constructor hasn't been executed yet.  Therefore, 
 * there is no object to attach the categories.
 *
 * This hash table holds pointers to categories that haven't been
 * attached to objects.  As objects are processed the category hash
 * table is searched for attachments.  If a category is found for the
 * object it is attached to the object and deleted from the hash table.
 */
cache_ptr    unclaimed_category_hash_table = NULL;

/*
 * This flag is used by the messager routines to determine if the run-time
 * has been initialized.  If the run-time isn't initialized then a
 * initialization clean up routine is called. 
 */
static int	initialized = 0;

/*
 * Records that hold pointers to arrays of records.  The terminal records are
 * method implementations. 
 *
 * The size of the first record is the number of unique classes in the
 * executable.  The second is the number of selectors. 
 *
 * The second record conatins methods that are visible to the class -- that is,
 * methods that are not overriden from the classt to the root object. 
 *
 * The cache pointers of class and meta class structures point to one of these
 * records. 
 */
static struct record *instance_method_record	= NULL;
static struct record *factory_method_record	= NULL;

/*
 * This structure is used to translate between selectors and their ASCII
 * representation.  A NULL terminated array of char*,
 * OBJC_SELECTOR_REFERENCES, is passed to the constructor routine: 
 * __objc_execClass. That routine places entries from that array into this
 * structure.  The location within OBJC_SELECTOR_REFERENCES where the string
 * was taken from is replaced with a small integer, the index into the array
 * inside selectorTranslateTable.  That integer then becomes the selector. 
 *
 * Selectors begin at 1 to numEntries.  A selector outside of that range is
 * considered an error.  The selector integers are used as the first index
 * into the instance_method_record and factory_method_record arrays. 
 */
static struct record *selector_record = NULL;

/*
 * This data structure is used in the special case where usual fatal error
 * functions are called but have been overridden in a class.  The value
 * returned by that function is returned to the calling object.  error_static
 * holds the returned value until it is retrieved by return_error_static
 * which returns it to the calling function. 
 */
static id	error_static;


/* Given a class and selector, return the selector's implementation.  */
static inline IMP	
get_imp (Class_t class, SEL sel)
{
  IMP	imp = NULL;
  imp = record_get (getClassNumber (class),
		    record_get ((unsigned int)sel, *class->cache));

  return imp;
}


static void
error (msg, arg1, arg2)
     char *msg, *arg1, *arg2;
{
#if 0 /* There is no portable way to get the program name.  Too bad.  */
  fprintf (stderr, "%s: ", programname);
#endif
  fprintf (stderr, msg, arg1, arg2);
  fprintf (stderr, "\n");
}

static void
fatal (msg, arg1, arg2)
     char *msg, *arg1, *arg2;
{
  error (msg, arg1, arg2);
  exit (1);
}

static void *
xmalloc (unsigned int size)
{
  void *ptr = (void *) malloc (size);
  if (ptr == 0)
    fatal ("virtual memory exceeded");
  return ptr;
}

static void *
xcalloc (unsigned int size, unsigned int units)
{
  void *ptr = (void *) calloc (size, units);
  if (ptr == 0)
    fatal ("virtual memory exceeded");
  return ptr;
}

void *
__objc_xmalloc (unsigned int size)
{
  void *ptr = (void *) malloc (size);
  if (ptr == 0)
    fatal ("virtual memory exceeded");
  return ptr;
}

void *
__objc_xrealloc (void *optr, unsigned int size)
{
  void *ptr = (void *) realloc (optr, size);
  if (ptr == 0)
    fatal ("virtual memory exceeded");
  return ptr;
}

void *
__objc_xcalloc (unsigned int size, unsigned int units)
{
  void *ptr = (void *) calloc (size, units);
  if (ptr == 0)
    fatal ("virtual memory exceeded");
  return ptr;
}

static inline char *
my_strdup (const char *str)
{
  char *new = (char *) xmalloc (strlen (str) + 1);
  strcpy (new, str);
	
  return new;
}


/*
 * This function is called by constructor functions generated for each module
 * compiled.  
 *
 * The purpose of this function is to gather the module pointers so that they
 * may be processed by the initialization clean up routine. 
 */
void 
__objc_execClass (Module_t module)
{
  /* Has we processed any constructors previously? 
     Flag used to indicate that some global data structures
     need to be built.  */
  static BOOL previous_constructors = 0;

  Symtab_t    symtab = module->symtab;
  Class_t     object_class;
  node_ptr node;
  SEL         *(*selectors)[] = (SEL *(*)[])symtab->refs;
  int         i;
  BOOL        incomplete = 0;


  assert (module->size == sizeof (Module));
  DEBUG_PRINTF ("received load module: %s\n",module->name);

  /* Header file data structure hack test.  */
  assert (sizeof (Class) == sizeof (MetaClass));

  /* On the first call of this routine, initialize
     some data structures.  */
  if (!previous_constructors) {

    /* Enable malloc debugging. This'll slow'er down! */
#if defined (DEBUG) && defined (NeXT)
    malloc_debug (62);
#endif

    /* Allocate and initialize the mutex.  */
    MUTEX_ALLOC (&runtimeMutex);
    MUTEX_INIT (runtimeMutex);

    /* Allocate the module hash table.  */
    module_hash_table
      = hash_new (MODULE_HASH_SIZE, (hash_func_type)hash_ptr,
		  (compare_func_type)compare_ptrs);

    /* Allocate a table for unclaimed categories.  */
    unclaimed_category_hash_table
      = hash_new (16, (hash_func_type)hash_ptr,
		  (compare_func_type)compare_ptrs);

    /* Allocate a master selector table if it doesn't exist.  */
    selector_record = record_new ();
    
    previous_constructors = 1;
  }

  /* Save the module pointer for later processing.  */
  hash_add (&module_hash_table, module, module);

  /* Parse the classes in the load module and gather selector information.  */
  DEBUG_PRINTF ("gathering selectors from module: %s\n",module->name);
  for (i = 0; i < symtab->cls_def_cnt; ++i) {
    Class_t class = (Class_t)symtab->defs[i];

    /* Make sure we have what we think.  */
    assert (class->info & CLS_CLASS);
    assert (class->class_pointer->info & CLS_META);
    DEBUG_PRINTF ("phase 1, processing class: %s\n", class->name);

    /* Store the class in the class table and assign class numbers.  */
    addClassToHash (class);

    /* Store all of the selectors in the class and meta class.  */
    record_methods_from_class (class);
    record_methods_from_class ((Class_t)class->class_pointer);

    /* Initialize the cache pointers.  */
    class->cache = &instance_method_record;
    class->class_pointer->cache = &factory_method_record;
  }

  /* Replace referenced selectors.  */
  for (i=0; i < symtab->sel_ref_cnt; ++i)
    (*selectors)[i] = record_selector ((const char*)(*selectors)[i]);

  /* Try to build the class hierarchy and initialize the data structures.  */
  object_class = objc_getClass ("Object");
  if (object_class) {
    
    /* Make sure we have what we think we have.  */
    assert (object_class->class_pointer->info & CLS_META);
    assert (object_class->info & CLS_CLASS);
    
    /* Connect the classes together (at least as much as we can).  */ 
    for (node = hash_next (class_hash_table, NULL); node;
	 node = hash_next (class_hash_table, node)) {
      Class_t class1 = node->value;

      /* Make sure we have what we think we have.  */
      assert (class1->info & CLS_CLASS);
      assert (class1->class_pointer->info & CLS_META);

      /* The class_pointer of all meta classes point to Object's meta class.  */
      class1->class_pointer->class_pointer = object_class->class_pointer; 

      /* Assign super class pointers */
      if (class1->super_class) {
	Class_t aSuperClass = objc_getClass ((char*)class1->super_class);

	if (aSuperClass) {

	  DEBUG_PRINTF ("making class connections for: %s\n", 
			class1->name);

	  class1->super_class = aSuperClass; 
	  if (class1->class_pointer->super_class)
	    class1->class_pointer->super_class = class1->super_class->class_pointer;

	  /* Mark the class as initialized.  */
	  class1->info |= CLS_RTI;
	} else
	  /* Couldn't find the class's super class.  */
	  incomplete = 1;
      }
    }
  } else
    /* Couldn't find class Object.  */
    incomplete = 1;

  /* Process category information from the module.  */
  for (i = 0; i < symtab->cat_def_cnt; ++i) {
    Category_t  category = symtab->defs[i + symtab->cls_def_cnt];
    Class_t     class = objc_getClass (category->class_name);

    /* If the class for the category exists then append its
       methods.  */
    if (class) {

      DEBUG_PRINTF ("processing categories from (module,object): %s, %s\n",
		    module->name, 
		    class_getClassName (class));

      /* Do instance methods.  */
      if (category->instance_methods)
        addMethodsToClass (class, category->instance_methods);

      /* Do class methods.  */
      if (category->class_methods)
        addMethodsToClass ((Class_t)class->class_pointer, category->class_methods);
    } else {
      /* The object to which the category methods belong can't
	 be found.  Save the information.  */
      hash_add (&unclaimed_category_hash_table, category, category);

      incomplete = 1;
    }
  }

  /* Scan the unclaimed category hash.  
     Attempt to attach any unclaimed categories to objects.  */
  for (node = hash_next (unclaimed_category_hash_table, NULL); node;
       node = hash_next (unclaimed_category_hash_table, node)) {
    Category_t  category = node->value;
    Class_t     class = objc_getClass (category->class_name);

    if (class) {

      DEBUG_PRINTF ("attaching stored categories to object: %s\n",
		    class_getClassName (class));

      /* Delete this class from the hash table.  */
      hash_remove (unclaimed_category_hash_table, category);
      node = NULL;

      if (category->instance_methods)
	addMethodsToClass (class, category->instance_methods);

      if (category->class_methods)
	addMethodsToClass ((Class_t)class->class_pointer,
			   category->class_methods);
    } else
      incomplete = 1;
  }

  /* Can we finish the run time initialization? */
  if (!incomplete) {

    initialize_dispatch_tables ();

    /* Debug run time test.
       We're initialized! */
    initialized = 1;

    /* Print out class tables if debugging.  */
    DEBUG_PRINTF ("dump of class tables from objcInit\n");
    debug_dump_classes ();
  }

}


IMP  
objc_msgSend (id receiver, SEL sel)
{
  /*
   * A method is always called by the compiler.  If a method wasn't
   * found then supply a default. 
   */
  IMP  imp = nil_method;


  /* The run time must be initialized at this point.
     Otherwise we get a message sent to a object with a bogus selector.  */
  assert (initialized);

  /* Objective-C allows messages to be sent to a nil object.  */
  if (receiver) {

    /* Check for common programmer error.  */
    if (!receiver->class_pointer) {
      fprintf (stderr, "method %s sent to deallocated object %#x\n", 
	       sel_getName (sel), receiver);
      abort ();
    }
    
    /* Initialize the class if need be.  */
    if (!(receiver->class_pointer->info & CLS_INITIALIZED))
      initialize_class (receiver->class_pointer->name);

    /*
     * If we're passed a object then its class_pointer is a Class.  If
     * we're passed a Class then its class_pointer is a MetaClass. 
     * Therefore, searching for a instance or class method
     * requires no special decision making here. 
     *
     * Look for the method. 
     */
    imp = get_imp (receiver->class_pointer, sel);

    /* If the method cannot be found then perform error handling.  */
    if (!imp)
      imp = handle_runtime_error (receiver, sel);
  }

  /* Nice debugging messages if enabled.  */
  if (objc_trace) {
    printf ("trace: objc_msgSend , obj=%#x, class=%s, method=%s\n",
	    receiver, 
	    receiver->class_pointer->name, 
	    sel_getName (sel));
    fflush (stdout);
  }
  
  return imp;
}


IMP 
objc_msgSendSuper (Super_t super, SEL sel)
{
  IMP	imp;


  assert (initialized);

  if (!(super->class->info & CLS_INITIALIZED))
    initialize_class (super->class->name);
  if (!(super->receiver->class_pointer->info & CLS_INITIALIZED))
    initialize_class (super->receiver->class_pointer->name);

  imp = get_imp (super->class, sel);
  
  if (!imp)
    imp = handle_runtime_error (super->receiver, sel);

  if (objc_trace) {
    printf ("trace: objc_msgSendSuper , obj=%#x, class=%s, method=%s\n",
	    super->receiver, 
	    super->receiver->class_pointer->name, 
	    sel_getName (sel));
    fflush (stdout);
  }

  return imp;
}


/*
 * This function is called by objc_msgSend or objc_msgSendSuper when a
 * message is sent to a object which it does not recognize. 
 */
static IMP  
handle_runtime_error (id object, SEL sel)
{
  IMP	imp;


  /*
   * If the object recognizes the doesNotRecognize: method then we're
   * going to send it. 
   */
  imp = get_imp (object->class_pointer, sel_getUid ("doesNotRecognize:"));
  if (imp)
    error_static = (*imp)(object, sel_getUid ("doesNotRecognize:"), sel);
  else {
    /*
     * The object doesn't recognize the method.  Check for
     * responding to error:.  If it does then sent it. 
     */
    char msg[256 + strlen (sel_getName (sel)) 
	     + strlen (object->class_pointer->name)];
        
    sprintf (msg, "%s does not recognize %s", 
	     object->class_pointer->name, sel_getName (sel));
        
    imp = get_imp (object->class_pointer, sel_getUid ("error:"));
    if (imp)
      error_static = (*imp)(object, sel_getUid ("error:"), msg);
    else {
      /*
       * The object doesn't respond to doesNotRecognize: or
       * error:;  Therefore, a default action is taken. 
       */
      fprintf (stderr, "%s\n", msg);
      abort ();
    }
  }

  /*
   * Either doesNotRecognize: or error: has been overridden.  We have
   * to return that value as the default action. 
   */
  return return_error_static;
}


/*
 * This function is used by the run-time to provide a method where nil
 * objects can receive messages. 
 *
 * This method simply returns self. 
 */
static id  
nil_method (id object, SEL sel, ...)
{
  return object;
}


/*
 * This function is used by the run-time to provide a method where nil
 * objects can receive messages. 
 *
 * This method simply returns self. 
 *
 * Note: multiple thread problem area. 
 */
static id  
return_error_static (id object, SEL sel, ...)
{
  return error_static;
}


/*
 * These variables provide a way for the defalut methods of object
 * allocation, destruction, and reallocation to be overridden. 
 */
id    (*_alloc)(Class_t)                  = objc_object_create;
id    (*_dealloc)(id)                     = objc_object_dispose;
id    (*_realloc)(id, unsigned int)              = objc_object_realloc;
id    (*_copy)(id)                        = objc_object_copy;
void  (*_error)(id, const char*, va_list) = objc_error;


id 
objc_object_create (Class_t class)
{
  id     object;


  assert (class);

  /*
   * Allocate memory for the object, initialize the memory to 0, and
   * set the object's class_pointer. 
   *
   * The object's class_pointer is the class's TEXT image.  It is used by
   * the messager as the key to the class hash for methods. 
   *
   * No need to initialize the class.  That was done in objcInit. 
   */
  object = (id) xcalloc (1, class->instance_size);
  object->class_pointer = class;

  return object;
}


id  
objc_object_dispose (id object)
{
  object->class_pointer = NULL;
  free (object);

  return nil;
}


id  
objc_object_realloc (id object, unsigned int length)
{
  id  obj;
  /* Can't resize a object smaller than its instance size.  */
  /* Don't use assert here;
     checks for user errors shouldn't depend on NDEBUG.  */
  if (length < object->class_pointer->instance_size)
    abort ();

  obj = (id) realloc (object, length);
  bzero ((char*)obj + object->class_pointer->instance_size,  
	 length - object->class_pointer->instance_size);
  
  return obj;
}


id  
objc_object_copy (id object)
{
  id  obj;
  obj = class_createInstance (object->class_pointer);
  bcopy (object, obj, objc_classSize (object));
  
  return obj;
}


void  
objc_error (id object, const char *fmt, va_list ap)
{
  vfprintf (stderr, fmt, ap);
  abort ();
}


/* Silly function to skip past a sequence of digits in a string.  */
static inline const char *
skip_digits (const char *str)
{
  while (isdigit (*str))
    ++str;

  return str;
}


unsigned int 
method_getNumberOfArguments (Method_t method)
{
  unsigned int       num = 0;
  const char *args = &method->method_types[1];
  
  
  while (*args) {
  
    /* Skip past size info.  */
    args = skip_digits (args);
    
    /* Argument type next.  */
    assert (*args);
    ++num;
    
    /* Step to next arg.  */
    ++args;
  }
  
  assert (num >= 2);
  return num;
}


unsigned int 
method_getArgumentInfo (Method_t method, int indx, const char **type, 
			int *offset)
{
  const char *args = skip_digits (&method->method_types[1]);
  int         i;

  
  assert (method_getNumberOfArguments (method) >= indx);

  /* Step to arg.  */
  for (i = 0; i < indx; ++i) {
    ++args;
    args = skip_digits (args);
  }
  
  /* Return arg data.  */
  *type = args++;
  *offset = atoi (args);
  
  return indx;
}


/* This function is not thread safe.  */
Ivar_t  
object_getIvarAddress (id object, const char *name)
{
  Class_t class = object->class_pointer; /* Here is the thread safe problem.  */
  Ivar_t  ivar = NULL;
  

  do {
    IvarList_t  ivars = class->ivars;
    int         i;
      
    /* Look at all of the ivar names.  */
    for (i = 0; i < ivars->ivar_count; ++i)
      if (!strcmp (name, ivars->ivar_list[i].ivar_name))
        ivar = &ivars->ivar_list[i];
           
    /*
     * If the ivar wasn't found then lets look to the
     * super class. 
     *
     * If the class is Object then the super class is NULL
     * and we're done. 
     */
    class = class->super_class;
        
  } while (!ivar && class);

  return ivar;
}


/*
 * Search for a method starting from the current class up its hierarchy.  
 *
 * Return a pointer to the method's method structure if found.  NULL otherwise. 
 */
Method_t  
searchForMethodInHierarchy (Class_t class, SEL sel)
{
  Method_t  	method = NULL;
  const char*	name;

  if (sel == 0)
    return NULL;

  name = sel_getName (sel);

  if (name == 0)
    return NULL;

  /*
   * Scan the method list of the class.  If the method isn't found in
   * the list then step to its super class. 
   */
  do {
    
    method = searchForMethodInList (class->methods, name);
    class = class->super_class;

  } while (!method && class);
    
  return method;
}


/*
 * Given a linked list of method and a method's name.  Search for the named
 * method's method structure. 
 *
 * Return a pointer to the method's method structure if found.  NULL otherwise. 
 */
Method_t  
searchForMethodInList (MethodList_t list, const char *name)
{
  MethodList_t  method_list = list;
  

  /* Check for bumbling.  */
  /* ??? Who generates the name?  Is it the user, or part of this file?
     If we crash here, whose fault is it?  */
  assert (name);

  /* If not found then we'll search the list.  */
  while (method_list) {
    int   i;
  
    /* Search the method list.  */
    for (i = 0; i < method_list->method_count; ++i) {
      Method_t method = &method_list->method_list[i];

      if (method->method_name)
	if (!strcmp (method->method_name, name))
	  return method;
    }
        
    /* The method wasn't found.  Follow the link to the next list of 
       methods.  */
    method_list = method_list->method_next;
  }
  
  return NULL;
}


/*
 * This function adds a method list to a class.  
 *
 * This function is typically called by another function specific to the
 * run-time.  As such this function does not worry about thread safe issued.  
 */
void  
addMethodsToClass (Class_t class, MethodList_t list)
{
  int i;
  
  
  /* Passing of a linked list is not allowed.  Do multiple calls.  */
  assert (!list->method_next);

  /* Check for duplicates.  */ 
  for (i = 0; i < list->method_count; ++i) {
    Method_t  method = &list->method_list[i];

    if (method->method_name)	/* Sometimes these are NULL */
      if (searchForMethodInList (class->methods, method->method_name)) {
	/*
	 * Duplication. Print a error message an change the
	 * method name to NULL. 
	 */
	fprintf (stderr, "attempt to add a existing method: %s\n",
		 method->method_name);
	method->method_name = NULL;
      }
  }
  
  /* Add the methods to the class's method list.  */
  list->method_next = class->methods;
  class->methods = list;
}


/*
 * This function removes the instance and factory methods in the passed list
 * from a class.  
 *
 * Methods are removed from a class by replacing the method's name with NULL. 
 *
 *
 * This function is typically called by another function specific to the
 * run-time.  As such this function does not worry about thread safe issued.  
 */
void  
class_removeMethods (Class_t class, MethodList_t method_list)
{
  int i;
  
  
  /* Passing of a linked list is not allowed.  Do multiple calls.  */
  assert (!method_list->method_next);

  /*
   * For each method in the list search the method lists erasing any
   * entries found. 
   */
  for (i = 0; i < method_list->method_count; ++i) {
    Method_t  kill_method = &method_list->method_list[i];
    Method_t  method;

    /* Remove any instance method found.  */
    method = searchForMethodInList (class->methods, 
				    kill_method->method_name);
    if (method)
      method->method_name = NULL;
      
    /* Remove any factory method found.  */
    method = searchForMethodInList (class->class_pointer->methods, 
				    kill_method->method_name);
    if (method)
      method->method_name = NULL;
  }
}


/*
 * This is a incomplete implementation of posing.   This function does the
 * bulk of the work but does not initialize the class method caches.  That is
 * a run-time specific operation. 
 *
 * I implement posing by hiding SUPER_CLASS, creating new class and meta
 * class structures, initializing it with IMPOSTOR, and changing it such
 * that it is identified as SUPER_CLASS. SUPER_CLASS remains in the
 * hierarchy but is inaccessible by the means. The class hierarchy is then re
 * arranged such that all of the subclasses of SUPER_CLASS now inherit from
 * the new class structures -- except the impostor itself. The only dramatic
 * effect on the application is that subclasses of SUPER_CLASS cannot do a 
 * [ ....  superClass ] and expect their real super class. 
 */
Class_t 
class_poseAs (Class_t impostor, Class_t super_class)
{
  Class_t     new_class = (Class_t) calloc (1, sizeof (Class));
  MetaClass_t new_meta_class = (MetaClass_t) calloc (1, sizeof (MetaClass));
  node_ptr node;
  char        *new_name = (char *) malloc (strlen (super_class->name) + 12);

  
  assert (new_class);
  assert (new_meta_class);
  assert (new_name);

  /* No dispatching while the the posing class is being built.
     The dispatch tables will be hacked on.  */
  MUTEX_LOCK (runtimeMutex);
	
  assert (impostor->info & CLS_CLASS);
  assert (super_class->info & CLS_CLASS);

  assert (impostor->instance_size == super_class->instance_size);

  /* Create the impostor class.  */
  new_class->class_pointer     = new_meta_class;
  new_class->super_class       = super_class;
  new_class->name              = super_class->name;
  new_class->version           = super_class->version;
  new_class->info              = super_class->info;
  new_class->instance_size     = super_class->instance_size;
  new_class->ivars             = super_class->ivars;
  new_class->methods           = impostor->methods;
  new_class->cache	      = &instance_method_record;
  
  /* Create the impostor meta class.  */
  new_meta_class->class_pointer = super_class->class_pointer->class_pointer;
  new_meta_class->super_class   = super_class->class_pointer->super_class;
  new_meta_class->name          = super_class->class_pointer->name;
  new_meta_class->version       = super_class->class_pointer->version;
  new_meta_class->info          = super_class->class_pointer->info;
  new_meta_class->instance_size = super_class->class_pointer->instance_size;
  new_meta_class->ivars         = super_class->class_pointer->ivars;
  new_meta_class->methods       = impostor->class_pointer->methods;
  new_meta_class->cache	      = &factory_method_record;

  /*
   * Delete the class from the hash table, change its name so that it
   * can no longer be found, then place it back into the hash table
   * using its new name. 
   *
   * Don't worry about the class number.  It is already assigned. 
   *
   * Don't worry about dangling pointers.  Life's a bitch.  (A little bit
   * of memory is lost with the hash key.)
   */
  hash_remove (class_hash_table, super_class->name);
  sprintf (new_name, "%s*", super_class->name);
  super_class->name       = new_name;
  super_class->class_pointer->name  = new_name;
  hash_add (&class_hash_table, super_class->name, super_class);
  
  /*
   * Now change all of the classes derived from super_class to be
   * derived from a impostor (except the impostor's impostor. 
   */
  for (node = hash_next (class_hash_table, NULL); node;
       node = hash_next (class_hash_table, node)) {
		
    Class_t	class1 = node->value;
    
    if (class1->super_class == super_class)
      if (class1 != impostor)
        class1->super_class = new_class;
  }

  /* Place the impostor class in class hash table
     and assign it a class number.  */
  addClassToHash (new_class);

  /* Reinitialize the dispatch tables.  */
  initialize_dispatch_tables ();

  MUTEX_UNLOCK (runtimeMutex);

  /* Print out class tables if debugging.  */
  DEBUG_PRINTF ("dump of class tables class_poseAs\n");
  debug_dump_classes ();

  return new_class;
}


/*
 * This routine is given a class and records all of the methods in its class
 * structure in the record table.  
 */
static void
record_methods_from_class (Class_t class)
{
  MethodList_t	method_list;
	
	
  method_list = class->methods;
  while (method_list) {
    record_methods_from_list (method_list);
    method_list = method_list->method_next;
  }
}


/*
 * This routine is given a list of methods and records each of the methods in
 * the record table.  This is the routine that does the actual recording
 * work. 
 */
static void
record_methods_from_list (MethodList_t method_list)
{
  int	i;
	
	
  for (i = 0; i < method_list->method_count; ++i) {
    Method_t method = &method_list->method_list[i];

    record_selector (method->method_name);
  }
}


SEL
sel_getUid (const STR name)
{
  int i;
	
	
  for (i = 1; i <= record_entries (selector_record); ++i)
    if (!strcmp (name, record_get (i, selector_record)))
      return (SEL)i;
	
  /* Unable to locate selector.  Return error value.  */
  return (SEL)0;
}


STR
sel_getName (SEL selector)
{
  return record_get ((unsigned int)selector, selector_record);
}


/*
 * Store the passed selector name in the selector record and return its
 * selector value (value returned by sel_getUid). 
 */
static SEL
record_selector (const char *sel)
{
  int j;
	
			
  /* Find either the selector in the table or an empty slot.  */
  for (j = 1; j <= record_entries (selector_record); ++j)
    if (!strcmp (sel,  record_get (j, selector_record)))
      return (SEL)j;
			
  /* Save the selector name.  */
  record_store (my_strdup (sel), selector_record);
  DEBUG_PRINTF ("Record: %s as: %#x\n", sel, j);

  return (SEL)j;		
}


/*
 * Initialize the dispatch tables.  This requires the initialization of the
 * instance_method_record and factory_method_record arrays and the arrays they
 * point to. 
 *
 * The first array is indexed by a class number.  Therefore its size is the
 * number of classes in the executable.  The second array is indexed by a
 * selector id.  Therefore its size is the number of unique selectors in the
 * application. 
 *
 * When a method is sent to a object its class number is extracted from the
 * class structure and used in the first array.  The selector id is used in
 * the second.  The result value is a method implementation. 
 */
static void
initialize_dispatch_tables (void)
{
  int	i;


  /* Check to make sure things are in place.  */
  assert (selector_record);

  /* Blow away the instance and factory method records.  */
  if (factory_method_record) {
    for (i = 1; i <= record_entries (factory_method_record); ++i)
      record_delete (record_get (i, factory_method_record));
    record_delete (factory_method_record);
  }
  if (instance_method_record) {
    for (i = 1; i <= record_entries (instance_method_record); ++i)
      record_delete (record_get (i, instance_method_record));
    record_delete (instance_method_record);
  }

  /* Reallocate the instance and factory method records.  */
  factory_method_record = record_new ();
  instance_method_record = record_new ();
  for (i = 1; i <= record_entries (selector_record); ++i) {
    record_store (record_new (), factory_method_record);
    record_store (record_new (), instance_method_record);
  }
	
  /* Fool all of the secondary records into thinking they have data.  */
  for (i = 1; i <= record_entries (selector_record); ++i) {
    struct record *record;
    node_ptr	node;
	
    record = record_get (i, factory_method_record);
    for (node = hash_next (module_hash_table, NULL); node;
	 node = hash_next (module_hash_table, node))
      record_store (NULL, record);
			
    record = record_get (i, instance_method_record);
    for (node = hash_next (module_hash_table, NULL); node;
	 node = hash_next (module_hash_table, node))
      record_store (NULL, record);
  }	
	
  /* For all classes fill in the methods implemented by the class and visiable
     from the class in the hierarchy.  Those methods are assigned to the
     class.  */
  for (i = 1; i <= record_entries (selector_record); ++i) { /* i is a sel */
    node_ptr	node;
	
    for (node = hash_next (class_hash_table, NULL); node;
	 node = hash_next (class_hash_table, node)) {
      Class_t     class = node->value;
      MetaClass_t meta_class = class->class_pointer;
      int	  class_number = getClassNumber (class);
      Method_t    method;

      /* DEBUG_PRINTF ("Assignment of sel=%s, class=%s (%#x, %#x)\n", 
	 sel_getName ((SEL)i), class->name,
	 searchForMethodInHierarchy (class, (SEL)i),
	 searchForMethodInHierarchy ((Class_t)meta_class, (SEL)i)); */

      method = searchForMethodInHierarchy (class, (SEL)i);
      if (method)
	record_store_at (class_number, method->method_imp,
			 record_get (i, instance_method_record));

      assert (class_number == getClassNumber ((Class_t)class->class_pointer));
      method = searchForMethodInHierarchy ((Class_t)meta_class, (SEL)i);
      if (method)
        record_store_at (class_number, method->method_imp,
                         record_get (i, factory_method_record));
    }
  }
}


/*
 * This method is called by the dispatch routines when a class has not been
 * initialized.  This method is responsible for initializing the class.  This
 * is accomplished by first testing the class itself for responding to the
 * +initialize method.  If such a method is implemented then it is called. 
 * Before exit, irregardless if the class implements +initialize, the class
 * is marked as initialized. 
 */
static void		
initialize_class (const char *name)
{
  Method_t	method = NULL;
  Class_t	class = objc_getClass (name);
  SEL		sel = sel_getUid ("initialize");

	
  /* The class should not be initialized at this point.  */
  assert (!(class->info & CLS_INITIALIZED));
  assert (!(class->class_pointer->info & CLS_INITIALIZED));

  /* Search for the +initialize method.
     Call it if it exists.  */
  if (sel)
    method = searchForMethodInList (class->class_pointer->methods,
				    sel_getName (sel));
  if (method) {
    IMP	imp;

    DEBUG_PRINTF ("Class: %s sending +%s\n", 
		  name, sel_getName (sel));
    imp = get_imp ((Class_t)class->class_pointer, sel);
    assert (imp);
    (*imp)((id)class, sel);
  }

  /* Mark the class as initialized.  */
  class->info	|= CLS_INITIALIZED;
  class->class_pointer->info	|= CLS_INITIALIZED;
}


/*
 * Silly little function that checks to make sure the class hash table is
 * initialized.  If it isn't initialized then do it. 
 */
static inline void
class_hash_init (void)
{
  static unsigned int	init = 0;
	
	
  if (!init)
    class_hash_table = hash_new (CLASS_HASH_SIZE, 
				 (hash_func_type)hash_string,
				 (compare_func_type)compare_strings);
  init = 1;
}


Class_t 
objc_getClass (const char *name)
{
  Class_t	class;


  /* Make sure the class hash table exists.  */
  class_hash_init ();

  class = hash_value_for_key (class_hash_table, name);
	
  return class;
}


MetaClass_t 
objc_getMetaClass (const char *name)
{
  /* Meta classes are pointed to by the class's class_pointer.
     Just get the class and return its class_pointer.  */
  return (objc_getClass (name))->class_pointer;
}


void
addClassToHash (Class_t class)
{
  Class_t	hClass;
	
	
  class_hash_init ();

  /* Check to see if the class is already in the hash table.  */
  hClass = hash_value_for_key (class_hash_table, class->name);
  if (!hClass) {
    
    /* The class isn't in the hash table.  Add the class and 
       assign a class number.  */
    static unsigned int	class_number = 1;
	
    setClassNumber (class, class_number);
    setClassNumber ((Class_t)class->class_pointer, class_number);
    ++class_number;
    
    hash_add (&class_hash_table, class->name, class);
  }
}


void  
debug_dump_classes (void)
{
  node_ptr node;
  int         i;


  DEBUG_PRINTF ("class tables\n");
  i = 0;
  for (node = hash_next (class_hash_table, NULL); node; 
       node = hash_next (class_hash_table, node)) {

    Class_t class = node->value;
      
    DEBUG_PRINTF ("Class { /*%#x*/\n", class);
    DEBUG_PRINTF ("   MetaClass_t  class_pointer = %#x\n", class->class_pointer);
    DEBUG_PRINTF ("   Class_t      super_class   = %#x\n", class->super_class);
    DEBUG_PRINTF ("   char         *name          = %s\n", class->name);
    DEBUG_PRINTF ("   long         version       = %ld\n", class->version);
    DEBUG_PRINTF ("   long         info          = %#x\n", class->info);
    DEBUG_PRINTF ("   long         instance_size = %ld\n", class->instance_size);
    DEBUG_PRINTF ("   IvarList_t   ivars         = %#x\n", class->ivars);
    DEBUG_PRINTF ("   MethodList_t methods       = %#x\n", class->methods);
    DEBUG_PRINTF ("   cache_ptr      cache         = %#x\n", class->cache);
    DEBUG_PRINTF ("}[%d];\n", i++);
  }
    
  i = 0;
  for (node = hash_next (class_hash_table, NULL); node; 
    node = hash_next (class_hash_table, node)) {

    Class_t class = (Class_t)((Class_t)(node->value))->class_pointer;
      
    DEBUG_PRINTF ("MetaClass { /*%#x*/\n", class);
    DEBUG_PRINTF ("   MetaClass_t  class_pointer = %#x\n", class->class_pointer);
    DEBUG_PRINTF ("   MetaClass_t  super_class   = %#x\n", class->super_class);
    DEBUG_PRINTF ("   char         *name          = %s\n", class->name);
    DEBUG_PRINTF ("   long         version       = %ld\n", class->version);
    DEBUG_PRINTF ("   long         info          = %#x\n", class->info);
    DEBUG_PRINTF ("   long         instance_size = %ld\n", class->instance_size);
    DEBUG_PRINTF ("   IvarList_t   ivars         = %#x\n", class->ivars);
    DEBUG_PRINTF ("   MethodList_t methods       = %#x\n", class->methods);
    DEBUG_PRINTF ("   cache_ptr      cache         = %#x\n", class->cache);
    DEBUG_PRINTF ("}[%d];\n", i++);
  }
}

