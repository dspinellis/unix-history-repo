/* Declare functions used within Objective C runtime support.
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
 

#ifndef __objc_proto_INCLUDE_GNU
#define __objc_proto_INCLUDE_GNU

/* This used to be #ifndef __OBJC__, but it turns out that
   object.m needs these declarations.  I don't understand why one
   might want to avoid them in object.m.  */

#if 1
/*
 * objc_getClass returns the id of the class 
 *  object for the aClassName class.   The class 
 *  object holds information used by instances of 
 *  the class.  
 *
 * Print a message to the standard error stream if 
 *  aClassName isn't part of the executable image.
 */
Class_t objc_getClass (const char *);

/*
 * objc_getMetaClass returns the id of the 
 *  meta class object for the aClassName class.  
 *  The meta class object holds information used 
 *  by the class object, just as the class 
 *  object holds information used by instances 
 *  of the class.  
 *
 * Print a message to the standard error stream 
 *  if aClassName isn't part of the executable image.
 */
MetaClass_t objc_getMetaClass (const char *);

/*
 * The compiler converts every message expression into a 
 *  call on one of these two functions.  Messages to 
 *  super are converted to calls on objc_msgSendSuper; 
 *  all others are converted to calls on objc_msgSend.
 *
 * These functions return the address of the method 
 *  implementation.  The compiler then generates calls
 *  to those methods passing the full argument array.
 *
 * Calls to objc_msgSend and objc_msgSendSuper 
 *  should be generated only by the compiler.  You shouldn't 
 *  call them directly in the Objective-C code you write.
 */
IMP objc_msgSend (id, SEL);

IMP objc_msgSendSuper (Super_t, SEL);
#endif

/*
 * Given the name of a variable within a class's 
 *  definition, return a pointer to a structure that
 *  describes it.
 */
Ivar_t object_getIvarAddress (id obj, const char *variableName);

/*
 * Given a class and a selector, return a pointer to the method's method
 * structure.  Return NULL if not found. 
 *
 * This is a method internal to the run-time. 
 */
Method_t searchForMethodInHierarchy (Class_t, SEL);


/*
 * The first function, sel_getUid, returns a selector that's 
 *  used at run time to identify the aName method.  Whenever 
 *  possible, you should use the @selector directive to 
 *  ask the compiler, rather than the run-time system, 
 *  to provide the selector for a method.  This function 
 *  should be used only if the name isn't known at compile 
 *  time.
 *
 * The second function, sel_getName, is the inverse 
 *  of the first.  It returns the name that was mapped to 
 *  aSelector.
 */
SEL sel_getUid (const STR);

const STR sel_getName (SEL);

/* 
 * This function returns the number of arguments that METHOD
 *  the takes.  This will be at least two, since it 
 *  includes the ªhiddenº arguments, self and _cmd, 
 *  which are the first two arguments passed to every 
 *  method implementation.
 */
unsigned int method_getNumberOfArguments (Method_t method);

/* This functiontakes an index into METHOD's argument 
 *  list and returns, by reference, the type of the argument 
 *  and the offset to the location of that argument in the 
 *  list.  Indices begin with 0.  The ªhiddenº arguments 
 *  self and _cmd are indexed at 0 and 1; method-specific 
 *  arguments begin at index 2.  The offset is measured in 
 *  bytes and depends on the size of arguments preceding the 
 *  indexed argument in the argument list.  The type is 
 *  encoded according to the conventions of the @encode 
 *  compiler directive.
 */
unsigned int method_getArgumentInfo (Method_t, int indx,
				     const char **type, int *offset);

/*
 * This function is used to support archiving when a unknown class is to read
 *  from a archive.  This function returns a instantiated object.  To further
 *  dearchive the object it should be sent: -readFrom:.
 *
 * This function positions the file pointer just past class Object's class
 *  data.
 */
id objc_objectFromFile (int fd);

/*
 * class_getInstanceMethod returns a pointer 
 *  to the data structure that describes the method.  
 *
 * The selector must identify an 
 *  instance method.
 *
 * Return a NULL pointer if SEL doesn't 
 *  identify a method defined in or inherited 
 *  by CLASS.
 */
static inline Method_t  
class_getInstanceMethod (Class_t class, SEL sel)
{
  return searchForMethodInHierarchy (class, sel);
}

/*
 * class_getClassMethod returns a pointer to 
 *  the data structure that describes the method.  
 *
 * The selector must identify a class (factory) method.  
 *
 * Return a NULL pointer if SEL doesn't 
 *  identify a method defined in or inherited by CLASS.
 */
static inline Method_t  
class_getClassMethod (MetaClass_t class, SEL sel)
{
  return searchForMethodInHierarchy ((Class_t)class, sel);
}

/*
 * This function returns the name of OBJ's 
 *  class.  anObject should be an instance 
 *  object, not a class object.
 */
static inline const char * 
object_getClassName (id obj)
{
  return obj->class_pointer->name;
}

/*
 * This function returns the name of the 
 *  class. 
 */
static inline const char *
class_getClassName (Class_t class)
{
  return class->name;
}

/*
 * Add a class to the class hash table and assign it a class number. 
 */
void addClassToHash (Class_t class);

/*
 * This function takes a list of methods and adds them to the method list of
 * a class.  The method list must remain intact during the lifetime of the
 * class. 
 */
void addMethodsToClass (Class_t, MethodList_t);

/*
 * This function creates a new instance of CLASS, initializes its class_pointer
 * instance variable to point to the class, and return the new instance.  
 *
 * All other instance variables are initialized to 0. 
 */
static inline id  
class_createInstance (Class_t class)
{
  return (*_alloc)(class);
}

/*
 * object_dispose frees the memory occupied by OBJ after setting its
 * class_pointer instance variable to nil, and returns nil.  The function it calls to
 * do this work can be changed by reassigning the _dealloc variable. 
 */
static inline id  
object_dispose (id obj)
{
  return (*_dealloc)(obj);
}

/*
 * object_copy creates a new object that's an exact copy of anObject and
 * return the new object.  The second argument, indexedIvarBytes, specifies
 * the number of additional bytes that should be allocated for the copy to
 * accommodate indexed instance variables; it serves the same purpose as the
 * second argument to class_createInstance.  The function that
 * object_copy calls to do this work can be changed by reassigning the
 * _copy variable. 
 */
static inline id  
object_copy (id obj)
{
  return (*_copy)(obj);
}

/*
 * object_realloc reallocates storage for anObject, adding numBytes if
 * possible.  The memory previously occupied by anObject is freed if it can't
 * be reused, and a pointer to the new location of anObject is returned.  The
 * function that object_realloc calls to do this work can be changed by
 * reassigning the _realloc variable. 
 */
static inline id  
object_realloc (id obj, unsigned int size)
{
  return (*_realloc)(obj, size);
}

/*
 * This function causes one class to pose as its super class.  Messages sent
 * to the super class will actually be sent to the posing class. 
 *
 * Instance variables should not be declared in the posing class.  The posing
 * class can add new methods to the class or override existing methods in the
 * super class. 
 */
Class_t class_poseAs (Class_t, Class_t);


/* These functions set and return the class version number. */
static inline void 
class_setVersion (Class_t class, long version)
{
  class->version = version ;
  class->class_pointer->version = version ;
}

static inline long
class_getVersion (Class_t class)
{
  return class->version ;
}


/*
 * Class numbers are stored in the class's info variable. This is temporary. 
 * Eventually we will allocate a member to the class so that some efficiency
 * can be gained by not shifting. 
 */
#define	CLASS_LOCATION_SHIFT (HOST_BITS_PER_LONG / 2)

static inline void
setClassNumber (Class_t class, unsigned long number)
{
  class->info |= number << CLASS_LOCATION_SHIFT;
}

static inline unsigned long
getClassNumber (Class_t class)
{
  return class->info >> CLASS_LOCATION_SHIFT;
}

/*
 * class_addInstanceMethods adds methods that can be 
 *  used by instances of the class and class_addClassMethods 
 *  adds methods used by the class object.  Before adding a 
 *  method, both functions check for duplicates.  A warning 
 *  is sent to the standard error stream if any ambiguities exist.
 *
 * The passed structure and its contents must exist for the the
 *  duration of the program.  These functions don't support
 *  linked structures.
 */
static inline void  
class_addInstanceMethods (Class_t class, MethodList_t method_list)
{
  addMethodsToClass (class, method_list);
}

static inline void  
class_addClassMethods (Class_t class, MethodList_t method_list)
{
  addMethodsToClass ((Class_t)class->class_pointer, method_list);
}

/*
 * This function returns the number of bytes that all of METHOD's
 *  arguments, taken together, would occupy on the stack.
 */
static inline unsigned int 
method_getSizeOfArguments (Method_t method)
{
  return atoi (&method->method_types[1]);
}

/*
 * This function returns the size in bytes of a 
 *  instance of OBJ.
 */
static inline long
objc_classSize (id obj)
{
  return obj->class_pointer->instance_size;
}

/* Some functions that I've been told are useful by Henry Flurry */

/* Returns a C string representing the ASCII rep of the selector. */
static inline const char *
SELNAME (SEL sel)
{
  return sel_getName (sel);
}

/*
 * Converts a C string to a SEL that can be used in perform: methods,
 * objc_msgSend, etc. 
 */
static inline SEL
SELUID (const STR str)
{
  return sel_getUid (str);
}

/*
 * Returns the class name of the object (or meta class name of a class
 * object), or some _nilName if obj is nil. 
 */
static inline const char *
NAMEOF (id obj)
{
  const char *name = 0;

  if (obj)
    {
      if (obj->class_pointer->info & CLS_CLASS)
	name = object_getClassName (obj);
      else if (((Class_t)obj)->class_pointer->info & CLS_META)
	name = class_getClassName ((Class_t)obj);
    }

  return name;
}

/*
 * These functions add and remove methods in a list from a class.  These
 * functions perform the actual work required for those functions.  
 *
 * The appropriate run-time is to provide the user callable functions to
 * perform these functions.  Typically those functions perform something
 * specific to their run-time type and call these functions to perform the 
 * actual work.
 */ 
void class_removeMethods (Class_t class, MethodList_t method_list);

/*
 * Find the named method in a linked list of methods. 
 */
Method_t searchForMethodInList (MethodList_t list, const char *name);

/*
 * printf is used if we're debugging.  If DEBUG isn't defined then this
 * def isn't defined thereby causing the compiler to eliminate the parameter
 * decl. 
 */
#ifdef DEBUG
#define DEBUG_PRINTF  printf
#else
#define DEBUG_PRINTF
#endif


/*
 * Function that dumps information about all of the classes to stdout. 
 */
void debug_dump_classes (void);

#endif /* not __objc_proto_INCLUDE_GNU */

