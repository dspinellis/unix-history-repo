/* This file contains the implementation of class Object.
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
 

#include "tconfig.h"
#include "gstdarg.h"
#include "object.h"
#include "objc-proto.h"

#include <errno.h>
#ifndef errno
extern int errno;
#endif

#define CLASS(class)  ((Class_t)class)


@implementation Object


+ new
{
  return class_createInstance (CLASS (self));
}


+ free {  return nil; }
- free {  return object_dispose (self); }


- copy {  return [self shallowCopy]; }


- shallowCopy
{
  return object_copy (self);
}


- deepCopy
{
  return class_createInstance ([self class]);
}


+ (Class_t)class      { return CLASS (self); }
+ (Class_t)superClass { return CLASS (self)->super_class; }
- (Class_t)class      { return isa; }
- (Class_t)superClass { return isa->super_class; }
- (const char *)name  { return object_getClassName (self); }
- self                { return self; }


- (unsigned int)hash
{
  return (unsigned int)self; /* gak!  Not portable. */
}


- (BOOL)isEqual:anObject
{
  return self == anObject ;
}


- (BOOL)isKindOf:(Class_t)aClassObject
{
  Class_t class;

  for (class = isa; class; class = class->super_class)
    if (class == aClassObject)
      return YES;

  return NO;
}


- (BOOL)isMemberOf:(Class_t)aClassObject
{
  return isa == aClassObject ;
}


- (BOOL)isKindOfGivenName:(const char*)aClassName
{
  Class_t   class;

  for (class = isa; class; class = class->super_class)
    if (!strcmp (class_getClassName (class), aClassName))
      return YES;

  return NO;
}


- (BOOL)isMemberOfGivenName:(const char*)aClassName
{
  return !strcmp ([self name], aClassName);
}


+ (BOOL)instancesRespondTo:(SEL)aSel
{
  if (class_getInstanceMethod (CLASS (self), aSel))
    return YES;
  
  return NO;
}


- (BOOL)respondsTo:(SEL)aSel
{
  if (class_getInstanceMethod (isa, aSel))
    return YES;

  return NO;
}


- perform:(SEL)aSel
{
  return (*((IMP)objc_msgSend (self, aSel)))(self, aSel);
}


- perform:(SEL)aSel with:aObject
{
  return (*((IMP)objc_msgSend (self, aSel)))(self, aSel, aObject);
}


+ poseAs:(Class_t)aClassObject
{
  return class_poseAs (self, aClassObject);
}

 
- subclassResponsibility:(SEL)aSel
{
  return [self error:"subclass should override %s", aSel];
}


- notImplemented:(SEL)aSel
{
  return [self error:"method %s not implemented", aSel];
}


- doesNotRecognize:(SEL)aSel
{
  return [self error:"%s does not recognize %s", [self name], aSel];
}

- error:(const char*)aString, ...
{
#define FMT "error: %s (instance)\n%s\n"

  char  fmt[strlen (FMT)
	    + strlen (isa->name)
	    + strlen (aString) + 8];
  va_list ap;
  
  sprintf (fmt, FMT, isa->name, aString);
  va_start (ap, aString);
  (*_error)(self, fmt, ap);
  va_end (ap);

#undef FMT
  return self;
}


+ error:(const char*)aString, ...
{
#define FMT "error: %s (class)\n%s\n"

  char  fmt[strlen (FMT)
	    + strlen (isa->name)
	    + strlen (aString) + 8];
  va_list ap;

  sprintf (fmt, FMT, isa->name, aString);
  va_start (ap, aString);
  (*_error)(self, fmt, ap);
  va_end (ap);

#undef FMT
  return self;
}


- storeOn:(int)aFd
{
  int   len;
  long  version = [[self class] version];
  
  if ((len = write (aFd, "#", strlen ("#"))) != -1)
    if ((len = write (aFd, [self name], strlen ([self name]) + 1)) != -1)
      len = write (aFd, &version, sizeof (version));
  
  if (len == -1)
    [self error:"error passivating object, errno=%d", errno];

  return self;
}


+ readFrom:(int)aFd
{
  id    aObj = nil;
  char  objName[256];
  int   len;
  
  
  if ((len = read (aFd, &objName, strlen ("#"))) != -1)
    if (objName[0] == '#') {
      long  version;
      int   i = 0;
      
      /* Read the object's class. */
      do {
        len = read (aFd, &objName[i], sizeof (char));
      } while (objName[i++] && (len != -1));
    
      /* Get its version. */
      if (len != -1)
        len = read (aFd, &version, sizeof (version));
      
      /* No errors???
	 Then create a object. */
      if (len != -1) {
	aObj = class_createInstance (objc_getClass (objName));
          
	/* If the object was 
	   successfully created then
	   tell it to dearchive
	   itself. */
	if (aObj)
	  [aObj readFrom:aFd];
      }
    }
    
  if (len == -1)
    [self error:"error activating object, errno=%d", errno];
  
  return aObj;
}


- readFrom:(int)aFd { return self; }


+ (int)version
{
  return class_getVersion (CLASS (self));
}


+ setVersion:(int)aVersion
{
  class_setVersion (CLASS (self), aVersion);
  
  return self;
}


@end


