#include <X/mit-copyright.h>

/* $Header: Xkeymap.h,v 10.5 86/02/01 15:41:53 tony Rel $ */
/* Copyright 1985, Massachusetts Institute of Technology */

#define X_KEYMAP_MAGIC 0372  /* magic number which must be first byte
    	    	    	    	of a keymap file */

/* KeyMap: contains single-byte character bindings, or indications
   that a keycode is actually bound in the extension or runtime table */

typedef unsigned char KeyMapElt [16];

#define UNBOUND (unsigned char)'\377'
#define EXTENSION_BOUND (unsigned char)'\376'
#define RUNTIME_TABLE_BOUND (unsigned char)'\375'
#define SingleCharBound(c) ((unsigned char)c < (unsigned char)'\375')

/* Extension: contains multiple-byte character bindings from
   the keymap file.  Not modified at runtime.  */

typedef struct {
    unsigned char keycode;
    unsigned char metabits;
    unsigned char length;
    } ExtensionHeader;

#define ExtensionHeaderSize 3
    /* since sizeof (ExtensionHeader) is 4 on some machines, e.g. Sun */

/* macro used to iterate through the extension */
#define NextExtension(this) \
  this = (ExtensionHeader *) ((char *)this + ExtensionHeaderSize + this->length)

#define DontCareMetaBits 0377
  /* appears in extension whenever the binding applies to all possible
     combinations of shift/lock/meta/control keys */
