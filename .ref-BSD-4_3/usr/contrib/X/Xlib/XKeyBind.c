#include <X/mit-copyright.h>

/* $Header: XKeyBind.c,v 10.8 86/02/01 15:35:46 tony Rel $ */
/* Copyright 1985, Massachusetts Institute of Technology */

#include "XlibInternal.h"
#include <sys/file.h>
#include <sys/stat.h>
#include "Xkeymap.h"
#include "Xkeyboard.h"
#include <stdio.h>
#include <strings.h>

#define EMPTY_ENTRY LeftMask 
   /* if the "metabits" field of a runtime table entry contains this,
    it's an empty entry */

static KeyMapElt *keymap = NULL;
static Bool inited = FALSE;

static ExtensionHeader *ext_begin, *ext_end;

/* Runtime table: contains multiple-byte character bindings defined
  at runtime with XRebindCode */

typedef struct {
    unsigned char keycode;
    unsigned char metabits;
    short length;
    char *value;
    } RuntimeTableEntry;

static RuntimeTableEntry
   *rt_begin,  /* first entry of runtime table */
   *rt_end,    /* this and all succeeding entries are empty */
   *rt_buf_end;/* points beyond end of allocated storage for table */

#define RT_INITIAL_SIZE 100  /* initial size of runtime table */
#define RT_INCREMENT 40  /* size to grow by if expanded */

static Initialize() {
    int file = -1;
    int filesize;
    unsigned char magic;
    struct stat filestat;
    char *getenv();
    char *filename;
    char *home = getenv ("HOME");
    inited = TRUE;
    if (home) {
	int homelen = strlen (home);
	char *keymapstr = "/.Xkeymap";
	int keymapstrlen = strlen (keymapstr);
	filename = malloc (homelen + keymapstrlen + 1);
	strncpy (filename, home, homelen+1);
	strncat (filename, keymapstr, keymapstrlen);
	file = open (filename, O_RDONLY, 0);
	}
    if (file < 0) {
	free (filename);
    	return;	    /* no keymap file found */
	}
    fstat (file, &filestat);
    filesize = filestat.st_size - 1; /* first byte is magic number */
    if (filesize < 256*sizeof(KeyMapElt)) {
	fprintf (stderr, "Keymap file %s is too small\n", filename);
	close (file);
	free (filename);
	return;
	}
    read (file, &magic, 1);
    if (magic != X_KEYMAP_MAGIC) {
	fprintf (stderr, 
	  "Keymap file %s doesn't begin with the proper magic number\n",
	  filename);
        close (file);
	free (filename);
      	return;
	}
    keymap = (KeyMapElt *) malloc (filesize);
    if (!keymap) {
	close (file);
	free (filename);
      	return;  /* couldn't malloc; just act like there isn't a keymap */
	}
    read (file, (char *) keymap, filesize);
    ext_begin = (ExtensionHeader *) (keymap + 256);
    ext_end = (ExtensionHeader *) (((char *) keymap) + filesize);
    rt_begin = (RuntimeTableEntry *) malloc (RT_INITIAL_SIZE*sizeof(RuntimeTableEntry));
    if (!rt_begin)
     	_XIOError (_XlibCurrentDisplay);
    rt_end = rt_begin;
    rt_buf_end = rt_begin + RT_INITIAL_SIZE;
    free (filename);
    close (file);
    }

/* this routine is used when initialization failed to find a
   valid keymap file */
static char *BackstopLookupMapping (event, nbytes)
    XKeyPressedEvent *event;
    int *nbytes;
    {
    int detail = event->detail;
    register int keycode = detail & ValueMask;
    extern KeyMapEntry StdMap[];
    static char c;
    short s;  /* needed to distinguish a real character (e.g. \0377) from -1 */
    s = StdMap [keycode] [KeyState(detail)];
    c = s;
    if ((detail & ShiftLockMask) && (c >= 'a') && (c <= 'z'))
    	c += ('A' - 'a');
    if (IsTypewriterKey(keycode)
      || keycode == KC_ESC || keycode == KC_BS || keycode == KC_LF)
	*nbytes = (s == -1 ? 0 : 1);
    else
    	*nbytes = 0;
    return (&c);
    }

char *XLookupMapping (event, nbytes)
    XKeyPressedEvent *event;
    int *nbytes;
    {
    int detail = event->detail;
    unsigned int metabits = FullKeyState (detail);
    unsigned int key = detail & ValueMask;
    register unsigned char *the_char;

    if (!inited)
      	Initialize();
    if (!keymap)
    	return (BackstopLookupMapping (event, nbytes));

    the_char = &keymap [key] [metabits];

    switch (*the_char) {

      	case UNBOUND: {
	    *nbytes = 0;
	    return (NULL);
	    }

      	case EXTENSION_BOUND: {
	    register ExtensionHeader *this;
	    for (this = ext_begin; this < ext_end; NextExtension(this))
	        if ((key == this->keycode)
	        && ((metabits == this->metabits) || (this->metabits == DontCareMetaBits))) {
	            *nbytes = this->length;
	            return ((char *)this + ExtensionHeaderSize);
	            }
	    /* if we get here, no match was found in the table extension */
	    *nbytes = 0;
	    return (NULL);
	    }

        case RUNTIME_TABLE_BOUND: {
	    register RuntimeTableEntry *entry;
	    for (entry = rt_begin; entry < rt_end; entry++)
	        if ((key == entry->keycode)
	        && ((metabits == entry->metabits) || (entry->metabits == DontCareMetaBits))) {
		    *nbytes = entry->length;
		    return (entry->value);
		    }

	    /* if we get here, no match was found in the runtime table */
	    *nbytes = 0;
	    return (NULL);
	    }

	default: {
	    *nbytes = 1;
	    return ((char *)the_char);
	    }
      	}

    }


XRebindCode (keycode, metabits, str, nbytes)
    unsigned int keycode, metabits;
    char *str;
    int nbytes;
    {
    unsigned char *table_char;
    metabits = FullKeyState (metabits);  /* shift meta bits to rightmost four bits */
    if (!inited)
    	Initialize();
    if (!keymap)
    	return;  /* no keymap file; what else can I do? */
    table_char = &keymap [keycode] [metabits];
    if (nbytes == 0) {
	if (*table_char == RUNTIME_TABLE_BOUND)
	    Unbind (keycode, metabits);
	*table_char = UNBOUND;
	return;
	}
    if ((nbytes == 1) && SingleCharBound (*str)) {
    	if (*table_char == RUNTIME_TABLE_BOUND)
	    Unbind (keycode, metabits);
	*table_char = *str;
	return;
	}
    
    /* the new binding is either multi-character, or one of the
       three reserved special characters */

    if (*table_char == RUNTIME_TABLE_BOUND) {
	/* entry is already in table; just change its binding */
	register RuntimeTableEntry *entry;
	for (entry = rt_begin; entry < rt_end; entry++)
	    if (keycode == entry->keycode && metabits == entry->metabits) {
		entry->value = str;
		entry->length = nbytes;
		return;
		}
	/* if we get here, entry wasn't found in table; shouldn't
	 * ever happen!  Not much to do but fall through to 
	 * the following code.  */
    	}

    /* new binding must go in a new entry in the table */
    *table_char = RUNTIME_TABLE_BOUND;
    if (rt_end < rt_buf_end) {
	rt_end->keycode = keycode;
	rt_end->metabits = metabits;
	rt_end->value = str;
	rt_end++->length = nbytes;
	return;
	}

    /* no room at end of table; look for holes in middle */
    {
    register RuntimeTableEntry *entry;
    for (entry = rt_begin; entry < rt_end; entry++)
    	if (entry->metabits == EMPTY_ENTRY) {
	    entry->keycode = keycode;
	    entry->metabits = metabits;
	    entry->value = str;
	    entry->length = nbytes;
	    return;
	    }
    }

    /* no room in table at all.  Must expand it. */
    {
    int rt_length = rt_end - rt_begin;
    realloc (&rt_begin, (rt_length+RT_INCREMENT)*sizeof (RuntimeTableEntry));
    rt_end = rt_begin + rt_length;
    rt_buf_end = rt_end + RT_INCREMENT;
    rt_end->keycode = keycode;
    rt_end->metabits = metabits;
    rt_end->value = str;
    rt_end++->length = nbytes;
    }
    }
    

static Unbind (keycode, metabits)
    unsigned int keycode, metabits;
    {
    register RuntimeTableEntry *entry;
    for (entry = rt_begin; entry < rt_end; entry++)
    	if (keycode == entry->keycode && metabits == entry->metabits) {
	    entry->metabits = EMPTY_ENTRY;
	    return;
	    }
    }
