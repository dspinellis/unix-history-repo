/* Manipulation of keymaps
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#include "config.h"
#include <stdio.h>
#undef NULL
#include "lisp.h"
#include "commands.h"
#include "buffer.h"

/* Actually allocate storage for these variables */

#ifdef HAVE_X_WINDOWS
struct Lisp_Vector *MouseMap;		/* Keymap for mouse commands */
#endif /* HAVE_X_WINDOWS */

struct Lisp_Vector *CurrentGlobalMap;	/* Current global keymap */

struct Lisp_Vector *GlobalMap;	/* default global key bindings */

struct Lisp_Vector *ESCmap;		/* The keymap used for globally
					   bound ESC-prefixed default
					   commands */

struct Lisp_Vector *CtlXmap;		/* The keymap used for globally
					   bound C-x-prefixed default
					   commands */

/* was MinibufLocalMap */
Lisp_Object Vminibuffer_local_map;
				/* The keymap used by the minibuf for local
				   bindings when spaces are allowed in the
				   minibuf */

/* was MinibufLocalNSMap */
Lisp_Object Vminibuffer_local_ns_map;			
				/* The keymap used by the minibuf for local
				   bindings when spaces are not encouraged
				   in the minibuf */

/* keymap used for minibuffers when doing completion */
/* was MinibufLocalCompletionMap */
Lisp_Object Vminibuffer_local_completion_map;

/* keymap used for minibuffers when doing completion and require a match */
/* was MinibufLocalMustMatchMap */
Lisp_Object Vminibuffer_local_must_match_map;

Lisp_Object Qkeymapp, Qkeymap;

DEFUN ("make-keymap", Fmake_keymap, Smake_keymap, 0, 0, 0,
  "Construct and return a new keymap, a vector of length 128.\n\
All entries in it are nil, meaning \"command undefined\".")
  ()
{
  Lisp_Object val;
  XFASTINT (val) = 0200;
  return Fmake_vector (val, Qnil);
}

DEFUN ("make-sparse-keymap", Fmake_sparse_keymap, Smake_sparse_keymap, 0, 0, 0,
  "Construct and return a new sparse-keymap list.\n\
Its car is 'keymap and its cdr is an alist of (CHAR . DEFINITION).\n\
Initially the alist is nil.")
  ()
{
  return Fcons (Qkeymap, Qnil);
}

/* This is used for installing the standard key bindings at initialization time.
 For example,  defkey (CtlXmap, Ctl('X'), "exchange-point-and-mark");  */

void
defkey (keymap, key, defname)
     struct Lisp_Vector *keymap;
     int key;
     char *defname;
{
  keymap->contents[key] = intern (defname);
}

void
ndefkey (keymap, key, defname)
     Lisp_Object keymap;
     int key;
     char *defname;
{
  store_in_keymap (keymap, key, intern (defname));
}

/* Define character fromchar in map frommap as an alias for character tochar in map tomap.
 Subsequent redefinitions of the latter WILL affect the former. */

#ifdef NOTDEF
void
synkey (frommap, fromchar, tomap, tochar)
     struct Lisp_Vector *frommap, *tomap;
     int fromchar, tochar;
{
  Lisp_Object v, c;
  XSET (v, Lisp_Vector, tomap);
  XFASTINT (c) = tochar;
  frommap->contents[fromchar] = Fcons (v, c);
}
#endif /* NOTDEF */

DEFUN ("keymapp", Fkeymapp, Skeymapp, 1, 1, 0,
  "Return t if ARG is a keymap.\n\
A keymap is a vector of length 128, or a list (keymap . ALIST),\n\
where alist elements look like (CHAR . DEFN).")
  (object)
     Lisp_Object object;
{
  Lisp_Object tem;
  tem = object;
  while (XTYPE (tem) == Lisp_Symbol)
    {
      tem = XSYMBOL (tem)->function;
      if (EQ (tem, Qunbound))
	return Qnil;
    }

  if ((XTYPE (tem) == Lisp_Vector && XVECTOR (tem)->size == 0200)
      || (LISTP (tem) && EQ (XCONS (tem)->car, Qkeymap)))
    return Qt;
  return Qnil;
}

Lisp_Object
get_keymap (object, argnumber)
     Lisp_Object object;
     int argnumber;
{
  Lisp_Object tem;

  while (1)
    {
      tem = object;
      while (XTYPE (tem) == Lisp_Symbol && !EQ (tem, Qunbound))
	tem = XSYMBOL (tem)->function;

      if ((XTYPE (tem) == Lisp_Vector && XVECTOR (tem)->size == 0200)
	  || (LISTP (tem) && EQ (XCONS (tem)->car, Qkeymap)))
	return tem;
      if (argnumber >= 0)
	object = wrong_type_argument (Qkeymapp, object);
      else return Qnil;
    }
}

Lisp_Object
get_keyelt (object)
     Lisp_Object object;
{
  Lisp_Object map, tem;

  while (map = get_keymap (Fcar_safe (object), -1),
	 tem = Fkeymapp (map),
	 !NULL (tem))
      /*(XTYPE (object) == Lisp_Cons && !EQ (XCONS (object)->car, Qkeymap))*/
    {
      object = Fcdr (object);
      if (LISTP (map))
	object = Fcdr (Fassq (object, Fcdr (map)));
      else
	object = Faref (map, object);
    }
  return object;
}

Lisp_Object
access_keymap (map, idx)
     Lisp_Object map;
     register int idx;
{
  register Lisp_Object val;
  if (idx < 0 || idx >= 0200)
    error ("Command key out of range 0-127");

  /* Get definition for character `idx' proper.  */
  if (LISTP (map))
    val = Fcdr (Fassq (make_number (idx), Fcdr (map)));
  else
    val = XVECTOR (map)->contents[idx];

  /* If nothing there, and `idx' is upper case,
     look under corresponding lower case character.  */

  if (NULL (val) && idx >= 'A' && idx <= 'Z')
    {
      if (LISTP (map))
	val = Fcdr (Fassq (make_number (idx + 'a' - 'A'), Fcdr (map)));
      else
	val = XVECTOR (map)->contents[idx + 'a' - 'A'];
    }
  return val;
}

Lisp_Object
store_in_keymap (keymap, idx, def)
     Lisp_Object keymap;
     int idx;
     Lisp_Object def;
{
  Lisp_Object tem;

  if (idx < 0 || idx >= 0200)
    error ("Command key out of range 0-127");

  if (LISTP (keymap))
    {
      tem = Fassq (make_number (idx), Fcdr (keymap));
      if (!NULL (tem))
	Fsetcdr (tem, def);
      else
	Fsetcdr (keymap, Fcons (Fcons (make_number (idx), def),
				Fcdr (keymap)));
    }
  else
    XVECTOR (keymap)->contents[idx] = def;

  return def;
}

DEFUN ("define-key", Fdefine_key, Sdefine_key, 3, 3, 0,
  "Args KEYMAP, KEYS, DEF.  Define key sequence KEYS, in KEYMAP, as DEF.\n\
KEYMAP is a keymap.  KEYS is a string meaning a sequence of keystrokes.\n\
DEF is usually a symbol with a function definition, suitable for use as a command.")
  (keymap, keys, def)
     Lisp_Object keymap;
     Lisp_Object keys;
     Lisp_Object def;
{
  unsigned char *p;
  int level;
  Lisp_Object tem;
  Lisp_Object cmd;

  keymap = get_keymap (keymap, 0);

  CHECK_STRING (keys, 1);
  p = XSTRING (keys)->data;
  level = XSTRING (keys)->size;

  while (1)
    {
      level--;

      if (!level)
	return store_in_keymap (keymap, *p, def);

      cmd = get_keyelt (access_keymap (keymap, *p));
      if (NULL (cmd))
	{
	  cmd = Fmake_sparse_keymap ();
	  store_in_keymap (keymap, *p, cmd);
	}
      tem = Fkeymapp (cmd);
      if (NULL (tem))
	error ("Key sequence %s uses invalid prefix characters",
	       XSTRING (keys)->data);

      keymap = get_keymap (cmd, 0);
      p++;
    }
}

/* Value is T if `keys' is too long; NIL if valid but has no definition. */

DEFUN ("lookup-key", Flookup_key, Slookup_key, 2, 2, 0,
  "In keymap KEYMAP, look up key sequence KEYS.  Return the definition.\n\
nil means undefined.\n\
Number as value means KEYS is \"too long\";\n\
that is, characters in it except for the last one\n\
fail to be a valid sequence of prefix characters in KEYMAP.\n\
The number is how many characters at the front of KEYS\n\
it takes to reach a non-prefix command.")
  (keymap, keys)
     Lisp_Object keymap;
     Lisp_Object keys;
{
  unsigned char *p;
  int level;
  Lisp_Object tem;
  Lisp_Object cmd;

  keymap = get_keymap (keymap, 0);

  CHECK_STRING (keys, 1);
  p = XSTRING (keys)->data;
  level = XSTRING (keys)->size;

  while (1)
    {
      level--;
      cmd = get_keyelt (access_keymap (keymap, *p++));
      if (!level)
	return cmd;

      tem = Fkeymapp (cmd);
      if (NULL (tem))
	return make_number (XSTRING (keys)->size - level);

      keymap = get_keymap (cmd, 0);
    }
}

DEFUN ("key-binding", Fkey_binding, Skey_binding, 1, 1, 0,
  "Return the definition for command KEYS in current keymaps.\n\
KEYS is a string, a sequence of keystrokes.\n\
The definition is probably a symbol with a function definition.")
  (keys)
     Lisp_Object keys;
{
  Lisp_Object map, value, value1;
  map = bf_cur->keymap;
  if (!NULL (map))
    {
      value = Flookup_key (map, keys);
      if (NULL (value))
	{
	  XSET (map, Lisp_Vector, CurrentGlobalMap);
	  value1 = Flookup_key (map, keys);
	  if (XTYPE (value1) == Lisp_Int)
	    return Qnil;
	  return value1;
	}
      else if (XTYPE (value) != Lisp_Int)
	return value;
    }
  XSET (map, Lisp_Vector, CurrentGlobalMap);
  return Flookup_key (map, keys);
}

DEFUN ("local-key-binding", Flocal_key_binding, Slocal_key_binding, 1, 1, 0,
  "Return the definition for command KEYS in current local keymap only.\n\
KEYS is a string, a sequence of keystrokes.\n\
The definition is probably a symbol with a function definition.")
  (keys)
     Lisp_Object keys;
{
  Lisp_Object map;
  map = bf_cur->keymap;
  if (NULL (map))
    return Qnil;
  return Flookup_key (map, keys);
}

DEFUN ("global-key-binding", Fglobal_key_binding, Sglobal_key_binding, 1, 1, 0,
  "Return the definition for command KEYS in current global keymap only.\n\
KEYS is a string, a sequence of keystrokes.\n\
The definition is probably a symbol with a function definition.")
  (keys)
     Lisp_Object keys;
{
  Lisp_Object map;
  XSET (map, Lisp_Vector, CurrentGlobalMap);
  return Flookup_key (map, keys);
}

DEFUN ("global-set-key", Fglobal_set_key, Sglobal_set_key, 2, 2,
  "kSet key globally: \nCSet key %s to command: ",
  "Give KEY a definition of COMMAND.\n\
COMMAND is a symbol naming an interactively-callable function.\n\
KEY is a string representing a sequence of keystrokes.\n\
Note that if KEY has a local definition in the current buffer\n\
that local definition will continue to shadow any global definition.")
  (keys, function)
     Lisp_Object keys, function;
{
  Lisp_Object map;
  XSET (map, Lisp_Vector, CurrentGlobalMap);
  CHECK_STRING (keys, 1);
  Fdefine_key (map, keys, function);
  return Qnil;
}

DEFUN ("local-set-key", Flocal_set_key, Slocal_set_key, 2, 2,
  "kSet key locally: \nCSet key %s locally to command: ",
  "Give KEY a local definition of COMMAND.\n\
COMMAND is a symbol naming an interactively-callable function.\n\
KEY is a string representing a sequence of keystrokes.\n\
The definition goes in the current buffer's local map,\n\
which is shared with other buffers in the same major mode.")
  (keys, function)
     Lisp_Object keys, function;
{
  Lisp_Object map;
  map = bf_cur->keymap;
  if (NULL (map))
    {
      map = Fmake_sparse_keymap ();
      bf_cur->keymap = map;
    }

  CHECK_STRING (keys, 1);
  Fdefine_key (map, keys, function);
  return Qnil;
}

DEFUN ("global-unset-key", Fglobal_unset_key, Sglobal_unset_key,
  1, 1, "kUnset key globally: ",
  "Remove global definition of KEY.\n\
KEY is a string representing a sequence of keystrokes.")
  (keys)
     Lisp_Object keys;
{
  return Fglobal_set_key (keys, Qnil);
}

DEFUN ("local-unset-key", Flocal_unset_key, Slocal_unset_key, 1, 1,
  "kUnset key locally: ",
  "Remove local definition of KEY.\n\
KEY is a string representing a sequence of keystrokes.")
  (keys)
     Lisp_Object keys;
{
  if (!NULL (bf_cur->keymap))
    Flocal_set_key (keys, Qnil);
  return Qnil;
}

DEFUN ("define-prefix-command", Fdefine_prefix_command, Sdefine_prefix_command, 1, 1, 0,
  "Define SYMBOL as a prefix command.\n\
A keymap is created and stored as SYMBOL's function definition.")
  (name)
     Lisp_Object name;
{
  Fset (name, Fmake_keymap ());
  return name;
}

DEFUN ("use-global-map", Fuse_global_map, Suse_global_map, 1, 1, 0,
  "Selects KEYMAP as the global keymap.")
  (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap, 0);
  CHECK_VECTOR (keymap, 0);
  CurrentGlobalMap = XVECTOR (keymap);
  return Qnil;
}

DEFUN ("use-local-map", Fuse_local_map, Suse_local_map, 1, 1, 0,
  "Selects KEYMAP as the local keymap.\n\
nil for KEYMAP means no local keymap.")
  (keymap)
     Lisp_Object keymap;
{
  if (!NULL (keymap))
    keymap = get_keymap (keymap, 0);

  bf_cur->keymap = keymap;

  return Qnil;
}

DEFUN ("current-local-map", Fcurrent_local_map, Scurrent_local_map, 0, 0, 0,
  "Return current buffer's local keymap, or nil if it has none.")
  ()
{
  return bf_cur->keymap;
}

DEFUN ("accessible-keymaps", Faccessible_keymaps, Saccessible_keymaps,
  1, 1, 0,
  "Find all keymaps accessible via prefix characters from KEYMAP.\n\
Returns a list of elements of the form (KEYS . MAP), where the sequence\n\
KEYS starting from KEYMAP gets you to MAP.  These elements are ordered\n\
so that the KEYS increase in length.  The first element is (\"\" . KEYMAP).")
  (startmap)
     Lisp_Object startmap;
{
  Lisp_Object maps, tail;
  Lisp_Object thismap, thisseq;
  Lisp_Object dummy;
  Lisp_Object tem;
  Lisp_Object cmd;
  int i;

  maps = Fcons (Fcons (build_string (""), get_keymap (startmap, 0)), Qnil);
  tail = maps;

  /* For each map in the list maps,
     look at any other maps it points to
     and stick them at the end if they are not already in the list */

  while (!NULL (tail))
    {
      thisseq = Fcar (Fcar (tail));
      thismap = Fcdr (Fcar (tail));
      for (i = 0; i < 0200; i++)
	{
	  cmd = get_keyelt (access_keymap (thismap, i));
	  if (NULL (cmd)) continue;
	  tem = Fkeymapp (cmd);
	  if (!NULL (tem))
	    {
		cmd = get_keymap (cmd, 0);
		tem = Frassq (cmd, maps);
		if (NULL (tem)) {
		      XFASTINT (dummy) = i;
		      dummy = concat2 (thisseq, Fchar_to_string (dummy));
		      nconc2 (tail, Fcons (Fcons (dummy, cmd), Qnil));
		}
	    }
	}
      tail = Fcdr (tail);
    }

  return maps;
}

Lisp_Object Qsingle_key_description, Qkey_description;

DEFUN ("key-description", Fkey_description, Skey_description, 1, 1, 0,
  "Return a pretty description of key-sequence KEYS.\n\
Control characters turn into \"C-foo\" sequences, meta into \"M-foo\"\n\
spaces are put between sequence elements, etc.")
  (keys)
     Lisp_Object keys;
{
  return Fmapconcat (Qsingle_key_description, keys, build_string (" "));
}

char *
push_key_description (c, p)
     register unsigned int c;
     register char *p;
{
  if (c >= 0200)
    {
      *p++ = 'M';
      *p++ = '-';
      c -= 0200;
    }
  if (c < 040)
    {
      if (c == 033)
	{
	  *p++ = 'E';
	  *p++ = 'S';
	  *p++ = 'C';
	}
      else if (c == Ctl('I'))
	{
	  *p++ = 'T';
	  *p++ = 'A';
	  *p++ = 'B';
	}
      else if (c == Ctl('J'))
	{
	  *p++ = 'L';
	  *p++ = 'F';
	  *p++ = 'D';
	}
      else if (c == Ctl('M'))
	{
	  *p++ = 'R';
	  *p++ = 'E';
	  *p++ = 'T';
	}
      else
	{
	  *p++ = 'C';
	  *p++ = '-';
	  if (c > 0 && c <= Ctl ('Z'))
	    *p++ = c + 0140;
	  else
	    *p++ = c + 0100;
	}
    }
  else if (c == 0177)
    {
      *p++ = 'D';
      *p++ = 'E';
      *p++ = 'L';
    }
  else if (c == ' ')
    {
      *p++ = 'S';
      *p++ = 'P';
      *p++ = 'C';
    }
  else
    *p++ = c;
  return p;  
}

DEFUN ("single-key-description", Fsingle_key_description, Ssingle_key_description, 1, 1, 0,
  "Return a pretty description of command character KEY.\n\
Control characters turn into C-whatever, etc.")
  (key)
     Lisp_Object key;
{
  unsigned char c;
  char tem[6];

  CHECK_NUMBER (key, 0);
  c = XINT (key) & 0377;

  *push_key_description (c, tem) = 0;

  return build_string (tem);
}

char *
push_text_char_description (c, p)
     register unsigned int c;
     register char *p;
{
  if (c >= 0200)
    {
      *p++ = 'M';
      *p++ = '-';
      c -= 0200;
    }
  if (c < 040)
    {
      *p++ = '^';
      *p++ = c + 64;		/* 'A' - 1 */
    }
  else if (c == 0177)
    {
      *p++ = '^';
      *p++ = '?';
    }
  else
    *p++ = c;
  return p;  
}

DEFUN ("text-char-description", Ftext_char_description, Stext_char_description, 1, 1, 0,
  "Return a pretty description of file-character CHAR.\n\
Control characters turn into \"C-char\", etc.")
  (chr)
     Lisp_Object chr;
{
  char tem[6];

  CHECK_NUMBER (chr, 0);

  *push_text_char_description (XINT (chr) & 0377, tem) = 0;

  return build_string (tem);
}

Lisp_Object where_is_in_buffer ();

DEFUN ("where-is-internal", Fwhere_is_internal, Swhere_is_internal, 1, 1, 0,
  "Return list of key sequences that currently invoke command DEFINITION.")
  (definition)
     Lisp_Object definition;
{
  return where_is_in_buffer (definition, bf_cur, 0);
}

Lisp_Object
where_is_in_buffer (definition, buf, firstonly)
     Lisp_Object definition;
     struct buffer *buf;
     int firstonly;		/* if true, return after finding one */
{
  Lisp_Object start1, start2;
  Lisp_Object maps;
  Lisp_Object this, map;
  Lisp_Object dummy;
  Lisp_Object found;
  Lisp_Object tem;
  register int i;

  XSET (start1, Lisp_Vector, CurrentGlobalMap);
  start2 = buf->keymap;

  if (!NULL (start2))
    maps = nconc2 (Faccessible_keymaps (start2),
		   Faccessible_keymaps (start1));
  else
    maps = Faccessible_keymaps (start1);

  found = Qnil;

  for (; !NULL (maps); maps = Fcdr (maps))
    {
      this = Fcar (Fcar (maps));	/* Key sequence to reach map */
      map = Fcdr (Fcar (maps));		/* The map that it reaches */
      if (LISTP (map))
	for (map = Fcdr (map); !NULL (map); map = Fcdr (map))
	  {
	    QUIT;
	    tem = Fcdr (Fcar (map));
	    if (EQ (tem, definition))
	      {
  		dummy = Fcar (Fcar (map));
		dummy = concat2 (this, Fchar_to_string (dummy));
		if (firstonly)
		  return dummy;
		found = Fcons (dummy, found);
	      }
	  }
      else
	for (i = 0; i < 0200; i++)	/* Search that map for a match */
	  {
	    QUIT;
	    tem = XVECTOR (map)->contents[i];
	    if (EQ (tem, definition))	/* Match found: record key sequence */
	      {
		XFASTINT (dummy) = i;	/* which is `this' followed by character i */
 		dummy = concat2 (this, Fchar_to_string (dummy));
		if (firstonly)
		  return dummy;
		found = Fcons (dummy, found);
	      }
	  }
    }
  return Fnreverse (found);
}

DEFUN ("where-is", Fwhere_is, Swhere_is, 1, 1, "CWhere is command: ",
  "Print message listing key sequences that invoke specified command.\n\
Argument is a command definition, usually a symbol with a function definition.")
  (definition)
     Lisp_Object definition;
{
  Lisp_Object tem;
  CHECK_SYMBOL (definition, 0);
  tem = Fmapconcat (Qkey_description, Fwhere_is_internal (definition), build_string (", "));
  if (XSTRING (tem)->size)
    message ("%s is on %s", XSYMBOL (definition)->name->data, XSTRING (tem)->data);
  else
    message ("%s is not on any keys", XSYMBOL (definition)->name->data);
  return Qnil;
}

Lisp_Object describe_buffer_bindings ();

DEFUN ("describe-bindings", Fdescribe_bindings, Sdescribe_bindings, 0, 0, "",
  "Show a list of all defined keys, and their definitions.\n\
The list is put in a buffer, which is displayed.")
  ()
{
  Lisp_Object thisbuf;
  XSETTYPE (thisbuf, Lisp_Buffer), XSETBUFFER (thisbuf, bf_cur);
  internal_with_output_to_temp_buffer ("*Help*", describe_buffer_bindings, thisbuf);
  return Qnil;
}

Lisp_Object
describe_buffer_bindings (descbuf)
     Lisp_Object descbuf;
{
  register Lisp_Object start1;
  char *heading = "key		binding\n---		-------\n";

  Fset_buffer (Vstandard_output);

  start1 = XBUFFER (descbuf)->keymap;
  if (!NULL (start1))
    {
      InsStr ("Local Bindings:\n");
      InsStr (heading);
      heading = 0;
      describe_map_tree (start1, 0);
      InsStr ("\n");
    }

  InsStr ("Global Bindings:\n");
  if (heading)
    InsStr (heading);

  XSET (start1, Lisp_Vector, CurrentGlobalMap);
  describe_map_tree (start1, 0);

  Fset_buffer (descbuf);
  return Qnil;
}

/* Insert a desription of the key bindings in STARTMAP,
   followed by those of all maps reachable through STARTMAP.
   If PARTIAL is nonzero, omit certain "uninteresting" commands
   (such as `undefined').  */

describe_map_tree (startmap, partial)
     Lisp_Object startmap;
     int partial;
{
  register Lisp_Object maps, elt;

  maps = Faccessible_keymaps (startmap);

  for (; !NULL (maps); maps = Fcdr (maps))
    {
      elt = Fcar (maps);
      describe_map (Fcdr (elt), Fcar (elt), partial);
    }
}

describe_command (definition)
     Lisp_Object definition;
{
  register Lisp_Object tem1;

  if (XTYPE (definition) == Lisp_Symbol)
    {
      XSETSTRING (tem1, XSYMBOL (definition)->name);
      InsCStr (XSTRING (tem1)->data, XSTRING (tem1)->size);
    }
  else
    {
      tem1 = Fkeymapp (definition);
      if (!NULL (tem1))
	InsStr ("Prefix Command");
      else
	InsStr ("??");
    }
}

/* Describe the contents of map MAP, assuming that this map
   itself is reached by the sequence of prefix keys KEYS (a string).
   PARTIAL is the same as in `describe_map_tree', above.  */

describe_map (map, keys, partial)
     Lisp_Object map, keys;
     int partial;
{
  register Lisp_Object keysdesc;

  InsStr ("\n");

  if (!NULL (keys) && XSTRING (keys)->size > 0)
    keysdesc = Fkey_description (keys);
  else
    keysdesc = Qnil;

  if (LISTP (map))
    describe_alist (Fcdr (map), keysdesc, describe_command, partial);
  else
    describe_vector (map, keysdesc, describe_command, partial);
}

describe_alist (alist, elt_prefix, elt_describer, partial)
     register Lisp_Object alist;
     register Lisp_Object elt_prefix;
     int (*elt_describer) ();
     int partial;
{
  Lisp_Object this;
  register Lisp_Object tem1, tem2;
  int indent;
  Lisp_Object suppress;

  if (partial)
    suppress = intern ("suppress-keymap");

  for (; LISTP (alist); alist = Fcdr (alist))
    {
      QUIT;
      tem1 = Fcar (Fcar (alist));
      tem2 = Fcdr (Fcar (alist));
      if (NULL (tem2)) continue;      
      if (XTYPE (tem2) == Lisp_Symbol && partial)
	{
	  this = Fget (tem2, suppress);
	  if (!NULL (this))
	    continue;
	}

      indent = 0;

      if (!NULL (elt_prefix))
	{
	  InsCStr (XSTRING (elt_prefix)->data, XSTRING (elt_prefix)->size);
	  InsCStr (" ", 1);
	  indent += XSTRING (elt_prefix)->size + 1;
	}

      this = Fsingle_key_description (tem1);
      InsCStr (XSTRING (this)->data, XSTRING (this)->size);
      indent += XSTRING (this)->size;

      InsCStr ("                    ", indent<16 ? 16-indent : 1);

      (*elt_describer) (tem2);
      InsCStr ("\n", 1);
    }
}

describe_vector (vector, elt_prefix, elt_describer, partial)
     register Lisp_Object vector;
     register Lisp_Object elt_prefix;
     int (*elt_describer) ();
     int partial;
{
  Lisp_Object this;
  Lisp_Object dummy;
  Lisp_Object tem1, tem2;
  register int i, size = XVECTOR (vector)->size;
  int indent;
  Lisp_Object suppress;

  if (partial)
    suppress = intern ("suppress-keymap");

  for (i = 0; i < size; i++)
    {
      QUIT;
      tem1 = XVECTOR (vector)->contents[i];
      if (NULL (tem1)) continue;      
      if (XTYPE (tem1) == Lisp_Symbol && partial)
	{
	  this = Fget (tem1, suppress);
	  if (!NULL (this))
	    continue;
	}

      indent = 0;

      if (!NULL (elt_prefix))
	{
	  InsCStr (XSTRING (elt_prefix)->data, XSTRING (elt_prefix)->size);
	  InsCStr (" ", 1);
	  indent += XSTRING (elt_prefix)->size + 1;
	}
      XFASTINT (dummy) = i;
      this = Fsingle_key_description (dummy);
      InsCStr (XSTRING (this)->data, XSTRING (this)->size);
      indent += XSTRING (this)->size;

      while (i + 1 < size && (tem2 = XVECTOR (vector)->contents[i+1], EQ(tem2, tem1)))
	i++;

      if (i != XINT (dummy))
	{
	  InsCStr (" .. ", 4);
	  indent += 4;
	  if (!NULL (elt_prefix))
	    {
	      InsCStr (XSTRING (elt_prefix)->data, XSTRING (elt_prefix)->size);
	      InsCStr (" ", 1);
	      indent += XSTRING (elt_prefix)->size + 1;
	    }
	  XFASTINT (dummy) = i;
	  this = Fsingle_key_description (dummy);
	  InsCStr (XSTRING (this)->data, XSTRING (this)->size);
	  indent += XSTRING (this)->size;
	}

      InsCStr ("                    ", indent<16 ? 16-indent : 1);

      tem1 = XVECTOR (vector)->contents[i];
      (*elt_describer) (tem1);
      InsCStr ("\n", 1);
    }
}

/* Apropos */
Lisp_Object apropos_predicate;
Lisp_Object apropos_accumulate;

static
apropos_accum (symbol, string)
     Lisp_Object symbol, string;
{
  register Lisp_Object tem;

  tem = Fstring_match (string, Fsymbol_name (symbol), Qnil);
  if (!NULL (tem) && !NULL (apropos_predicate))
    tem = call1 (apropos_predicate, symbol);
  if (!NULL (tem))
    apropos_accumulate = Fcons (symbol, apropos_accumulate);
}

static Lisp_Object
apropos1 (list)
     register Lisp_Object list;
{
  struct buffer *old = bf_cur;
  register Lisp_Object symbol, col, tem;

  while (!NULL (list))
    {
      QUIT;

      symbol = Fcar (list);
      list = Fcdr (list);

      tem = where_is_in_buffer (symbol, bf_cur, 0);
      tem = Fmapconcat (Qkey_description, tem, build_string (", "));
      XFASTINT (col) = 30;

      SetBfp (XBUFFER (Vstandard_output));
      Fprin1 (symbol, Qnil);
      Findent_to (col, Qnil);
      Fprinc (tem, Qnil);
      Fterpri (Qnil);
      tem = Ffboundp (symbol);
      if (!NULL (tem))
        tem = Fdocumentation (symbol);
      if (XTYPE (tem) == Lisp_String)
	insert_first_line ("  Function: ", tem);
      tem = Fget (symbol, Qvariable_documentation);
      if (XTYPE (tem) == Lisp_String)
	insert_first_line ("  Variable: ", tem);
      SetBfp (old);
    }
  return Qnil;
}

static
insert_first_line (prefix, str)
     char *prefix;
     Lisp_Object str;
{
  extern char *index ();
  register unsigned char *p;
  register unsigned char *p1;
  register unsigned char *p2;

  InsStr (prefix);

 retry:
  p = XSTRING (str)->data;
  p1 = (unsigned char *) index (p, '\n');

  for (p2 = p; *p2 && p2 != p1; p2++)
    if (p2[0] == '\\' && p2[1] == '[')
      {
	str = Fsubstitute_command_keys (str);
	goto retry;
      }

  InsCStr (p, p1 ? p1 - p : strlen (p));
  InsCStr ("\n", 1);
}

DEFUN ("apropos", Fapropos, Sapropos, 1, 3, "sApropos: ",
  "Show all symbols whose names contain match for REGEXP.\n\
If optional arg PRED is non-nil, (funcall PRED SYM) is done\n\
for each symbol and a symbol is mentioned if that returns non-nil.\n\
Returns list of symbols found; if third arg NOPRINT is non-nil,\n\
does not display them, just returns the list.")
  (string, pred, noprint)
     Lisp_Object string, pred, noprint;
{
  struct gcpro gcpro1, gcpro2;
  CHECK_STRING (string, 0);
  apropos_predicate = pred;
  GCPRO2 (apropos_predicate, apropos_accumulate);
  apropos_accumulate = Qnil;
  map_obarray (Vobarray, apropos_accum, string);
  apropos_accumulate = Fsort (apropos_accumulate, Qstring_lessp);
  if (NULL (noprint))
    internal_with_output_to_temp_buffer ("*Help*", apropos1,
					 apropos_accumulate);
  UNGCPRO;
  return apropos_accumulate;
}

DEFUN ("execute-extended-command", Fexecute_extended_command, Sexecute_extended_command,
  1, 1, "P",
  "Read function name, then read its arguments and call it.")
  (prefixarg)
     Lisp_Object prefixarg;
{
  Lisp_Object function;
  char buf[40];

  buf[0] = 0;

  if (EQ (prefixarg, Qminus))
    strcpy (buf, "- ");
  else if (LISTP (prefixarg) && XINT (XCONS (prefixarg)->car) == 4)
    strcpy (buf, "C-u ");
  else if (LISTP (prefixarg) && XTYPE (XCONS (prefixarg)->car) == Lisp_Int)
    sprintf (buf, "%d ", XINT (XCONS (prefixarg)->car));
  else if (XTYPE (prefixarg) == Lisp_Int)
    sprintf (buf, "%d ", XINT (prefixarg));

  strcat (buf, "M-x ");

  function = Fcompleting_read (build_string (buf), Vobarray, Qcommandp, Qt, Qnil);

  Vprefix_arg = prefixarg;
  return Fcommand_execute (Fintern (function, Vobarray), Qt);
}

syms_of_keymap ()
{
  Lisp_Object tem;

  Qkeymap = intern ("keymap");
  staticpro (&Qkeymap);

/* Initialize the keymaps standardly used.
   Each one is the value of a Lisp variable, and is also
   pointed to by a C variable */

#ifdef HAVE_X_WINDOWS
  tem = Fmake_keymap ();
  MouseMap = XVECTOR (tem);
  Fset (intern ("mouse-map"), tem);
#endif /* HAVE_X_WINDOWS */

  tem = Fmake_keymap ();
  GlobalMap = XVECTOR (tem);
  Fset (intern ("global-map"), tem);

  tem = Fmake_keymap ();
  ESCmap = XVECTOR (tem);
  Fset (intern ("esc-map"), tem);
  Ffset (intern ("ESC-prefix"), tem);

  tem = Fmake_keymap ();
  CtlXmap = XVECTOR (tem);
  Fset (intern ("ctl-x-map"), tem);
  Ffset (intern ("Control-X-prefix"), tem);

  DefLispVar ("minibuffer-local-map", &Vminibuffer_local_map,
    "Default keymap to use when reading from the minibuffer.");
  Vminibuffer_local_map = Fmake_sparse_keymap ();

  DefLispVar ("minibuffer-local-ns-map", &Vminibuffer_local_ns_map,
    "The keymap used by the minibuf for local bindings when spaces are not\n\
to be allowed in input string.");
  Vminibuffer_local_ns_map = Fmake_sparse_keymap ();

  DefLispVar ("minibuffer-local-completion-map", &Vminibuffer_local_completion_map,
    "Keymap to use when reading from the minibuffer with completion.");
  Vminibuffer_local_completion_map = Fmake_sparse_keymap ();

  DefLispVar ("minibuffer-local-must-match-map", &Vminibuffer_local_must_match_map,
    "Keymap to use when reading from the minibuffer with completion and\n\
an exact match of one of the completions is required.");
  Vminibuffer_local_must_match_map = Fmake_sparse_keymap ();

  CurrentGlobalMap = GlobalMap;

  Qsingle_key_description = intern ("single-key-description");
  staticpro (&Qsingle_key_description);

  Qkey_description = intern ("key-description");
  staticpro (&Qkey_description);

  Qkeymapp = intern ("keymapp");
  staticpro (&Qkeymapp);

  defsubr (&Skeymapp);
  defsubr (&Smake_keymap);
  defsubr (&Smake_sparse_keymap);
  defsubr (&Skey_binding);
  defsubr (&Slocal_key_binding);
  defsubr (&Sglobal_key_binding);
  defsubr (&Sglobal_set_key);
  defsubr (&Slocal_set_key);
  defsubr (&Sdefine_key);
  defsubr (&Slookup_key);
  defsubr (&Sglobal_unset_key);
  defsubr (&Slocal_unset_key);
  defsubr (&Suse_global_map);
  defsubr (&Suse_local_map);
  defsubr (&Scurrent_local_map);
  defsubr (&Saccessible_keymaps);
  defsubr (&Skey_description);
  defsubr (&Ssingle_key_description);
  defsubr (&Stext_char_description);
  defsubr (&Swhere_is_internal);
  defsubr (&Swhere_is);
  defsubr (&Sdescribe_bindings);
  defsubr (&Sapropos);
  defsubr (&Sexecute_extended_command);
}

keys_of_keymap ()
{
  defkey (GlobalMap, 033, "ESC-prefix");
  defkey (GlobalMap, Ctl('X'), "Control-X-prefix");
  defkey (ESCmap, 'x', "execute-extended-command");
}
