/* Manipulation of keymaps
   Copyright (C) 1985, 1986, 1987, 1988, 1990 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include <stdio.h>
#undef NULL
#include "lisp.h"
#include "commands.h"
#include "buffer.h"

#define min(a, b) ((a) < (b) ? (a) : (b))

/* Actually allocate storage for these variables */

#ifdef HAVE_X_WINDOWS
Lisp_Object MouseMap;		/* Keymap for mouse commands */
#endif /* HAVE_X_WINDOWS */

Lisp_Object global_map;

Lisp_Object Vglobal_map;

Lisp_Object Vesc_map;

Lisp_Object Vctl_x_map;

/* Keymap used for minibuffers with self-inserting space.  */
Lisp_Object Vminibuffer_local_map;

/* Keymap used for minibuffers when space does not self insert.  */
Lisp_Object Vminibuffer_local_ns_map;			

/* Keymap used for minibuffers when doing completion */
Lisp_Object Vminibuffer_local_completion_map;

/* Keymap used for minibuffers when doing completion and require a match */
Lisp_Object Vminibuffer_local_must_match_map;

Lisp_Object Qkeymapp, Qkeymap;

/* A char over 0200 in a key sequence
   is equivalent to prefixing with this character.  */

extern int meta_prefix_char;

DEFUN ("make-keymap", Fmake_keymap, Smake_keymap, 0, 0, 0,
  "Construct and return a new keymap, a vector of length 128.\n\
All entries in it are nil, meaning \"command undefined\".")
  ()
{
  register Lisp_Object val;
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

/* Install a standard key binding at initialization time.
   For example,
     ndefkey (Vctl_x_map, Ctl ('X'), "exchange-point-and-mark");  */

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
  register Lisp_Object tem;
  tem = object;
  while (XTYPE (tem) == Lisp_Symbol)
    {
      tem = XSYMBOL (tem)->function;
      if (EQ (tem, Qunbound))
	return Qnil;
      QUIT;
    }

  if ((XTYPE (tem) == Lisp_Vector && XVECTOR (tem)->size == 0200)
      || (CONSP (tem) && EQ (XCONS (tem)->car, Qkeymap)))
    return Qt;
  return Qnil;
}

Lisp_Object
get_keymap_1 (object, error)
     Lisp_Object object;
     int error;
{
  register Lisp_Object tem;

  while (1)
    {
      tem = object;
      while (XTYPE (tem) == Lisp_Symbol && !EQ (tem, Qunbound))
	{
	  tem = XSYMBOL (tem)->function;
	  QUIT;
	}
      if ((XTYPE (tem) == Lisp_Vector && XVECTOR (tem)->size == 0200)
	  || (CONSP (tem) && EQ (XCONS (tem)->car, Qkeymap)))
	return tem;
      if (error)
	object = wrong_type_argument (Qkeymapp, object);
      else return Qnil;
    }
}

Lisp_Object
get_keymap (object)
     Lisp_Object object;
{
  return get_keymap_1 (object, 1);
}

Lisp_Object
get_keyelt (object)
     register Lisp_Object object;
{
  register Lisp_Object map, tem;

  while (map = get_keymap_1 (Fcar_safe (object), 0),
	 tem = Fkeymapp (map),
	 !NULL (tem))
      /*(XTYPE (object) == Lisp_Cons && !EQ (XCONS (object)->car, Qkeymap))*/
    {
      object = Fcdr (object);
      if (CONSP (map))
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
  if (CONSP (map))
    val = Fcdr (Fassq (make_number (idx), Fcdr (map)));
  else
    val = XVECTOR (map)->contents[idx];

  return val;
}

Lisp_Object
store_in_keymap (keymap, idx, def)
     Lisp_Object keymap;
     register int idx;
     register Lisp_Object def;
{
  register Lisp_Object tem;

  if (idx < 0 || idx >= 0200)
    error ("Command key out of range 0-127");

  if (CONSP (keymap))
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

DEFUN ("copy-keymap", Fcopy_keymap, Scopy_keymap, 1, 1, 0,
  "Return a copy of the keymap KEYMAP.\n\
The copy starts out with the same definitions of KEYMAP,\n\
but changing either the copy or KEYMAP does not affect the other.\n\
Any key definitions that are subkeymaps are recursively copied.")
  (keymap)
     Lisp_Object keymap;
{
  register Lisp_Object copy, tem;

  keymap = get_keymap (keymap);
  if (XTYPE (keymap) == Lisp_Vector)
    {
      register int i;
      copy = Fcopy_sequence (keymap);
      for (i = 0; i < XVECTOR (copy)->size; i++)
	if (tem = Fkeymapp (XVECTOR (copy)->contents[i]), !NULL (tem))
	  XVECTOR (copy)->contents[i] = Fcopy_keymap (XVECTOR (copy)->contents[i]);
    }
  else
    {
      register Lisp_Object tail;
      copy = Fcopy_alist (keymap); 
      for (tail = copy; CONSP (tail); tail = XCONS (tail)->cdr)
	{
	  register Lisp_Object elt;
	  elt = XCONS (tail)->car;
	  if (CONSP (elt) && (tem = Fkeymapp (XCONS (elt)->cdr), !NULL (tem)))
	    XCONS (elt)->cdr = Fcopy_keymap (XCONS (elt)->cdr);
	}
    }
  return copy;
}

DEFUN ("define-key", Fdefine_key, Sdefine_key, 3, 3, 0,
  "Args KEYMAP, KEY, DEF.  Define key sequence KEY, in KEYMAP, as DEF.\n\
KEYMAP is a keymap.  KEY is a string meaning a sequence of keystrokes.\n\
DEF is anything that can be a key's definition:\n\
 nil (means key is undefined in this keymap),\n\
 a command (a Lisp function suitable for interactive calling)\n\
 a string (treated as a keyboard macro),\n\
 a keymap (to define a prefix key),\n\
 a list (KEYMAP . CHAR), meaning use definition of CHAR in map KEYMAP,\n\
 or a symbol.  The symbol's function definition is used as the key's\n\
definition, and may be any of the above (including another symbol).")
  (keymap, key, def)
     register Lisp_Object keymap;
     Lisp_Object key;
     Lisp_Object def;
{
  register int idx;
  register int c;
  register Lisp_Object tem;
  register Lisp_Object cmd;
  int metized = 0;

  keymap = get_keymap (keymap);

  CHECK_STRING (key, 1);
  if (XSTRING (key)->size == 0)
    return Qnil;

  idx = 0;
  while (1)
    {
      c = XSTRING (key)->data[idx];
      if (c >= 0200 && !metized)
	{
	  c = meta_prefix_char;
	  metized = 1;
	}
      else
	{
	  c &= 0177;
	  metized = 0;
	  idx++;
	}

      if (idx == XSTRING (key)->size)
	return store_in_keymap (keymap, c, def);

      cmd = get_keyelt (access_keymap (keymap, c));
      if (NULL (cmd))
	{
	  cmd = Fmake_sparse_keymap ();
	  store_in_keymap (keymap, c, cmd);
	}
      tem = Fkeymapp (cmd);
      if (NULL (tem))
	error ("Key sequence %s uses invalid prefix characters",
	       XSTRING (key)->data);

      keymap = get_keymap (cmd);
    }
}

/* Value is number if KEY is too long; nil if valid but has no definition. */

DEFUN ("lookup-key", Flookup_key, Slookup_key, 2, 2, 0,
  "In keymap KEYMAP, look up key sequence KEY.  Return the definition.\n\
nil means undefined.  See doc of define-key for kinds of definitions.\n\
Number as value means KEY is \"too long\";\n\
that is, characters in it except for the last one\n\
fail to be a valid sequence of prefix characters in KEYMAP.\n\
The number is how many characters at the front of KEY\n\
it takes to reach a non-prefix command.")
  (keymap, key)
     register Lisp_Object keymap;
     Lisp_Object key;
{
  register int idx;
  register Lisp_Object tem;
  register Lisp_Object cmd;
  register int c;
  int metized = 0;

  keymap = get_keymap (keymap);

  CHECK_STRING (key, 1);
  if (XSTRING (key)->size == 0)
    return Qnil;

  idx = 0;
  while (1)
    {
      c = XSTRING (key)->data[idx];
      if (c >= 0200 && !metized)
	{
	  c = meta_prefix_char;
	  metized = 1;
	}
      else
	{
	  c &= 0177;
	  metized = 0;
	  idx++;
	}

      cmd = get_keyelt (access_keymap (keymap, c));
      if (idx == XSTRING (key)->size)
	return cmd;

      tem = Fkeymapp (cmd);
      if (NULL (tem))
	return make_number (idx);

      keymap = get_keymap (cmd);
      QUIT;
    }
}

DEFUN ("key-binding", Fkey_binding, Skey_binding, 1, 1, 0,
  "Return the definition for command KEYS in current keymaps.\n\
KEYS is a string, a sequence of keystrokes.\n\
The definition is probably a symbol with a function definition.")
  (keys)
     Lisp_Object keys;
{
  register Lisp_Object map, value, value1;
  map = current_buffer->keymap;
  if (!NULL (map))
    {
      value = Flookup_key (map, keys);
      if (NULL (value))
	{
	  XSET (map, Lisp_Vector, global_map);
	  value1 = Flookup_key (map, keys);
	  if (XTYPE (value1) == Lisp_Int)
	    return Qnil;
	  return value1;
	}
      else if (XTYPE (value) != Lisp_Int)
	return value;
    }
  XSET (map, Lisp_Vector, global_map);
  return Flookup_key (map, keys);
}

DEFUN ("local-key-binding", Flocal_key_binding, Slocal_key_binding, 1, 1, 0,
  "Return the definition for command KEYS in current local keymap only.\n\
KEYS is a string, a sequence of keystrokes.\n\
The definition is probably a symbol with a function definition.")
  (keys)
     Lisp_Object keys;
{
  register Lisp_Object map;
  map = current_buffer->keymap;
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
  register Lisp_Object map;
  XSET (map, Lisp_Vector, global_map);
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
  register Lisp_Object map;
  XSET (map, Lisp_Vector, global_map);
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
  register Lisp_Object map;
  map = current_buffer->keymap;
  if (NULL (map))
    {
      map = Fmake_sparse_keymap ();
      current_buffer->keymap = map;
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
  if (!NULL (current_buffer->keymap))
    Flocal_set_key (keys, Qnil);
  return Qnil;
}

DEFUN ("define-prefix-command", Fdefine_prefix_command, Sdefine_prefix_command, 1, 1, 0,
  "Define SYMBOL as a prefix command.\n\
A keymap is created and stored as SYMBOL's function definition.")
  (name)
     Lisp_Object name;
{
  Ffset (name, Fmake_keymap ());
  return name;
}

DEFUN ("use-global-map", Fuse_global_map, Suse_global_map, 1, 1, 0,
  "Selects KEYMAP as the global keymap.")
  (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap);
  CHECK_VECTOR (keymap, 0);
  global_map = keymap;
  return Qnil;
}

DEFUN ("use-local-map", Fuse_local_map, Suse_local_map, 1, 1, 0,
  "Selects KEYMAP as the local keymap.\n\
nil for KEYMAP means no local keymap.")
  (keymap)
     Lisp_Object keymap;
{
  if (!NULL (keymap))
    keymap = get_keymap (keymap);

  current_buffer->keymap = keymap;

  return Qnil;
}

DEFUN ("current-local-map", Fcurrent_local_map, Scurrent_local_map, 0, 0, 0,
  "Return current buffer's local keymap, or nil if it has none.")
  ()
{
  return current_buffer->keymap;
}

DEFUN ("current-global-map", Fcurrent_global_map, Scurrent_global_map, 0, 0, 0,
  "Return the current global keymap.")
  ()
{
  return global_map;
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
  register Lisp_Object thismap, thisseq;
  register Lisp_Object dummy;
  register Lisp_Object tem;
  register Lisp_Object cmd;
  register int i;

  maps = Fcons (Fcons (build_string (""), get_keymap (startmap)), Qnil);
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
	      cmd = get_keymap (cmd);
	      tem = Frassq (cmd, maps);
	      if (NULL (tem))
		{
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
  register unsigned char c;
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
Control characters turn into \"^char\", etc.")
  (chr)
     Lisp_Object chr;
{
  char tem[6];

  CHECK_NUMBER (chr, 0);

  *push_text_char_description (XINT (chr) & 0377, tem) = 0;

  return build_string (tem);
}

DEFUN ("where-is-internal", Fwhere_is_internal, Swhere_is_internal, 1, 3, 0,
  "Return list of key sequences that currently invoke command DEFINITION\n\
in KEYMAP or (current-global-map).  If KEYMAP is nil, only search for\n\
keys in the global map.\n\
\n\
If FIRSTONLY is non-nil, returns a string representing the first key\n\
sequence found, rather than a list of all possible key sequences.")
  (definition, local_keymap, firstonly)
     Lisp_Object definition, local_keymap, firstonly;
{
  Lisp_Object start1;
  register Lisp_Object maps;
  Lisp_Object found;

  XSET (start1, Lisp_Vector, global_map);

  if (!NULL (local_keymap))
    maps = nconc2 (Faccessible_keymaps (get_keymap (local_keymap)),
		   Faccessible_keymaps (start1));
  else
    maps = Faccessible_keymaps (start1);

  found = Qnil;

  for (; !NULL (maps); maps = Fcdr (maps))
    {
      register this = Fcar (Fcar (maps)); /* Key sequence to reach map */
      register map = Fcdr (Fcar (maps)); /* The map that it reaches */
      register int i = 0;
	 
      if (CONSP (map))
	map = Fcdr (map);

      /* If the MAP is a vector, I increments and eventually reaches 0200.
	 Otherwise I remains 0; MAP is cdr'd and eventually becomes nil.  */

      while (!NULL (map) && i < 0200)
	{
	  register Lisp_Object elt, dummy;

	  QUIT;
	  if (CONSP (map))
	    {
	      elt = Fcdr (Fcar (map));
	      dummy = Fcar (Fcar (map));
	      map = Fcdr (map);
	    }
	  else
	    {
	      elt = XVECTOR (map)->contents[i];
	      XFASTINT (dummy) = i;
	      i++;
	    }

	  if (XTYPE (definition) != Lisp_Cons)
	    elt = get_keyelt (elt);

	  /* End this iteration if this element does not match
	     the target.  */

	  if (XTYPE (definition) == Lisp_Cons)
	    {
	      Lisp_Object tem;
	      tem = Fequal (elt, definition);
	      if (NULL (tem))
		continue;
	    }
	  else
	    if (!EQ (elt, definition))
	      continue;

	  /* We have found a match.
	     Construct the key sequence where we found it.  */

	  dummy = concat2 (this, Fchar_to_string (dummy));

	  /* Verify that this key binding is not shadowed
	     by another binding for the same key,
	     before we say it exists.
	     The mechanism: look for local definition of this key
	     and if it is defined and does not match what we found
	     then ignore this key.
	     Either nil or number as value from Flookup_key
	     means undefined.  */

	  if (!NULL (local_keymap))
	    elt = Flookup_key (local_keymap, dummy);
	  if (!NULL (elt) && XTYPE (elt) != Lisp_Int)
	    {
	      if (XTYPE (definition) == Lisp_Cons)
		{
		  Lisp_Object tem;
		  tem = Fequal (elt, definition);
		  if (NULL (tem))
		    continue;
		}
	      else
		if (!EQ (elt, definition))
		  continue;
	    }

	  /* It is a true unshadowed match  Record it.  */

	  if (!NULL (firstonly))
	    return dummy;
	  found = Fcons (dummy, found);
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
  register Lisp_Object tem;
  CHECK_SYMBOL (definition, 0);
  tem = Fmapconcat (Qkey_description,
		    Fwhere_is_internal (definition, current_buffer->keymap, Qnil),
		    build_string (", "));
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
  register Lisp_Object thisbuf;
  XSET (thisbuf, Lisp_Buffer, current_buffer);
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
      describe_map_tree (start1, 0, Qnil);
      InsStr ("\n");
    }

  InsStr ("Global Bindings:\n");
  if (heading)
    InsStr (heading);

  XSET (start1, Lisp_Vector, global_map);
  describe_map_tree (start1, 0, XBUFFER (descbuf)->keymap);

  Fset_buffer (descbuf);
  return Qnil;
}

/* Insert a desription of the key bindings in STARTMAP,
   followed by those of all maps reachable through STARTMAP.
   If PARTIAL is nonzero, omit certain "uninteresting" commands
   (such as `undefined').
   If SHADOW is non-nil, don't mention keys which would be shadowed by it */

describe_map_tree (startmap, partial, shadow)
     Lisp_Object startmap, shadow;
     int partial;
{
  register Lisp_Object maps, elt, sh;

  maps = Faccessible_keymaps (startmap);

  for (; !NULL (maps); maps = Fcdr (maps))
    {
      elt = Fcar (maps);
      sh = Fcar (elt);
      if (NULL (shadow))
	sh = Qnil;
      else if (XTYPE (sh) == Lisp_String &&
	       XSTRING (sh)->size == 0)
	sh = shadow;
      else
	{
	  sh = Flookup_key (shadow, Fcar (elt));
	  if (XTYPE (sh) == Lisp_Int)
	    sh = Qnil;
	}
      if (NULL (sh) || !NULL (Fkeymapp (sh)))
	describe_map (Fcdr (elt), Fcar (elt), partial, sh);
    }
}

describe_command (definition)
     Lisp_Object definition;
{
  register Lisp_Object tem1;

  Findent_to (make_number (16), make_number (1));

  if (XTYPE (definition) == Lisp_Symbol)
    {
      XSET (tem1, Lisp_String, XSYMBOL (definition)->name);
      insert1 (tem1);
      InsStr ("\n");
    }
  else
    {
      tem1 = Fkeymapp (definition);
      if (!NULL (tem1))
	InsStr ("Prefix Command\n");
      else
	InsStr ("??\n");
    }
}

/* Describe the contents of map MAP, assuming that this map
   itself is reached by the sequence of prefix keys STRING (a string).
   PARTIAL and SHADOW are the same as in `describe_map_tree' above.  */

describe_map (map, string, partial, shadow)
     Lisp_Object map, string;
     int partial;
     Lisp_Object shadow;
{
  register Lisp_Object keysdesc;

  if (!NULL (string) && XSTRING (string)->size > 0)
    keysdesc = concat2 (Fkey_description (string), build_string (" "));
  else
    keysdesc = Qnil;

  if (CONSP (map))
    describe_alist (Fcdr (map), keysdesc, describe_command,
		    partial, shadow);
  else
    describe_vector (map, keysdesc, describe_command,
		     partial, shadow);
}

describe_alist (alist, elt_prefix, elt_describer, partial, shadow)
     register Lisp_Object alist;
     Lisp_Object elt_prefix;
     int (*elt_describer) ();
     int partial;
     Lisp_Object shadow;
{
  Lisp_Object this;
  Lisp_Object tem1, tem2;
  Lisp_Object suppress;
  Lisp_Object kludge = Qnil;
  int first = 1;
  struct gcpro gcpro1, gcpro2;

  if (partial)
    suppress = intern ("suppress-keymap");

  for (; CONSP (alist); alist = Fcdr (alist))
    {
      QUIT;
      tem1 = Fcar (Fcar (alist));
      tem2 = get_keyelt (Fcdr (Fcar (alist)));
      if (NULL (tem2)) continue;
      if (XTYPE (tem2) == Lisp_Symbol && partial)
	{
	  this = Fget (tem2, suppress);
	  if (!NULL (this))
	    continue;
	}

      if (!NULL (shadow))
	{
	  Lisp_Object tem;
	  if (NULL (kludge)) kludge = build_string ("x");
	  XSTRING (kludge)->data[0] = XINT (tem1);
	  tem = Flookup_key (shadow, kludge);
	  if (!NULL (tem)) continue;
	}

      if (first)
	{
	  insert ("\n", 1);
	  first = 0;
	}

      GCPRO2 (elt_prefix, tem2);
      if (!NULL (elt_prefix))
	insert1 (elt_prefix);

      insert1 (Fsingle_key_description (tem1));

      (*elt_describer) (tem2);
      UNGCPRO;
    }
}

describe_vector (vector, elt_prefix, elt_describer, partial, shadow)
     register Lisp_Object vector;
     Lisp_Object elt_prefix;
     int (*elt_describer) ();
     int partial;
     Lisp_Object shadow;
{
  Lisp_Object this;
  Lisp_Object dummy;
  Lisp_Object tem1, tem2;
  register int i, size = XVECTOR (vector)->size;
  Lisp_Object suppress;
  Lisp_Object kludge;
  int first = 1;
  struct gcpro gcpro1, gcpro2;

  tem1 = Qnil;
  kludge = Qnil;
  GCPRO2 (elt_prefix, tem1);

  if (partial)
    suppress = intern ("suppress-keymap");

  for (i = 0; i < size; i++)
    {
      QUIT;
      tem1 = get_keyelt (XVECTOR (vector)->contents[i]);
      if (NULL (tem1)) continue;      
      if (XTYPE (tem1) == Lisp_Symbol && partial)
	{
	  this = Fget (tem1, suppress);
	  if (!NULL (this))
	    continue;
	}

      if (!NULL (shadow))
	{
	  Lisp_Object tem;
	  if (NULL (kludge)) kludge = build_string ("x");
	  XSTRING (kludge)->data[0] = XINT (i);
	  tem = Flookup_key (shadow, kludge);
	  if (!NULL (tem)) continue;
	}

      if (first)
	{
	  insert ("\n", 1);
	  first = 0;
	}

      if (!NULL (elt_prefix))
	insert1 (elt_prefix);

      XFASTINT (dummy) = i;
      insert1 (Fsingle_key_description (dummy));

      while (i + 1 < size
	     && (tem2 = get_keyelt (XVECTOR (vector)->contents[i+1]),
		 EQ (tem2, tem1)))
	i++;

      if (i != XINT (dummy))
	{
	  insert (" .. ", 4);
	  if (!NULL (elt_prefix))
	    insert1 (elt_prefix);

	  XFASTINT (dummy) = i;
	  insert1 (Fsingle_key_description (dummy));
	}

      (*elt_describer) (tem1);
    }

  UNGCPRO;
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
  struct buffer *old = current_buffer;
  register Lisp_Object symbol, col, tem;

  while (!NULL (list))
    {
      Lisp_Object min_cols;

      QUIT;

      symbol = Fcar (list);
      list = Fcdr (list);

      tem = Fwhere_is_internal (symbol, current_buffer->keymap, Qnil);
      tem = Fmapconcat (Qkey_description, tem, build_string (", "));
      XFASTINT (col) = 30;

      set_buffer_internal (XBUFFER (Vstandard_output));
      Fprin1 (symbol, Qnil);
      XFASTINT (min_cols) = 1;
      Findent_to (col, min_cols);
      Fprinc (tem, Qnil);
      Fterpri (Qnil);
      tem = Ffboundp (symbol);
      if (!NULL (tem))
        tem = Fdocumentation (symbol);
      if (XTYPE (tem) == Lisp_String)
	insert_first_line ("  Function: ", tem);
      tem = Fdocumentation_property (symbol, Qvariable_documentation);
      if (XTYPE (tem) == Lisp_String)
	insert_first_line ("  Variable: ", tem);
      set_buffer_internal (old);
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
  struct gcpro gcpro1;

  GCPRO1 (str);
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

  insert (p, p1 ? p1 - p : strlen (p));
  insert ("\n", 1);
  UNGCPRO;
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
  MouseMap = tem;
  Fset (intern ("mouse-map"), tem);
#endif /* HAVE_X_WINDOWS */

  tem = Fmake_keymap ();
  Vglobal_map = tem;
  Fset (intern ("global-map"), tem);

  tem = Fmake_keymap ();
  Vesc_map = tem;
  Fset (intern ("esc-map"), tem);
  Ffset (intern ("ESC-prefix"), tem);

  tem = Fmake_keymap ();
  Vctl_x_map = tem;
  Fset (intern ("ctl-x-map"), tem);
  Ffset (intern ("Control-X-prefix"), tem);

  DEFVAR_LISP ("minibuffer-local-map", &Vminibuffer_local_map,
    "Default keymap to use when reading from the minibuffer.");
  Vminibuffer_local_map = Fmake_sparse_keymap ();

  DEFVAR_LISP ("minibuffer-local-ns-map", &Vminibuffer_local_ns_map,
    "The keymap used by the minibuf for local bindings when spaces are not\n\
to be allowed in input string.");
  Vminibuffer_local_ns_map = Fmake_sparse_keymap ();

  DEFVAR_LISP ("minibuffer-local-completion-map", &Vminibuffer_local_completion_map,
    "Keymap to use when reading from the minibuffer with completion.");
  Vminibuffer_local_completion_map = Fmake_sparse_keymap ();

  DEFVAR_LISP ("minibuffer-local-must-match-map", &Vminibuffer_local_must_match_map,
    "Keymap to use when reading from the minibuffer with completion and\n\
an exact match of one of the completions is required.");
  Vminibuffer_local_must_match_map = Fmake_sparse_keymap ();

  global_map = Vglobal_map;

  Qsingle_key_description = intern ("single-key-description");
  staticpro (&Qsingle_key_description);

  Qkey_description = intern ("key-description");
  staticpro (&Qkey_description);

  Qkeymapp = intern ("keymapp");
  staticpro (&Qkeymapp);

  defsubr (&Skeymapp);
  defsubr (&Smake_keymap);
  defsubr (&Smake_sparse_keymap);
  defsubr (&Scopy_keymap);
  defsubr (&Skey_binding);
  defsubr (&Slocal_key_binding);
  defsubr (&Sglobal_key_binding);
  defsubr (&Sglobal_set_key);
  defsubr (&Slocal_set_key);
  defsubr (&Sdefine_key);
  defsubr (&Slookup_key);
  defsubr (&Sglobal_unset_key);
  defsubr (&Slocal_unset_key);
  defsubr (&Sdefine_prefix_command);
  defsubr (&Suse_global_map);
  defsubr (&Suse_local_map);
  defsubr (&Scurrent_local_map);
  defsubr (&Scurrent_global_map);
  defsubr (&Saccessible_keymaps);
  defsubr (&Skey_description);
  defsubr (&Ssingle_key_description);
  defsubr (&Stext_char_description);
  defsubr (&Swhere_is_internal);
  defsubr (&Swhere_is);
  defsubr (&Sdescribe_bindings);
  defsubr (&Sapropos);
}

keys_of_keymap ()
{
  ndefkey (Vglobal_map, 033, "ESC-prefix");
  ndefkey (Vglobal_map, Ctl ('X'), "Control-X-prefix");
}
