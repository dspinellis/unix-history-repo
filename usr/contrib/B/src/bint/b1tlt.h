/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1tlt.h,v 1.4 85/08/22 16:42:12 timo Exp $
*/

#ifndef INTEGRATION

/* Private definitions for B texts, lists and tables */

typedef struct telita {
    HEADER; btreeptr root;
} a_telita, *telita;

#define Itemtype(v) (((telita) (v))->len) /* Itemtype */
#define Root(v) (((telita) (v))->root)
#define Tltsize(v) (Root(v) EQ Bnil ? 0 : Size(Root(v)))

#define Character(v)	((bool) (Type(v) EQ Tex && Tltsize(v) EQ 1))
value mkchar(); 	/* char c */

#else INTEGRATION

/************************************************************************/
/* Private definitions for small texts, lists and tables module         */
/* A text is modelled as a sequence of len characters.                  */
/*                                                                      */
/* A list is modelled as a sequence of len values,                      */
/*         each of which corresponds to a list entry.                   */
/*                                                                      */
/* A table is modelled as a sequence of len values,                     */
/*         each of which corresponds to a table entry;                  */
/*     table entries are modelled as a compound with two fields.        */
/************************************************************************/

#define Cts(v) (*Ats(v))
#define Dts(v) (*(Ats(v)+1))

#define List_elem(l, i) (*(Ats(l)+i)) /*counts from 0; takes no copy*/
#define Key(t, i) (Ats(*(Ats(t)+i))) /*Ditto*/
#define Assoc(t, i) (Ats(*(Ats(t)+i))+1) /*Ditto*/

bool found();
value list_elem();
value key_elem();

#endif INTEGRATION
