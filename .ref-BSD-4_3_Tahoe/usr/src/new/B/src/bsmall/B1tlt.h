/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: B1tlt.h,v 1.1 84/06/28 00:48:39 timo Exp $ */

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
