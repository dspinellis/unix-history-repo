/*
 *      HELP.H
 *      UTREE help definitions.
 *      3.01-um klin, Sat Apr 20 11:02:33 1991
 *      3.03-um klin, Sat Feb 15 18:33:14 1992, Minor changes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#if     defined(_MAIN_) && !defined(lint)
static char sccsid_help[] = "@(#) utree 3.03-um (klin) Feb 15 1992 help.h";
#endif  /* _MAIN_ && !lint */

/* Max length of a menu item in the help menuline                       */
#define ITEMLEN 16

/*
 *      Utree help pages are hold in a dynamically linked list of the
 *      following data structure _hlist which is built up at runtime.
 *      The record contains information about help items from a help
 *      file, i.e. a menu title or item, the starting position of a help
 *      page about this item in the help file and the number of lines
 *      of this help page. Displaying a help page about an item is
 *      therefore searching this item in the help page list, positioning
 *      to the starting position in the help file and putting as many
 *      lines from the helpfile to screen as this help page has.
 */

typedef struct _hlist {
  char item[ITEMLEN];           /* Menu line item                       */
  char hotkey;                  /* Menu hot key                         */
  int nlines;                   /* Number of lines in help file         */
  long position;                /* Start position in help file          */
  struct _hlist *next;          /* Pointer to next help page            */
} hlist;

#define HNULL   ((hlist *) 0)   /* The hlist NIL pointer                */

/*
 *      Access to items of hlist record is done with macros
 *      to hide this record and for abbreviation.
 */

#define HITEM(p)        ((p)->item)
#define HHKEY(p)        ((p)->hotkey)
#define HNLIN(p)        ((p)->nlines)
#define HSPOS(p)        ((p)->position)
#define HNEXT(p)        ((p)->next)
