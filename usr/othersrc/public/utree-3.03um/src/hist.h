/*
 *      HIST.H
 *      UTREE history list definitions.
 *      3.03-um klin, Tue Feb 11 22:35:29 1992, Shell command history added
 *            d klin, Thu Apr  2 09:07:41 1992, Time stamps for history added
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#if     defined(_MAIN_) && !defined(lint)
static char sccsid_hist[] = "@(#) utree 3.03d-um (klin) Apr  2 1992 hist.h";
#endif  /* _MAIN_ && !lint */

/*
 *      Shell commands are hold in a dynamically lined list of
 *      struct _clist.
 */

typedef struct _clist {
  glist list;                   /* Containing command and pointers      */
  int length;                   /* Length of current command            */
  int number;                   /* Event number                         */
  int counter;                  /* Event counter                        */
  time_t time;                  /* Time stamp                           */
} clist;

#define CHNULL  ((clist *) 0)   /* The clist NIL pointer                */

/*
 *      Access to entries in the command history list is done
 *      with macros to hide this record and for abbreviation.
 */

#define CHLST(p)        (&(p)->list)
#define CHCMD(p)        ((p)->list.string)
#define CHPRV(p)        ((p)->list.prev)
#define CHNXT(p)        ((p)->list.next)
#define CHLEN(p)        ((p)->length)
#define CHNUM(p)        ((p)->number)
#define CHCNT(p)        ((p)->counter)
#define CHTIM(p)        ((p)->time)
