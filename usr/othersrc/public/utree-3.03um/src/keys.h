/*
 *      KEYS.H
 *      UTREE key definitions.
 *      3.01-um klin, Sat Apr 20 11:02:33 1991
 *      3.02-um klin, Sun Nov 10 19:46:21 1991, Function key handling changed
 *              klin, Sun Nov 24 12:12:13 1991, Extensions for XENIX reported
 *                                              by Rolf Gebhardt (RG 11/22/91)
 *      3.03-um klin, Sat Feb 11 19:52:04 1992, Line editor extended
 *              klin, Sun Feb 23 17:34:01 1992, Key handling and key bindings
 *            a klin, Sun Mar 15 19:08:25 1992, Bug fix in handling 8 bit chars
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#if     defined(_MAIN_) && !defined(lint)
static char sccsid_keys[] = "@(#) utree 3.03a-um (klin) Mar 15 1992 keys.h";
#endif  /* _MAIN_ && !lint */

/*
 *      Return values from getline() and editline()
 */

#define IN_OK   1               /* Input not empty and ok               */
#define IN_NUL  0               /* Input empty                          */
#define IN_INT  -1              /* Interrupt on input                   */
#define IN_EOF  -2              /* EOF on input                         */
#define IN_HLP  -3              /* Help key on input                    */
#define IN_NXT  -4              /* Next key on input                    */
#define IN_PRV  -5              /* Previous key on input                */
#define IN_SIZ  -6              /* Screen size changed                  */

/*
 *      Key symbols and initial bindings = return values from getkey()
 */

#define K_SEL   0x000a          /* Select/accept line = NEWLINE         */
#define K_FORW  0xff10          /* Forward character/file/directory     */
#define K_BACK  0xff11          /* Backward character/file/directory    */
#define K_NEXT  0xff12          /* Next listentry/file/directory        */
#define K_PREV  0xff13          /* Previous listentry/file/directory    */
#define K_NPAG  0xff14          /* Next page                            */
#define K_PPAG  0xff15          /* Previous page                        */
#define K_HOME  0xff16          /* Beginning line/files/directories     */
#define K_END   0xff17          /* End line/files/directories           */
#define K_UP    0xff18          /* Scroll up files/directories          */
#define K_DOWN  0xff19          /* Scroll down or delete character      */
#define K_INS   0xff20          /* Change to dir/toggle insert mode     */
#define K_DEL   0xff21          /* Change to parent/delete char back    */
#define K_KILL  0xff22          /* Kill line                            */
#define K_MARK  0xff23          /* Set mark on line/file/directory      */
#define K_GOTO  0xff24          /* Goto mark on line/file/directory     */
#define K_TAG   0xff25          /* Goto tagged dir/file or transpose    */
#define K_HELP  0xff26          /* Help                                 */
#define K_REFR  0xff27          /* Refresh                              */
#define K_CANC  0xff28          /* Cancel line                          */
#define K_BRK   0xff29          /* Break command/line                   */
#define K_EOF   0xff30          /* EOF                                  */
#define K_STR   0xffe0          /* Insert string                        */
#define K_INV   0xffff          /* Invalid symbol                       */
#define K_SIZE  0xff            /* Screen size changed                  */

/*
 *      All key symbol definitions are hold in an array of record type
 *      struct _kname to permit dynamic key bindings.
 */

typedef unsigned short ksym;    /* Type of key symbol                   */
typedef unsigned char kchar;    /* Type of key strings                  */
#define UNULL   ((kchar *) 0)   /* The kchar NIL pointer                */

typedef struct _kname {
  glist list;                   /* Contains key name and pointers       */
  ksym  symbol;                 /* Key symbol (See definitions above)   */
  char *comment;                /* Meaning of key                       */
} kname;

#ifdef  _MAIN_
kname keynames[] = {
  { { "SELECT",   GNULL, GNULL }, K_SEL,  "Select/accept line" },
  { { "FORWARD",  GNULL, GNULL }, K_FORW, "Forward character/file/directory" },
  { { "BACKWARD", GNULL, GNULL }, K_BACK, "Backward character/file/directory" },
  { { "NEXT",     GNULL, GNULL }, K_NEXT, "Next string/file/directory" },
  { { "PREVIOUS", GNULL, GNULL }, K_PREV, "Previous string/file/directory" },
  { { "NEXTPAGE", GNULL, GNULL }, K_NPAG, "Next page/scroll forward line" },
  { { "PREVPAGE", GNULL, GNULL }, K_PPAG, "Previous page/scroll backward line" },
  { { "BEGIN",    GNULL, GNULL }, K_HOME, "Beginning of line/files/directories" },
  { { "END",      GNULL, GNULL }, K_END,  "End of line/files/directories" },
  { { "UP",       GNULL, GNULL }, K_UP,   "Scroll up files/directories" },
  { { "DOWN",     GNULL, GNULL }, K_DOWN, "Scroll down or delete character" },
  { { "INSERT",   GNULL, GNULL }, K_INS,  "Change to dir/toggle insert mode" },
  { { "DELETE",   GNULL, GNULL }, K_DEL,  "Change to parent/delete char backward" },
  { { "KILL",     GNULL, GNULL }, K_KILL, "Kill input line", },
  { { "SETMARK",  GNULL, GNULL }, K_MARK, "Set mark on line/file/directory" },
  { { "GOTOMARK", GNULL, GNULL }, K_GOTO, "Goto mark on line/file/directory" },
  { { "GOTOTAG",  GNULL, GNULL }, K_TAG,  "Goto tagged dir/file or transpose" },
  { { "HELP",     GNULL, GNULL }, K_HELP, "Help" },
  { { "REFRESH",  GNULL, GNULL }, K_REFR, "Refresh screen/input line" },
  { { "CANCEL",   GNULL, GNULL }, K_CANC, "Delete input line" },
  { { "BREAK",    GNULL, GNULL }, K_BRK,  "Break command/line" },
  { { "EXIT",     GNULL, GNULL }, K_EOF,  "Exit" },
  { { "\"",       GNULL, GNULL }, K_STR,  "Insert string" },
  { { NULL } }                  /* End of entries                       */
};
#else   /* ! _MAIN_ */
extern kname keynames[];
#endif  /* _MAIN_ */

/*
 *      Access to entries in the keyname list is done with macros
 *      to hide this record and for abbreviation.
 */

#define KNLST(n)        (&keynames[n].list)
#define KNNAM(n)        (keynames[n].list.string)
#define KNPRV(n)        (keynames[n].list.prev)
#define KNNXT(n)        (keynames[n].list.next)
#define KNSYM(n)        (keynames[n].symbol)
#define KNCOM(n)        (keynames[n].comment)

/*
 *      Key bindings are hold in a dynamically linked list of
 *      record type klist. The key binding and translation list
 *      is built up at startup time from default bindings and
 *      user defined bindings.
 */

typedef struct _klist {
  glist list;                   /* Contains key name and pointers       */
  kchar *string;                /* Key string                           */
  ksym  symbol;                 /* Key symbol (See defines above)       */
  char  *insert;                /* String to insert                     */
  int   userdefined;            /* User defined binding                 */
} klist;

#define KNULL   ((klist *) 0)   /* The klist NIL pointer                */

/*
 *      Acces to items of klist record is done with macros
 *      to hide this record and for abbreviation.
 */

#define KBNAM(p)        ((p)->list.string)
#define KBPRV(p)        ((p)->list.prev)
#define KBNXT(p)        ((p)->list.next)
#define KBSYM(p)        ((p)->symbol)
#define KBSTR(p)        ((p)->string)
#define KBINS(p)        ((p)->insert)
#define KBUSR(p)        ((p)->userdefined)

/* Special macro for key string comparison character per character      */
#define KBCHR(p, n)     ((p)->string[n])

#ifdef  _MAIN_
klist *kroot = KNULL;
#else
extern klist *kroot;
#endif  /* _MAIN_ */
