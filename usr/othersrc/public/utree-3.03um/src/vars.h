/*
 *      VARS.H
 *      UTREE variables definitions.
 *      3.01-um klin, Wed May  1 10:29:37 1991
 *      3.03-um klin, Tue Feb 11 14:18:50 1992, Generic lists for variables
 *                                              and file type commands
 *              klin, Sat Feb 15 14:44:52 1992, Video handling and partinioning of
 *                                              directory and file windows changed
 *              klin, Sun Feb 23 18:45:19 1992, Keybindings and variable
 *                                              AUTOSAVE added
 *            e klin, Sat Apr 11 11:05:54 1992, Use colors for video attributes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#if     defined(_MAIN_) && !defined(lint)
static char sccsid_vars[] = "@(#) utree 3.03e-um (klin) Apr 11 1992 vars.h";
#endif  /* _MAIN_ && !lint */

/*
 *      File type dependent commands are hold in a dynamically linked
 *      list of struct _xlist.
 */

typedef struct _xlist {
  glist list;                   /*  Contains filetype and pointers      */
  char *command;                /*  Command format to execute           */
  char *comment;                /*  Additional comment                  */
} xlist;

#define XNULL   ((xlist *) 0)   /* The xlist NIL pointer                */

/*
 *      Access to entries in the file type command list is done
 *      with macros to hide this record and for abbreviation.
 */

#define XLIST(p)        (&(p)->list)
#define XTYPE(p)        ((p)->list.string)
#define XPREV(p)        ((p)->list.prev)
#define XNEXT(p)        ((p)->list.next)
#define XCOMD(p)        ((p)->command)
#define XCOMM(p)        ((p)->comment)

#ifdef  _MAIN_
xlist *xroot = XNULL;           /* Root of file type command list       */
#else   /* !_MAIN_ */
extern xlist *xroot;
#endif  /* _MAIN_ */

/*
 *      Variables are hold in a table of struct _vlist which is
 *      initialized and linked at startup. All predefined variables
 *      are accessed via index into this table.
 */

typedef struct _vlist {
  glist list;                   /*  Contains variable name and pointers */
  char *shortcut;               /*  Variable short cut                  */
  int type;                     /*  Type of variable                    */
  int number;                   /*  Variable number (see below)         */
  char *defval;                 /*  Default value (if defined)          */
  char *value;                  /*  Variable value                      */
  char *comment;                /*  Additional comment                  */
} vlist;

#define VNULL   ((vlist *) 0)   /* The vlist NIL pointer                */

/*
 *      Access to entries in the variable list is done with macros
 *      to hide this record and for abbreviation.
 */

#define VARLST(n)       (&vtable[n].list)
#define VARNAM(n)       (vtable[n].list.string)
#define VARPRV(n)       (vtable[n].list.prev)
#define VARNXT(n)       (vtable[n].list.next)
#define VARCUT(n)       (vtable[n].shortcut)
#define VARTYP(n)       (vtable[n].type)
#define VARNUM(n)       (vtable[n].number)
#define VARDEF(n)       (vtable[n].defval)
#define VARVAL(n)       (vtable[n].value)
#define VARCOM(n)       (vtable[n].comment)

/* Assign vtable entry to vlist pointer. Check if variable is set       */
#define VARTAB(n)       (&vtable[n])
#define VARSET(n)       (vtable[n].value)

/*
 *      The same macros for access via pointers.
 */

#define VLIST(p)        (&(p)->list)
#define VNAME(p)        ((p)->list.string)
#define VPREV(p)        ((p)->list.prev)
#define VNEXT(p)        ((p)->list.next)
#define VSCUT(p)        ((p)->shortcut)
#define VTYPE(p)        ((p)->type)
#define VNUMB(p)        ((p)->number)
#define VDFLT(p)        ((p)->defval)
#define VVALE(p)        ((p)->value)
#define VCOMM(p)        ((p)->comment)

/* Flag values for variables/commands settings                          */
#define VC_TST  0               /* Test only. Don't set                 */
#define VC_SET  1               /* Set but don't effect change flag     */
#define VC_CHG  2               /* Set and affect change flag           */

/* Types of variables                                                   */
#define VT_B    0               /* Boolean type                         */
#define VT_N    1               /* Numerical value                      */
#define VT_S    2               /* General string                       */
#define VT_U    3               /* User tree or file command string     */
#define VT_O    4               /* Other user defined strings           */

/* Values for boolean variables                                         */
#define VB_ON   ((char *) 1)
#define VB_OFF  ((char *) 0)

/* Indices in variable table. See variable table below                  */
#define V_BL    0               /* Allow bell                           */
#define V_CL    1               /* Clock in echo line                   */
#define V_GC    2               /* Use graphic character set            */
#define V_TS    3               /* Use terminal scroll funcs            */
#define V_ST    4               /* Scan tree for changes                */
#define V_WD    5               /* Warn about unreadable dirs           */
#define V_LS    6               /* Lexical sort filelists               */
#define V_AS    7               /* Save definition changes              */
#define V_VM    8               /* Video attribute using                */
#define V_TI    9               /* Tree indention                       */
#define V_FL    10              /* File lines on tree screen            */
#define V_HS    11              /* Size of history list                 */
#define V_SH    12              /* Interaktive shell                    */
#define V_ED    13              /* File editor                          */
#define V_EDO   14              /* Editor options                       */
#define V_PG    15              /* File viewer/pager                    */
#define V_PGO   16              /* Viewer options                       */
#define V_XD    17              /* File hex dumper                      */
#define V_XDO   18              /* Hex dumper options                   */
#define V_LP    19              /* File printer                         */
#define V_LPO   20              /* Printer options                      */
#define V_BK    21              /* Directory backup                     */
#define V_BKO   22              /* Backup options                       */
#define V_TC0   22              /* Dummy: used for calculation only     */
#define V_TC1   23              /* User defined tree commands           */
#define V_TC2   24
#define V_TC3   25
#define V_TC4   26
#define V_TC5   27
#define V_TC6   28
#define V_TC7   29
#define V_TC8   30
#define V_TC9   31
#define V_FC0   31              /* Dummy: used for calculation only     */
#define V_FC1   32              /* User defined file commands           */
#define V_FC2   33
#define V_FC3   34
#define V_FC4   35
#define V_FC5   36
#define V_FC6   37
#define V_FC7   38
#define V_FC8   39
#define V_FC9   40
#ifdef  USEANSICOLORS
# define V_UC   41              /* Color setting flag */
#endif  /* USEANSICOLORS */

/*
 *      Table defining default variable settings (See cmds.h).
 */

#ifdef  _MAIN_
vlist vtable[] = {
  { { "BELL",       GNULL, GNULL }, "BL",  VT_B, V_BL,  VB_ON,    NULL, "Allow ringing of bell" },
#ifdef  UTCLOCK
  { { "CLOCK",      GNULL, GNULL }, "CL",  VT_B, V_CL,  VB_ON,    NULL, "Display/update clock" },
#else   /* !UTCLOCK */
  { { "CLOCK",      GNULL, GNULL }, "CL",  VT_B, V_CL,  VB_OFF,   NULL, "Display/update clock" },
#endif  /* UTCLOCK */
  { { "GRAPHCHARS", GNULL, GNULL }, "GC",  VT_B, V_GC,  VB_ON,    NULL, "Use graphic charset" },
  { { "TERMSCROLL", GNULL, GNULL }, "TS",  VT_B, V_TS,  VB_ON,    NULL, "Use terminal scrolling" },
  { { "SCANTREE",   GNULL, GNULL }, "ST",  VT_B, V_ST,  VB_ON,    NULL, "Scan tree for changes" },
  { { "WARNDIRS",   GNULL, GNULL }, "WD",  VT_B, V_WD,  VB_ON,    NULL, "Directory warnings" },
  { { "LEXSORT",    GNULL, GNULL }, "LS",  VT_B, V_LS,  VB_ON,    NULL, "Lexical sort of filelists" },
  { { "AUTOSAVE",   GNULL, GNULL }, "AS",  VT_B, V_AS,  VB_ON,    NULL, "Save definition/history changes" },
  { { "VIDEOMODE",  GNULL, GNULL }, "VM",  VT_N, V_VM,  "2",      NULL, "Video mode setting (0..2)" },
  { { "TREEINDENT", GNULL, GNULL }, "TI",  VT_N, V_TI,  "9",      NULL, "Max tree indention (3..9)" },
  { { "FILELINES",  GNULL, GNULL }, "FL",  VT_N, V_FL,  "3",      NULL, "Max file lines (1..9)" },
  { { "HISTSIZE",   GNULL, GNULL }, "HS",  VT_N, V_HS,  "22",     NULL, "Size of history list (readonly)" },
  { { "SHELL",      GNULL, GNULL }, "SH",  VT_S, V_SH,  SHELL,    NULL, "Shell" },
  { { "EDITOR",     GNULL, GNULL }, "ED",  VT_S, V_ED,  EDITPRG,  NULL, "Text editor" },
  { { "EDITOPTS",   GNULL, GNULL }, "EO",  VT_S, V_EDO, NULL,     NULL, "Editor options" },
  { { "PAGER",      GNULL, GNULL }, "PG",  VT_S, V_PG,  PAGEPRG,  NULL, "Text pager" },
  { { "PAGEOPTS",   GNULL, GNULL }, "PO",  VT_S, V_PGO, NULL,     NULL, "Pager options" },
  { { "XDUMPER",    GNULL, GNULL }, "XD",  VT_S, V_XD,  DUMPPRG,  NULL, "Hex dumper" },
  { { "XDUMPOPTS",  GNULL, GNULL }, "XO",  VT_S, V_XDO, NULL,     NULL, "Dumper options" },
  { { "LPRINTER",   GNULL, GNULL }, "LP",  VT_S, V_LP,  PRINTPRG, NULL, "Printer command" },
  { { "LPRINTOPTS", GNULL, GNULL }, "LO",  VT_S, V_LPO, NULL,     NULL, "Printer options" },
#ifdef UTBCKUP
  { { "BACKUP",     GNULL, GNULL }, "BK",  VT_S, V_BK,  UTBCKUP,  NULL, "Backup program" },
#else   /* !UTBCKUP */
  { { "BACKUP",     GNULL, GNULL }, "BK",  VT_S, V_BK,  NULL,     NULL, "Backup program" },
#endif  /* UTBCKUP */
  { { "BACKUPOPTS", GNULL, GNULL }, "BO",  VT_S, V_BKO, NULL,     NULL, "Backup options" },
  { { "TREECMD1",   GNULL, GNULL }, "T1",  VT_U, V_TC1, NULL,     NULL, NULL },
  { { "TREECMD2",   GNULL, GNULL }, "T2",  VT_U, V_TC2, NULL,     NULL, NULL },
  { { "TREECMD3",   GNULL, GNULL }, "T3",  VT_U, V_TC3, NULL,     NULL, NULL },
  { { "TREECMD4",   GNULL, GNULL }, "T4",  VT_U, V_TC4, NULL,     NULL, NULL },
  { { "TREECMD5",   GNULL, GNULL }, "T5",  VT_U, V_TC5, NULL,     NULL, NULL },
  { { "TREECMD6",   GNULL, GNULL }, "T6",  VT_U, V_TC6, NULL,     NULL, NULL },
  { { "TREECMD7",   GNULL, GNULL }, "T7",  VT_U, V_TC7, NULL,     NULL, NULL },
  { { "TREECMD8",   GNULL, GNULL }, "T8",  VT_U, V_TC8, NULL,     NULL, NULL },
  { { "TREECMD9",   GNULL, GNULL }, "T9",  VT_U, V_TC9, NULL,     NULL, NULL },
  { { "FILECMD1",   GNULL, GNULL }, "F1",  VT_U, V_FC1, NULL,     NULL, NULL },
  { { "FILECMD2",   GNULL, GNULL }, "F2",  VT_U, V_FC2, NULL,     NULL, NULL },
  { { "FILECMD3",   GNULL, GNULL }, "F3",  VT_U, V_FC3, NULL,     NULL, NULL },
  { { "FILECMD4",   GNULL, GNULL }, "F4",  VT_U, V_FC4, NULL,     NULL, NULL },
  { { "FILECMD5",   GNULL, GNULL }, "F5",  VT_U, V_FC5, NULL,     NULL, NULL },
  { { "FILECMD6",   GNULL, GNULL }, "F6",  VT_U, V_FC6, NULL,     NULL, NULL },
  { { "FILECMD7",   GNULL, GNULL }, "F7",  VT_U, V_FC7, NULL,     NULL, NULL },
  { { "FILECMD8",   GNULL, GNULL }, "F8",  VT_U, V_FC8, NULL,     NULL, NULL },
  { { "FILECMD9",   GNULL, GNULL }, "F9",  VT_U, V_FC9, NULL,     NULL, NULL },
#ifdef  USEANSICOLORS
  { { "USECOLORS",  GNULL, GNULL }, "UC",  VT_B, V_UC,  VB_ON,    NULL, "Use colors if defined" },
#endif  /* USEANSICOLORS */
  { { NULL } }                  /* End of predefined variables          */
};
/* Possible user defined variables may follow, but are not implemented  */
#else
extern vlist vtable[];
#endif  /* _MAIN_ */
