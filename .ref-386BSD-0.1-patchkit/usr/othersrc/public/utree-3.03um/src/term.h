/*
 *      TERM.H
 *      UTREE terminal and screen definitions.
 *      3.01-um klin, Wed May  1 14:34:34 1991
 *              klin, Sat Oct 26 15:27:00 1991, Some additions
 *      3.02-um klin, Fri Nov  1 10:44:45 1991, Screen layout changed
 *              klin, Sun Nov 24 15:12:48 1991, Video attributes changed
 *      3.03-um klin, Tue Feb 11 19:39:09 1992, Video attributes changed
 *            e klin, Sat Apr 11 11:05:54 1992, Use colors for video attributes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#if     defined(_MAIN_) && !defined(lint)
static char sccsid_term[] = "@(#) utree 3.03e-um (klin) Apr 1992 term.h";
#endif  /* _MAIN_ && !lint */

#define MINCOLS 80              /* Min number of screen columns needed  */
#define MINLINS 24              /* Min number of screen lines needed    */

/* Return values from getline()                                         */
#define EMPTY   0               /* Input is empty                       */
#define DONE    1               /* Input not empty and ok               */

/* Video attribute flags                                                */
#define VA_NORMAL       0x00    /* Reset all video attributes           */
#define VA_REVERSE      0x01    /* Video reverse   attribute            */
#define VA_BLINK        0x02    /* Video blink     attribute            */
#define VA_HALF         0x04    /* Half bright     attribute            */
#define VA_BOLD         0x08    /* Video bold      attribute            */
#define VA_UNDERLINE    0x10    /* Video underline attribute            */

/* Cursor functions                                                     */
#define CF_VISIBLE      0x01    /* Cursor visible                       */
#define CF_INVISIBLE    0x02    /* Cursor invisible                     */
#define CF_SAVE         0x04    /* Save cursor position                 */
#define CF_RESTORE      0x08    /* Restore cursor position              */

/* Keypad functions                                                     */
#define KP_NORMAL       0x00    /* Switch keypad to normal mode         */
#define KP_XMIT         0x01    /* Switch keypad to transmit mode       */

/* Graphical charset functions                                          */
#define GC_OFF          0x00    /* Turn off graph charset               */
#define GC_ON           0x01    /* Turn on graph charset                */

/* The graph charset is accessed and hidden by the following macros     */
#define GC_HB   _graphset[0]    /* Horizontal bar                       */
#define GC_VB   _graphset[1]    /* Vertical bar                         */
#define GC_LT   _graphset[2]    /* Left tee                             */
#define GC_RT   _graphset[3]    /* Right tree                           */
#define GC_TT   _graphset[4]    /* Top tee                              */
#define GC_BT   _graphset[5]    /* Bottom tee                           */
#define GC_UL   _graphset[6]    /* Upper left corner                    */
#define GC_LL   _graphset[7]    /* Lower left corner                    */
#define GC_UR   _graphset[8]    /* Upper right corner                   */
#define GC_LR   _graphset[9]    /* Lower right corner                   */
#define GC_TG   _graphset[10]   /* Tag sign (diamond or plus)           */

#define NGRAPH  11              /* Max number of graphic characters     */

#ifdef  USEANSICOLORS
# define CS_BLACK       0       /* Ansi compatible color codes          */
# define CS_RED         1
# define CS_GREEN       2
# define CS_BROWN       3
# define CS_BLUE        4
# define CS_MAGENTA     5
# define CS_CYAN        6
# define CS_WHITE       7
# define CS_INIT        -1
# define CS_RESET       -2
#endif  /* USEANSICOLORS */

#ifdef _MAIN_
int columns, lines;             /* Number of screen columns and lines   */
int glitchcap;                  /* Standout mode/underline glitch flag  */
int scrollcap;                  /* Window/scrolling capabilities flag   */
int cursorcap;                  /* Cursor capabilities                  */
int videocap;                   /* Video capabilities flag              */
int graphcap;                   /* Graphic character set flag           */
char _graphset[NGRAPH];         /* Graphic character set                */
# ifdef  USEANSICOLORS
int colorcap;                   /* Can colors                           */
# endif /* USEANSICOLORS */
#else   /* !_MAIN_ */
extern int columns, lines;
extern int glitchcap, scrollcap, cursorcap, videocap, graphcap;
extern char _graphset[];
# ifdef  USEANSICOLORS
extern int colorcap;
# endif /* USEANSICOLORS */
#endif  /* _MAIN_ */

#ifdef  putchar                 /* Don't use stdio.h's putchar macro    */
# undef putchar
#endif
#ifdef  getchar                 /* Don't use stdio.h's getchar macro    */
# undef getchar
#endif

