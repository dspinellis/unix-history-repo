/*
 *      CMDS.H
 *      UTREE system command and filename definitions.
 *      3.03-um klin, Tue Feb 11 19:23:58 1992, Splitted from conf.h
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#if     defined(_MAIN_) && !defined(lint)
static char sccsid_cmds[] = "@(#) utree 3.03-um (klin) Feb 11 1992 cmds.h";
#endif  /* _MAIN_ && !lint */

/*
 *      SOME DEFAULT SYSTEM COMMANDS.
 */

/* May be overriden by environment or utree variables.                  */
#ifdef  BSD
# define SHELL    "csh"         /* Default shell. See csh(1)            */
# define EDITPRG  "vi"          /* Default editor. See vi(1)            */
# define PAGEPRG  "more"        /* Default pager. See more(1)           */
# define DUMPPRG  "od"          /* Default dumper. See od(1)            */
# define PRINTPRG "lpr"         /* Default printer. See lpr(1)          */
#else   /* SYSV */
# define SHELL    "sh"          /* Default shell. See sh(1)             */
# define EDITPRG  "ed"          /* Default editor. See ed(1)            */
# define PAGEPRG  "pg"          /* Default pager. See pg(1)             */
# define DUMPPRG  "od"          /* Default dumper. See od(1)            */
# define PRINTPRG "lp"          /* Default printer. See lp(1)           */
#endif  /* BSD */

#define CPFILE  "cp"            /* Copy files. See cp(1)                */
#define MVFILE  "mv"            /* Move files. See mv(1)                */
#define LSFILE  "ls"            /* List files. See ls(1)                */
#define RMFILE  "rm"            /* Remove files. See rm(1)              */
#define RMOPTS  "-rf"           /* Remove all option                    */
#define MKDIR   "mkdir"         /* Create directory. See mkdir(1)       */
#define RMDIR   "rmdir"         /* Remove directory. See rmdir(1)       */
#define DUDIR   "du -s"         /* Directory disk usage. See du(1)      */
#define GRFILE  "grep"          /* Search in files. See grep(1)         */
#define GROPTS  "-c"            /* Print count only option              */

/*
 *      FILENAMES USED BY UTREE.
 */

#define UTBCKUP "utree.backup"  /* Utree backup shell script            */
#define UTBACK  "utree.bak"     /* Utree backup file list               */
#define UTHELP  "utree.help"    /* Utree help pages                     */
#define UTSTART "utree"         /* Utree startup file in home or libdir */
#define UTLIST  ".utreelist"    /* Utree tree list file in $HOME        */
#define UTHIST  ".utreehist"    /* Utree command history file in $HOME  */
