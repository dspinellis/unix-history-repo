/*
 *
 * zle.h - header file for line editor
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#ifdef ZLEGLOBALS
#define ZLEXTERN
#else
#define ZLEXTERN extern
#endif

#ifdef ZLE

/* cursor position */
ZLEXTERN int cs;

/* line length */
ZLEXTERN int ll;

/* size of line buffer */
ZLEXTERN int linesz;

/* location of mark */
ZLEXTERN int mark;

/* last character pressed */
ZLEXTERN int c;

/* the z_ binding id for this key */
ZLEXTERN int bindk;

/* command argument */
ZLEXTERN int mult;

/* insert mode/overwrite mode flag */
ZLEXTERN int insmode;

/* cost of last update */
ZLEXTERN int cost;

/* flags associated with last command */
ZLEXTERN int lastcmd;

/* column position before last LINEMOVE movement */
ZLEXTERN int lastcol;

/* != 0 if we're getting a vi range */
ZLEXTERN int virangeflag;

/* kludge to get cw and dw to work right */
ZLEXTERN int wordflag;

#endif

/* last named command done */
ZLEXTERN int lastnamed;

/* != 0 if we're done editing */
ZLEXTERN int done;

/* length of prompt on screen */
ZLEXTERN int pptlen;

/* current history line number */
ZLEXTERN int histline;

ZLEXTERN int eofsent;

/* != 0 if we need to call resetvideo() */
ZLEXTERN int resetneeded;

/* != 0 if the line editor is active */
ZLEXTERN int zleactive;

/* the line buffer */
ZLEXTERN unsigned char *line;

/* the cut buffer */
ZLEXTERN char *cutbuf;

/* prompt and rprompt */
ZLEXTERN char *pmpt, *pmpt2;

/* the last line in the history (the current one) */
ZLEXTERN char *curhistline;

/* the status line */
ZLEXTERN char *statusline;

/* 1 if a complete added a slash at the end of a directory name */
ZLEXTERN int addedslash;

/*
	the current history line and cursor position for the top line
	on the buffer stack
*/

ZLEXTERN int stackhist,stackcs;

/* != 0 if we are in the middle of a menu completion */
ZLEXTERN int menucmp;

/* != 0 if we are making undo records */
ZLEXTERN int undoing;

/* last vi change buffer */
ZLEXTERN int vichgbufsz,vichgbufptr,vichgflag;
ZLEXTERN char *vichgbuf;

ZLEXTERN int viinsbegin;

typedef void bindfunc DCLPROTO((void));
typedef bindfunc *F;

struct key {
	struct hashnode *next; int canfree; char *nam; /* hash data */
	int func;			/* function code for this key */
	char *str;			/* string corresponding to this key,
								if func = z_sequenceleadin				 */
	int len;				/* length of string */
	};
struct zlecmd {
	char *name;			/* name of function */
	F func;				/* handler function */
	int flags;
	};

/* undo event */

struct undoent {
	int pref;		/* number of initial chars unchanged */
	int suff;		/* number of trailing chars unchanged */
	int len;			/* length of changed chars */
	int cs;			/* cursor pos before change */
	char *change;	/* NOT null terminated */
	};

#define UNDOCT 64

struct undoent undos[UNDOCT];

/* the line before last mod (for undo purposes) */
ZLEXTERN unsigned char *lastline;

/* buffer specified with "x */
ZLEXTERN int vibufspec;

ZLEXTERN int undoct,lastcs;

ZLEXTERN char *visrchstr;
ZLEXTERN int visrchsense;

#define ZLE_MOVEMENT  	 1
#define ZLE_MENUCMP  	 2
#define ZLE_UNDO      	 4
#define ZLE_YANK     	 8
#define ZLE_LINEMOVE 	 16
#define ZLE_ARG      	 32
#define ZLE_NAMEDBUFFER 128
#define ZLE_KILL        (64|ZLE_NAMEDBUFFER)
#define ZLE_HISTSEARCH  256
#define ZLE_NEGARG      512
#define ZLE_INSERT     1024

typedef struct key *Key;

ZLEXTERN int *bindtab;
extern int emacsbind[256];
ZLEXTERN int altbindtab[256],mainbindtab[256];
extern int viinsbind[],vicmdbind[];
ZLEXTERN int vimarkcs[27],vimarkline[27];

#define KRINGCT 8
ZLEXTERN char *kring[KRINGCT];
ZLEXTERN int kringnum;
ZLEXTERN char *vibuf[36];

#define z_acceptandhold 0
#define z_acceptandinfernexthistory 1
#define z_acceptandmenucomplete 2
#define z_acceptline 3
#define z_acceptlineanddownhistory 4
#define z_backwardchar 5
#define z_backwarddeletechar 6
#define z_backwarddeleteword 7
#define z_backwardkillline 8
#define z_backwardkillword 9
#define z_backwardword 10
#define z_beginningofbufferorhistory 11
#define z_beginningofhistory 12
#define z_beginningofline 13
#define z_beginningoflinehist 14
#define z_capitalizeword 15
#define z_clearscreen 16
#define z_completeword 17
#define z_copyprevword 18
#define z_copyregionaskill 19
#define z_deletechar 20
#define z_deletecharorlist 21
#define z_deleteword 22
#define z_digitargument 23
#define z_downcaseword 24
#define z_downhistory 25
#define z_downlineorhistory 26
#define z_endofbufferorhistory 27
#define z_endofhistory 28
#define z_endofline 29
#define z_endoflinehist 30
#define z_exchangepointandmark 31
#define z_executelastnamedcmd 32
#define z_executenamedcmd 33
#define z_expandhistory 34
#define z_expandorcomplete 35
#define z_expandword 36
#define z_forwardchar 37
#define z_forwardword 38
#define z_getline 39
#define z_gosmacstransposechars 40
#define z_historyincrementalsearchbackward 41
#define z_historyincrementalsearchforward 42
#define z_historysearchbackward 43
#define z_historysearchforward 44
#define z_infernexthistory 45
#define z_insertlastword 46
#define z_killbuffer 47
#define z_killline 48
#define z_killregion 49
#define z_killwholeline 50
#define z_listchoices 51
#define z_listexpand 52
#define z_magicspace 53
#define z_menucompleteword 54
#define z_menuexpandorcomplete 55
#define z_overwritemode 56
#define z_pushline 57
#define z_quotedinsert 58
#define z_quoteline 59
#define z_quoteregion 60
#define z_redisplay 61
#define z_reversemenucomplete 62
#define z_runhelp 63
#define z_selfinsert 64
#define z_selfinsertunmeta 65
#define z_sendbreak 66
#define z_sendstring 67
#define z_sequenceleadin 68
#define z_setmarkcommand 69
#define z_spellword 70
#define z_toggleliteralhistory 71
#define z_transposechars 72
#define z_transposewords 73
#define z_undefinedkey 74
#define z_undo 75
#define z_universalargument 76
#define z_upcaseword 77
#define z_uphistory 78
#define z_uplineorhistory 79
#define z_viaddeol 80
#define z_viaddnext 81
#define z_vibackwardblankword 82
#define z_vibackwardchar 83
#define z_vibackwarddeletechar 84
#define z_vibeginningofline 85
#define z_vicapslockpanic 86
#define z_vichange 87
#define z_vichangeeol 88
#define z_vichangewholeline 89
#define z_vicmdmode 90
#define z_videlete 91
#define z_videletechar 92
#define z_vidigitorbeginningofline 93
#define z_viendofline 94
#define z_vifetchhistory 95
#define z_vifindnextchar 96
#define z_vifindnextcharskip 97
#define z_vifindprevchar 98
#define z_vifindprevcharskip 99
#define z_vifirstnonblank 100
#define z_viforwardblankword 101
#define z_viforwardblankwordend 102
#define z_viforwardchar 103
#define z_viforwardwordend 104
#define z_vigotocolumn 105
#define z_vigotomark 106
#define z_vigotomarkline 107
#define z_vihistorysearchbackward 108
#define z_vihistorysearchforward 109
#define z_viindent 110
#define z_viinsert 111
#define z_viinsertbol 112
#define z_vijoin 113
#define z_vimatchbracket 114
#define z_viopenlineabove 115
#define z_viopenlinebelow 116
#define z_vioperswapcases 117
#define z_viputafter 118
#define z_virepeatchange 119
#define z_virepeatfind 120
#define z_virepeatsearch 121
#define z_vireplace 122
#define z_vireplacechars 123
#define z_virevrepeatfind 124
#define z_virevrepeatsearch 125
#define z_visetbuffer 126
#define z_visetmark 127
#define z_visubstitute 128
#define z_viswapcase 129
#define z_viundochange 130
#define z_viunindent 131
#define z_viyank 132
#define z_viyankeol 133
#define z_whichcommand 134
#define z_yank 135
#define z_yankpop 136
#define z_emacsbackwardword 137
#define z_emacsforwardword 138
#define z_killword 139
#define z_vikillline 140
#define z_vibackwardkillword 141
#define z_expandcmdpath 142
#define z_negargument 143
#define z_poundinsert 144
#define z_viforwardword 145
#define z_vibackwardword 146
#define ZLECMDCOUNT 147

extern struct zlecmd zlecmds[];

