/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/ed.defns.c,v 3.10 1991/11/26 04:28:26 christos Exp $ */
/*
 * ed.defns.c: Editor function definitions and initialization
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: ed.defns.c,v 3.10 1991/11/26 04:28:26 christos Exp $")

#include "ed.h"

static	void		ed_InitMetaBindings 	__P((void));

PFCmd   CcFuncTbl[] = {		/* table of available commands */
    e_unassigned,
/* no #define here -- this is a dummy to detect initing of the key map */
    e_unassigned,
#define		F_UNASSIGNED	1
    e_insert,
#define		F_INSERT	2
    e_newline,
#define		F_NEWLINE	3
    e_delprev,
#define		F_DELPREV	4
    e_delnext,
#define		F_DELNEXT	5
    e_toend,
#define		F_TOEND		6
    e_tobeg,
#define		F_TOBEG		7
    e_charback,
#define		F_CHARBACK	8
    e_charfwd,
#define		F_CHARFWD	9
    e_quote,
#define		F_QUOTE		10
    e_startover,
#define		F_STARTOVER	11
    e_redisp,
#define		F_REDISP	12
    e_tty_int,
#define		F_TTY_INT	13
    e_wordback,
#define		F_WORDBACK	14
    e_wordfwd,
#define		F_WORDFWD	15
    e_cleardisp,
#define		F_CLEARDISP	16
    e_complete,
#define		F_COMPLETE	17
    e_correct,
#define		F_CORRECT	18
    e_up_hist,
#define		F_UP_HIST	19
    e_down_hist,
#define		F_DOWN_HIST	20
    e_up_search_hist,
#define		F_UP_SEARCH_HIST	21
    e_down_search_hist,
#define		F_DOWN_SEARCH_HIST	22
    e_helpme,
#define		F_HELPME	23
    e_list_choices,
#define		F_LIST_CHOICES	24
    e_delwordprev,
#define		F_DELWORDPREV	25
    e_delwordnext,
#define		F_DELWORDNEXT	26
    e_digit,
#define		F_DIGIT		27
    e_killend,
#define		F_KILLEND	28
    e_killbeg,
#define		F_KILLBEG	29
    e_metanext,
#define		F_METANEXT	30
    e_send_eof,
#define		F_SEND_EOF	31
    e_charswitch,
#define		F_CHARSWITCH	32
    e_which,
#define		F_WHICH		33
    e_yank_kill,
#define		F_YANK_KILL	34
    e_tty_dsusp,
#define		F_TTY_DSUSP	35
    e_tty_flusho,
#define		F_TTY_FLUSHO	36
    e_tty_quit,
#define		F_TTY_QUIT	37
    e_tty_tsusp,
#define		F_TTY_TSUSP	38
    e_tty_stopo,
#define		F_TTY_STOPO	39
    e_tty_starto,
#define		F_TTY_STARTO	40
    e_argfour,
#define		F_ARGFOUR	41
    e_set_mark,
#define		F_SET_MARK	42
    e_exchange_mark,
#define		F_EXCHANGE_MARK	43
    e_last_item,
#define		F_LAST_ITEM	44
    e_list_delnext,
#define		F_LIST_DELNEXT	45
    v_cmd_mode,
#define		V_CMD_MODE	46
    v_insert,
#define		V_INSERT	47
    e_argdigit,
#define		F_ARGDIGIT	48
    e_killregion,
#define		F_KILLREGION	49
    e_copyregion,
#define		F_COPYREGION	50
    e_gcharswitch,
#define		F_GCHARSWITCH	51
    e_run_fg_editor,
#define		F_RUN_FG_EDITOR	52
    e_unassigned,	/* place holder for sequence lead in character */
#define		F_XKEY		53
    e_uppercase,
#define         F_CASEUPPER     54
    e_lowercase,
#define         F_CASELOWER     55
    e_capitolcase,
#define         F_CASECAPITAL   56
    v_zero,
#define		V_ZERO		57
    v_add,
#define		V_ADD		58
    v_addend,
#define		V_ADDEND	59
    v_wordbegnext,
#define		V_WORDBEGNEXT	60
    e_killall,
#define		F_KILLALL	61
    e_unassigned,
/* F_EXTENDNEXT removed */
    v_insbeg,
#define		V_INSBEG	63
    v_replmode,
#define		V_REPLMODE	64
    v_replone,
#define		V_REPLONE	65
    v_substline,
#define		V_SUBSTLINE	66
    v_substchar,
#define		V_SUBSTCHAR	67
    v_chgtoend,
#define		V_CHGTOEND	68
    e_list_eof,
#define		F_LIST_EOF	69
    e_list_glob,
#define		F_LIST_GLOB	70
    e_expand_history,
#define		F_EXPAND_HISTORY	71
    e_magic_space,
#define		F_MAGIC_SPACE	72
    e_insovr,
#define		F_INSOVR	73
    v_cm_complete,
#define		V_CM_COMPLETE	74
    e_copyprev,
#define		F_COPYPREV	75
    e_correctl,
#define		F_CORRECT_L	76
    e_expand_glob,
#define		F_EXPAND_GLOB	77
    e_expand_vars,
#define		F_EXPAND_VARS	78
    e_toggle_hist,
#define		F_TOGGLE_HIST	79
    v_change_case,
#define		V_CHGCASE	80
    e_expand,
#define		F_EXPAND	81
    e_load_average,
#define		F_LOAD_AVERAGE	82
    v_delprev,
#define		V_DELPREV	83
    v_delmeta,
#define		V_DELMETA	84
    v_wordfwd,
#define		V_WORDFWD	85
    v_wordback,
#define		V_WORDBACK	86
    v_endword,
#define		V_ENDWORD	87
    v_eword,
#define		V_EWORD		88
    v_undo,
#define		V_UNDO		89
    v_ush_meta,
#define		V_USH_META	90
    v_dsh_meta,
#define		V_DSH_META	91
    v_rsrch_fwd,
#define		V_RSRCH_FWD	92
    v_rsrch_back,
#define		V_RSRCH_BACK	93
    v_char_fwd,
#define		V_CHAR_FWD	94
    v_char_back,
#define		V_CHAR_BACK	95
    v_chgmeta,
#define		V_CHGMETA	96
    e_inc_fwd,
#define		F_INC_FWD	97
    e_inc_back,
#define		F_INC_BACK	98
    v_rchar_fwd,
#define		V_RCHAR_FWD	99
    v_rchar_back,
#define		V_RCHAR_BACK	100
    v_charto_fwd,
#define		V_CHARTO_FWD	101
    v_charto_back,
#define		V_CHARTO_BACK	102
    e_normalize_path,
#define		F_PATH_NORM	103
    0				/* DUMMY VALUE */
#define		F_NUM_FNS	104
};

KEYCMD  NumFuns = F_NUM_FNS;

KEYCMD  CcKeyMap[256];		/* the real key map */
KEYCMD  CcAltMap[256];		/* the alternative key map */

KEYCMD  CcEmacsMap[] = {
/* keymap table, each index into above tbl; should be 256*sizeof(KEYCMD)
   bytes long */

    F_SET_MARK,			/* ^@ */
    F_TOBEG,			/* ^A */
    F_CHARBACK,			/* ^B */
    F_TTY_INT,			/* ^C */
    F_LIST_DELNEXT,		/* ^D */
    F_TOEND,			/* ^E */
    F_CHARFWD,			/* ^F */
    F_UNASSIGNED,		/* ^G */
    F_DELPREV,			/* ^H */
    F_COMPLETE,			/* ^I */
    F_NEWLINE,			/* ^J */
    F_KILLEND,			/* ^K */
    F_CLEARDISP,		/* ^L */
    F_NEWLINE,			/* ^M */
    F_DOWN_HIST,		/* ^N */
    F_TTY_FLUSHO,		/* ^O */
    F_UP_HIST,			/* ^P */
    F_TTY_STARTO,		/* ^Q */
    F_REDISP,			/* ^R */
    F_TTY_STOPO,		/* ^S */
    F_CHARSWITCH,		/* ^T */
    F_KILLALL,			/* ^U */
    F_QUOTE,			/* ^V */
    F_KILLREGION,		/* ^W */
    F_XKEY,			/* ^X */
    F_YANK_KILL,		/* ^Y */
    F_TTY_TSUSP,		/* ^Z */
    F_METANEXT,			/* ^[ */
    F_TTY_QUIT,			/* ^\ */
    F_TTY_DSUSP,		/* ^] */
    F_UNASSIGNED,		/* ^^ */
    F_UNASSIGNED,		/* ^_ */
    F_INSERT,			/* SPACE */
    F_INSERT,			/* ! */
    F_INSERT,			/* " */
    F_INSERT,			/* # */
    F_INSERT,			/* $ */
    F_INSERT,			/* % */
    F_INSERT,			/* & */
    F_INSERT,			/* ' */
    F_INSERT,			/* ( */
    F_INSERT,			/* ) */
    F_INSERT,			/* * */
    F_INSERT,			/* + */
    F_INSERT,			/* , */
    F_INSERT,			/* - */
    F_INSERT,			/* . */
    F_INSERT,			/* / */
    F_DIGIT,			/* 0 */
    F_DIGIT,			/* 1 */
    F_DIGIT,			/* 2 */
    F_DIGIT,			/* 3 */
    F_DIGIT,			/* 4 */
    F_DIGIT,			/* 5 */
    F_DIGIT,			/* 6 */
    F_DIGIT,			/* 7 */
    F_DIGIT,			/* 8 */
    F_DIGIT,			/* 9 */
    F_INSERT,			/* : */
    F_INSERT,			/* ; */
    F_INSERT,			/* < */
    F_INSERT,			/* = */
    F_INSERT,			/* > */
    F_INSERT,			/* ? */
    F_INSERT,			/* @ */
    F_INSERT,			/* A */
    F_INSERT,			/* B */
    F_INSERT,			/* C */
    F_INSERT,			/* D */
    F_INSERT,			/* E */
    F_INSERT,			/* F */
    F_INSERT,			/* G */
    F_INSERT,			/* H */
    F_INSERT,			/* I */
    F_INSERT,			/* J */
    F_INSERT,			/* K */
    F_INSERT,			/* L */
    F_INSERT,			/* M */
    F_INSERT,			/* N */
    F_INSERT,			/* O */
    F_INSERT,			/* P */
    F_INSERT,			/* Q */
    F_INSERT,			/* R */
    F_INSERT,			/* S */
    F_INSERT,			/* T */
    F_INSERT,			/* U */
    F_INSERT,			/* V */
    F_INSERT,			/* W */
    F_INSERT,			/* X */
    F_INSERT,			/* Y */
    F_INSERT,			/* Z */
    F_INSERT,			/* [ */
    F_INSERT,			/* \ */
    F_INSERT,			/* ] */
    F_INSERT,			/* ^ */
    F_INSERT,			/* _ */
    F_INSERT,			/* ` */
    F_INSERT,			/* a */
    F_INSERT,			/* b */
    F_INSERT,			/* c */
    F_INSERT,			/* d */
    F_INSERT,			/* e */
    F_INSERT,			/* f */
    F_INSERT,			/* g */
    F_INSERT,			/* h */
    F_INSERT,			/* i */
    F_INSERT,			/* j */
    F_INSERT,			/* k */
    F_INSERT,			/* l */
    F_INSERT,			/* m */
    F_INSERT,			/* n */
    F_INSERT,			/* o */
    F_INSERT,			/* p */
    F_INSERT,			/* q */
    F_INSERT,			/* r */
    F_INSERT,			/* s */
    F_INSERT,			/* t */
    F_INSERT,			/* u */
    F_INSERT,			/* v */
    F_INSERT,			/* w */
    F_INSERT,			/* x */
    F_INSERT,			/* y */
    F_INSERT,			/* z */
    F_INSERT,			/* { */
    F_INSERT,			/* | */
    F_INSERT,			/* } */
    F_INSERT,			/* ~ */
    F_DELPREV,			/* ^? */
    F_UNASSIGNED,		/* M-^@ */
    F_UNASSIGNED,		/* M-^A */
    F_UNASSIGNED,		/* M-^B */
    F_UNASSIGNED,		/* M-^C */
    F_LIST_CHOICES,		/* M-^D */
    F_UNASSIGNED,		/* M-^E */
    F_UNASSIGNED,		/* M-^F */
    F_UNASSIGNED,		/* M-^G */
    F_DELWORDPREV,		/* M-^H */
    F_COMPLETE,			/* M-^I */
    F_UNASSIGNED,		/* M-^J */
    F_UNASSIGNED,		/* M-^K */
    F_CLEARDISP,		/* M-^L */
    F_UNASSIGNED,		/* M-^M */
    F_UNASSIGNED,		/* M-^N */
    F_UNASSIGNED,		/* M-^O */
    F_UNASSIGNED,		/* M-^P */
    F_UNASSIGNED,		/* M-^Q */
    F_UNASSIGNED,		/* M-^R */
    F_UNASSIGNED,		/* M-^S */
    F_UNASSIGNED,		/* M-^T */
    F_UNASSIGNED,		/* M-^U */
    F_UNASSIGNED,		/* M-^V */
    F_UNASSIGNED,		/* M-^W */
    F_UNASSIGNED,		/* M-^X */
    F_UNASSIGNED,		/* M-^Y */
    F_RUN_FG_EDITOR,		/* M-^Z */
    F_COMPLETE,			/* M-^[ */
    F_UNASSIGNED,		/* M-^\ */
    F_UNASSIGNED,		/* M-^] */
    F_UNASSIGNED,		/* M-^^ */
    F_COPYPREV,			/* M-^_ */
    F_EXPAND_HISTORY,		/* M-SPACE */
    F_EXPAND_HISTORY,		/* M-! */
    F_UNASSIGNED,		/* M-" */
    F_UNASSIGNED,		/* M-# */
    F_CORRECT_L,		/* M-$ */
    F_UNASSIGNED,		/* M-% */
    F_UNASSIGNED,		/* M-& */
    F_UNASSIGNED,		/* M-' */
    F_UNASSIGNED,		/* M-( */
    F_UNASSIGNED,		/* M-) */
    F_UNASSIGNED,		/* M-* */
    F_UNASSIGNED,		/* M-+ */
    F_UNASSIGNED,		/* M-, */
    F_UNASSIGNED,		/* M-- */
    F_UNASSIGNED,		/* M-. */
    F_UNASSIGNED,		/* M-/ */
    F_ARGDIGIT,			/* M-0 */
    F_ARGDIGIT,			/* M-1 */
    F_ARGDIGIT,			/* M-2 */
    F_ARGDIGIT,			/* M-3 */
    F_ARGDIGIT,			/* M-4 */
    F_ARGDIGIT,			/* M-5 */
    F_ARGDIGIT,			/* M-6 */
    F_ARGDIGIT,			/* M-7 */
    F_ARGDIGIT,			/* M-8 */
    F_ARGDIGIT,			/* M-9 */
    F_UNASSIGNED,		/* M-: */
    F_UNASSIGNED,		/* M-; */
    F_UNASSIGNED,		/* M-< */
    F_UNASSIGNED,		/* M-= */
    F_UNASSIGNED,		/* M-> */
    F_WHICH,			/* M-? */
    F_UNASSIGNED,		/* M-@ */
    F_UNASSIGNED,		/* M-A */
    F_WORDBACK,			/* M-B */
    F_CASECAPITAL,		/* M-C */
    F_DELWORDNEXT,		/* M-D */
    F_UNASSIGNED,		/* M-E */
    F_WORDFWD,			/* M-F */
    F_UNASSIGNED,		/* M-G */
    F_HELPME,			/* M-H */
    F_UNASSIGNED,		/* M-I */
    F_UNASSIGNED,		/* M-J */
    F_UNASSIGNED,		/* M-K */
    F_CASELOWER,		/* M-L */
    F_UNASSIGNED,		/* M-M */
    F_DOWN_SEARCH_HIST,		/* M-N */
    F_XKEY,			/* M-O *//* extended key esc PWP Mar 88 */
    F_UP_SEARCH_HIST,		/* M-P */
    F_UNASSIGNED,		/* M-Q */
    F_TOGGLE_HIST,		/* M-R */
    F_CORRECT,			/* M-S */
    F_UNASSIGNED,		/* M-T */
    F_CASEUPPER,		/* M-U */
    F_UNASSIGNED,		/* M-V */
    F_COPYREGION,		/* M-W */
    F_UNASSIGNED,		/* M-X */
    F_UNASSIGNED,		/* M-Y */
    F_UNASSIGNED,		/* M-Z */
    F_XKEY,			/* M-[ *//* extended key esc -mf Oct 87 */
    F_UNASSIGNED,		/* M-\ */
    F_UNASSIGNED,		/* M-] */
    F_UNASSIGNED,		/* M-^ */
    F_LAST_ITEM,		/* M-_ */
    F_UNASSIGNED,		/* M-` */
    F_UNASSIGNED,		/* M-a */
    F_WORDBACK,			/* M-b */
    F_CASECAPITAL,		/* M-c */
    F_DELWORDNEXT,		/* M-d */
    F_UNASSIGNED,		/* M-e */
    F_WORDFWD,			/* M-f */
    F_UNASSIGNED,		/* M-g */
    F_HELPME,			/* M-h */
    F_UNASSIGNED,		/* M-i */
    F_UNASSIGNED,		/* M-j */
    F_UNASSIGNED,		/* M-k */
    F_CASELOWER,		/* M-l */
    F_UNASSIGNED,		/* M-m */
    F_DOWN_SEARCH_HIST,		/* M-n */
    F_UNASSIGNED,		/* M-o */
    F_UP_SEARCH_HIST,		/* M-p */
    F_UNASSIGNED,		/* M-q */
    F_TOGGLE_HIST,		/* M-r */
    F_CORRECT,			/* M-s */
    F_UNASSIGNED,		/* M-t */
    F_CASEUPPER,		/* M-u */
    F_UNASSIGNED,		/* M-v */
    F_COPYREGION,		/* M-w */
    F_UNASSIGNED,		/* M-x */
    F_UNASSIGNED,		/* M-y */
    F_UNASSIGNED,		/* M-z */
    F_UNASSIGNED,		/* M-{ */
    F_UNASSIGNED,		/* M-| */
    F_UNASSIGNED,		/* M-} */
    F_UNASSIGNED,		/* M-~ */
    F_DELWORDPREV		/* M-^? */
};

/*
 * keymap table for vi.  Each index into above tbl; should be
 * 256 entries long.  Vi mode uses a sticky-extend to do command mode:
 * insert mode characters are in the normal keymap, and command mode
 * in the extended keymap.
 */
KEYCMD  CcViMap[] = {
#ifdef KSHVI
    F_UNASSIGNED,		/* ^@ */
    F_INSERT,			/* ^A */
    F_INSERT,			/* ^B */
    F_INSERT,			/* ^C */
    F_INSERT,			/* ^D */
    F_INSERT,			/* ^E */
    F_INSERT,			/* ^F */
    F_INSERT,			/* ^G */
    V_DELPREV,			/* ^H */   /* BackSpace key */
    F_COMPLETE,			/* ^I */   /* Tab Key  */
    F_NEWLINE,			/* ^J */
    F_INSERT,			/* ^K */
    F_INSERT,			/* ^L */
    F_NEWLINE,			/* ^M */
    F_INSERT,			/* ^N */
    F_INSERT,			/* ^O */
    F_INSERT,			/* ^P */
    F_TTY_STARTO,		/* ^Q */
    F_INSERT,			/* ^R */
    F_INSERT,			/* ^S */
    F_INSERT,			/* ^T */
    F_INSERT,			/* ^U */
    F_QUOTE,			/* ^V */
    F_DELWORDPREV,		/* ^W */  /* Only until start edit pos */
    F_INSERT,			/* ^X */
    F_INSERT,			/* ^Y */
    F_INSERT,			/* ^Z */
    V_CMD_MODE,			/* ^[ */  /* [ Esc ] key */
    F_TTY_QUIT,			/* ^\ */
    F_INSERT,			/* ^] */
    F_INSERT,			/* ^^ */
    F_INSERT,			/* ^_ */
#else /* !KSHVI */
    F_UNASSIGNED,		/* ^@ */   /* NOTE: These mapping do NOT */
    F_TOBEG,			/* ^A */   /* Correspond well to the KSH */
    F_CHARBACK,			/* ^B */   /* VI editting assignments    */
    F_TTY_INT,			/* ^C */   /* On the other hand they are */
    F_LIST_EOF,			/* ^D */   /* convenient any many people */
    F_TOEND,			/* ^E */   /* have gotten used to them   */
    F_CHARFWD,			/* ^F */
    F_LIST_GLOB,		/* ^G */
    F_DELPREV,			/* ^H */   /* BackSpace key */
    F_COMPLETE,			/* ^I */   /* Tab Key */
    F_NEWLINE,			/* ^J */
    F_KILLEND,			/* ^K */
    F_CLEARDISP,		/* ^L */
    F_NEWLINE,			/* ^M */
    F_DOWN_HIST,		/* ^N */
    F_TTY_FLUSHO,		/* ^O */
    F_UP_HIST,			/* ^P */
    F_TTY_STARTO,		/* ^Q */
    F_REDISP,			/* ^R */
    F_TTY_STOPO,		/* ^S */
    F_CHARSWITCH,		/* ^T */
    F_KILLBEG,			/* ^U */
    F_QUOTE,			/* ^V */
    F_DELWORDPREV,		/* ^W */
    F_EXPAND,			/* ^X */
    F_TTY_DSUSP,		/* ^Y */
    F_TTY_TSUSP,		/* ^Z */
    V_CMD_MODE,			/* ^[ */
    F_TTY_QUIT,			/* ^\ */
    F_UNASSIGNED,		/* ^] */
    F_UNASSIGNED,		/* ^^ */
    F_UNASSIGNED,		/* ^_ */
#endif  /* KSHVI */
    F_INSERT,			/* SPACE */
    F_INSERT,			/* ! */
    F_INSERT,			/* " */
    F_INSERT,			/* # */
    F_INSERT,			/* $ */
    F_INSERT,			/* % */
    F_INSERT,			/* & */
    F_INSERT,			/* ' */
    F_INSERT,			/* ( */
    F_INSERT,			/* ) */
    F_INSERT,			/* * */
    F_INSERT,			/* + */
    F_INSERT,			/* , */
    F_INSERT,			/* - */
    F_INSERT,			/* . */
    F_INSERT,			/* / */
    F_INSERT,			/* 0 */
    F_INSERT,			/* 1 */
    F_INSERT,			/* 2 */
    F_INSERT,			/* 3 */
    F_INSERT,			/* 4 */
    F_INSERT,			/* 5 */
    F_INSERT,			/* 6 */
    F_INSERT,			/* 7 */
    F_INSERT,			/* 8 */
    F_INSERT,			/* 9 */
    F_INSERT,			/* : */
    F_INSERT,			/* ; */
    F_INSERT,			/* < */
    F_INSERT,			/* = */
    F_INSERT,			/* > */
    F_INSERT,			/* ? */
    F_INSERT,			/* @ */
    F_INSERT,			/* A */
    F_INSERT,			/* B */
    F_INSERT,			/* C */
    F_INSERT,			/* D */
    F_INSERT,			/* E */
    F_INSERT,			/* F */
    F_INSERT,			/* G */
    F_INSERT,			/* H */
    F_INSERT,			/* I */
    F_INSERT,			/* J */
    F_INSERT,			/* K */
    F_INSERT,			/* L */
    F_INSERT,			/* M */
    F_INSERT,			/* N */
    F_INSERT,			/* O */
    F_INSERT,			/* P */
    F_INSERT,			/* Q */
    F_INSERT,			/* R */
    F_INSERT,			/* S */
    F_INSERT,			/* T */
    F_INSERT,			/* U */
    F_INSERT,			/* V */
    F_INSERT,			/* W */
    F_INSERT,			/* X */
    F_INSERT,			/* Y */
    F_INSERT,			/* Z */
    F_INSERT,			/* [ */
    F_INSERT,			/* \ */
    F_INSERT,			/* ] */
    F_INSERT,			/* ^ */
    F_INSERT,			/* _ */
    F_INSERT,			/* ` */
    F_INSERT,			/* a */
    F_INSERT,			/* b */
    F_INSERT,			/* c */
    F_INSERT,			/* d */
    F_INSERT,			/* e */
    F_INSERT,			/* f */
    F_INSERT,			/* g */
    F_INSERT,			/* h */
    F_INSERT,			/* i */
    F_INSERT,			/* j */
    F_INSERT,			/* k */
    F_INSERT,			/* l */
    F_INSERT,			/* m */
    F_INSERT,			/* n */
    F_INSERT,			/* o */
    F_INSERT,			/* p */
    F_INSERT,			/* q */
    F_INSERT,			/* r */
    F_INSERT,			/* s */
    F_INSERT,			/* t */
    F_INSERT,			/* u */
    F_INSERT,			/* v */
    F_INSERT,			/* w */
    F_INSERT,			/* x */
    F_INSERT,			/* y */
    F_INSERT,			/* z */
    F_INSERT,			/* { */
    F_INSERT,			/* | */
    F_INSERT,			/* } */
    F_INSERT,			/* ~ */
    F_DELPREV,			/* ^? */
    F_UNASSIGNED,		/* M-^@ */
    F_UNASSIGNED,		/* M-^A */
    F_UNASSIGNED,		/* M-^B */
    F_UNASSIGNED,		/* M-^C */
    F_UNASSIGNED,		/* M-^D */
    F_UNASSIGNED,		/* M-^E */
    F_UNASSIGNED,		/* M-^F */
    F_UNASSIGNED,		/* M-^G */
    F_UNASSIGNED,		/* M-^H */
    F_UNASSIGNED,		/* M-^I */
    F_UNASSIGNED,		/* M-^J */
    F_UNASSIGNED,		/* M-^K */
    F_UNASSIGNED,		/* M-^L */
    F_UNASSIGNED,		/* M-^M */
    F_UNASSIGNED,		/* M-^N */
    F_UNASSIGNED,		/* M-^O */
    F_UNASSIGNED,		/* M-^P */
    F_UNASSIGNED,		/* M-^Q */
    F_UNASSIGNED,		/* M-^R */
    F_UNASSIGNED,		/* M-^S */
    F_UNASSIGNED,		/* M-^T */
    F_UNASSIGNED,		/* M-^U */
    F_UNASSIGNED,		/* M-^V */
    F_UNASSIGNED,		/* M-^W */
    F_UNASSIGNED,		/* M-^X */
    F_UNASSIGNED,		/* M-^Y */
    F_UNASSIGNED,		/* M-^Z */
    F_UNASSIGNED,		/* M-^[ */
    F_UNASSIGNED,		/* M-^\ */
    F_UNASSIGNED,		/* M-^] */
    F_UNASSIGNED,		/* M-^^ */
    F_UNASSIGNED,		/* M-^_ */
    F_UNASSIGNED,		/* M-SPACE */
    F_UNASSIGNED,		/* M-! */
    F_UNASSIGNED,		/* M-" */
    F_UNASSIGNED,		/* M-# */
    F_UNASSIGNED,		/* M-$ */
    F_UNASSIGNED,		/* M-% */
    F_UNASSIGNED,		/* M-& */
    F_UNASSIGNED,		/* M-' */
    F_UNASSIGNED,		/* M-( */
    F_UNASSIGNED,		/* M-) */
    F_UNASSIGNED,		/* M-* */
    F_UNASSIGNED,		/* M-+ */
    F_UNASSIGNED,		/* M-, */
    F_UNASSIGNED,		/* M-- */
    F_UNASSIGNED,		/* M-. */
    F_UNASSIGNED,		/* M-/ */
    F_UNASSIGNED,		/* M-0 */
    F_UNASSIGNED,		/* M-1 */
    F_UNASSIGNED,		/* M-2 */
    F_UNASSIGNED,		/* M-3 */
    F_UNASSIGNED,		/* M-4 */
    F_UNASSIGNED,		/* M-5 */
    F_UNASSIGNED,		/* M-6 */
    F_UNASSIGNED,		/* M-7 */
    F_UNASSIGNED,		/* M-8 */
    F_UNASSIGNED,		/* M-9 */
    F_UNASSIGNED,		/* M-: */
    F_UNASSIGNED,		/* M-; */
    F_UNASSIGNED,		/* M-< */
    F_UNASSIGNED,		/* M-= */
    F_UNASSIGNED,		/* M-> */
    F_UNASSIGNED,		/* M-? */
    F_UNASSIGNED,		/* M-@ */
    F_UNASSIGNED,		/* M-A */
    F_UNASSIGNED,		/* M-B */
    F_UNASSIGNED,		/* M-C */
    F_UNASSIGNED,		/* M-D */
    F_UNASSIGNED,		/* M-E */
    F_UNASSIGNED,		/* M-F */
    F_UNASSIGNED,		/* M-G */
    F_UNASSIGNED,		/* M-H */
    F_UNASSIGNED,		/* M-I */
    F_UNASSIGNED,		/* M-J */
    F_UNASSIGNED,		/* M-K */
    F_UNASSIGNED,		/* M-L */
    F_UNASSIGNED,		/* M-M */
    F_UNASSIGNED,		/* M-N */
    F_UNASSIGNED,		/* M-O */
    F_UNASSIGNED,		/* M-P */
    F_UNASSIGNED,		/* M-Q */
    F_UNASSIGNED,		/* M-R */
    F_UNASSIGNED,		/* M-S */
    F_UNASSIGNED,		/* M-T */
    F_UNASSIGNED,		/* M-U */
    F_UNASSIGNED,		/* M-V */
    F_UNASSIGNED,		/* M-W */
    F_UNASSIGNED,		/* M-X */
    F_UNASSIGNED,		/* M-Y */
    F_UNASSIGNED,		/* M-Z */
    F_UNASSIGNED,		/* M-[ */
    F_UNASSIGNED,		/* M-\ */
    F_UNASSIGNED,		/* M-] */
    F_UNASSIGNED,		/* M-^ */
    F_UNASSIGNED,		/* M-_ */
    F_UNASSIGNED,		/* M-` */
    F_UNASSIGNED,		/* M-a */
    F_UNASSIGNED,		/* M-b */
    F_UNASSIGNED,		/* M-c */
    F_UNASSIGNED,		/* M-d */
    F_UNASSIGNED,		/* M-e */
    F_UNASSIGNED,		/* M-f */
    F_UNASSIGNED,		/* M-g */
    F_UNASSIGNED,		/* M-h */
    F_UNASSIGNED,		/* M-i */
    F_UNASSIGNED,		/* M-j */
    F_UNASSIGNED,		/* M-k */
    F_UNASSIGNED,		/* M-l */
    F_UNASSIGNED,		/* M-m */
    F_UNASSIGNED,		/* M-n */
    F_UNASSIGNED,		/* M-o */
    F_UNASSIGNED,		/* M-p */
    F_UNASSIGNED,		/* M-q */
    F_UNASSIGNED,		/* M-r */
    F_UNASSIGNED,		/* M-s */
    F_UNASSIGNED,		/* M-t */
    F_UNASSIGNED,		/* M-u */
    F_UNASSIGNED,		/* M-v */
    F_UNASSIGNED,		/* M-w */
    F_UNASSIGNED,		/* M-x */
    F_UNASSIGNED,		/* M-y */
    F_UNASSIGNED,		/* M-z */
    F_UNASSIGNED,		/* M-{ */
    F_UNASSIGNED,		/* M-| */
    F_UNASSIGNED,		/* M-} */
    F_UNASSIGNED,		/* M-~ */
    F_UNASSIGNED		/* M-^? */
};

KEYCMD  CcViCmdMap[] = {
    F_UNASSIGNED,		/* ^@ */
    F_TOBEG,			/* ^A */
    F_UNASSIGNED,		/* ^B */
    F_TTY_INT,			/* ^C */
    F_LIST_CHOICES,		/* ^D */
    F_TOEND,			/* ^E */
    F_UNASSIGNED,		/* ^F */
    F_LIST_GLOB,		/* ^G */
    F_CHARBACK,			/* ^H */
    V_CM_COMPLETE,		/* ^I */
    F_NEWLINE,			/* ^J */
    F_KILLEND,			/* ^K */
    F_CLEARDISP,		/* ^L */
    F_NEWLINE,			/* ^M */
    F_DOWN_HIST,		/* ^N */
    F_TTY_FLUSHO,		/* ^O */
    F_UP_HIST,			/* ^P */
    F_TTY_STARTO,		/* ^Q */
    F_REDISP,			/* ^R */
    F_TTY_STOPO,		/* ^S */
    F_UNASSIGNED,		/* ^T */
    F_KILLBEG,			/* ^U */
    F_UNASSIGNED,		/* ^V */
    F_DELWORDPREV,		/* ^W */
    F_EXPAND,			/* ^X */
    F_UNASSIGNED,		/* ^Y */
    F_UNASSIGNED,		/* ^Z */
    F_METANEXT,			/* ^[ */
    F_TTY_QUIT,			/* ^\ */
    F_UNASSIGNED,		/* ^] */
    F_UNASSIGNED,		/* ^^ */
    F_UNASSIGNED,		/* ^_ */
    F_CHARFWD,			/* SPACE */
    F_EXPAND_HISTORY,		/* ! */
    F_UNASSIGNED,		/* " */
    F_UNASSIGNED,		/* # */
    F_TOEND,			/* $ */
    F_UNASSIGNED,		/* % */
    F_UNASSIGNED,		/* & */
    F_UNASSIGNED,		/* ' */
    F_UNASSIGNED,		/* ( */
    F_UNASSIGNED,		/* ) */
    F_EXPAND_GLOB,		/* * */
    F_DOWN_HIST,		/* + */
    V_RCHAR_BACK,		/* , */	
    F_UP_HIST,			/* - */	
    F_UNASSIGNED,		/* . */
    V_DSH_META,			/* / */
    V_ZERO,			/* 0 */
    F_ARGDIGIT,			/* 1 */
    F_ARGDIGIT,			/* 2 */
    F_ARGDIGIT,			/* 3 */
    F_ARGDIGIT,			/* 4 */
    F_ARGDIGIT,			/* 5 */
    F_ARGDIGIT,			/* 6 */
    F_ARGDIGIT,			/* 7 */
    F_ARGDIGIT,			/* 8 */
    F_ARGDIGIT,			/* 9 */
    F_UNASSIGNED,		/* : */
    V_RCHAR_FWD,		/* ; */
    F_UNASSIGNED,		/* < */
    F_UNASSIGNED,		/* = */
    F_UNASSIGNED,		/* > */
    V_USH_META,			/* ? */
    F_UNASSIGNED,		/* @ */
    V_ADDEND,			/* A */
    V_WORDBACK,			/* B */
    V_CHGTOEND,			/* C */
    F_KILLEND,			/* D */
    V_ENDWORD,			/* E */
    V_CHAR_BACK,		/* F */
    F_UNASSIGNED,		/* G */
    F_UNASSIGNED,		/* H */
    V_INSBEG,			/* I */
    F_DOWN_SEARCH_HIST,		/* J */
    F_UP_SEARCH_HIST,		/* K */
    F_UNASSIGNED,		/* L */
    F_UNASSIGNED,		/* M */
    V_RSRCH_BACK,		/* N */
    F_XKEY,			/* O */
    F_UNASSIGNED,		/* P */
    F_UNASSIGNED,		/* Q */
    V_REPLMODE,			/* R */
    V_SUBSTLINE,		/* S */
    V_CHARTO_BACK,		/* T */
    F_UNASSIGNED,		/* U */
    F_EXPAND_VARS,		/* V */
    V_WORDFWD,			/* W */
    F_DELPREV,			/* X */
    F_UNASSIGNED,		/* Y */
    F_UNASSIGNED,		/* Z */
    F_XKEY,			/* [ */
    F_UNASSIGNED,		/* \ */
    F_UNASSIGNED,		/* ] */
    F_TOBEG,			/* ^ */
    F_UNASSIGNED,		/* _ */
    F_UNASSIGNED,		/* ` */
    V_ADD,			/* a */
    F_WORDBACK,			/* b */
    V_CHGMETA,			/* c */
    V_DELMETA,			/* d */
    V_EWORD,			/* e */
    V_CHAR_FWD,			/* f */
    F_UNASSIGNED,		/* g */
    F_CHARBACK,			/* h */
    V_INSERT,			/* i */
    F_DOWN_HIST,		/* j */
    F_UP_HIST,			/* k */
    F_CHARFWD,			/* l */
    F_UNASSIGNED,		/* m */
    V_RSRCH_FWD,		/* n */
    F_UNASSIGNED,		/* o */
    F_UNASSIGNED,		/* p */
    F_UNASSIGNED,		/* q */
    V_REPLONE,			/* r */
    V_SUBSTCHAR,		/* s */
    V_CHARTO_FWD,		/* t */
    V_UNDO,			/* u */
    F_EXPAND_VARS,		/* v */
    V_WORDBEGNEXT,		/* w */
    F_DELNEXT,			/* x */
    F_UNASSIGNED,		/* y */
    F_UNASSIGNED,		/* z */
    F_UNASSIGNED,		/* { */
    F_UNASSIGNED,		/* | */
    F_UNASSIGNED,		/* } */
    V_CHGCASE,			/* ~ */
    F_DELPREV,			/* ^? */
    F_UNASSIGNED,		/* M-^@ */
    F_UNASSIGNED,		/* M-^A */
    F_UNASSIGNED,		/* M-^B */
    F_UNASSIGNED,		/* M-^C */
    F_UNASSIGNED,		/* M-^D */
    F_UNASSIGNED,		/* M-^E */
    F_UNASSIGNED,		/* M-^F */
    F_UNASSIGNED,		/* M-^G */
    F_UNASSIGNED,		/* M-^H */
    F_UNASSIGNED,		/* M-^I */
    F_UNASSIGNED,		/* M-^J */
    F_UNASSIGNED,		/* M-^K */
    F_UNASSIGNED,		/* M-^L */
    F_UNASSIGNED,		/* M-^M */
    F_UNASSIGNED,		/* M-^N */
    F_UNASSIGNED,		/* M-^O */
    F_UNASSIGNED,		/* M-^P */
    F_UNASSIGNED,		/* M-^Q */
    F_UNASSIGNED,		/* M-^R */
    F_UNASSIGNED,		/* M-^S */
    F_UNASSIGNED,		/* M-^T */
    F_UNASSIGNED,		/* M-^U */
    F_UNASSIGNED,		/* M-^V */
    F_UNASSIGNED,		/* M-^W */
    F_UNASSIGNED,		/* M-^X */
    F_UNASSIGNED,		/* M-^Y */
    F_UNASSIGNED,		/* M-^Z */
    F_UNASSIGNED,		/* M-^[ */
    F_UNASSIGNED,		/* M-^\ */
    F_UNASSIGNED,		/* M-^] */
    F_UNASSIGNED,		/* M-^^ */
    F_UNASSIGNED,		/* M-^_ */
    F_UNASSIGNED,		/* M-SPACE */
    F_UNASSIGNED,		/* M-! */
    F_UNASSIGNED,		/* M-" */
    F_UNASSIGNED,		/* M-# */
    F_UNASSIGNED,		/* M-$ */
    F_UNASSIGNED,		/* M-% */
    F_UNASSIGNED,		/* M-& */
    F_UNASSIGNED,		/* M-' */
    F_UNASSIGNED,		/* M-( */
    F_UNASSIGNED,		/* M-) */
    F_UNASSIGNED,		/* M-* */
    F_UNASSIGNED,		/* M-+ */
    F_UNASSIGNED,		/* M-, */
    F_UNASSIGNED,		/* M-- */
    F_UNASSIGNED,		/* M-. */
    F_UNASSIGNED,		/* M-/ */
    F_UNASSIGNED,		/* M-0 */
    F_UNASSIGNED,		/* M-1 */
    F_UNASSIGNED,		/* M-2 */
    F_UNASSIGNED,		/* M-3 */
    F_UNASSIGNED,		/* M-4 */
    F_UNASSIGNED,		/* M-5 */
    F_UNASSIGNED,		/* M-6 */
    F_UNASSIGNED,		/* M-7 */
    F_UNASSIGNED,		/* M-8 */
    F_UNASSIGNED,		/* M-9 */
    F_UNASSIGNED,		/* M-: */
    F_UNASSIGNED,		/* M-; */
    F_UNASSIGNED,		/* M-< */
    F_UNASSIGNED,		/* M-= */
    F_UNASSIGNED,		/* M-> */
    F_HELPME,			/* M-? */
    F_UNASSIGNED,		/* M-@ */
    F_UNASSIGNED,		/* M-A */
    F_UNASSIGNED,		/* M-B */
    F_UNASSIGNED,		/* M-C */
    F_UNASSIGNED,		/* M-D */
    F_UNASSIGNED,		/* M-E */
    F_UNASSIGNED,		/* M-F */
    F_UNASSIGNED,		/* M-G */
    F_UNASSIGNED,		/* M-H */
    F_UNASSIGNED,		/* M-I */
    F_UNASSIGNED,		/* M-J */
    F_UNASSIGNED,		/* M-K */
    F_UNASSIGNED,		/* M-L */
    F_UNASSIGNED,		/* M-M */
    F_UNASSIGNED,		/* M-N */
    F_XKEY,			/* M-O *//* extended key esc PWP Mar 88 */
    F_UNASSIGNED,		/* M-P */
    F_UNASSIGNED,		/* M-Q */
    F_UNASSIGNED,		/* M-R */
    F_UNASSIGNED,		/* M-S */
    F_UNASSIGNED,		/* M-T */
    F_UNASSIGNED,		/* M-U */
    F_UNASSIGNED,		/* M-V */
    F_UNASSIGNED,		/* M-W */
    F_UNASSIGNED,		/* M-X */
    F_UNASSIGNED,		/* M-Y */
    F_UNASSIGNED,		/* M-Z */
    F_XKEY,			/* M-[ *//* extended key esc -mf Oct 87 */
    F_UNASSIGNED,		/* M-\ */
    F_UNASSIGNED,		/* M-] */
    F_UNASSIGNED,		/* M-^ */
    F_UNASSIGNED,		/* M-_ */
    F_UNASSIGNED,		/* M-` */
    F_UNASSIGNED,		/* M-a */
    F_UNASSIGNED,		/* M-b */
    F_UNASSIGNED,		/* M-c */
    F_UNASSIGNED,		/* M-d */
    F_UNASSIGNED,		/* M-e */
    F_UNASSIGNED,		/* M-f */
    F_UNASSIGNED,		/* M-g */
    F_UNASSIGNED,		/* M-h */
    F_UNASSIGNED,		/* M-i */
    F_UNASSIGNED,		/* M-j */
    F_UNASSIGNED,		/* M-k */
    F_UNASSIGNED,		/* M-l */
    F_UNASSIGNED,		/* M-m */
    F_UNASSIGNED,		/* M-n */
    F_UNASSIGNED,		/* M-o */
    F_UNASSIGNED,		/* M-p */
    F_UNASSIGNED,		/* M-q */
    F_UNASSIGNED,		/* M-r */
    F_UNASSIGNED,		/* M-s */
    F_UNASSIGNED,		/* M-t */
    F_UNASSIGNED,		/* M-u */
    F_UNASSIGNED,		/* M-v */
    F_UNASSIGNED,		/* M-w */
    F_UNASSIGNED,		/* M-x */
    F_UNASSIGNED,		/* M-y */
    F_UNASSIGNED,		/* M-z */
    F_UNASSIGNED,		/* M-{ */
    F_UNASSIGNED,		/* M-| */
    F_UNASSIGNED,		/* M-} */
    F_UNASSIGNED,		/* M-~ */
    F_UNASSIGNED		/* M-^? */
};


struct KeyFuncs FuncNames[] = {
    "backward-char", F_CHARBACK,
    "Move back a character",
    "backward-delete-char", F_DELPREV,
    "Delete the character behind cursor",
    "backward-delete-word", F_DELWORDPREV,
    "Cut from beginning of current word to cursor - saved in cut buffer",
    "backward-kill-line", F_KILLBEG,
    "Cut from beginning of line to cursor - save in cut buffer",
    "backward-word", F_WORDBACK,
    "Move to beginning of current word",
    "beginning-of-line", F_TOBEG,
    "Move to beginning of line",
    "capitalize-word", F_CASECAPITAL,
    "Capitalize the characters from cursor to end of current word",
    "change-case", V_CHGCASE,
    "Vi change case of character under cursor and advance one character",
    "change-till-end-of-line", V_CHGTOEND,	/* backwards compat. */
    "Vi change to end of line",
    "clear-screen", F_CLEARDISP,
    "Clear screen leaving current line on top",
    "complete-word", F_COMPLETE,
    "Complete current word",
    "copy-prev-word", F_COPYPREV,
    "Copy current word to cursor",
    "copy-region-as-kill", F_COPYREGION,
    "Copy area between mark and cursor to cut buffer",
    "delete-char", F_DELNEXT,
    "Delete character under cursor",
    "delete-char-or-list", F_LIST_DELNEXT,
    "Delete character under cursor or list completions if at end of line",
    "delete-word", F_DELWORDNEXT,
    "Cut from cursor to end of current word - save in cut buffer",
    "digit", F_DIGIT,
    "Adds to argument if started or enters digit",
    "digit-argument", F_ARGDIGIT,
    "Digit that starts argument",
    "down-history", F_DOWN_HIST,
    "Move to next history line",
    "downcase-word", F_CASELOWER,
    "Lowercase the characters from cursor to end of current word",
    "end-of-file", F_SEND_EOF,
    "Indicate end of file",
    "end-of-line", F_TOEND,
    "Move cursor to end of line",
    "exchange-point-and-mark", F_EXCHANGE_MARK,
    "Exchange the cursor and mark",
    "expand-glob", F_EXPAND_GLOB,
    "Expand file name wildcards",
    "expand-history", F_EXPAND_HISTORY,
    "Expand history escapes",
    "expand-line", F_EXPAND,
    "Expand the history escapes in a line",
    "expand-variables", F_EXPAND_VARS,
    "Expand variables",
    "forward-char", F_CHARFWD,
    "Move forward one character",
    "forward-word", F_WORDFWD,
    "Move forward to end of current word",
    "gosmacs-transpose-chars", F_GCHARSWITCH,
    "Exchange the two characters before the cursor",
    "history-search-backward", F_UP_SEARCH_HIST,
    "Search in history backwards for line beginning as current",
    "history-search-forward", F_DOWN_SEARCH_HIST,
    "Search in history forward for line beginning as current",
    "insert-last-word", F_LAST_ITEM,
    "Insert last item of previous command",
    "i-search-fwd", F_INC_FWD,
    "Incremental search forward",
    "i-search-back", F_INC_BACK,
    "Incremental search backwards",
    "keyboard-quit", F_STARTOVER,
    "Clear line",
    "kill-line", F_KILLEND,
    "Cut to end of line and save in cut buffer",
    "kill-region", F_KILLREGION,
    "Cut area between mark and cursor and save in cut buffer",
    "kill-whole-line", F_KILLALL,
    "Cut the entire line and save in cut buffer",
    "list-choices", F_LIST_CHOICES,
    "List choices for completion",
    "list-glob", F_LIST_GLOB,
    "List file name wildcard matches",
    "list-or-eof", F_LIST_EOF,
    "List choices for completion or indicate end of file if empty line",
    "load-average", F_LOAD_AVERAGE,
    "Display load average and current process status",
    "magic-space", F_MAGIC_SPACE,
    "Expand history escapes and insert a space",
    "newline", F_NEWLINE,
    "Execute command",
    "normalize-path", F_PATH_NORM,
    "Expand pathnames, eliminating leading .'s and ..'s",
    "overwrite-mode", F_INSOVR,
    "Switch from insert to overwrite mode or vice versa",
    "prefix-meta", F_METANEXT,
    "Add 8th bit to next character typed",
    "quoted-insert", F_QUOTE,
    "Add the next character typed to the line verbatim",
    "redisplay", F_REDISP,
    "Redisplay everything",
    "run-fg-editor", F_RUN_FG_EDITOR,
    "Restart stopped editor",
    "run-help", F_HELPME,
    "Look for help on current command",
    "self-insert-command", F_INSERT,
    "This character is added to the line",
    "sequence-lead-in", F_XKEY,
    "This character is the first in a character sequence",
    "set-mark-command", F_SET_MARK,
    "Set the mark at cursor",
    "spell-word", F_CORRECT,
    "Correct the spelling of current word",
    "spell-line", F_CORRECT_L,
    "Correct the spelling of entire line",
    "toggle-literal-history", F_TOGGLE_HIST,
    "Toggle between literal and lexical current history line",
    "transpose-chars", F_CHARSWITCH,
    "Exchange the character to the left of the cursor with the one under",
    "transpose-gosling", F_GCHARSWITCH,
    "Exchange the two characters before the cursor",
    /* EGS: make Convex Users happy */
    "tty-dsusp", F_TTY_DSUSP,
    "Tty delayed suspend character",
    "tty-flush-output", F_TTY_FLUSHO,
    "Tty flush output character",
    "tty-sigintr", F_TTY_INT,
    "Tty interrupt character",
    "tty-sigquit", F_TTY_QUIT,
    "Tty quit character",
    "tty-sigtsusp", F_TTY_TSUSP,
    "Tty suspend character",
    "tty-start-output", F_TTY_STARTO,
    "Tty allow output character",
    "tty-stop-output", F_TTY_STOPO,
    "Tty disallow output character",
    "undefined-key", F_UNASSIGNED,
    "Indicates unbound character",
    "universal-argument", F_ARGFOUR,
    "Emacs universal argument (argument times 4)",
    "up-history", F_UP_HIST,
    "Move to previous history line",
    "upcase-word", F_CASEUPPER,
    "Uppercase the characters from cursor to end of current word",
    "vi-beginning-of-next-word", V_WORDBEGNEXT,
    "Vi goto the beginning of next word",
    "vi-add", V_ADD,
    "Vi enter insert mode after the cursor",
    "vi-add-at-eol", V_ADDEND,
    "Vi enter insert mode at end of line",
    "vi-chg-case", V_CHGCASE,
    "Vi change case of character under cursor and advance one character",
    "vi-chg-meta", V_CHGMETA,
    "Vi change prefix command",
    "vi-chg-to-eol", V_CHGTOEND,
    "Vi change to end of line",
    "vi-cmd-mode", V_CMD_MODE,
    "Enter vi command mode (use alternative key bindings)",
    "vi-cmd-mode-complete", V_CM_COMPLETE,
    "Vi command mode complete current word",
    "vi-delprev", V_DELPREV,
    "Vi move to previous character (backspace)",
    "vi-delmeta", V_DELMETA,
    "Vi delete prefix command",
    "vi-endword", V_ENDWORD,
    "Vi move to the end of the current space delimited word",
    "vi-eword", V_EWORD,
    "Vi move to the end of the current word",
    "vi-char-back", V_CHAR_BACK,
    "Vi move to the character specified backwards",
    "vi-char-fwd", V_CHAR_FWD,
    "Vi move to the character specified forward",
    "vi-charto-back", V_CHARTO_BACK,
    "Vi move up to the character specified backwards",
    "vi-charto-fwd", V_CHARTO_FWD,
    "Vi move up to the character specified forward",
    "vi-insert", V_INSERT,
    "Enter vi insert mode",
    "vi-insert-at-bol", V_INSBEG,
    "Enter vi insert mode at beginning of line",
    "vi-repeat-char-fwd", V_RCHAR_FWD,
    "Vi repeat current character search in the same search direction",
    "vi-repeat-char-back", V_RCHAR_BACK,
    "Vi repeat current character search in the opposite search direction",
    "vi-repeat-search-fwd", V_RSRCH_FWD,
    "Vi repeat current search in the same search direction",
    "vi-repeat-search-back", V_RSRCH_BACK,
    "Vi repeat current search in the opposite search direction",
    "vi-replace-char", V_REPLONE,
    "Vi replace character under the cursor with the next character typed",
    "vi-replace-mode", V_REPLMODE,
    "Vi replace mode",
    "vi-search-back", V_USH_META,
    "Vi search history backwards",
    "vi-search-fwd", V_DSH_META,
    "Vi search history forward",
    "vi-substitute-char", V_SUBSTCHAR,
    "Vi replace character under the cursor and enter insert mode",
    "vi-substitute-line", V_SUBSTLINE,
    "Vi replace entire line",
    "vi-word-back", V_WORDBACK,
    "Vi move to the previous word",
    "vi-word-fwd", V_WORDFWD,
    "Vi move to the next word",
    "vi-undo", V_UNDO,
    "Vi undo last change",
    "vi-zero", V_ZERO,
    "Vi goto the beginning of line",
    "which-command", F_WHICH,
    "Perform which of current command",
    "yank", F_YANK_KILL,
    "Paste cut buffer at cursor position",
    0, 0
};

#ifdef DEBUG_EDIT
void
CheckMaps()
{				/* check the size of the key maps */
    int     c1 = (256 * sizeof(KEYCMD));

    if ((sizeof(CcKeyMap)) != c1)
	xprintf("CcKeyMap should be 256 entries, but is %d.\r\n",
		(sizeof(CcKeyMap) / sizeof(KEYCMD)));

    if ((sizeof(CcAltMap)) != c1)
	xprintf("CcAltMap should be 256 entries, but is %d.\r\n",
		(sizeof(CcAltMap) / sizeof(KEYCMD)));

    if ((sizeof(CcEmacsMap)) != c1)
	xprintf("CcEmacsMap should be 256 entries, but is %d.\r\n",
		(sizeof(CcEmacsMap) / sizeof(KEYCMD)));

    if ((sizeof(CcViMap)) != c1)
	xprintf("CcViMap should be 256 entries, but is %d.\r\n",
		(sizeof(CcViMap) / sizeof(KEYCMD)));

    if ((sizeof(CcViCmdMap)) != c1)
	xprintf("CcViCmdMap should be 256 entries, but is %d.\r\n",
		(sizeof(CcViCmdMap) / sizeof(KEYCMD)));
}

#endif

bool    MapsAreInited = 0;
bool    NLSMapsAreInited = 0;
bool    NoNLSRebind;

void
ed_InitNLSMaps()
{
    register int i;

    if (AsciiOnly)
	return;
    if (NoNLSRebind)
	return;
    for (i = 0200; i <= 0377; i++) {
	if (Isprint(i)) {
	    CcKeyMap[i] = F_INSERT;
	}
    }
    NLSMapsAreInited = 1;
}

static void
ed_InitMetaBindings()
{
    Char    buf[3];
    register int i;
    KEYCMD *map;

    map = CcKeyMap;
    for (i = 0; i <= 0377 && CcKeyMap[i] != F_METANEXT; i++);
    if (i > 0377) {
	for (i = 0; i <= 0377 && CcAltMap[i] != F_METANEXT; i++);
	if (i > 0377) {
	    i = 033;
	    if (VImode)
		map = CcAltMap;
	}
	else {
	    map = CcAltMap;
	}
    }
    buf[0] = i;
    buf[2] = 0;
    for (i = 0200; i <= 0377; i++) {
	if (map[i] != F_INSERT && map[i] != F_UNASSIGNED && map[i] != F_XKEY) {
	    buf[1] = i & ASCII;
	    AddXkey(buf, XmapCmd((int) map[i]), XK_CMD);
	}
    }
    map[buf[0]] = F_XKEY;
}

void
ed_InitVIMaps()
{
    register int i;

    VImode = 1;
    (void) ResetXmap(VImode);
    for (i = 0; i < 256; i++) {
	CcKeyMap[i] = CcViMap[i];
	CcAltMap[i] = CcViCmdMap[i];
    }
    ed_InitMetaBindings();
    (void) ed_InitNLSMaps();
    BindArrowKeys();
}

void
ed_InitEmacsMaps()
{
    register int i;
    Char    buf[3];

    VImode = 0;
    (void) ResetXmap(VImode);
    for (i = 0; i < 256; i++) {
	CcKeyMap[i] = CcEmacsMap[i];
	CcAltMap[i] = F_UNASSIGNED;
    }
    ed_InitMetaBindings();
    (void) ed_InitNLSMaps();
    buf[0] = 030;
    buf[2] = 0;
    buf[1] = 030;
    AddXkey(buf, XmapCmd(F_EXCHANGE_MARK), XK_CMD);
    buf[1] = '*';
    AddXkey(buf, XmapCmd(F_EXPAND_GLOB),   XK_CMD);
    buf[1] = '$';
    AddXkey(buf, XmapCmd(F_EXPAND_VARS),   XK_CMD);
    buf[1] = 'G';
    AddXkey(buf, XmapCmd(F_LIST_GLOB),     XK_CMD);
    buf[1] = 'g';
    AddXkey(buf, XmapCmd(F_LIST_GLOB),     XK_CMD);
    buf[1] = 'n';
    AddXkey(buf, XmapCmd(F_PATH_NORM),     XK_CMD);
    buf[1] = 'N';
    AddXkey(buf, XmapCmd(F_PATH_NORM),     XK_CMD);
    BindArrowKeys();
}

void
ed_InitMaps()
{
    if (MapsAreInited)
	return;
#ifdef VIDEFAULT
    ed_InitVIMaps();
#else
    ed_InitEmacsMaps();
#endif

    MapsAreInited = 1;
}
