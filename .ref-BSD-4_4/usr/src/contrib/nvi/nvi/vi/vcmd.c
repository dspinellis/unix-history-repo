/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
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

#ifndef lint
static char sccsid[] = "@(#)vcmd.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include "vi.h"
#include "vcmd.h"

/*
 * This array maps keystrokes to vi command functions.
 */
VIKEYS const vikeys [MAXVIKEY + 1] = {
/* 000 NUL -- The code in vi.c expects key 0 to be undefined. */
	{NULL},
/* 001  ^A */
	{v_searchw,	V_ABS|V_CNT|V_KEYW|V_RCM_SET,
	    "search forward for cursor word: [count]^A"},
/* 002  ^B */
	{v_pageup,	V_ABS|V_CNT,
	    "page up by screens: [count]^B"},
/* 003  ^C */
	{NULL,		0,
	    "interrupt a search: ^C"},
/* 004  ^D */
	{v_hpagedown,	V_ABS|V_CNT,
	    "page down by half screens (set count): [count]^D"},
/* 005  ^E */
	{v_linedown,	V_CNT,
	    "page down by lines: [count]^E"},
/* 006  ^F */
	{v_pagedown,	V_ABS|V_CNT,
	    "page down by screens: [count]^F"},
/* 007  ^G */
	{v_status,	0,
	    "file status: ^G"},
/* 010  ^H */
	{v_left,	V_CNT|V_MOVE|V_RCM_SET,
	    "move left by columns: [count]^H"},
/* 011  ^I */
	{NULL},
/* 012  ^J */
	{v_down,	V_CNT|V_LMODE|V_MOVE|V_RCM,
	    "move down by lines: [count]^J"},
/* 013  ^K */
	{NULL},
/* 014  ^L */
	{v_redraw,	0,
	    "redraw screen: ^L"},
/* 015  ^M */
	{v_down,	V_CNT|V_LMODE|V_MOVE|V_RCM_SETFNB,
	    "move down by lines (first non-blank): [count]^M"},
/* 016  ^N */
	{v_down,	V_CNT|V_LMODE|V_MOVE|V_RCM,
	    "move down by lines: [count]^N"},
/* 017  ^O */
	{NULL},
/* 020  ^P */
	{v_up,		V_CNT|V_LMODE|V_MOVE|V_RCM,
	    "move up by lines: [count]^P"},
/* 021  ^Q -- not available, used for hardware flow control. */
	{NULL},
/* 022  ^R */
	{v_redraw,	0,
	    "redraw screen: ^R"},
/* 023  ^S -- not available, used for hardware flow control. */
	{NULL},
/* 024  ^T */
	{v_tagpop,	V_RCM_SET,
	    "tag pop: ^T"},
/* 025  ^U */
	{v_hpageup,	V_ABS|V_CNT,
	    "half page up (set count): [count]^U"},
/* 026  ^V */
	{NULL,		0,
	    "insert a literal character: ^V"},
/* 027  ^W */
	{v_window,	0,
	    "switch windows: ^W"},
/* 030  ^X */
	{NULL},
/* 031  ^Y */
	{v_lineup,	V_CNT,
	    "page up by lines: [count]^Y"},
/* 032  ^Z */
	{v_stop,	0,
	    "suspend: ^Z"},
/* 033  ^[ */
	{NULL,		0,
	    "return to command mode: ^["},
/* 034  ^\ */
	{NULL},
/* 035  ^] */
	{v_tagpush,	V_KEYW|V_RCM_SET,
	    "tag push cursor word: ^]"},
/* 036  ^^ */
	{v_switch,	0,
	    "change to previous file: ^^"},
/* 037  ^_ */
	{NULL},
/* 040 ' ' */
	{v_right,	V_CNT|V_MOVE|V_RCM_SET,
	    "move right by columns: [count]' '"},
/* 041   ! */
	{v_filter,	V_CNT|V_DOT|V_MOTION|V_RCM_SET,
	    "filter through command(s): [count]![count]motion command(s)"},
/* 042   " */
	{NULL},
/* 043   # */
	{v_increment,	V_CHAR|V_CNT|V_DOT|V_KEYNUM|V_RCM_SET,
	    "number increment/decrement: [count]#[#+-]"},
/* 044   $ */
	{v_dollar,	V_CNT|V_MOVE|V_RCM_SETLAST,
	    "move to last column: [count]$"},
/* 045   % */
	{v_match,	V_ABS|V_MOVE|V_RCM_SET,
	    "move to match: %"},
/* 046   & */
	{v_again,	0,
	    "repeat substitution: &"},
/* 047   ' */
	{v_gomark,	V_ABS|V_CHAR|V_LMODE|V_MOVE|V_RCM_SETFNB,
	    "move to mark (first non-blank): '['a-z]"},
/* 050   ( */
	{v_sentenceb,	V_CNT|V_MOVE|V_RCM_SET,
	    "move back sentence: [count]("},
/* 051   ) */
	{v_sentencef,	V_ABS|V_CNT|V_MOVE|V_RCM_SET,
	    "move forward sentence: [count])"},
/* 052   * */
	{v_errlist,	0,
	    "step through compile errors: *"},
/* 053   + */
	{v_down,	V_CNT|V_LMODE|V_MOVE|V_RCM_SETFNB,
	    "move down by lines (first non-blank): [count]+"},
/* 054   , */
	{v_chrrepeat,	V_CNT|V_MOVE|V_RCM_SET,
	    "reverse last F, f, T or t search: [count],"},
/* 055   - */
	{v_up,		V_CNT|V_LMODE|V_MOVE|V_RCM_SETFNB,
	    "move up by lines (first non-blank): [count]-"},
/* 056   . */
	{NULL,		0,
	    "repeat the last command: ."},
/* 057   / */
	{v_searchf,	V_ABS|V_MOVE|V_RCM_SET,
	    "search forward: /RE"},
/* 060   0 */
	{v_zero,	V_MOVE|V_RCM_SET,
	    "move to first character: 0"},
/* 061   1 */
	{NULL},
/* 062   2 */
	{NULL},
/* 063   3 */
	{NULL},
/* 064   4 */
	{NULL},
/* 065   5 */
	{NULL},
/* 066   6 */
	{NULL},
/* 067   7 */
	{NULL},
/* 070   8 */
	{NULL},
/* 071   9 */
	{NULL},
/* 072   : */
	{v_ex,		0,
	    "ex command: :command [| command] ..."},
/* 073   ; */
	{v_chrepeat,	V_CNT|V_MOVE|V_RCM_SET,
	    "repeat last F, f, T or t search: [count];"},
/* 074   < */
	{v_shiftl,	V_CNT|V_DOT|V_MOTION|V_RCM_SET,
	    "shift lines left: [count]<[count]motion"},
/* 075   = */
	{NULL},
/* 076   > */
	{v_shiftr,	V_CNT|V_DOT|V_MOTION|V_RCM_SET,
	    "shift lines right: [count]>[count]motion"},
/* 077   ? */
	{v_searchb,	V_ABS|V_MOVE|V_RCM_SET,
	    "search backward: /RE"},
/* 100   @ */
	{v_at,		V_RBUF|V_RCM_SET,
	    "execute buffer: @buffer"},
/* 101   A */
	{v_iA,		V_CNT|V_DOT|V_RCM_SET,
	    "append to line: [count]A"},
/* 102   B */
	{v_wordB,	V_CNT|V_MOVE|V_RCM_SET,
	    "move back bigword: [count]B"},
/* 103   C */
	{v_Change,	V_CNT|V_DOT|V_OBUF|V_RCM_SET,
	    "change to end-of-line: [buffer][count]C"},
/* 104   D */
	{v_Delete,	V_CNT|V_DOT|V_OBUF|V_RCM_SET,
	    "delete to end-of-line: [buffer][count]D"},
/* 105   E */
	{v_wordE,	V_CNT|V_MOVE|V_RCM_SET,
	    "move to end of bigword: [count]E"},
/* 106   F */
	{v_chF,		V_CHAR|V_CNT|V_MOVE|V_RCM_SET,
	    "character in line backward search: [count]F character"},
/* 107   G */
	{v_lgoto,	V_ABS|V_CNT|V_LMODE|V_MOVE|V_RCM_SETFNB,
	    "move to line: [count]G"},
/* 110   H */
	{v_home,	V_CNT|V_LMODE|V_MOVE|V_RCM_SETFNB,
	    "move to count lines from screen top: [count]H"},
/* 111   I */
	{v_iI,		V_CNT|V_DOT|V_RCM_SET,
	    "insert at line beginning: [count]I"},
/* 112   J */
	{v_join,	V_CNT|V_DOT|V_RCM_SET,
	    "join lines: [count]J"},
/* 113   K */
	{NULL},
/* 114   L */
	{v_bottom,	V_CNT|V_LMODE|V_MOVE|V_RCM_SETFNB,
	    "move to screen bottom: [count]L"},
/* 115   M */
	{v_middle,	V_LMODE|V_MOVE|V_RCM_SETFNB,
	    "move to screen middle: M"},
/* 116   N */
	{v_searchN,	V_ABS|V_MOVE|V_RCM_SET,
	    "reverse last search: n"},
/* 117   O */
	{v_iO,		V_CNT|V_DOT|V_RCM_SET,
	    "insert above line: [count]O"},
/* 120   P */
	{v_Put,		V_CNT|V_DOT|V_OBUF|V_RCM_SET,
	    "insert before cursor from buffer: [buffer]P"},
/* 121   Q */
	{v_exmode,	0,
	    "switch to ex mode: Q"},
/* 122   R */
	{v_Replace,	V_CNT|V_DOT|V_RCM_SET,
	    "replace characters: [count]R"},
/* 123   S */
	{v_change,	V_CNT|V_DOT|V_OBUF|V_RCM_SET,
	    "substitute lines: [buffer][count]S"},
/* 124   T */
	{v_chT,		V_CHAR|V_CNT|V_MOVE|V_RCM_SET,
	    "before character in line backward search: [count]T character"},
/* 125   U */
	{v_Undo,	V_RCM_SET,
	    "undo current line: U"},
/* 126   V */
	{NULL},
/* 127   W */
	{v_wordW,	V_CNT|V_MOVE|V_RCM_SET,
	    "move to next bigword: [count]W"},
/* 130   X */
	{v_Xchar,	V_CNT|V_DOT|V_OBUF|V_RCM_SET,
	    "delete character before cursor: [buffer][count]X"},
/* 131   Y */
	{v_Yank,	V_CNT|V_LMODE|V_OBUF,
	    "copy line: [buffer][count]Y"},
/* 132   Z */
	{v_exit,	0,
	    "save file and exit: ZZ"},
/* 133   [ */
	{v_sectionb,	V_ABS|V_LMODE|V_MOVE|V_RCM_SET,
	    "move back section: ]]"},
/* 134   \ */
	{NULL},
/* 135   ] */
	{v_sectionf,	V_ABS|V_LMODE|V_MOVE|V_RCM_SET,
	    "move forward section: [["},
/* 136   ^ */
	{v_first,	V_MOVE|V_RCM_SETFNB,
	    "move to first non-blank: ^"},
/* 137   _ */
	{v_first,	V_MOVE|V_RCM_SET,
	    "move to first non-blank: _"},
/* 140   ` */
	{v_gomark,	V_ABS|V_CHAR|V_MOVE|V_RCM_SET,
	    "move to mark: `[`a-z]"},
/* 141   a */
	{v_ia,		V_CNT|V_DOT|V_RCM_SET,
	    "append after cursor: [count]a"},
/* 142   b */
	{v_wordb,	V_CNT|V_MOVE|V_RCM_SET,
	    "move back word: [count]b"},
/* 143   c */
	{v_change,	V_CNT|V_DOT|V_MOTION|V_OBUF|V_RCM_SET|VC_C,
	    "change: [buffer][count]c[count]motion"},
/* 144   d */
	{v_delete,	V_CNT|V_DOT|V_MOTION|V_OBUF|V_RCM_SET|VC_D,
	    "delete: [buffer][count]d[count]motion"},
/* 145   e */
	{v_worde,	V_CNT|V_MOVE|V_RCM_SET,
	    "move to end of word: [count]e"},
/* 146   f */
	{v_chf,		V_CHAR|V_CNT|V_MOVE|V_RCM_SET,
	    "character in line forward search: [count]f character"},
/* 147   g */
	{NULL},
/* 150   h */
	{v_left,	V_CNT|V_MOVE|V_RCM_SET,
	    "move left by columns: [count]h"},
/* 151   i */
	{v_ii,		V_CNT|V_DOT|V_RCM_SET,
	    "insert before cursor: [count]i"},
/* 152   j */
	{v_down,	V_CNT|V_LMODE|V_MOVE|V_RCM,
	    "move down by lines: [count]j"},
/* 153   k */
	{v_up,		V_CNT|V_LMODE|V_MOVE|V_RCM,
	    "move up by lines: [count]k"},
/* 154   l */
	{v_right,	V_CNT|V_MOVE|V_RCM_SET,
	    "move right by columns: [count]l"},
/* 155   m */
	{v_mark,	V_CHAR,
	    "set mark: m[a-z]"},
/* 156   n */
	{v_searchn,	V_ABS|V_MOVE|V_RCM_SET,
	    "repeat last search: n"},
/* 157   o */
	{v_io,		V_CNT|V_DOT|V_RCM_SET,
	    "append after line: [count]o"},
/* 160   p */
	{v_put,		V_CNT|V_DOT|V_OBUF|V_RCM_SET,
	    "insert after cursor from buffer: [buffer]P"},
/* 161   q */
	{NULL},
/* 162   r */
	{v_replace,	V_CHAR|V_CNT|V_DOT|V_RCM_SET,
	    "replace character: [count]r character"},
/* 163   s */
	{v_subst,	V_CNT|V_DOT|V_OBUF|V_RCM_SET,
	    "substitute character: [buffer][count]s"},
/* 164   t */
	{v_cht,		V_CHAR|V_CNT|V_MOVE|V_RCM_SET,
	    "before character in line forward search: [count]t character"},
/* 165   u */
	{v_undo,	V_RCM_SET,
	    "undo last change: u"},
/* 166   v */
	{NULL},
/* 167   w */
	{v_wordw,	V_CNT|V_MOVE|V_RCM_SET,
	    "move to next word: [count]w"},
/* 170   x */
	{v_xchar,	V_CNT|V_DOT|V_OBUF|V_RCM_SET,
	    "delete character: [buffer][count]x"},
/* 171   y */
	{v_yank,	V_CNT|V_MOTION|V_OBUF|VC_Y,
	    "copy text: [buffer][count]y[count]motion"},
/* 172   z */
	/*
	 * DON'T set the V_CHAR flag, the char isn't required,
	 * so it's handled specially in getcmd().
	 */
	{v_z, 		V_CNT|V_RCM_SETFNB,
	    "redraw window: [line]z[window_size][.-<CR>]"},
/* 173   { */
	{v_paragraphb,	V_ABS|V_CNT|V_LMODE|V_MOVE|V_RCM_SET,
	    "move back paragraph: [count]{"},
/* 174   | */
	{v_ncol,	V_ABS|V_CNT|V_MOVE|V_RCM_SET,
	    "move to column: [count]|"},
/* 175   } */
	{v_paragraphf,	V_ABS|V_CNT|V_LMODE|V_MOVE|V_RCM_SET,
	    "move forward paragraph: [count]}"},
/* 176   ~ */
	{v_ulcase,	V_CNT|V_DOT|V_RCM_SET,
	    "reverse case: [count]~"},
};
