/* vi.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


#include "config.h"
#include <ctype.h>
#include "vi.h"



/* This array describes what each key does */
#define NO_FUNC		(MARK (*)())0
#define NO_ARGS		0
#define CURSOR_COUNT	1
#define CURSOR		2
#define CURSOR_CNT_KEY	3
#define CURSOR_MOVED	4
#define CURSOR_EOL	5
#define ZERO		6
#define DIGIT		7
#define CURSOR_TEXT	8
#define CURSOR_CNT_CMD	9
#define KEYWORD		10
#define NO_FLAGS	0x00
#define	MVMT		0x01	/* this is a movement command */
#define PTMV		0x02	/* this can be *part* of a movement command */
#define FRNT		0x04	/* after move, go to front of line */
#define INCL		0x08	/* include last char when used with c/d/y */
#define LNMD		0x10	/* use line mode of c/d/y */
#define NCOL		0x20	/* this command can't change the column# */
#define NREL		0x40	/* this is "non-relative" -- set the '' mark */
#define SDOT		0x80	/* set the "dot" variables, for the "." cmd */
static struct keystru
{
	MARK	(*func)();	/* the function to run */
	uchar	args;		/* description of the args needed */
	uchar	flags;		/* other stuff */
}
	vikeys[] =
{
/* NUL not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^A  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^B  page backward	*/	{m_scroll,	CURSOR_CNT_CMD,	FRNT},
/* ^C  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^D  scroll dn 1/2page*/	{m_scroll,	CURSOR_CNT_CMD,	NCOL},
/* ^E  scroll up	*/	{m_scroll,	CURSOR_CNT_CMD,	NCOL},
/* ^F  page forward	*/	{m_scroll,	CURSOR_CNT_CMD,	FRNT},
/* ^G  show file status	*/	{v_status,	NO_ARGS, 	NO_FLAGS},
/* ^H  move left, like h*/	{m_left,	CURSOR_COUNT,	MVMT},
/* ^I  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^J  move down	*/	{m_updnto,	CURSOR_CNT_CMD,	MVMT|LNMD},
/* ^K  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^L  redraw screen	*/	{v_redraw,	NO_ARGS,	NO_FLAGS},
/* ^M  mv front next ln */	{m_updnto,	CURSOR_CNT_CMD,	MVMT|FRNT|LNMD},
/* ^N  move down	*/	{m_updnto,	CURSOR_CNT_CMD,	MVMT|LNMD},
/* ^O  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^P  move up		*/	{m_updnto,	CURSOR_CNT_CMD,	MVMT|LNMD},
/* ^Q  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^R  redraw screen	*/	{v_redraw,	NO_ARGS,	NO_FLAGS},
/* ^S  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^T  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^U  scroll up 1/2page*/	{m_scroll,	CURSOR_CNT_CMD,	NCOL},
/* ^V  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^W  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^X  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^Y  scroll down	*/	{m_scroll,	CURSOR_CNT_CMD,	NCOL},
/* ^Z  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ESC not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^\  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* ^]  keyword is tag	*/	{v_tag,		KEYWORD,	NO_FLAGS},
/* ^^  previous file	*/	{v_switch,	CURSOR,		NO_FLAGS},
/* ^_  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/* SPC move right,like l*/	{m_right,	CURSOR_COUNT,	MVMT},
/*  !  run thru filter	*/	{v_filter,	CURSOR_MOVED,	FRNT|LNMD|INCL},
/*  "  select cut buffer*/	{v_selcut,	CURSOR_CNT_KEY,	PTMV},
#ifndef NO_EXTENSIONS
/*  #  increment number	*/	{v_increment,	KEYWORD,	SDOT},
#else
/*  #  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
/*  $  move to rear	*/	{m_rear,	CURSOR,		MVMT|INCL},
/*  %  move to match	*/	{m_match,	CURSOR,		MVMT|INCL},
/*  &  repeat subst	*/	{v_again,	CURSOR_MOVED,	SDOT|NCOL|LNMD|INCL},
/*  '  move to a mark	*/	{m_tomark,	CURSOR_CNT_KEY,	MVMT|FRNT|NREL|LNMD|INCL},
#ifndef NO_SENTENCE
/*  (  mv back sentence	*/	{m_bsentence,	CURSOR_COUNT,	MVMT},
/*  )  mv fwd sentence	*/	{m_fsentence,	CURSOR_COUNT,	MVMT},
#else
/*  (  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/*  )  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
#ifndef NO_ERRLIST
/*  *  errlist		*/	{v_errlist,	CURSOR,		FRNT|NREL},
#else
/*  *  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
/*  +  mv front next ln */	{m_updnto,	CURSOR_CNT_CMD,	MVMT|FRNT|LNMD},
#ifndef NO_CHARSEARCH
/*  ,  reverse [fFtT] cmd*/	{m__ch,		CURSOR_CNT_CMD,	MVMT|INCL},
#else
/*  ,  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
/*  -  mv front prev ln	*/	{m_updnto,	CURSOR_CNT_CMD,	MVMT|FRNT|LNMD},
/*  .  special...	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/*  /  forward search	*/	{m_fsrch,	CURSOR_TEXT,	MVMT|NREL},
/*  0  part of count?	*/	{NO_FUNC,	ZERO,		MVMT|PTMV},
/*  1  part of count	*/	{NO_FUNC,	DIGIT,		PTMV},
/*  2  part of count	*/	{NO_FUNC,	DIGIT,		PTMV},
/*  3  part of count	*/	{NO_FUNC,	DIGIT,		PTMV},
/*  4  part of count	*/	{NO_FUNC,	DIGIT,		PTMV},
/*  5  part of count	*/	{NO_FUNC,	DIGIT,		PTMV},
/*  6  part of count	*/	{NO_FUNC,	DIGIT,		PTMV},
/*  7  part of count	*/	{NO_FUNC,	DIGIT,		PTMV},
/*  8  part of count	*/	{NO_FUNC,	DIGIT,		PTMV},
/*  9  part of count	*/	{NO_FUNC,	DIGIT,		PTMV},
/*  :  run single EX cmd*/	{v_1ex,		CURSOR_TEXT,	NO_FLAGS},
#ifndef NO_CHARSEARCH
/*  ;  repeat [fFtT] cmd*/	{m__ch,		CURSOR_CNT_CMD,	MVMT|INCL},
#else
/*  ;  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
/*  <  shift text left	*/	{v_lshift,	CURSOR_MOVED,	SDOT|FRNT|LNMD|INCL},
/*  =  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/*  >  shift text right	*/	{v_rshift,	CURSOR_MOVED,	SDOT|FRNT|LNMD|INCL},
/*  ?  backward search	*/	{m_bsrch,	CURSOR_TEXT,	MVMT|NREL},
#ifndef NO_AT
/*  @  execute a cutbuf */	{v_at,		CURSOR_CNT_KEY,	NO_FLAGS},
#else
/*  @  undefined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
/*  A  append at EOL	*/	{v_insert,	CURSOR_CNT_CMD,	SDOT},
/*  B  move back Word	*/	{m_bword,	CURSOR_CNT_CMD,	MVMT},
/*  C  change to EOL	*/	{v_change,	CURSOR_EOL,	SDOT},
/*  D  delete to EOL	*/	{v_delete,	CURSOR_EOL,	SDOT},
/*  E  move end of Word	*/	{m_eword,	CURSOR_CNT_CMD,	MVMT|INCL},
#ifndef NO_CHARSEARCH
/*  F  move bk to char	*/	{m_Fch,		CURSOR_CNT_KEY,	MVMT|INCL},
#else
/*  F  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
/*  G  move to line #	*/	{m_updnto,	CURSOR_CNT_CMD,	MVMT|NREL|LNMD|FRNT|INCL},
/*  H  move to row	*/	{m_row,		CURSOR_CNT_CMD,	MVMT|FRNT},
/*  I  insert at front	*/	{v_insert,	CURSOR_CNT_CMD,	SDOT},
/*  J  join lines	*/	{v_join,	CURSOR_COUNT,	SDOT},
#ifndef NO_EXTENSIONS
/*  K  look up keyword	*/	{v_keyword,	KEYWORD,	NO_FLAGS},
#else
/*  K  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
/*  L  move to last row	*/	{m_row,		CURSOR_CNT_CMD,	MVMT|FRNT},
/*  M  move to mid row	*/	{m_row,		CURSOR_CNT_CMD,	MVMT|FRNT},
/*  N  reverse prev srch*/	{m_Nsrch,	CURSOR,		MVMT},
/*  O  insert above line*/	{v_insert,	CURSOR_CNT_CMD,	SDOT},
/*  P  paste before	*/	{v_paste,	CURSOR_CNT_CMD,	NO_FLAGS},
/*  Q  quit to EX mode	*/	{v_quit,	NO_ARGS,	NO_FLAGS},
/*  R  overtype		*/	{v_overtype,	CURSOR,		SDOT},
/*  S  change line	*/	{v_change,	CURSOR_MOVED,	SDOT},
#ifndef NO_CHARSEARCH
/*  T  move bk to char	*/	{m_Tch,		CURSOR_CNT_KEY,	MVMT|INCL},
#else
/*  T  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
/*  U  undo whole line	*/	{v_undoline,	CURSOR,		FRNT},
/*  V  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/*  W  move forward Word*/	{m_fword,	CURSOR_CNT_CMD,	MVMT},
/*  X  delete to left	*/	{v_xchar,	CURSOR_CNT_CMD,	SDOT},
/*  Y  yank text	*/	{v_yank,	CURSOR_MOVED,	NCOL},
/*  Z  save file & exit	*/	{v_xit,		CURSOR_CNT_KEY,	NO_FLAGS},
/*  [  move back section*/	{m_bsection,	CURSOR_CNT_KEY,	MVMT|LNMD|NREL},
/*  \  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/*  ]  move fwd section */	{m_fsection,	CURSOR_CNT_KEY,	MVMT|LNMD|NREL},
/*  ^  move to front	*/	{m_front,	CURSOR,		MVMT},
/*  _  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/*  `  move to mark	*/	{m_tomark,	CURSOR_CNT_KEY,	MVMT|NREL},
/*  a  append at cursor	*/	{v_insert,	CURSOR_CNT_CMD,	SDOT},
/*  b  move back word	*/	{m_bword,	CURSOR_CNT_CMD,	MVMT},
/*  c  change text	*/	{v_change,	CURSOR_MOVED,	SDOT},
/*  d  delete op	*/	{v_delete,	CURSOR_MOVED,	SDOT|NCOL},
/*  e  move end word	*/	{m_eword,	CURSOR_CNT_CMD,	MVMT|INCL},
#ifndef NO_CHARSEARCH
/*  f  move fwd for char*/	{m_fch,		CURSOR_CNT_KEY,	MVMT|INCL},
#else
/*  f  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
/*  g  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/*  h  move left	*/	{m_left,	CURSOR_COUNT,	MVMT},
/*  i  insert at cursor	*/	{v_insert,	CURSOR_CNT_CMD,	SDOT},
/*  j  move down	*/	{m_updnto,	CURSOR_CNT_CMD,	MVMT|NCOL|LNMD},
/*  k  move up		*/	{m_updnto,	CURSOR_CNT_CMD,	MVMT|NCOL|LNMD},
/*  l  move right	*/	{m_right,	CURSOR_COUNT,	MVMT},
/*  m  define a mark	*/	{v_mark,	CURSOR_CNT_KEY,	NO_FLAGS},
/*  n  repeat prev srch	*/	{m_nsrch,	CURSOR, 	MVMT},
/*  o  insert below line*/	{v_insert,	CURSOR_CNT_CMD,	SDOT},
/*  p  paste after	*/	{v_paste,	CURSOR_CNT_CMD,	NO_FLAGS},
/*  q  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/*  r  replace chars	*/	{v_replace,	CURSOR_CNT_KEY,	SDOT},
/*  s  subst N chars	*/	{v_subst,	CURSOR_COUNT,	SDOT},
#ifndef NO_CHARSEARCH
/*  t  move fwd to char	*/	{m_tch,		CURSOR_CNT_KEY,	MVMT|INCL},
#else
/*  t  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
#endif
/*  u  undo		*/	{v_undo,	CURSOR,		NO_FLAGS},
/*  v  not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS},
/*  w  move fwd word	*/	{m_fword,	CURSOR_CNT_CMD,	MVMT},
/*  x  delete character	*/	{v_xchar,	CURSOR_CNT_CMD,	SDOT},
/*  y  yank text	*/	{v_yank,	CURSOR_MOVED,	NCOL},
/*  z  adjust scrn row	*/	{m_z, 		CURSOR_CNT_KEY,	NCOL},
/*  {  back paragraph	*/	{m_bparagraph,	CURSOR_COUNT,	MVMT|LNMD},
/*  |  move to column	*/	{m_tocol,	CURSOR_COUNT,	NREL},
/*  }  fwd paragraph	*/	{m_fparagraph,	CURSOR_COUNT,	MVMT|LNMD},
/*  ~  upper/lowercase	*/	{v_ulcase,	CURSOR_COUNT,	SDOT},
/* DEL not defined	*/	{NO_FUNC,	NO_ARGS,	NO_FLAGS}
};



void vi()
{
	REG int			key;	/* keystroke from user */
	long			count;	/* numeric argument to some functions */
	REG struct keystru	*keyptr;/* pointer to vikeys[] element */
	MARK			tcurs;	/* temporary cursor */
	int			prevkey;/* previous key, if d/c/y/</>/! */
	MARK			range;	/* start of range for d/c/y/</>/! */
	char			text[100];
	int			dotkey;	/* last "key" of a change */
	int			dotpkey;/* last "prevkey" of a change */
	int			dotkey2;/* last extra "getkey()" of a change */
	int			dotcnt;	/* last "count" of a change */
	int			firstkey;
	REG int			i;

	/* tell the redraw() function to start from scratch */
	redraw(MARK_UNSET, FALSE);

#ifdef lint
	/* lint says that "range" might be used before it is set.  This
	 * can't really happen due to the way "range" and "prevkey" are used,
	 * but lint doesn't know that.  This line is here ONLY to keep lint
	 * happy.
	 */
	range = 0L;
#endif

	/* safeguard against '.' with no previous command */
	dotkey = 0;

	/* go immediately into insert mode, if ":set inputmode" */
	firstkey = 0;
#ifndef NO_EXTENSIONS
	if (*o_inputmode)
	{
		firstkey = 'i';
	}
#endif

	/* Repeatedly handle VI commands */
	for (count = 0, prevkey = '\0'; mode == MODE_VI; )
	{
		/* if we've moved off the undoable line, then we can't undo it at all */
		if (markline(cursor) != U_line)
		{
			U_line = 0L;
		}

		/* report any changes from the previous command */
		if (rptlines >= *o_report)
		{
			redraw(cursor, FALSE);
			msg("%ld lines %s", rptlines, rptlabel);
		}
		rptlines = 0L;

		/* get the next command key.  It must be ASCII */
		if (firstkey)
		{
			key = firstkey;
			firstkey = 0;
		}
		else
		{
			do
			{
				key = getkey(WHEN_VICMD);
			} while (key < 0 || key > 127);
		}

		/* change cw and cW commands to ce and cE, respectively */
		/* (Why?  because the real vi does it that way!) */
		if (prevkey == 'c')
		{
			if (key == 'w')
				key = 'e';
			else if (key == 'W')
				key = 'E';

			/* wouldn't work right at the end of a word unless we
			 * backspace one character before doing the move.  This
			 * will fix most cases.  !!! but not all.
			 */
			if (markidx(cursor) > 0 && (key == 'e' || key == 'E'))
			{
				cursor--;
			}
		}

		/* look up the structure describing this command */
		keyptr = &vikeys[key];

		/* if we're in the middle of a d/c/y/</>/! command, reject
		 * anything but movement or a doubled version like "dd".
		 */
		if (prevkey && key != prevkey && !(keyptr->flags & (MVMT|PTMV)))
		{
			beep();
			prevkey = 0;
			count = 0;
			continue;
		}

		/* set the "dot" variables, if we're supposed to */
		if ((keyptr->flags & SDOT)
		 || (prevkey && vikeys[prevkey].flags & SDOT))
		{
			dotkey = key;
			dotpkey = prevkey;
			dotkey2 = '\0';
			dotcnt = count;

			/* remember the line before any changes are made */
			if (U_line != markline(cursor))
			{
				U_line = markline(cursor);
				strcpy(U_text, fetchline(U_line));
			}
		}

		/* if this is "." then set other vars from the "dot" vars */
		if (key == '.')
		{
			key = dotkey;
			keyptr = &vikeys[key];
			prevkey = dotpkey;
			if (prevkey)
			{
				range = cursor;
			}
			if (count == 0)
			{
				count = dotcnt;
			}
			doingdot = TRUE;

			/* remember the line before any changes are made */
			if (U_line != markline(cursor))
			{
				U_line = markline(cursor);
				strcpy(U_text, fetchline(U_line));
			}
		}
		else
		{
			doingdot = FALSE;
		}

		/* process the key as a command */
		tcurs = cursor;
		switch (keyptr->args)
		{
		  case ZERO:
			if (count == 0)
			{
				tcurs = cursor & ~(BLKSIZE - 1);
				break;
			}
			/* else fall through & treat like other digits... */

		  case DIGIT:
			count = count * 10 + key - '0';
			break;

		  case KEYWORD:
			/* if not on a keyword, fail */
			pfetch(markline(cursor));
			key = markidx(cursor);
			if (isascii(ptext[key])
				&& !isalnum(ptext[key]) && ptext[key] != '_')
			{
				tcurs = MARK_UNSET;
				break;
			}

			/* find the start of the keyword */
			while (key > 0 && (!isascii(ptext[key-1]) ||
			isalnum(ptext[key - 1]) || ptext[key - 1] == '_'))
			{
				key--;
			}
			tcurs = (cursor & ~(BLKSIZE - 1)) + key;

			/* copy it into a buffer, and NUL-terminate it */
			i = 0;
			do
			{
				text[i++] = ptext[key++];
			} while (!isascii(ptext[key]) || isalnum(ptext[key]) || ptext[key] == '_');
			text[i] = '\0';

			/* call the function */
			tcurs = (*keyptr->func)(text, tcurs, count);
			count = 0L;
			break;

		  case NO_ARGS:
			if (keyptr->func)
			{
				(*keyptr->func)();
			}
			else
			{
				beep();
			}
			count = 0L;
			break;
	
		  case CURSOR_COUNT:
			tcurs = (*keyptr->func)(cursor, count);
			count = 0L;
			break;
	
		  case CURSOR:
			tcurs = (*keyptr->func)(cursor);
			count = 0L;
			break;

		  case CURSOR_CNT_KEY:
			if (doingdot)
			{
				tcurs = (*keyptr->func)(cursor, count, dotkey2);
			}
			else
			{
				/* get a key */
				i = getkey(0);
				if (i == '\033') /* ESC */
				{
					count = 0;
					tcurs = MARK_UNSET;
					break; /* exit from "case CURSOR_CNT_KEY" */
				}
				else if (i == ('V' & 0x1f))
				{
					i = getkey(0);
				}

				/* if part of an SDOT command, remember it */
				 if (keyptr->flags & SDOT
				 || (prevkey && vikeys[prevkey].flags & SDOT))
				{
					dotkey2 = i;
				}

				/* do it */
				tcurs = (*keyptr->func)(cursor, count, i);
			}
			count = 0L;
			break;
	
		  case CURSOR_MOVED:
			/* '&' and uppercase keys always act like doubled */
			if (key == '&' || isascii(key) && isupper(key))
			{
				prevkey = key;
			}

			if (prevkey)
			{
				/* doubling up a command */
				if (!count) count = 1L;
				range = cursor;
				tcurs = range + MARK_AT_LINE(count - 1L);
				count = 0L;
			}
			else
			{
				prevkey = key;
				range = cursor;
				key = -1; /* so we don't think we doubled yet */
			}
			break;

		  case CURSOR_EOL:
			prevkey = key;
			/* a zero-length line needs special treatment */
			pfetch(markline(cursor));
			if (plen == 0)
			{
				/* act on a zero-length section of text */
				range = tcurs = cursor;
				key = ' ';
			}
			else
			{
				/* act like CURSOR_MOVED with '$' movement */
				range = cursor;
				tcurs = m_rear(cursor, 1L);
				key = '$';
			}
			count = 0L;
			keyptr = &vikeys[key];
			break;

		  case CURSOR_TEXT:
		  	do
		  	{	
				text[0] = key;
				if (vgets(key, text + 1, sizeof text - 1) >= 0)
				{
					/* reassure user that <CR> was hit */
					qaddch('\r');
					refresh();

					/* call the function with the text */
					tcurs = (*keyptr->func)(cursor, text);
				}
				else
				{
					if (exwrote || mode == MODE_COLON)
					{
						redraw(MARK_UNSET, FALSE);
					}
					mode = MODE_VI;
				}
			} while (mode == MODE_COLON);
			count = 0L;
			break;

		  case CURSOR_CNT_CMD:
			tcurs = (*keyptr->func)(cursor, count, key);
			count = 0L;
			break;
		}

		/* if that command took us out of vi mode, then exit the loop
		 * NOW, without tweaking the cursor or anything.  This is very
		 * important when mode == MODE_QUIT.
		 */
		if (mode != MODE_VI)
		{
			break;
		}

		/* now move the cursor, as appropriate */
		if (keyptr->args == CURSOR_MOVED)
		{
			/* the < and > keys have FRNT,
			 * but it shouldn't be applied yet
			 */
			tcurs = adjmove(cursor, tcurs, 0);
		}
		else
		{
			tcurs = adjmove(cursor, tcurs, (int)keyptr->flags);
		}

		/* was that the end of a d/c/y/</>/! command? */
		if (prevkey && (prevkey == key || (keyptr->flags & MVMT)) && count == 0L)
		{
			/* if the movement command failed, cancel operation */
			if (tcurs == MARK_UNSET)
			{
				prevkey = 0;
				count = 0;
				continue;
			}

			/* make sure range=front and tcurs=rear.  Either way,
			 * leave cursor=range since that's where we started.
			 */
			cursor = range;
			if (tcurs < range)
			{
				range = tcurs;
				tcurs = cursor;
			}


			/* adjust for line mode & inclusion of last char/line */
			i = (keyptr->flags | vikeys[prevkey].flags);
			if (key == prevkey)
			{
				i |= (INCL|LNMD);
			}
			switch (i & (INCL|LNMD))
			{
			  case INCL:
				tcurs++;
				break;

			  case INCL|LNMD:
				tcurs += BLKSIZE;
				/* fall through... */

			  case LNMD:
				range &= ~(BLKSIZE - 1);
				tcurs &= ~(BLKSIZE - 1);
				break;
			}

			/* run the function */
			tcurs = (*vikeys[prevkey].func)(range, tcurs);
			(void)adjmove(cursor, cursor, 0);
			cursor = adjmove(cursor, tcurs, (int)vikeys[prevkey].flags);

			/* cleanup */
			prevkey = 0;
		}
		else if (!prevkey)
		{
			cursor = tcurs;
		}
	}
}

/* This function adjusts the MARK value that they return; here we make sure
 * it isn't past the end of the line, and that the column hasn't been
 * *accidentally* changed.
 */
MARK adjmove(old, new, flags)
	MARK		old;	/* the cursor position before the command */
	REG MARK	new;	/* the cursor position after the command */
	int		flags;	/* various flags regarding cursor mvmt */
{
	static int	colno;	/* the column number that we want */
	REG char	*text;	/* used to scan through the line's text */
	REG int		i;

#ifdef DEBUG
	watch();
#endif

	/* if the command failed, bag it! */
	if (new == MARK_UNSET)
	{
		beep();
		return old;
	}

	/* if this is a non-relative movement, set the '' mark */
	if (flags & NREL)
	{
		mark[26] = old;
	}

	/* make sure it isn't past the end of the file */
	if (markline(new) < 1)
	{
		new = MARK_FIRST;
	}
	else if (markline(new) > nlines)
	{
		new = MARK_LAST;
	}

	/* fetch the new line */
	pfetch(markline(new));

	/* move to the front, if we're supposed to */
	if (flags & FRNT)
	{
		new = m_front(new, 1L);
	}

	/* change the column#, or change the mark to suit the column# */
	if (!(flags & NCOL))
	{
		/* change the column# */
		i = markidx(new);
		if (i == BLKSIZE - 1)
		{
			new &= ~(BLKSIZE - 1);
			if (plen > 0)
			{
				new += plen - 1;
			}
			colno = BLKSIZE * 8; /* one heck of a big colno */
		}
		else if (plen > 0)
		{
			if (i >= plen)
			{
				new = (new & ~(BLKSIZE - 1)) + plen - 1;
			}
			colno = idx2col(new, ptext, FALSE);
		}
		else
		{
			new &= ~(BLKSIZE - 1);
			colno = 0;
		}
	}
	else
	{
		/* adjust the mark to get as close as possible to column# */
		for (i = 0, text = ptext; i <= colno && *text; text++)
		{
			if (*text == '\t' && !*o_list)
			{
				i += *o_tabstop - (i % *o_tabstop);
			}
			else if (UCHAR(*text) < ' ' || *text == 127)
			{
				i += 2;
			}
#ifndef NO_CHARATTR
			else if (*o_charattr && text[0] == '\\' && text[1] == 'f' && text[2])
			{
				text += 2; /* plus one more in "for()" stmt */
			}
#endif
			else
			{
				i++;
			}
		}
		if (text > ptext)
		{
			text--;
		}
		new = (new & ~(BLKSIZE - 1)) + (int)(text - ptext);
	}

	return new;
}


#ifdef DEBUG
watch()
{
	static wasset;

	if (*origname)
	{
		wasset = TRUE;
	}
	else if (wasset)
	{
		msg("origname was clobbered");
		endwin();
		abort();
	}

	if (nlines == 0)
	{
		msg("nlines=0");
		endwin();
		abort();
	}
}
#endif
