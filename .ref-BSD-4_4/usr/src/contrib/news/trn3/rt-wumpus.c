/* $Id: rt-wumpus.c,v 3.0 1992/12/14 00:14:00 davison Trn $
*/

#include "EXTERN.h"
#include "common.h"
#include "cache.h"
#include "ng.h"
#include "head.h"
#include "util.h"
#include "term.h"
#include "final.h"
#include "ngdata.h"
#include "artio.h"
#include "backpage.h"
#include "rthread.h"
#include "rt-select.h"
#include "INTERN.h"
#include "rt-wumpus.h"

static char tree_indent[] = {
    ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0,
    ' ', ' ', ' ', ' ', 0,   ' ', ' ', ' ', ' ', 0
};

char letters[] = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+";

static ARTICLE *tree_article;

static int max_depth, max_line = -1;
static int first_depth, first_line;
static int my_depth, my_line;
static bool node_on_line;
static int node_line_cnt;

static int line_num;
static int header_indent;

static char *tree_lines[11];
static char tree_buff[128], *str;

/* Prepare tree display for inclusion in the article header.
*/
void
init_tree()
{
    ARTICLE *thread;
    SUBJECT *sp;
    int num;

    while (max_line >= 0)		/* free any previous tree data */
	free(tree_lines[max_line--]);

    if (!(tree_article = curr_artp) || !tree_article->subj)
	return;
    if (!(thread = tree_article->subj->thread))
	return;
    /* Enumerate our subjects for display */
    sp = thread->subj;
    num = 0;
    do {
	sp->misc = num++;
	sp = sp->thread_link;
    } while (sp != thread->subj);

    max_depth = max_line = my_depth = my_line = node_line_cnt = 0;
    find_depth(thread, 0);

    if (max_depth <= 5) {
	first_depth = 0;
    } else {
	if (my_depth+2 > max_depth) {
	    first_depth = max_depth - 5;
	} else if ((first_depth = my_depth - 3) < 0) {
	    first_depth = 0;
	}
	max_depth = first_depth + 5;
    }
    if (--max_line < max_tree_lines) {
	first_line = 0;
    } else {
	if (my_line + max_tree_lines/2 > max_line) {
	    first_line = max_line - (max_tree_lines-1);
	} else if ((first_line = my_line - (max_tree_lines-1)/2) < 0) {
	    first_line = 0;
	}
	max_line = first_line + max_tree_lines-1;
    }

    str = tree_buff;		/* initialize first line's data */
    *str++ = ' ';
    node_on_line = FALSE;
    line_num = 0;
    /* cache our portion of the tree */
    cache_tree(thread, 0, tree_indent);

    max_depth = (max_depth-first_depth) * 5;	/* turn depth into char width */
    max_line -= first_line;			/* turn max_line into count */
    /* shorten tree if lower lines aren't visible */
    if (node_line_cnt < max_line) {
	max_line = node_line_cnt + 1;
    }
}

/* A recursive routine to find the maximum tree extents and where we are.
*/
static void
find_depth(article, depth)
ARTICLE *article;
int depth;
{
    if (depth > max_depth) {
	max_depth = depth;
    }
    for (;;) {
	if (article == tree_article) {
	    my_depth = depth;
	    my_line = max_line;
	}
	if (article->child1) {
	    find_depth(article->child1, depth+1);
	} else {
	    max_line++;
	}
	if (!(article = article->sibling)) {
	    break;
	}
    }
}

/* Place the tree display in a maximum of 11 lines x 6 nodes.
*/
static void
cache_tree(ap, depth, cp)
ARTICLE *ap;
int depth;
char *cp;
{
    int depth_mode;

    cp[1] = ' ';
    if (depth >= first_depth && depth <= max_depth) {
	cp += 5;
	depth_mode = 1;
    } else if (depth+1 == first_depth) {
	depth_mode = 2;
    } else {
	cp = tree_indent;
	depth_mode = 0;
    }
    for (;;) {
	switch (depth_mode) {
	case 1: {
	    char ch;

	    *str++ = ((ap->flags & AF_HAS_RE) || ap->parent) ? '-' : ' ';
	    if (ap == tree_article)
		*str++ = '*';
	    if (ap->flags & AF_READ) {
		*str++ = '(';
		ch = ')';
	    } else if (!selected_only || (ap->flags & AF_SEL)) {
		*str++ = '[';
		ch = ']';
	    } else {
		*str++ = '<';
		ch = '>';
	    }
	    if (ap == recent_artp && ap != tree_article)
		*str++ = '@';
	    *str++ = letter(ap);
	    *str++ = ch;
	    if (ap->child1) {
		*str++ = (ap->child1->sibling? '+' : '-');
	    }
	    if (ap->sibling)
		*cp = '|';
	    else
		*cp = ' ';
	    node_on_line = TRUE;
	    break;
	}
	case 2:
	    *tree_buff = (!ap->child1)? ' ' :
		(ap->child1->sibling)? '+' : '-';
	    break;
	default:
	    break;
	}
	if (ap->child1) {
	    cache_tree(ap->child1, depth+1, cp);
	    cp[1] = '\0';
	} else {
	    if (!node_on_line && first_line == line_num) {
		first_line++;
	    }
	    if (line_num >= first_line) {
		if (str[-1] == ' ') {
		    str--;
		}
		*str = '\0';
		tree_lines[line_num-first_line]
			= safemalloc(str-tree_buff + 1);
		strcpy(tree_lines[line_num - first_line], tree_buff);
		if (node_on_line) {
		    node_line_cnt = line_num - first_line;
		}
	    }
	    line_num++;
	    node_on_line = FALSE;
	}
	if (!(ap = ap->sibling) || line_num > max_line)
	    break;
	if (!ap->sibling)
	    *cp = '\\';
	if (!first_depth)
	    tree_indent[5] = ' ';
	strcpy(tree_buff, tree_indent+5);
	str = tree_buff + strlen(tree_buff);
    }
}

/* Output a header line with possible tree display on the right hand side.
** Does automatic wrapping of lines that are too long.
*/
int
tree_puts(orig_line, header_line, use_underline)
char *orig_line;
ART_LINE header_line;
int use_underline;
{
    char *buf;
    register char *line, *cp, *end;
    int pad_cnt, wrap_at;
    ART_LINE start_line = header_line;
    int i;
    char ch;

    /* Make a modifiable copy of the line */
    buf = safemalloc(strlen(orig_line) + 2);  /* yes, I mean "2" */
    strcpy(buf, orig_line);
    line = buf;

    /* Change any embedded control characters to spaces */
    for (end = line; *end && *end != '\n'; end++) {
	if ((unsigned char)*end < ' ') {
	    *end = ' ';
	}
    }
    *end = '\0';

    if (!*line) {
	strcpy(line, " ");
	end = line+1;
    }

    /* If this is the first subject line, output it with a preceeding [1] */
    if (ThreadedGroup && use_underline && (unsigned char)*line > ' ') {
#ifdef NOFIREWORKS
	no_sofire();
#endif
	standout();
	putchar('[');
	putchar(letter(curr_artp));
	putchar(']');
	un_standout();
	putchar(' ');
	header_indent = 4;
	line += 9;
	i = 0;
    } else {
	if (*line != ' ') {
	    /* A "normal" header line -- output keyword and set header_indent
	    ** _except_ for the first line, which is a non-standard header.
	    */
	    if (!header_line || !(cp = index(line, ':')) || *++cp != ' ') {
		header_indent = 0;
	    } else {
		*cp = '\0';
		fputs(line, stdout);
		putchar(' ');
		header_indent = ++cp - line;
		line = cp;
		if (!*line) {
		    *--line = ' ';
		}
	    }
	    i = 0;
	} else {
	    /* Skip whitespace of continuation lines and prepare to indent */
	    while (*++line == ' ') {
		;
	    }
	    i = header_indent;
	}
    }
    for ( ; *line; i = header_indent) {
#ifdef CLEAREOL
	maybe_eol();
#endif
	if (i) {
	    putchar('+');
	    while (--i) {
		putchar(' ');
	    }
	}
	/* If no (more) tree lines, wrap at COLS-1 */
	if (max_line < 0 || header_line > max_line+1) {
	    wrap_at = COLS-1;
	} else {
	    wrap_at = COLS - max_depth - 5 - 3;
	}
	/* Figure padding between header and tree output, wrapping long lines */
	pad_cnt = wrap_at - (end - line + header_indent);
	if (pad_cnt <= 0) {
	    cp = line + wrap_at - header_indent - 1;
	    pad_cnt = 1;
	    while (cp > line && *cp != ' ') {
		if (*--cp == ',' || *cp == '.' || *cp == '-' || *cp == '!') {
		    cp++;
		    break;
		}
		pad_cnt++;
	    }
	    if (cp == line) {
		cp += wrap_at - header_indent;
		pad_cnt = 0;
	    }
	    ch = *cp;
	    *cp = '\0';
	    /* keep rn's backpager happy */
	    vwtary(artline, vrdary(artline - 1));
	    artline++;
	} else {
	    cp = end;
	    ch = '\0';
	}
	if (use_underline) {
	    underprint(line);
	} else {
	    fputs(line, stdout);
	}
	*cp = ch;
	/* Skip whitespace in wrapped line */
	while (*cp == ' ') {
	    cp++;
	}
	line = cp;
	/* Check if we've got any tree lines to output */
	if (wrap_at != COLS-1 && header_line <= max_line) {
	    char *cp1, *cp2;

	    do {
		putchar(' ');
	    } while (pad_cnt--);
	    /* Check string for the '*' flagging our current node
	    ** and the '@' flagging our prior node.
	    */
	    cp = tree_lines[header_line];
	    cp1 = index(cp, '*');
	    cp2 = index(cp, '@');
	    if (cp1 != Nullch) {
		*cp1 = '\0';
	    }
	    if (cp2 != Nullch) {
		*cp2 = '\0';
	    }
	    fputs(cp, stdout);
	    /* Handle standout output for '*' and '@' marked nodes, then
	    ** continue with the rest of the line.
	    */
	    while (cp1 || cp2) {
		standout();
		if (cp1 && (!cp2 || cp1 < cp2)) {
		    cp = cp1;
		    cp1 = Nullch;
		    *cp++ = '*';
		    putchar(*cp++);
		    putchar(*cp++);
		} else {
		    cp = cp2;
		    cp2 = Nullch;
		    *cp++ = '@';
		}
		putchar(*cp++);
		un_standout();
		if (*cp) {
		    fputs(cp, stdout);
		}
	    }/* while */
	}/* if */
	putchar('\n') FLUSH;
	header_line++;
    }/* for remainder of line */

    /* free allocated copy of line */
    free(buf);

    /* return number of lines displayed */
    return header_line - start_line;
}

/* Output any parts of the tree that are left to display.  Called at the
** end of each header.
*/
int
finish_tree(last_line)
ART_LINE last_line;
{
    ART_LINE start_line = last_line;

    while (last_line <= max_line) {
	artline++;
	last_line += tree_puts("+", last_line, 0);
	vwtary(artline, artpos);	/* keep rn's backpager happy */
    }
    return last_line - start_line;
}

/* Output the entire article tree for the user.
*/
void
entire_tree(ap)
ARTICLE *ap;
{
    ARTICLE *thread;
    SUBJECT *sp;
    int num;

    if (!ap) {
#ifdef VERBOSE
	IF (verbose)
	    fputs("\nNo article tree to display.\n", stdout) FLUSH;
	ELSE
#endif
#ifdef TERSE
	    fputs("\nNo tree.\n", stdout) FLUSH;
#endif
	return;
    }

    if (!ThreadedGroup) {
	ThreadedGroup = TRUE;
	printf("Threading the group. "), fflush(stdout);
	thread_open();
	if (!ThreadedGroup) {
	    printf("*failed*\n") FLUSH;
	    return;
	}
	count_subjects(CS_NORM);
	putchar('\n') FLUSH;
    }
    if (!(ap->flags & AF_THREADED))
	parseheader(article_num(ap));
    if (check_page_line())
	return;
    putchar('\n');
    thread = ap->subj->thread;
    /* Enumerate our subjects for display */
    sp = thread->subj;
    num = 0;
    do {
	if (check_page_line())
	    return;
	printf("[%c] %s\n",letters[num>9+26+26? 9+26+26:num],sp->str+4) FLUSH;
	sp->misc = num++;
	sp = sp->thread_link;
    } while (sp != thread->subj);
    if (check_page_line())
	return;
    putchar('\n');
    if (check_page_line())
	return;
    putchar(' ');
    buf[3] = '\0';
    display_tree(thread, tree_indent);

    if (check_page_line())
	return;
    putchar('\n');
}

/* A recursive routine to output the entire article tree.
*/
static void
display_tree(article, cp)
ARTICLE *article;
char *cp;
{
    if (cp - tree_indent > COLS || page_line < 0)
	return;
    cp[1] = ' ';
    cp += 5;
    for (;;) {
	putchar(((article->flags&AF_HAS_RE) || article->parent) ? '-' : ' ');
	if (article->flags & AF_READ) {
	    buf[0] = '(';
	    buf[2] = ')';
	} else if (!selected_only || (article->flags & AF_SEL)) {
	    buf[0] = '[';
	    buf[2] = ']';
	} else {
	    buf[0] = '<';
	    buf[2] = '>';
	}
	buf[1] = letter(article);
	if (article == curr_artp) {
	    standout();
	    fputs(buf, stdout);
	    un_standout();
	} else if (article == recent_artp) {
	    putchar(buf[0]);
	    standout();
	    putchar(buf[1]);
	    un_standout();
	    putchar(buf[2]);
	} else {
	    fputs(buf, stdout);
	}

	if (article->sibling) {
	    *cp = '|';
	} else {
	    *cp = ' ';
	}
	if (article->child1) {
	    putchar((article->child1->sibling)? '+' : '-');
	    display_tree(article->child1, cp);
	    cp[1] = '\0';
	} else {
	    putchar('\n') FLUSH;
	}
	if (!(article = article->sibling)) {
	    break;
	}
	if (!article->sibling) {
	    *cp = '\\';
	}
	tree_indent[5] = ' ';
	if (check_page_line()) {
	    return;
	}
	fputs(tree_indent+5, stdout);
    }
}

static int
check_page_line()
{
    if (page_line < 0)
	return -1;
    if (page_line >= LINES || int_count) {
      register int cmd = -1;
	if (int_count || (cmd = get_anything())) {
	    page_line = -1;		/* disable further printing */
	    if (cmd > 0)
		pushchar(cmd);
	    return cmd;
	}
    }
    page_line++;
    return 0;
}

/* Calculate the subject letter representation.  "Place-holder" nodes
** are marked with a ' ', others get a letter in the sequence:
**	' ', '1'-'9', 'A'-'Z', 'a'-'z', '+'
*/
static char
letter(ap)
register ARTICLE *ap;
{
    int subj = ap->subj->misc;

    if (!(ap->flags & AF_CACHED)
     && (absfirst < first_cached || last_cached < lastart
      || !cached_all_in_range))
	return '?';
    if (ap->flags & AF_MISSING)
	return ' ';
    return letters[subj > 9+26+26 ? 9+26+26 : subj];
}
