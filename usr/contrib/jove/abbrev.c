/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"

#ifdef ABBREV

#include "io.h"
#include "ctype.h"

#define HASHSIZE	20

struct abbrev {
	unsigned int	a_hash;
	char	*a_abbrev,
		*a_phrase;
	struct abbrev	*a_next;
	data_obj	*a_cmdhook;
};

#define GLOBAL	NMAJORS
static struct abbrev	*A_tables[NMAJORS + 1][HASHSIZE] = {0};

int AutoCaseAbbrev = 1;

static unsigned int
hash(a)
register char	*a;
{
	register unsigned int	hashval = 0;
	register int	c;

	while (c = *a++)
		hashval = (hashval << 2) + c;

	return hashval;
}

static
def_abbrev(table)
struct abbrev	*table[HASHSIZE];
{
	char	abbrev[100],
		phrase[100];

	strcpy(abbrev, ask((char *) 0, "abbrev: "));
	strcpy(phrase, ask((char *) 0, "abbrev: %s phrase: ", abbrev));
	define(table, abbrev, phrase);
}

static struct abbrev *
lookup(table, abbrev)
register struct abbrev	*table[HASHSIZE];
register char	*abbrev;
{
	register struct abbrev	*ap;
	unsigned int	h;

	h = hash(abbrev);
	for (ap = table[h % HASHSIZE]; ap; ap = ap->a_next)
		if (ap->a_hash == h && strcmp(ap->a_abbrev, abbrev) == 0)
			break;
	return ap;
}

static
define(table, abbrev, phrase)
register struct abbrev	*table[HASHSIZE];
char	*abbrev,
	*phrase;
{
	register struct abbrev	*ap;

	ap = lookup(table, abbrev);
	if (ap == 0) {
		register unsigned int	h = hash(abbrev);

		ap = (struct abbrev *) emalloc(sizeof *ap);
		ap->a_hash = h;
		ap->a_abbrev = copystr(abbrev);
		h %= HASHSIZE;
		ap->a_next = table[h];
		ap->a_cmdhook = 0;
		table[h] = ap;
	} else
		free(ap->a_phrase);
	ap->a_phrase = copystr(phrase);
}

AbbrevExpand()
{
	Bufpos	point;
	char	wordbuf[100];
	register char	*wp = wordbuf,
			*cp;
	register int	c;
	int	UC_count = 0;
	struct abbrev	*ap;

	DOTsave(&point);
	exp = 1;
    WITH_TABLE(curbuf->b_major)
	BackWord();
	while (curchar < point.p_char && ismword(c = linebuf[curchar])) {
		if (AutoCaseAbbrev) {
			if (isupper(c)) {
				UC_count++;
				c = tolower(c);
			}
		}

		*wp++ = c;
		curchar++;
	}
	*wp = '\0';
    END_TABLE();

	if ((ap = lookup(A_tables[curbuf->b_major], wordbuf)) == 0 &&
	    (ap = lookup(A_tables[GLOBAL], wordbuf)) == 0) {
		SetDot(&point);
		return;
	}
	DoTimes(DelPChar(), (wp - wordbuf));

	for (cp = ap->a_phrase; c = *cp; ) {
		if (AutoCaseAbbrev) {
			Insert(islower(c) && UC_count &&
			       (cp == ap->a_phrase || (UC_count > 1 && (*(cp - 1) == ' '))) ?
				toupper(c) : c);
		}
		else {
			Insert(c);
		}
		cp++;
	}

	if (ap->a_cmdhook != 0)
		ExecCmd(ap->a_cmdhook);
}

static char	*mode_names[NMAJORS + 1] = {
	"Fundamental",
	"Text Mode",
	"C Mode",
#ifdef LISP
	"Lisp Mode",
#endif
	"Global"
};

static
save_abbrevs(file)
char	*file;
{
	File	*fp;
	struct abbrev	*ap,
			**tp;
	char	buf[LBSIZE];
	int	i,
		count = 0;

	fp = open_file(file, buf, F_WRITE, COMPLAIN, QUIET);
	for (i = 0; i <= GLOBAL; i++) {
		fprintf(fp, "------%s abbrevs------\n", mode_names[i]);
		for (tp = A_tables[i]; tp < &A_tables[i][HASHSIZE]; tp++)
			for (ap = *tp; ap; ap = ap->a_next) {
				fprintf(fp, "%s:%s\n",
					ap->a_abbrev,
					ap->a_phrase);
				count++;
			}
	}
	f_close(fp);
	add_mess(" %d written.", count);
}

static
rest_abbrevs(file)
char	*file;
{
	int	eof = 0,
		mode = -1,	/* Will be ++'d immediately */
		lnum = 0;
	char	*phrase_p;
	File	*fp;
	char	buf[LBSIZE];

	fp = open_file(file, buf, F_READ, COMPLAIN, QUIET);
	while (mode <= GLOBAL) {
		eof = f_gets(fp, genbuf, LBSIZE);
		if (eof || genbuf[0] == '\0')
			break;
		lnum++;
		if (strncmp(genbuf, "------", 6) == 0) {
			mode++;
			continue;
		}
		if (mode == -1)
fmterr:			complain("Abbrev. format error, line %d.", file, lnum);
		phrase_p = index(genbuf, ':');
		if (phrase_p == 0)
			goto fmterr;
		*phrase_p++ = '\0';	/* Null terminate the abbrev. */
		define(A_tables[mode], genbuf, phrase_p);
	}
	f_close(fp);
	message(NullStr);
}

DefGAbbrev()
{
	def_abbrev(A_tables[GLOBAL]);
}

DefMAbbrev()
{
	def_abbrev(A_tables[curbuf->b_major]);
}

SaveAbbrevs()
{
	char	filebuf[FILESIZE];

	save_abbrevs(ask_file((char *) 0, filebuf));
}

RestAbbrevs()
{
	char	filebuf[FILESIZE];

	rest_abbrevs(ask_file((char *) 0, filebuf));
}

EditAbbrevs()
{
	char	*tname = "jove_wam.$$$",
		*EditName = "Abbreviation Edit";
	Buffer	*obuf = curbuf,
		*ebuf;

	if (ebuf = buf_exists(EditName)) {
		if (ebuf->b_type != B_SCRATCH)
			confirm("Over-write buffer %b?", ebuf);
	}
	SetBuf(ebuf = do_select(curwind, EditName));
	ebuf->b_type = B_SCRATCH;
	initlist(ebuf);
	/* Empty buffer.  Save the definitions to a tmp file
	   and read them into this buffer so we can edit them. */
	save_abbrevs(tname);
	read_file(tname, NO);
	message("[Edit definitions and then type C-X C-C]");
	Recur();		/* We edit them ... now */
	/* RESetBuf in case we deleted the buffer while we were editing. */
	SetBuf(ebuf = do_select(curwind, EditName));
	if (IsModified(ebuf)) {
		SetBuf(ebuf);
		file_write(tname, 0);
		rest_abbrevs(tname);
		unmodify();
	}
	(void) unlink(tname);
	SetBuf(do_select(curwind, obuf->b_name));
}

BindMtoW()
{
	struct abbrev	*ap;
	char	*word;
	data_obj	*hook;

	word = ask((char *) 0, "Word: ");

	if ((ap = lookup(A_tables[curbuf->b_major], word)) == 0 &&
	    (ap = lookup(A_tables[GLOBAL], word)) == 0)
	    	complain("%s: unknown abbrev.", word);

	hook = findmac("Macro: ");
	if (hook == 0)
		complain("[Undefined macro]");
	ap->a_cmdhook = hook;
}

#endif ABBREV
