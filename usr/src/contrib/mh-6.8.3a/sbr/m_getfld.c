/* m_getfld.c - read/parse a message */
#ifndef	lint
static char ident[] = "@(#)$Id: m_getfld.c,v 1.15 1993/02/26 21:57:14 jromine Exp $";
#endif /* lint */

#include "../h/mh.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <ctype.h>


/* This module has a long and checkered history.  First, it didn't burst
   maildrops correctly because it considered two CTRL-A:s in a row to be
   an inter-message delimiter.  It really is four CTRL-A:s followed by a
   newline.  Unfortunately, MMDF will convert this delimiter *inside* a
   message to a CTRL-B followed by three CTRL-A:s and a newline.  This
   caused the old version of m_getfld() to declare eom prematurely.  The
   fix was a lot slower than

		c == '\001' && peekc (iob) == '\001'

   but it worked, and to increase generality, UUCP style maildrops could
   be parsed as well.  Unfortunately the speed issue finally caught up with
   us since this routine is at the very heart of MH.

   To speed things up considerably, the routine Eom() was made an auxilary
   function called by the macro eom().  Unless we are bursting a maildrop,
   the eom() macro returns FALSE saying we aren't at the end of the
   message.

   The next thing to do is to read the mtstailor file and initialize
   delimiter[] and delimlen accordingly...

   After mhl was made a built-in in msh, m_getfld() worked just fine
   (using m_unknown() at startup).  Until one day: a message which was
   the result of a bursting was shown. Then, since the burst boundaries
   aren't CTRL-A:s, m_getfld() would blinding plunge on past the boundary.
   Very sad.  The solution: introduce m_eomsbr().  This hook gets called
   after the end of each line (since testing for eom involves an fseek()).
   This worked fine, until one day: a message with no body portion arrived.
   Then the

		   while (eom (c = Getc (iob), iob))
			continue;

   loop caused m_getfld() to return FMTERR.  So, that logic was changed to
   check for (*eom_action) and act accordingly.

   This worked fine, until one day: someone didn't use four CTRL:A's as
   their delimiters.  So, the bullet got bit and we read mts.h and
   continue to struggle on.  It's not that bad though, since the only time
   the code gets executed is when inc (or msh) calls it, and both of these
   have already called mts_init().

   ------------------------
   (Written by Van Jacobson for the mh6 m_getfld, January, 1986):

   This routine was accounting for 60% of the cpu time used by most mh
   programs.  I spent a bit of time tuning and it now accounts for <10%
   of the time used.  Like any heavily tuned routine, it's a bit
   complex and you want to be sure you understand everything that it's
   doing before you start hacking on it.  Let me try to emphasize
   that:  every line in this atrocity depends on every other line,
   sometimes in subtle ways.  You should understand it all, in detail,
   before trying to change any part.  If you do change it, test the
   result thoroughly (I use a hand-constructed test file that exercises
   all the ways a header name, header body, header continuation,
   header-body separator, body line and body eom can align themselves
   with respect to a buffer boundary).  "Minor" bugs in this routine
   result in garbaged or lost mail.

   If you hack on this and slow it down, I, my children and my
   children's children will curse you.

   This routine gets used on three different types of files: normal,
   single msg files, "packed" unix or mmdf mailboxs (when used by inc)
   and packed, directoried bulletin board files (when used by msh).
   The biggest impact of different file types is in "eom" testing.  The
   code has been carefully organized to test for eom at appropriate
   times and at no other times (since the check is quite expensive).
   I have tried to arrange things so that the eom check need only be
   done on entry to this routine.  Since an eom can only occur after a
   newline, this is easy to manage for header fields.  For the msg
   body, we try to efficiently search the input buffer to see if
   contains the eom delimiter.  If it does, we take up to the
   delimiter, otherwise we take everything in the buffer.  (The change
   to the body eom/copy processing produced the most noticeable
   performance difference, particularly for "inc" and "show".)

   There are three qualitatively different things this routine busts
   out of a message: field names, field text and msg bodies.  Field
   names are typically short (~8 char) and the loop that extracts them
   might terminate on a colon, newline or max width.  I considered
   using a Vax "scanc" to locate the end of the field followed by a
   "bcopy" but the routine call overhead on a Vax is too large for this
   to work on short names.  If Berkeley ever makes "inline" part of the
   C optimiser (so things like "scanc" turn into inline instructions) a
   change here would be worthwhile.

   Field text is typically 60 - 100 characters so there's (barely)
   a win in doing a routine call to something that does a "locc"
   followed by a "bmove".  About 30% of the fields have continuations
   (usually the 822 "received:" lines) and each continuation generates
   another routine call.  "Inline" would be a big win here, as well.

   Messages, as of this writing, seem to come in two flavors: small
   (~1K) and long (>2K).  Most messages have 400 - 600 bytes of headers
   so message bodies average at least a few hundred characters.
   Assuming your system uses reasonably sized stdio buffers (1K or
   more), this routine should be able to remove the body in large
   (>500 byte) chunks.  The makes the cost of a call to "bcopy"
   small but there is a premium on checking for the eom in packed
   maildrops.  The eom pattern is always a simple string so we can
   construct an efficient pattern matcher for it (e.g., a Vax "matchc"
   instruction).  Some thought went into recognizing the start of
   an eom that has been split across two buffers.

   This routine wants to deal with large chunks of data so, rather
   than "getc" into a local buffer, it uses stdio's buffer.  If
   you try to use it on a non-buffered file, you'll get what you
   deserve.  This routine "knows" that struct FILEs have a _ptr
   and a _cnt to describe the current state of the buffer and
   it knows that _filbuf ignores the _ptr & _cnt and simply fills
   the buffer.  If stdio on your system doesn't work this way, you
   may have to make small changes in this routine.
   
   This routine also "knows" that an EOF indication on a stream is
   "sticky" (i.e., you will keep getting EOF until you reposition the
   stream).  If your system doesn't work this way it is broken and you
   should complain to the vendor.  As a consequence of the sticky
   EOF, this routine will never return any kind of EOF status when
   there is data in "name" or "buf").
  */


#define Getc(iob)	getc(iob)
#define eom(c,iob)	(msg_style != MS_DEFAULT && \
			 (((c) == *msg_delim && m_Eom(c,iob)) ||\
			  (eom_action && (*eom_action)(c))))

static unsigned char *matchc();
static unsigned char *locc();

static unsigned char **pat_map;

extern int msg_count;	/* defined in sbr/m_msgdef.c = 0
			 * disgusting hack for "inc" so it can 
			 * know how many characters were stuffed
			 * in the buffer on the last call (see
			 * comments in uip/scansbr.c) */

extern int msg_style;	/* defined in sbr/m_msgdef.c = MS_DEFAULT */
/*
 * The "full" delimiter string for a packed maildrop consists
 * of a newline followed by the actual delimiter.  E.g., the
 * full string for a Unix maildrop would be: "\n\nFrom ".
 * "Fdelim" points to the start of the full string and is used
 * in the BODY case of the main routine to search the buffer for
 * a possible eom.  Msg_delim points to the first character of
 * the actual delim. string (i.e., fdelim+1).  Edelim
 * points to the 2nd character of actual delimiter string.  It
 * is used in m_Eom because the first character of the string
 * has been read and matched before m_Eom is called.
 */
extern char *msg_delim;	/*  defined in sbr/m_msgdef.c = "" */
static unsigned char *fdelim;
static unsigned char *delimend;
static int  fdelimlen;
static unsigned char *edelim;
static int  edelimlen;

static int  (*eom_action) () = NULL;

#ifdef _FSTDIO
#define	_ptr	_p		/* Gag */
#define	_cnt	_r		/* Retch */
#define	_filbuf	__srget		/* Puke */
#endif

/*  */

m_getfld (state, name, buf, bufsz, iob)
int		state;
int		bufsz;
unsigned char	*name,
		*buf;
register FILE	*iob;
{
    register unsigned char  *cp;
    register unsigned char  *bp;
    register unsigned char  *ep;
    register unsigned char  *sp;
    register int    cnt;
    register int    c;
    register int    i;
    register int    j;

    if ((c = Getc(iob)) < 0) {
	msg_count = 0;
	*buf = 0;
	return FILEEOF;
    }
    if (eom (c, iob)) {
	if (! eom_action) {
	    /* flush null messages */
	    while ((c = Getc(iob)) >= 0 && eom (c, iob))
		;
	    if (c >= 0)
		(void) ungetc(c, iob);
	}
	msg_count = 0;
	*buf = 0;
	return FILEEOF;
    }

    switch (state) {
	case FLDEOF: 
	case BODYEOF: 
	case FLD: 
	    if (c == '\n' || c == '-') {
		/* we hit the header/body separator */
		while (c != '\n' && (c = Getc(iob)) >= 0)
		    ;

		if (c < 0 || (c = Getc(iob)) < 0 || eom (c, iob)) {
		    if (! eom_action) {
			/* flush null messages */
			while ((c = Getc(iob)) >= 0 && eom (c, iob))
			    ;
			if (c >= 0)
			    (void) ungetc(c, iob);
		    }
		    msg_count = 0;
		    *buf = 0;
		    return FILEEOF;
		}
		state = BODY;
		goto body;
	    }
	    /*
	     * get the name of this component.  take characters up
	     * to a ':', a newline or NAMESZ-1 characters, whichever
	     * comes first.  
	     */
	    cp = name; i = NAMESZ - 1;
	    for (;;) {
		bp = sp = (unsigned char *) iob->_ptr - 1;
		j = (cnt = iob->_cnt+1) < i ? cnt : i;
		while ((c = *bp++) != ':' && c != '\n' && --j >= 0)
		    *cp++ = c;

		j = bp - sp;
		if ((cnt -= j) <= 0) {
		    if (_filbuf(iob) == EOF) {
			*cp = *buf = 0;
			advise (NULLCP, "eof encountered in field \"%s\"",
				name);
			return FMTERR;
		    }
		} else {
		    iob->_ptr = bp + 1;
		    iob->_cnt = cnt - 1;
		}
		if (c == ':')
		    break;

		/*
		 * something went wrong.  possibilities are:
		 *  . hit a newline (error)
		 *  . got more than namesz chars. (error)
		 *  . hit the end of the buffer. (loop)
		 */
		if (c == '\n') {
		    *cp = *buf = 0;
		    advise (NULLCP, "eol encountered in field \"%s\"", name);
		    state = FMTERR;
		    goto finish;
		}
		if ((i -= j) <= 0) {
		    *cp = *buf = 0;
		    advise (NULLCP, "field name \"%s\" exceeds %d bytes",
			    name, NAMESZ - 1);
		    state = LENERR;
		    goto finish;
		}
	    }

	    while (isspace (*--cp) && cp >= name)
		;
	    *++cp = 0;
	    /* fall through */

	case FLDPLUS: 
	    /*
	     * get (more of) the text of a field.  take
	     * characters up to the end of this field (newline
	     * followed by non-blank) or bufsz-1 characters.
	     */
	    cp = buf; i = bufsz-1;
	    for (;;) {
		cnt = iob->_cnt++; bp = (unsigned char *) --iob->_ptr;
		c = cnt < i ? cnt : i;
		while (ep = locc( c, bp, '\n' )) {
		    /*
		     * if we hit the end of this field, return.
		     */
		    if ((j = *++ep) != ' ' && j != '\t') {
			j = ep - (unsigned char *) iob->_ptr;
			(void) bcopy( iob->_ptr, cp, j);
			iob->_ptr = ep; iob->_cnt -= j;
			cp += j;
			state = FLD;
			goto finish;
		    }
		    c -= ep - bp; bp = ep;
		}
		/*
		 * end of input or dest buffer - copy what we've found.
		 */
		c += bp - (unsigned char *) iob->_ptr;
		(void) bcopy( iob->_ptr, cp, c);
		i -= c; cp += c;
		if (i <= 0) {
		    /* the dest buffer is full */
		    iob->_cnt -= c; iob->_ptr += c;
		    state = FLDPLUS;
		    break;
		}
		/* 
		 * There's one character left in the input buffer.
		 * Copy it & fill the buffer.  If the last char
		 * was a newline and the next char is not whitespace,
		 * this is the end of the field.  Otherwise loop.
		 */
		--i;
		*cp++ = j = *(iob->_ptr + c);
		c = _filbuf(iob);
		if ((j == '\0' || j == '\n') && c != ' ' && c != '\t') {
		    if (c != EOF)
			--iob->_ptr, ++iob->_cnt;
		    state = FLD;
		    break;
		}
	    }
	    break;

	case BODY: 
	body:
	    /*
	     * get the message body up to bufsz characters or the
	     * end of the message.  Sleazy hack: if bufsz is negative
	     * we assume that we were called to copy directly into
	     * the output buffer and we don't add an eos.
	     */
	    i = (bufsz < 0) ? -bufsz : bufsz-1;
	    bp = (unsigned char *) --iob->_ptr; cnt = ++iob->_cnt;
	    c = (cnt < i ? cnt : i);
	    if (msg_style != MS_DEFAULT && c > 1) {
		/*
		 * packed maildrop - only take up to the (possible)
		 * start of the next message.  This "matchc" should
		 * probably be a Boyer-Moore matcher for non-vaxen,
		 * particularly since we have the alignment table
		 * all built for the end-of-buffer test (next).
		 * But our vax timings indicate that the "matchc"
		 * instruction is 50% faster than a carefully coded
		 * B.M. matcher for most strings.  (So much for elegant
		 * algorithms vs. brute force.)  Since I (currently)
		 * run MH on a vax, we use the matchc instruction. --vj
		 */
		if (ep = matchc( fdelimlen, fdelim, c, bp ) )
		    c = ep - bp + 1;
		else {
		    /*
		     * There's no delim in the buffer but there may be
		     * a partial one at the end.  If so, we want to leave
		     * it so the "eom" check on the next call picks it up.
		     * Use a modified Boyer-Moore matcher to make this
		     * check relatively cheap.  The first "if" figures
		     * out what position in the pattern matches the last
		     * character in the buffer.  The inner "while" matches
		     * the pattern against the buffer, backwards starting
		     * at that position.  Note that unless the buffer
		     * ends with one of the characters in the pattern
		     * (excluding the first and last), we do only one test.
		     */
		    ep = bp + c - 1;
		    if (sp = pat_map[*ep]) {
			do {
			    cp = sp;
			    while (*--ep == *--cp)
			    ;
			    if (cp < fdelim) {
				if (ep >= bp)
				    /*
				     * ep < bp means that all the buffer
				     * contains is a prefix of delim.
				     * If this prefix is really a delim, the
				     * m_eom call at entry should have found
				     * it.  Thus it's not a delim and we can
				     * take all of it.
				     */
				    c = (ep - bp) + 2;
			    break;
			}
			    /* try matching one less char of delim string */
			    ep = bp + c - 1;
			} while (--sp > fdelim);
		    }
		}
	    }
	    (void) bcopy( bp, buf, c );
	    iob->_cnt -= c;
	    iob->_ptr += c;
	    if (bufsz < 0) {
		msg_count = c;
		return (state);
	    }
	    cp = buf + c;
	    break;

	default: 
	    adios (NULLCP, "m_getfld() called with bogus state of %d", state);
    }
finish:;
    *cp = 0;
    msg_count = cp - buf;
    return (state);
}

/*  */

#ifdef	RPATHS
static char  unixbuf[BUFSIZ] = "";
#endif /* RPATHS */

void
m_unknown(iob)
	register FILE *iob;
{
    register	int c;
    register	long pos;
    char	text[10];
    register    char *cp;
    register	char *delimstr;

    msg_style = MS_UNKNOWN;

    /* Figure out what the message delimitter string is for this
     * maildrop.  (This used to be part of m_Eom but I didn't like
     * the idea of an "if" statement that could only succeed on the
     * first call to m_Eom getting executed on each call, i.e., at
     * every newline in the message).
     *
     * If the first line of the maildrop is a Unix "from" line, we say the
     * style is UUCP and eat the rest of the line.  Otherwise we say the style
     * is MMDF & look for the delimiter string specified when MH was built
     * (or from the mtstailor file).
     */
    pos = ftell (iob);
    if (fread (text, sizeof *text, 5, iob) == 5
	    && strncmp (text, "From ", 5) == 0) {
	msg_style = MS_UUCP;
	delimstr = "\nFrom ";
#ifndef	RPATHS
	while ((c = getc (iob)) != '\n' && c >= 0)
	    ;
#else /* RPATHS */
	cp = unixbuf;
	while ((c = getc (iob)) != '\n')
	    *cp++ = c;
	*cp = 0;
#endif /* RPATHS */
    } else {
	/* not a Unix style maildrop */
	(void) fseek (iob, pos, 0);
	if (mmdlm2 == NULLCP || *mmdlm2 == 0)
	    mmdlm2 = "\001\001\001\001\n";
	delimstr = mmdlm2;
	msg_style = MS_MMDF;
    }
    c = strlen (delimstr);
    fdelim = (unsigned char *)malloc((unsigned)c + 3);
    *fdelim++ = '\0';
    *fdelim = '\n';
    msg_delim = (char *)fdelim+1;
    edelim = (unsigned char *)msg_delim+1;
    fdelimlen = c + 1;
    edelimlen = c - 1;
    (void)strcpy(msg_delim, delimstr);
    delimend = (unsigned char *)msg_delim + edelimlen;
    if (edelimlen <= 1)
	adios (NULLCP, "maildrop delimiter must be at least 2 bytes");
    /*
     * build a Boyer-Moore end-position map for the matcher in m_getfld.
     * N.B. - we don't match just the first char (since it's the newline
     * separator) or the last char (since the matchc would have found it
     * if it was a real delim).
     */
    pat_map = (unsigned char **) calloc (256, sizeof (unsigned char *));

    for (cp = (char *)fdelim + 1; cp < (char *)delimend; cp++ )
	pat_map[*cp] = (unsigned char *)cp;

    if (msg_style == MS_MMDF) {
	/* flush extra msg hdrs */
	while ((c = Getc(iob)) >= 0 && eom (c, iob))
	    ;
	if (c >= 0)
	    (void) ungetc(c, iob);
    }
}


void m_eomsbr (action)
int     (*action) ();
{
    if (eom_action = action) {
	msg_style = MS_MSH;
	*msg_delim = 0;
	fdelimlen = 1;
	delimend = fdelim;
    } else {
	msg_style = MS_MMDF;
	msg_delim = (char *)fdelim + 1;
	fdelimlen = strlen((char *)fdelim);
	delimend = (unsigned char *)(msg_delim + edelimlen);
    }
}

/*  */

/* test for msg delimiter string */

int  m_Eom (c, iob)
register int     c;
register FILE   *iob;
{
    register long pos = 0L;
    register int i;
    char    text[10];
#ifdef	RPATHS
    register    char *cp;
#endif /* RPATHS */

    pos = ftell (iob);
    if ((i = fread (text, sizeof *text, edelimlen, iob)) != edelimlen
	    || strncmp (text, (char *)edelim, edelimlen)) {
	if (i == 0 && msg_style == MS_UUCP)
	    /* the final newline in the (brain damaged) unix-format
	     * maildrop is part of the delimitter - delete it.
	     */
	    return 1;

#ifdef	notdef
	(void) fseek (iob, pos, 0);
#else
	(void) fseek (iob, (long)(pos-1), 0);
	(void) getc (iob);		/* should be OK */
#endif /* !notdef */
	return 0;
    }

    if (msg_style == MS_UUCP) {
#ifndef	RPATHS
	while ((c = getc (iob)) != '\n')
	    if (c < 0)
		break;
#else /* RPATHS */
	cp = unixbuf;
	while ((c = getc (iob)) != '\n' && c >= 0)
	    *cp++ = c;
	*cp = 0;
#endif /* RPATHS */
    }

    return 1;
}

/*  */

#ifdef	RPATHS
char   *unixline () {
    register char  *cp,
                   *dp,
                   *pp;
    static char unixfrom[BUFSIZ];

    pp = unixfrom;
    if (cp = dp = index (unixbuf, ' ')) {
	while (cp = index (cp + 1, 'r'))
	    if (strncmp (cp, "remote from ", 12) == 0) {
		*cp = 0;
		(void) sprintf (pp, "%s!", cp + 12);
		pp += strlen (pp);
		break;
	    }
	if (cp == NULL)
	    cp = unixbuf + strlen (unixbuf);
	if ((cp -= 25) >= dp)
	    *cp = 0;
    }

    (void) sprintf (pp, "%s\n", unixbuf);
    unixbuf[0] = 0;
    return unixfrom;
}
#endif /* RPATHS */

/*  */

#if (vax && !lint)
	asm(".align 1");
	asm("_matchc: .word 0");
	asm("	movq 4(ap),r0");
	asm("	movq 12(ap),r2");
	asm("	matchc  r0,(r1),r2,(r3)");
	asm("	beql 1f");
	asm("	movl 4(ap),r3");
	asm("1:	subl3  4(ap),r3,r0");
	asm("	ret");
#else
static unsigned char *
matchc( patln, pat, strln, str )
	int patln;
	char *pat;
	int strln;
	register char *str;
{
	register char *es = str + strln - patln;
	register char *sp;
	register char *pp;
	register char *ep = pat + patln;
	register char pc = *pat++;

	for(;;) {
		while (pc != *str++)
			if (str > es)
				return 0;

		sp = str; pp = pat;
		while (pp < ep && *sp++ == *pp)
			pp++;
		if (pp >= ep) 
			return ((unsigned char *)--str);
	}
}
#endif

/*  */

/*
 * Locate character "term" in the next "cnt" characters of "src".
 * If found, return its address, otherwise return 0.
 */
#if (vax && !lint)
	asm(".align 1");
	asm("_locc: .word 0");
	asm("	movq  4(ap),r0");
	asm("	locc  12(ap),r0,(r1)");
	asm("	beql  1f");
	asm("	movl  r1,r0");
	asm("1:	ret");
#else
static unsigned char *
locc( cnt, src, term )
	register int  cnt;
	register unsigned char *src;
	register unsigned char term;
{
    while (*src++ != term && --cnt > 0);

    return (cnt > 0 ? --src : (unsigned char *)0);
}
#endif

/*  */

#if	!defined (BSD42) && !defined (bcopy)
int	bcmp (b1, b2, length)
register char *b1,
	      *b2;
register int   length;
{
    while (length-- > 0)
	if (*b1++ != *b2++)
	    return 1;

    return 0;
}


bcopy (b1, b2, length)
register char *b1,
	      *b2;
register int   length;
{
    while (length-- > 0)
	*b2++ = *b1++;
}


bzero (b, length)
register char *b;
register int   length;
{
    while (length-- > 0)
	*b++ = 0;
}
#endif /* not BSD42 */
