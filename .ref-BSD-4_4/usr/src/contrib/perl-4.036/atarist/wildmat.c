/*  $Revision: 4.0.1.1 $
**
**  Do shell-style pattern matching for ?, \, [], and * characters.
**  Might not be robust in face of malformed patterns; e.g., "foo[a-"
**  could cause a segmentation violation.  It is 8bit clean.
**
**  Written by Rich $alz, mirror!rs, Wed Nov 26 19:03:17 EST 1986.
**  Rich $alz is now <rsalz@bbn.com>.
**  April, 1991:  Replaced mutually-recursive calls with in-line code
**  for the star character.
**
**  Special thanks to Lars Mathiesen <thorinn@diku.dk> for the ABORT code.
**  This can greatly speed up failing wildcard patterns.  For example:
**	pattern: -*-*-*-*-*-*-12-*-*-*-m-*-*-*
**	text 1:	 -adobe-courier-bold-o-normal--12-120-75-75-m-70-iso8859-1
**	text 2:	 -adobe-courier-bold-o-normal--12-120-75-75-X-70-iso8859-1
**  Text 1 matches with 51 calls, while text 2 fails with 54 calls.  Without
**  the ABORT, then it takes 22310 calls to fail.  Ugh.  The following
**  explanation is from Lars:
**  The precondition that must be fulfilled is that DoMatch will consume
**  at least one character in text.  This is true if *p is neither '*' nor
**  '\0'.)  The last return has ABORT instead of FALSE to avoid quadratic
**  behaviour in cases like pattern "*a*b*c*d" with text "abcxxxxx".  With
**  FALSE, each star-loop has to run to the end of the text; with ABORT
**  only the last one does.
**
**  Once the control of one instance of DoMatch enters the star-loop, that
**  instance will return either TRUE or ABORT, and any calling instance
**  will therefore return immediately after (without calling recursively
**  again).  In effect, only one star-loop is ever active.  It would be
**  possible to modify the code to maintain this context explicitly,
**  eliminating all recursive calls at the cost of some complication and
**  loss of clarity (and the ABORT stuff seems to be unclear enough by
**  itself).  I think it would be unwise to try to get this into a
**  released version unless you have a good test data base to try it out
**  on.
*/

#define TRUE			1
#define FALSE			0
#define ABORT			-1


    /* What character marks an inverted character class? */
#define NEGATE_CLASS		'^'
    /* Is "*" a common pattern? */
#define OPTIMIZE_JUST_STAR
    /* Do tar(1) matching rules, which ignore a trailing slash? */
#undef MATCH_TAR_PATTERN


/*
**  Match text and p, return TRUE, FALSE, or ABORT.
*/
static int
DoMatch(text, p)
    char	*text;
    char	*p;
{
    int	last;
    int	matched;
    int	reverse;

    for ( ; *p; text++, p++) {
	if (*text == '\0' && *p != '*')
	    return ABORT;
	switch (*p) {
	case '\\':
	    /* Literal match with following character. */
	    p++;
	    /* FALLTHROUGH */
	default:
	    if (*text != *p)
		return FALSE;
	    continue;
	case '?':
	    /* Match anything. */
	    continue;
	case '*':
	    while (*++p == '*')
		/* Consecutive stars act just like one. */
		continue;
	    if (*p == '\0')
		/* Trailing star matches everything. */
		return TRUE;
	    while (*text)
		if ((matched = DoMatch(text++, p)) != FALSE)
		    return matched;
	    return ABORT;
	case '[':
	    reverse = p[1] == NEGATE_CLASS ? TRUE : FALSE;
	    if (reverse)
		/* Inverted character class. */
		p++;
	    for (last = 0400, matched = FALSE; *++p && *p != ']'; last = *p)
		/* This next line requires a good C compiler. */
		if (*p == '-' ? *text <= *++p && *text >= last : *text == *p)
		    matched = TRUE;
	    if (matched == reverse)
		return FALSE;
	    continue;
	}
    }

#ifdef	MATCH_TAR_PATTERN
    if (*text == '/')
	return TRUE;
#endif	/* MATCH_TAR_ATTERN */
    return *text == '\0';
}


/*
**  User-level routine.  Returns TRUE or FALSE.
*/
int
wildmat(text, p)
    char	*text;
    char	*p;
{
#ifdef	OPTIMIZE_JUST_STAR
    if (p[0] == '*' && p[1] == '\0')
	return TRUE;
#endif	/* OPTIMIZE_JUST_STAR */
    return DoMatch(text, p) == TRUE;
}

#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#if __STDC__
#ifdef unix
#define _SIZE_T	/* unix defines size_t in sys/types.h */
#endif
#ifndef _COMPILER_H
#  include <compiler.h>
#endif
#include <stddef.h>
#include <stdlib.h>
#else
extern char *malloc(), *realloc();
extern char *rindex(),  *strdup();
#define __PROTO(x) ()
#endif
#include <string.h>

#define MAX_DIR	32	/* max depth of dir recursion */
static struct {
	char *dir, *patt;
} dir_stack[MAX_DIR];
static int stack_p;
static char **matches;
static int nmatches;

static void *ck_memalloc __PROTO((void *));
#define ck_strdup(p) ck_memalloc(strdup(p))
#define ck_malloc(s) ck_memalloc(malloc(s))
#define ck_realloc(p, s) ck_memalloc(realloc(p, s))


#define DEBUGX(x) 

/*
 * return true if patt contains a wildcard char
 */
int contains_wild(patt)
char *patt;
{
    char c;
    char *p;

    /* only check for wilds in the basename part of the pathname only */
    if((p = rindex(patt, '/')) == NULL)
	p = rindex(patt, '\\');
    if(!p)
	p = patt;

    while((c = *p++))
	if((c == '*') || (c == '?') || (c == '['))
	    return 1;
    return 0;
}

#ifndef ZOO
void free_all()
{
    char **p;

    if(!matches)
	return;

    for(p = matches; *p; p++)
	free(*p);
    free(matches);
    matches = NULL;
}
#endif

static void push(dir, patt)
char *dir;
char *patt;
{
    if(stack_p < (MAX_DIR - 2))
	stack_p++;
    else
    {
	fprintf(stderr,"directory stack overflow\n");
	exit(99);
    }
    dir_stack[stack_p].dir = dir;
    dir_stack[stack_p].patt = patt;
}

/*
 * glob patt
 * if decend_dir is true, recursively decend any directories encountered.
 * returns pointer to all matches encountered.
 * if the initial patt is a directory, and decend_dir is true, it is
 * equivalent to specifying the pattern "patt\*"
 *
 * Restrictions:
 *  - handles wildcards only in the base part of a pathname
 *    ie: will not handle \foo\*\bar\ (wildcard in the middle of pathname)
 *
 *  - max dir recursion is MAX_DIR
 *
 *  - on certain failures it will just skip potential matches as if they
 *    were not present.
 *
 *  ++jrb	bammi@cadence.com
 */
static char **do_match __PROTO((int decend_dir));

char **glob(patt, decend_dir)
char *patt;
int decend_dir;
{
    char *dir, *basepatt, *p;
    struct stat s;

    DEBUGX((fprintf(stderr,"glob(%s, %d)\n", patt, decend_dir)));
    matches = NULL;
    nmatches = 0;
    stack_p = -1;

    /* first check for wildcards */
    if(contains_wild(patt))
    {
	/* break it up into dir and base patt, do_matches and return */
	p = ck_strdup(patt);
	if((basepatt = rindex(p, '/')) == NULL)
	    basepatt = rindex(p, '\\');
        if(basepatt)
        {
	    dir = p;
	    *basepatt++ = '\0';
	    basepatt = ck_strdup(basepatt);
        }
        else
        {
	    dir = ck_strdup(".");
	    basepatt = p;
        }

        if(strcmp(basepatt, "*.*") == 0)
        {
            /* the desktop, and other braindead shells strike again */
            basepatt[1] = '\0';
        }
	push(dir, basepatt);
	DEBUGX((fprintf(stderr, "calling %s, %s\n", dir, basepatt)));
	return do_match(decend_dir);
    }

    /* if no wilds, check for dir */
    if(decend_dir && (!stat(patt, &s)))
    {
	if((s.st_mode & S_IFMT) == S_IFDIR)
	{   /* is a dir */
	    size_t len = strlen(patt);
	    
	    dir = ck_strdup(patt);
	    --len;
	    if(len && ((dir[len] == '/') 
#ifdef atarist
	       || (dir[len] == '\\')
#endif
	     ))
		dir[len] = '\0';
	    basepatt = ck_strdup("*");
	    push(dir, basepatt);
	    DEBUGX((fprintf(stderr, "calling %s, %s\n", dir, basepatt)));
	    return do_match(decend_dir);
        }
    }
    return NULL;
}

static char **do_match(decend_dir)
int decend_dir;
{
    DIR *dirp;
    struct dirent *d;
    struct stat s;
    char *dir, *basepatt;

    while(stack_p >= 0)
    {
	dir = ck_strdup(dir_stack[stack_p].dir); 
	free(dir_stack[stack_p].dir);
        basepatt = ck_strdup(dir_stack[stack_p].patt);
	free(dir_stack[stack_p--].patt);
	
        DEBUGX((fprintf(stderr,"dir %s patt %s stack %d\n", dir, basepatt, stack_p)));

    	dirp = opendir(dir);
	if(!dirp)
	{
	    free(dir);
	    DEBUGX((fprintf(stderr,"no dir\n")));
	    continue;
        }
	
        while((d = readdir(dirp)))
        {
	    char *p = ck_malloc(strlen(dir) + strlen(d->d_name) + 2L);
            if(strcmp(dir, "."))
                                     /* If we have a full pathname then */
	    {			     /* let's append the directory info */
		strcpy(p, dir);
#ifndef unix
		strcat(p, "\\");
#else
		strcat(p, "/");
#endif
		strcat(p, d->d_name);
	    }
	    else		      /* Otherwise, the name is just fine, */
		strcpy(p, d->d_name); /* there's no need for './' -- bjsjr */

	    DEBUGX((fprintf(stderr, "Testing %s\n", p)));
	    if(!stat(p, &s))	/* if stat fails, ignore it */
            {
	        if( ((s.st_mode & S_IFMT) == S_IFREG) ||
		    ((s.st_mode & S_IFMT) == S_IFLNK) )
	        {  /* it is a file/symbolic link */
		    if(wildmat(d->d_name, basepatt))
		    {  /* it matches pattern */
			DEBUGX((fprintf(stderr,"File Matched\n")));
			if(matches == NULL)
			    matches = (char **)ck_malloc(sizeof(char *));
			else
			    matches = (char **)
			      ck_realloc(matches, (nmatches+1)*sizeof(char *)); 
			matches[nmatches++] = p;
		    } /* no match */
		    else
		    {
			DEBUGX((fprintf(stderr,"No File Match\n")));
		        free(p);
		    }
		} else if(decend_dir && ((s.st_mode & S_IFMT) == S_IFDIR))
		{
		    if(!((!strcmp(d->d_name,".")) || (!strcmp(d->d_name, "..")
#ifdef atarist
    		         || (!strcmp(d->d_name, ".dir"))
#endif
    		    )))
		    {
			char *push_p = ck_strdup("*");
			push(p, push_p);
			DEBUGX((fprintf(stderr,"Dir pushed\n")));
		    }
		    else
		    {
			DEBUGX((fprintf(stderr, "DIR skipped\n")));
			free(p);
		    }
	        }
		else
		{
		    DEBUGX((fprintf(stderr, "Not a dir/no decend\n")));
		    free(p);
		}
	    } /* stat */
	    else
	    {
		DEBUGX((fprintf(stderr, "Stat failed\n")));
		free(p);
	    }
        } /* while readdir */
        closedir(dirp);
	free(basepatt);
	free(dir);
	DEBUGX((fprintf(stderr, "Dir done\n\n")));
    } /* while dirs in stack */
    
    if(!nmatches)
    {
	DEBUGX((fprintf(stderr, "No matches\n")));
	return NULL;
    }
    
    matches = (char **)realloc(matches, (nmatches+1)*sizeof(char *));
    if(!matches)
    {  return NULL; }
    matches[nmatches] = NULL;
    DEBUGX((fprintf(stderr, "%d matches\n", nmatches)));    
    return matches;
}

#ifdef ZOO
#include "errors.i"
#endif

static void *ck_memalloc(p)
void *p;
{
    if(!p)
    {
#ifndef ZOO
        fprintf(stderr, "Out of memory\n");
	exit(98);
#else
        prterror('f', no_memory);
#endif
    }
    return p;
}

#ifdef TEST_GLOB
void test(path, dec)
char *path;
int dec;
{
    char **m;
    char **matches;

    printf("Testing %s %d\n", path, dec);
    matches = glob(path, dec);
    if(!matches)
    {
	printf("No matches\n");
    }
    else
    {
        for(m = matches; *m; m++)
	    printf("%s\n", *m);
	putchar('\n');
        free_all();
    }
}

int main()
{
#ifndef unix
    test("e:\\lib\\*.olb", 0);
    test("e:\\lib", 0);
    test("e:\\lib\\", 1);
#else
    test("/net/acae127/home/bammi/News/comp.sources.misc/*.c", 0);
    test("/net/acae127/home/bammi/News/comp.sources.misc", 0);
    test("/net/acae127/home/bammi/News/comp.sources.misc", 1);
    test("/net/acae127/home/bammi/atari/cross-gcc", 1);
#endif
    
    return 0;
}

#endif

#ifdef	TEST_WILDMAT
#include <stdio.h>

/* Yes, we use gets not fgets.  Sue me. */
extern char	*gets();


main()
{
    char	pattern[80];
    char	text[80];

    printf("Wildmat tester.  Enter pattern, then strings to test.\n");
    printf("A blank line gets prompts for a new pattern; a blank pattern\n");
    printf("exits the program.\n\n");

    for ( ; ; ) {
	printf("Enter pattern:  ");
	if (gets(pattern) == NULL)
	    break;
	for ( ; ; ) {
	    printf("Enter text:  ");
	    if (gets(text) == NULL)
		exit(0);
	    if (text[0] == '\0')
		/* Blank line; go back and get a new pattern. */
		break;
	    printf("      %s\n", wildmat(text, pattern) ? "YES" : "NO");
	}
    }

    exit(0);
    /* NOTREACHED */
}
#endif	/* TEST_WILDMAT */
