/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)assign.c	1.1 */

/*
 *   ASSIGN.C
 *
 *   Programmer:  D. G. Korn
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   ASSIGN (NODE, STRING)
 *
 *        Assign STRING to NODE.
 *
 *   FASSIGN (NODE, STRING)
 *
 *        Assign STRING to NODE even if readonly.
 *
 *
 *
 *   See Also:  asscadr(III), asslong(III), unassign(III), valup(III)
 */

#include	"name.h"
#include        "flags.h"
#ifdef MULTIBYTE
#include        "national.h"
#endif /* MULTIBYTE */

void	assign();
void	fassign();

extern char *malloc();
extern char *strcpy();
#ifdef BSD
#define strchr index
#endif	/* BSD */
extern char	*strchr();
extern void	utol(),ltou();
extern char	*itos();
extern void	free();
extern void	rjust();
extern void	failed();
#ifdef NAME_SCOPE
extern struct Namnod *copy_nod();
#endif	/* NAME_SCOPE */
union Namval *aget_up();

#ifdef MULTIBYTE
static unsigned char *savep;
static unsigned char savechars[ESS_MAXCHAR+1];
static int ja_size();
#else
#define size	 np->namsz
#endif /* MULTIBYTE */

/*
 *   ASSIGN (NODE, STRING)
 *
 *        struct Namnod *NODE;
 *     
 *        char *STRING;
 *
 *   Assign the string given by STRING to the Namnod given by
 *   NODE.  STRING is converted according to the namflg field
 *   of NODE before assignment.  
 *
 *   If NODE is an array, then the element given by the
 *   current index is assigned to.
 *   
 *   Any freeable space associated with the old value of NODE
 *   is released.
 *
 *   If the copy on write,C_WRITE flag is set then the assignment
 *   is made on a copy of the node created on the last shell tree.
 * 
 */

static char forced = 0;

void fassign(node,string)
struct Namnod *node;
char *string;
{
	forced++;
	assign(node,string);
	forced = 0;
#ifdef apollo
	if(attest(node,N_EXPORT))
	{
		extern char *valup();
		short namlen, vallen;
		char *vp = valup(node);
		namlen =strlen(node->namid);
		vallen = strlen(vp);
		ev_$set_var(node->namid,&namlen,vp,&vallen);
	}
#endif /* apollo */
}

void assign(node,string)
struct Namnod *node;
char *string;
{
	register char *sp=string;
	register struct Namnod *np=node;
	register union Namval *up;
	register char *cp;
#ifdef MULTIBYTE
	register int size;
#endif /* MULTIBYTE */
	register int dot = 0;
#ifdef apollo
	/* reserve space for UNIX to host file name translation */
	char pathname[256];
	short pathlen;
#endif	/* apollo */
#ifdef NAME_SCOPE
	if (attest (np,C_WRITE))
		np = copy_nod(np,1);
#endif	/* NAME_SCOPE */
	up= &np->value.namval;
	if (forced==0 && attest (np, N_RDONLY))
		failed(np->namid,wtfailed);
	if (attest (np, ARRAY))
		up = aget_up(np,up);
	if (attest (np, IN_DIR))
		up = up->up;
	if (attest (np, INT_GER))
	{
		long l, aeval();
		if (attest (np, CPOIN_TER))
		{
			up->cp = sp;
			return;
		}
		l = (sp? aeval(sp) : (lastbase=10,0));
		if(np->namsz == 0)
			np->namsz = lastbase;
		if (attest (np, BLT_NOD))
		{
			(*up->fp->f_ap)(l);
			return;
		}
		if(up->lp==0)
			up->lp = (long*)malloc((unsigned)sizeof(long));
		*(up->lp) = l;
		if(l && *sp++ == '0')
			np->value.namflg |= UN_SIGN;
		return;
	}
	if(attest (np,(N_IMPORT|N_EXPORT))==(N_IMPORT|N_EXPORT))
	{
		/* get rid of imported value */
		char *cp = strchr(np->namid,'=');
		if(cp)
			*cp = 0;
		pattrib(np,~N_IMPORT);
	}
#ifdef apollo
	if (attest (np, A_FLAG) && sp)
	{
		/* this routine returns the host file name given the UNIX name */
		/* other non-unix hosts that use file name mapping should change this */
		unix_fio_$get_name(sp,pathname,&pathlen);
		pathname[pathlen] = 0;
		sp = pathname;
	}
#endif	/* apollo */
	if ((attest (np, R_JUST|Z_FILL|L_JUST)) && sp)
	{
		for(;*sp == ' '|| *sp=='\t';sp++);
        	if ((attest (np, Z_FILL)) && (attest (np, L_JUST)))
			for(;*sp=='0';sp++);
#ifdef MULTIBYTE
		if(size = np->namsz)
			size = ja_size((unsigned char*)sp,size,attest(np,R_JUST|Z_FILL));
#endif /* MULTIBYTE */
	}
	if ((!attest (np, N_FREE|N_ALLOC)) && (up->cp != NULL))
		free(up->cp);
	if (attest (np, N_ALLOC))
		cp = up->cp;
	else
	{
        	np->value.namflg &= ~N_FREE;
        	if (sp)
			 cp = malloc(((unsigned)((dot=strlen(sp))>size?dot:size)+1));
		else
			cp = NULL;
		up->cp = cp;
	}
	if (!sp)
		return;
	if (attest (np, L_TO_U))
		ltou(sp,cp);
	else if (attest (np, U_TO_L))
		utol(sp,cp);
	else
        	strcpy (cp, sp);
	if (attest (np, R_JUST) && attest (np, Z_FILL))
		rjust(cp,size,'0');
	else if (attest (np, R_JUST))
		rjust(cp,size,' ');
	else if (attest (np, L_JUST))
        {
         	sp = strlen (cp) + cp;
		*(cp = (cp + size)) = 0;
		for (; sp < cp; *sp++ = ' ');
         }
#ifdef MULTIBYTE
	/* restore original string */
	if(savep)
		ja_restore();
#endif /* MULTIBYTE */
	return;
}


/*
 * Get the Namval pointer for an array.
 * Allocate the space if necessary
 */

union Namval *aget_up(np,up)
struct Namnod *np;
register union Namval *up;
{
	register int dot;
	register struct Nodval *nv;
	dot = up->aray->adot;
	if (dot > arsize (abound (np)))
   		failed (itos(dot), subscript);
	if ((nv = up->aray->val[dot]) == NULL)
	{
		nv = (struct Nodval*)malloc ((unsigned)sizeof (struct Nodval));
		nv->namflg = np->value.namflg & ~ARRAY;
		nv->namval.cp = NULL;
		up->aray->val[dot] = nv;
	}
	return(&(unmark (nv)->namval));
}

#ifdef MULTIBYTE
/*
 * handle left and right justified fields for multi-byte chars
 * given physical size, return a logical size which reflects the
 * screen width of multi-byte characters
 * Multi-width characters replaced by spaces if they cross the boundary
 * <type> is non-zero for right justified  fields
 */

static int ja_size(str,size,type)
unsigned char *str;
int size;
{
	register unsigned char *cp = str;
	register int c;
	register int n = size;
	int oldn;
	while(c = *cp++)
	{
		oldn = n;
		/* find character set number */
		c = echarset(c);
		/* allow room for excess input bytes */
		if(c)
		{
			n += (in_csize(c)-out_csize(c)+(c>=2));
			cp += (in_csize(c)-(c==1));
		}
		size -= out_csize(c);
		if(size<=0 && type==0)
			break;
	}
	/* check for right justified fields that need truncating */
	if(size <0)
	{
		if(type==0)
		{
			/* left justified and character crosses field boundary */
			n = oldn;
			/* save boundary char and replace with spaces */
			size = in_csize(c)+(c>2);
			savechars[size] = 0;
			while(size--)
			{
				savechars[size] = *--cp;
				*cp = ' ';
			}
			savep = cp;
		}
		size = -size;
		if(type)
			n -= (ja_size(str,size,0)-size);
	}
	return(n);
}

int ja_restore()
{
	register unsigned char *cp = savechars;
	while(*cp)
		*savep++ = *cp++;
	savep = 0;
}
#endif /* MULTIBYTE */
