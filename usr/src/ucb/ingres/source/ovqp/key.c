# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<tree.h>
# include	"../decomp/globs.h"
# include	"strategy.h"
# include	<sccs.h>

SCCSID(@(#)key.c	7.1	2/5/81)



/*
**	Exactkey checks to see if the relation described
**	by "ap" can be used in a hashed scan.
**	All the key domains of the relation must
**	have simple clauses of equality associated
**	with them in the qualification.
**
**	Returns 0 if the relation can't be used.
**
**	Returns > 0 if it can.
*/

exactkey(ap, key)
struct accessparam	*ap;
struct key		*key;
{
	register struct accessparam	*a;
	register struct key		*k;
	register struct simp		*s;
	int				d, i, j;

#	ifdef xOTR1
	if (tTf(85, -1))
		printf("Exactkey\n");
#	endif

	a = ap;
	k = key;
	i = 0;
	if (a->mode == EXACTKEY)
	{

		for (i = 0; d = a->keydno[i]; i++)
		{
	
			s = De.ov_simp;
			for (j = 0; j < De.ov_nsimp; j++)
			{
				if (s->relop == opEQ && s->att == d)
				{
					k->keysym = s->const;
					k->dnumber = (a->sec_index == TRUE) ? i+1 : d;
					k++;
#					ifdef xOTR1
					if (tTf(85, 1))
					{
						printf("exact key on dom %d\tvalue=", d);
						prsym(s->const);
					}
#					endif
					break;
				}
				s++;
			}
			if (j == De.ov_nsimp)
			{
				i = 0;	/* failure. at lease one key isn't used */
				break;
			}
		}
		k->dnumber = 0;	/* mark end of list */
	}
#	ifdef xOTR1
	if (tTf(85, 9))
		printf("exactkey returning %d\n", i);
#	endif
	return (i);
}
/*
**	Range key checks if the relation described by
**	"ap" is ISAM and there are simple clauses
**	on the first key and any additional keys.
**
**	Rangekey accumulates both high and low keys,
**	which are not necessary the same. If it
**	every finds a high or a low key on the first
**	domain of the relation then success=TRUE.
**
**	Returns  1 if Rangekey ok
**		 0 if Rangekey is not ok
**		-1 if Rangekey ok and all clauses are equality clauses
*/

rangekey(ap, l, h)
struct accessparam	*ap;
struct key		*l;
struct key		*h;
{
	register struct key	*low, *high;
	register struct simp	*s;
	struct accessparam	*a;
	int			sec_indx, d, i;
	int			rel, success, ns, lowkey, allexact;

#	ifdef xOTR1
	if (tTf(85, 5))
		printf("Rangekey\n");
#	endif

	a = ap;
	sec_indx  = a->sec_index == TRUE;
	low = l;
	high = h;
	allexact = -1;	/* assume all clauses equality clauses */
	s = De.ov_simp;
	success = FALSE;
	if (a->mode == LRANGEKEY)
	{

		for (ns = 0; ns < De.ov_nsimp; ns++)
		{
			rel = s->relop;
			for (i = 0; d = a->keydno[i]; i++)
			{
				if (d == s->att)
				{
					/* this is either a high range value or low range value */
					lowkey = (rel == opGTGE);
					if (lowkey || rel == opEQ)
					{
						/* low range key */
#						ifdef xOTR1
						if (tTf(85, 6))
							printf("low key on dom %d\t", d);
#						endif
						low->keysym = s->const;
						low->dnumber = sec_indx ? i+1 : d;
						low++;
					}
					if (!lowkey || rel == opEQ)
					{
						/* high range key */
#						ifdef xOTR1
						if  (tTf(85, 6))
							printf("high key on dom %d\t", d);
#						endif
						high->keysym = s->const;
						high->dnumber = sec_indx ? i+1 : d;
						high++;
					}
#					ifdef xOTR1
					if (tTf(85, 6))
						prsym(s->const);
#					endif
					if (i == 0)
						success = TRUE;
					if (rel != opEQ)
						allexact = 1;	/* at least one inequality */
					break;
				}
			}
			s++;	/* try next simple clause */
		}
	}

	high->dnumber = 0;	/* mark end of list */
	low->dnumber = 0;	/* mask end of list */

	/* if success then return whether all clauses were equality */
	if (success)
		success = allexact;

#	ifdef xOTR1
	if (tTf(85, 5))
		printf("rangekey returning %d\n", success);
#	endif
	return (success);
}
/*
**	Setallkey takes a key struct, decodes it and
**	calls setkey with each value.
**
**	Called from strategy().
**
**	returns 0 if ok.
**	returns -1 in the special case of a deblanked hashkey
**	being bigger than the corresponding domain.
*/

setallkey(relkey, keytuple)
struct key	*relkey;
char		*keytuple;
{
	register struct key	*k;
	register SYMBOL		*sk;
	register int		dnum;
	struct symbol		**s;
	char			*p, temp[256];
	int			l;

	clearkeys(De.ov_scanr);
	k = relkey;
	while (dnum = k->dnumber)
	{
		s = &k->keysym;
		sk = (SYMBOL *) De.ov_stack;
		getsymbol(sk, &s);	/* copy symbol to stack. caution:getsym changes the value of s. */
		rcvt(sk, De.ov_scanr->relfrmt[dnum], De.ov_scanr->relfrml[dnum]);	/* convert key to correct type */
		p = (char *)&sk->value;

		if (sk->type == CHAR)
		{
			/*
			** The length of a character key must
			** be made equal to the domain length.
			** The key is copied to a temp place
			** and a null byte is inserted at the
			** end. In addition, if the key without
			** blanks is longer than the domain and
			** this is an exactkey, then the query
			** is false.
			*/
			p = temp;
			l = cmove(sk, p);	/* copy symbol to temp removing blanks & nulls */
#			ifdef xOTR1
			if (tTf(86, 9))
				printf("length is %d\n", l);
#			endif
			if (De.ov_fmode == EXACTKEY && l > (De.ov_scanr->relfrml[dnum] & 0377))
				/* key too large. qualification is false */
				return (-1);
		}
		setkey(De.ov_scanr, keytuple, p, dnum);	/* set the key */
		k++;
	}
#	ifdef xOTR1
	if (tTf(86, 8))
		printup(De.ov_scanr, keytuple);
#	endif
	return (0);
}
/*
**	Cmove copies a char symbol into "dest".
**	It stops when the length is reached or
**	when a null byte is found.
**
**	returns the number of non-blank chars
**	in the string.
*/

cmove(sym, dest)
SYMBOL	*sym;
char	*dest;
{
	register char	*d, *s;
	register int	l;
	int		blank;

	s = sym->value.sym_data.cptype;	/* s points to the char string */
	d = dest;
	blank = 0;

	for (l = (sym->len & 0377); l--; s++)
	{
		*d++ = *s;
		if (*s == ' ')
			blank++;
		if (*s == '\0')
		{
			d--;
			break;
		}
	}

	*d = '\0';
	return ((d - dest) - blank);	/* return length of string */
}
/*
**	Indexcheck is called by scan() to check whether
**	a secondary index tuple satisfies the simple
**	clauses under which it was scanned.
**
**	Returns 1 if the tuple is ok,
**		0 otherwise.
*/

indexcheck()
{
	register int	i;

	if (De.ov_fmode == EXACTKEY)
		i = keycheck(De.ov_lkey_struct, De.ov_keyl, 0);	/* check for equality */
	else
	{
		i = keycheck(De.ov_lkey_struct, De.ov_keyl, 1);	/* check for >= */
		/* If the lowkey passed, check the highkey also */
		if (i)
			i = keycheck(De.ov_hkey_struct, De.ov_keyh, -1);	/* check for <= */
	}
#	ifdef xOTR1
	if (tTf(86, 10))
		printf("indexcheck ret %d\n", i);
#	endif
	return (i);
}
/*
**	Keycheck compares De.ov_intup with keytuple
**	according to the domains specified in the
**	"keys" struct.
**
**	mode is either >0, =0, <0 depending on
**	whether check is for De.ov_intup >= keytuple,
**	De.ov_intup == keytuple, De.ov_intup <= keytuple respectively
**
**	returns TRUE or FALSE accordingly.
*/

keycheck(keys, keytuple, mode)
struct key	*keys;
char		*keytuple;
int		mode;
{
	register struct key	*k;
	register char		*kp;
	register int		dnum;
	int			offset, i, success;

	kp = keytuple;
	success = TRUE;

	for (k = keys; dnum = k->dnumber; k++)
	{

		offset = De.ov_scanr->reloff[dnum];
		if (i = icompare(&De.ov_intup[offset], &kp[offset], De.ov_scanr->relfrmt[dnum], De.ov_scanr->relfrml[dnum] & I1MASK))
		{
			if (i < 0 && mode < 0 || i > 0 && mode > 0)
				continue;
			success = FALSE;
			break;
		}
	}
	return (success);
}
