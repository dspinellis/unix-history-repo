>>>>>  THIS CODE HAS NOT BEEN CONVERTED TO THE VAX.
>>>>>  IT IS PROVIDED ONLY FOR YOUR PERUSAL.
# include	<param.h>
# include 	<user.h>
# include	<proc.h>
# include	<ildr.h>
/*
 *	/dev/lock header
 */
/*	
 *	ilwrite() : write driver
 *		1. copy Lock request info to lockbuf
 *		2. follow action in l_act
		3. Error return conditions
			-1: lockrequest fails(only on act=1)
			-2: attempt to release a lock not set
			    by calling program
			-3: illegal action requested
 */
ilwrite()
{
	register char *lp;
	int	waitflg;
	struct Lockreq	lockbuf;
	register struct Lockreq *ll;
	int	c;
	int	k,i;
	int	blockflag;

	if (u.u_count != KEYSIZE + 3)
	{
		u.u_error = -4;
		return;
	}
/*
 *		copy lockrequest info to lockbuf
 */
	lp = &lockbuf.lr_act;
	while ((c = cpass()) >= 0)
		*lp++ = c;
	lockbuf.lr_pid = u.u_procp->p_pid;
	ll = &lockbuf;
	if (( ll->lr_act < A_RLS1)
	&& ((ll->lr_type < T_CS) || (ll->lr_type > T_DB )
	   || (ll->lr_mod < M_EXCL) || (ll->lr_mod > M_SHARE )))
	{
		u.u_error = -5;
		return;
	}
/*
 *		follow action from lock request
 */
	switch(ll->lr_act)
	{
	  case A_RTN:
					/*
					 * attempt to set lock.
					 * error return if failure.
					 */
		blockflag = FALSE;
		for ( i = 0; i <= ll->lr_type; i++)
			if (Lockset[i] == 0)
				blockflag = TRUE;
		if (blockflag || ilunique(ll) >= 0)
			u.u_error = -1;
		else
			ilenter(ll);
		break;

	  case A_SLP:
				/* attempt to set lock.
				 * sleep on blocking address if failure.
				 */
		do
		{
			do
			{
				blockflag = TRUE;
				for ( i = 0; i <= ll->lr_type; i++)
					if (Lockset[i] == 0)
					{
						sleep(&Lockset[i],LOCKPRI);
						blockflag = FALSE;
					}
			}
			while (!blockflag);
			if (( i = ilunique(ll)) >= 0 )
			{
				blockflag = FALSE;
				Locktab[i].l_wflag = W_ON;
				sleep(&Locktab[i],LOCKPRI);
			}
		}
		while (!blockflag);
		ilenter(ll);
		break;

	  case A_RLS1:
				/* remove 1 lock */
		if ((i = ilfind(ll)) >= 0)
		{
			ilrm(i,ll->lr_pid);
		}
		else
			u.u_error = -2;
		break;

	  case A_RLSA:
				/* remove all locks for this process id*/
		ilrma(ll->lr_pid);
		break;

	  case A_ABT:		/* remove all locks */
		ilclose();
		break;

	  default :
		u.u_error = -3;
	}
}
/*
 *	ilunique- check for match on key
 *	
 *	return index of Locktab if match found
 *	else return -1
 */
ilunique(ll)
struct	Lockreq	*ll;
{
	register int	k;
	register struct Lockform	*p;
	register struct Lockreq	*q;

	q = ll;
	for (k = 0; k < NLOCKS; k++)
	{
		p = &Locktab[k];
		if ((p->l_mod != M_EMTY)
		&& (ilcomp(p->l_key,q->lr_key) == 0)
		&& (p->l_type == q->lr_type)
		&& ( (p->l_mod == M_EXCL) || (q->lr_mod == M_EXCL)) )
			return(k);
	}
	return(-1);
}
ilfind(ll)
struct	Lockreq	*ll;
{
	register int	k;
	register struct Lockform	*p;
	register struct Lockreq	*q;

	q = ll;
	for (k = 0; k < NLOCKS; k++)
	{
		p = &Locktab[k];
		if ((p->l_mod != M_EMTY)
		&& (ilcomp(p->l_key,q->lr_key) == 0)
		&& (p->l_type == q->lr_type)
		&& (p->l_pid == q->lr_pid))
			return(k);
	}
	return(-1);
}
/*
 *	remove the lth Lock
 *		if the correct user is requesting the move.
 */
ilrm(l,llpid)
int l;
int	llpid;
{
	register	a;
	register	k;


	a = &Locktab[l];
	if (a->l_pid == llpid && a->l_mod != M_EMTY)
	{
		a->l_mod = M_EMTY;
		a->l_pid = 0;
		if (a->l_wflag == W_ON)
		{
			a->l_wflag = W_OFF;
			wakeup(&Locktab[l]);
		}
		for (k = 0; k <= a->l_type; k++)
		{
			Lockset[k]++;
			if (Lockset[k] == 1)
				wakeup(&Lockset[k]);
		}
	}
}
/*
 *	ilrma releases all locks for a given process id(pd)
 */
ilrma(pd)
int pd;
{
	register int	i;

	for ( i = 0; i < NLOCKS; i++ )
			ilrm(i,pd);
}
/*
 *	enter Lockbuf in locktable
 *	return position in Locktable
 *	error return of -1
 */
ilenter(ll)
struct Lockreq	*ll;
{
	int	k,l;
	register char	*f,*t;
	register struct Lockform	*p;

	for (k = 0; k < NLOCKS; k++)
	{
		p = &Locktab[k];
		if (p->l_mod == M_EMTY)
		{
			p->l_pid = ll->lr_pid;
			p->l_type = ll->lr_type;
			p->l_mod = ll->lr_mod;
			f = ll->lr_key;
			t = p->l_key;
			for (l = 0; l < KEYSIZE; l++)
				*t++ = *f++;
			for (l = 0; l <= ll->lr_type; l++)
				Lockset[l]--;
			return(k);
		}
	}
	return (-1);
}
/*
 *	ilcomp- string compare
 *	  	returns 0 if strings match
 *		returns -1 otherwise
 */
ilcomp(st1,st2)
char *st1,*st2;
{
	register int	k;
	register char	*s1,*s2;

	s1 = st1;
	s2 = st2;
	for (k = 0; k < KEYSIZE; k++)
		if ( *s1++ != *s2++)
			return (-1);
	return (0);
}
/*
 *	ilclose- releases all locks
 */
ilclose()
{
	register int	k;
	register char *c;

	for (k = 0; k < NLOCKS; k++)
		wakeup( &Locktab[k] );
	for (k = 0; k < 4; k++)
		wakeup( &Lockset[k]);
	for (c = &Locktab[0].l_pid; c < &Locktab[NLOCKS]; c++)
		*c = 0;
	Lockset[0] = NLOCKS;
	Lockset[1] = PLOCKS;
	Lockset[2] = RLOCKS;
	Lockset[3] = DLOCKS;
}
