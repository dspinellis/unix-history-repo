#include "old.h"

/*
 *	mobility
 *	1 for each potential move
 */

wheur1()
{
	int *p1, *p2, *p3, i;
	int pto;

	if(amp[-2]) {
		i = amp[-3];
		pto = board[i];
		board[i] = 0;
	}
	p1 = lmp;
	wgen();
	p2 = lmp;
	lmp = p1;
	bgen();
	p3 = lmp;
	lmp = p1;
	i = p2-p3;
	i =>> 1;
	if(amp[-2])
		board[amp[-3]] = pto;
	return(i);
}

/*
 *	opening 'goodies'
 */

wheur2()
{
	int i;

	i = 0;
	if(game > 2) return(i);
	/*
	 * minor pieces out
	 * knights before bishops
	 */
	i =+ 9*((board[57] != -2)+
		(board[62] != -2));
	i =+ 8*((board[58] != -3)+
		(board[61] != -3));
	/*
	 * blocked central pawns
	 */
	if(board[51] == -1 && board[51-8]!=0) i =- 10;
	if(board[52] == -1 && board[52-8]!=0) i =- 10;
	return(i);
}

/*
 *	castle
 */

wheur3()
{
	int i;

	i = 0;
	/*
	 * queenside ability
	 */
	if(flag&02 && board[48]== -1 && board[49]== -1 && board[50]== -1)
		i =+ 20;
	/*
	 *  kingside ability
	 */
	if(flag&01 && board[53]== -1 && board[54]== -1 && board[55]== -1)
		i =+ 20;
	/*
	 * if both
	 */
	if(i == 40)
		i = 22;
	/*
	 * if castled,
	 * keep pawns in
	 */
	if(wkpos==58)
		if(board[50]== -1 && (board[48]== -1 || board[48-8]== -1) &&
			(board[49]== -1 || board[49-8]== -1))
				i =+ 40;
	if(wkpos==62)
		if(board[53]== -1 && (board[54]== -1 || board[54-8]== -1) &&
			(board[55]== -1 || board[55-8]== -1))
				i =+ 40;
	return(i);
}

/*
 *	prance
 *	a percentage if the
 *	piece on the move
 *	can be driven back
 *	by a smaller piece
 */

wheur4()
{
	int *p1, *p2, ploc, i;

	if(amp[-1] != 1) return(0);
	ploc = amp[-3];
	if(board[ploc] == -1) return(0);
	if(xheur(ploc)) return(0);
	p1 = lmp;
	p2 = p1;
	bagen();
	i = 0;
	while(p2 != lmp) {
		p2++;
		bmove(*p2++);
		i = xheur(ploc);
		bremove();
		if(i)
			break;
	}
	lmp = p1;
	return(i);
}

/*
 *	control
 *	center control
 *		opening
 *		beginning
 *	king control
 *		middle
 *		end
 */

wheur5()
{
	int i, j, k;
	int s, n, d, pto;

	if(amp[-2]) {
		i = amp[-3];
		pto = board[i];
		board[i] = 0;
	}
	i = 64;
	while(i--)
		control[i] = 0;
	if(game < 2) {
		i = 64;
		while(i--)
			control[i] =+ center[i];
	}
	if(mantom) {
		if((flag&03)==0)
			srnd(wkpos);
	} else {
		if((flag&030)==0)
			srnd(bkpos);
	}
	i = 64;
	s = 0;
	while(i--) {
		n = control[i]*100;
		attack(i);
		j = 0;
		while(k = attacv[j++]) {
			d = (pval+6)[k];
			if(d < 0)
				s =- n/(-d); else
				s =+ n/d;
		}
	}
	if(amp[-2])
		board[amp[-3]] = pto;
	return(-s);
}

/*
 * mate threat
 * minus for captures
 */
wheur6()
{
	int i;

	i = 0;
	*amp++ = -1;
	if(wattack(bkpos))
		if(mate(2, 0))
			i =+ 15;
	amp--;
	return(i);
}
