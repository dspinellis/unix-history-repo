#include "old.h"

/*
 *	mobility
 *	1 for each potential move
 */

bheur1()
{

	return(-wheur1());
}

/*
 *	opening 'goodies'
 *	10 for each minor piece out
 *	-10 for blocking kq pawns
 */

bheur2()
{
	int i, mt;

	i = 0;
	if(game > 2) return(i);
	mt = amp[-1];
	if(mt == 2 || mt == 3) i =+ 30;
	if(mt == 0) i =- 20;
	i =+ 9*((board[1] != 2)+
		(board[6] != 2));
	i =+ 8*((board[2] != 3)+
		(board[5] != 3));

	/*
	 * -10 for blocked central pawns
	 */
	if(board[11]==1 && board[11+8]!=0) i =- 10;
	if(board[12]==1 && board[12+8]!=0) i =- 10;
	return(i);
}

/*
 *	ability to castle
 *	22 for both flags
 *	20 for one flag
 */

bheur3()
{
	int i;

	i = 0;
	/*
	 * queenside ability
	 */
	if(flag&020 && board[8]==1 && board[9]==1 && board[10]==1)
		i =+ 20;
	/*
	 *  kingside ability
	 */
	if(flag&010 && board[13]==1 && board[14]==1 && board[15]==1)
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
	if(bkpos==2)
		if(board[10]==1 && (board[8]==1 || board[8+8]==1) &&
			(board[9]==1 || board[9+8]==1))
				i =+ 40;
	if(bkpos==6)
		if(board[13]==1 && (board[14]==1 || board[14+8]==1) &&
			(board[15]== -1 || board[15+8]== -1))
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

bheur4()
{
	int *p1, *p2, ploc, i;

	if(amp[-1] != 1) return(0);
	ploc = amp[-3];
	if(board[ploc] == 1) return(0);
	if(xheur(ploc)) return(0);
	p1 = lmp;
	p2 = p1;
	wagen();
	i = 0;
	while(p2 != lmp) {
		p2++;
		wmove(*p2++);
		i = xheur(ploc);
		wremove();
		if(i)
			break;
	}
	lmp = p1;
	return(-i);
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

bheur5()
{

	return(-wheur5());
}

/*
 * mate threat
 * bad to capture
 */
bheur6()
{
	int i;

	*amp++ = -1;
	i = 0;
	if(battack(wkpos))
		if(mate(2, 0))
			i =+ 15;
	amp--;
	return(i);
}
