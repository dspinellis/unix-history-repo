#include "old.h"

bagen()
{
	int *p1, *p2, v;

	p1 = lmp;
	if((flag&010)!=0)
	if(board[5]==0 && board[6]==0 && board[7]==4)
	if(wattack(4) && wattack(5) && wattack(6))
		btry(4, 0, 2); /* kingside castle */
	if((flag&020)!=0)
	if(board[0]==4 && board[1]==0 && board[2]==0 && board[3]==0)
	if(wattack(2) && wattack(3) && wattack(4))
		btry(4, 0, -2); /* queenside castle */
	bgen();
	p2 = p1;
	while(p2 != lmp) {
		v = *p2++;
		bmove(*p2);
		if(wattack(bkpos)) {
			*p1++ = v;
			*p1++ = *p2;
		}
		p2++;
		bremove();
	}
	lmp = p1;
}

btry(from, mask, offset)
int from, mask, offset;
{

	if((dir[from]&mask)==0)
		bcheck(from, from+offset);
}

bcheck(from, to)
int from, to;
{

	if(board[to]>0) return(1);
	*lmp++ = (pval+6)[board[to]]-value;
	*lmp++ = (from<<8)|to;
	return(board[to] != 0);
}

wagen()
{
	int *p1, *p2, v;

	p1 = lmp;
	if((flag&1)!=0)
	if(board[61]==0 && board[62]==0 && board[63]== -4)
	if(battack(60) && battack(61) && battack(62))
		wtry(60, 0, 2); /* kingside castle */
	if((flag&2)!=0)
	if(board[56]== -4 && board[57]==0 && board[58]==0 && board[59]==0)
	if(battack(58) && battack(59) && battack(60))
		wtry(60, 0, -2); /* queenside castle */
	wgen();
	p2 = p1;
	while(p2 != lmp) {
		v = *p2++;
		wmove(*p2);
		if(battack(wkpos)) {
			*p1++ = v;
			*p1++ = *p2;
		}
		p2++;
		wremove();
	}
	lmp = p1;
}

wtry(from, mask, offset)
int from, mask, offset;
{

	if((dir[from]&mask)==0)
		wcheck(from, from+offset);
}

wcheck(from, to)
int from, to;
{

	if(board[to]<0) return(1);
	*lmp++ = value-(pval+6)[board[to]];
	*lmp++ = (from<<8)|to;
	return(board[to] != 0);
}

