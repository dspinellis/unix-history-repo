#include "old.h"

setup()
{
	char bd[64];
	char *p, *ip;
	int i, err, nkng, c;
	int wkp, bkp;

	for(p=bd; p<bd+64; )
		*p++ = 0;
	err = 0;
	nkng = 101;
	p = bd;
	for(i=0; i<8; i++) {
		ip = p+8;

	loop:
		switch(c = getchar()) {

		case 'K':
			nkng =- 100;
			c = 6;
			bkp = p-bd;
			break;

		case 'k':
			nkng--;
			c = -6;
			wkp = p-bd;
			break;

		case 'P':
			c = 1;
			break;

		case 'p':
			c = -1;
			break;

		case 'N':
			c = 2;
			break;

		case 'n':
			c = -2;
			break;

		case 'B':
			c = 3;
			break;

		case 'b':
			c = -3;
			break;

		case 'R':
			c = 4;
			break;

		case 'r':
			c = -4;
			break;

		case 'Q':
			c = 5;
			break;

		case 'q':
			c = -5;
			break;

		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
			p =+ c-'0';
			goto loop;

		case ' ':
			p++;
			goto loop;

		case '\n':
			if(p > ip)
				err++;
			p = ip;
			continue;

		default:
			err++;
			if(c <= 0)
				onhup();
			goto loop;
		}
		if(p < ip)
			*p++ = c;
		goto loop;
	}
	if(nkng)
		err++;
	if(err) {
		printf("Illegal setup\n");
		return;
	}
	for(i=0; i<64; i++)
		board[i] = bd[i];
	amp = ambuf+1;
	lmp = lmbuf+1;
	eppos = 64;
	bookp = 0;
	mantom = 0;
	moveno = 1;
	wkpos = wkp;
	bkpos = bkp;
	flag = 0;
	if(wkpos == 60) {
		if(board[56] == -4)
			flag =| 2;
		if(board[63] == -4)
			flag =| 1;
	}
	if(bkpos == 4) {
		if(board[0] == 4)
			flag =| 020;
		if(board[7] == 4)
			flag =| 010;
	}
	printf("Setup successful\n");
}
