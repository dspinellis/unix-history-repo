# include <stdio.h>

#
#define NIL (-1)
#define MAXGMOV 10
#define MAXIMOVES 1000
	char level;		/*'b'=beginner, 'i'=intermediate, 'e'=expert*/
int die1;
int die2;
int i;
int j;
int l;
int m;
int count;
int red[]     {0,2,0,0,0,0,0,0,0,0,0,0,5,
		 0,0,0,0,3,0,5,0,0,0,0,0,
		 0,0,0,0,0,0};
int white[]   {0,2,0,0,0,0,0,0,0,0,0,0,5,
		 0,0,0,0,3,0,5,0,0,0,0,0,
		 0,0,0,0,0,0};
int probability[]{0,11,12,13,14,15,16,
		    06,05,04,03,02,01};
int imoves;
int goodmoves[MAXGMOV] ;
int probmoves[MAXGMOV] ;
struct {int pos[4],mov[4];} moves[MAXIMOVES] ;

main()
{
	int t,k,n,go[5];
	char s[100];
	go[5]=NIL;
	srand();
	printf( "Do you want instructions? Type 'y' for yes,\n");
	printf( "anything else means no.?? ");
	getstr(s);
	if(*s=='y')instructions();
	printf( "Choose the level of your oppponent.\n");
	printf( "Type 'b' for beginner, or 'i' for intermediate.\n");
	printf( "Anything else gets you an expert.?? ");
	level='e';
	getstr(s);
	if(*s=='b')level='b';
	else if(*s=='i')level='i';
	printf( "You will play red. Do you wan't to move first?\n");
	printf( "Type 'y' for yes, anything else means no.?? ");
	getstr(s);
	if(*s=='y')goto nowhmove;
whitesmv:
	roll();
	printf( "white rolls %d,%d\n",die1,die2);
	printf( "white's move is:");
	if(nextmove(white,red)==NIL)goto nowhmove;
	if(piececount(white,0,24)==0){
	    printf( "White wins\n");
	    printf( "Aren't you ashamed. You've been beaten by a computer.\n");
	    exit();
	}
nowhmove:
	prtbrd();

	roll();
retry:
	printf( "your roll is %d,  %d\n",die1,die2);
	printf( "your move, please?? ");
	getstr(s);
	if(*s==0){
	    printf( "red's move skipped\n");
	    goto whitesmv;
	}
	n=sscanf(s,"%d%d%d%d%d",&go[0],&go[1],&go[2],&go[3],&go[4]);
	if((die1!=die2&&n>2)||n>4){
	    printf( "you've made too many moves\n");
	    goto retry;
	}
	go[n]=NIL;
	if(*s=='-'){
	    go[0]= -go[0];
	    t=die1;
	    die1=die2;
	    die2=t;
	}
	for(k=0;k<n;k++){
	    if(0<=go[k] && go[k]<=24)continue;
	    else{
		printf( "move %d is illegal\n",go[k]);
		goto retry;
	    }
	}
	if(play(red,white,go))goto retry;
	if(piececount(red,0,24)==0){
	    printf( "Red wins.\n");
	    printf( "Congratulations! You have just defeated a dumb machine.\n");
	    exit();
	}
	goto whitesmv;
}

getstr(s)
char *s;
{
	while((*s=getchar())!='\n')s++;
	*s=0;
}

play(player,playee,pos)
int *player,*playee,pos[];
{
	int k,n,die,ipos;
	for(k=0;k<player[0];k++){  /*blots on player[0] must be moved first*/
	    if(pos[k]==NIL)break;
	    if(pos[k]!=0){
		printf( "piece on position 0 must be moved first\n");
		return(-1);
	    }
	}
	for(k=0;(ipos=pos[k])!=NIL;k++){
	    die=k?die2:die1;
	    n=25-ipos-die;
	    if(player[ipos]==0)goto badmove;
	    if(n>0&&playee[n]>=2)goto badmove;
	    if(n<=0){
		if(piececount(player,0,18)!=0)goto badmove;
		if((ipos+die)!=25&&
		    piececount(player,19,24-die)!=0)goto badmove;
	    }
	    player[ipos]--;
	    player[ipos+die]++;
	}
	for(k=0;pos[k]!=NIL;k++){
	    die=k?die2:die1;
	    n=25-pos[k]-die;
	    if(n>0 && playee[n]==1){
		playee[n]=0;
		playee[0]++;
	    }
	}
	return(0);

badmove:
	printf( "Move %d is not legal.\n",ipos);
	while(k--){
	    die=k?die2:die1;
	    player[pos[k]]++;
	    player[pos[k]+die]--;
	}
	return(-1);
}
nextmove(player,playee)
int *player,*playee;
{
	int k;
	imoves=0;
	movegen(player,playee);
	if(die1!=die2){
	k=die1;
	die1=die2;
	die2=k;
	movegen(player,playee);
	}
	if(imoves==0){
	    printf( "roll was %d,%d; no white move possible\n",die1,die2);
	    return(NIL);
	}
	k=strategy(player,playee);		/*select kth possible move*/
	prtmov(k);
	update(player,playee,k);
	return(0);
}
prtmov(k)
int k;
{
	int n;
	if(k==NIL)printf( "no move possible\n");
	else for(n=0;n<4;n++){
	    if(moves[k].pos[n]==NIL)break;
	    printf( "    %d, %d",25-moves[k].pos[n],moves[k].mov[n]);
	}
	printf( "\n");
}
update(player,playee,k)
int *player,*playee,k;
{
	int n,t;
	for(n=0;n<4;n++){
	    if(moves[k].pos[n]==NIL)break;
	    player[moves[k].pos[n]]--;
	    player[moves[k].pos[n]+moves[k].mov[n]]++;
	    t=25-moves[k].pos[n]-moves[k].mov[n];
	    if(t>0 && playee[t]==1){
		playee[0]++;
		playee[t]--;
	    }
	}
}
piececount(player,startrow,endrow)
int *player,startrow,endrow;
{
	int sum;
	sum=0;
	while(startrow<=endrow)
	sum=+player[startrow++];
	return(sum);
}
/*
prtmovs()
{
	int i1,i2;
	printf( "possible moves are\n");
	for(i1=0;i1<imoves;i1++){
		printf( "\n%d",i1);
		for(i2=0;i2<4;i2++){
			if(moves[i1].pos[i2]==NIL)break;
			printf( "%d, %d",moves[i1].pos[i2],moves[i1].mov[i2]);
		}
	}
	printf( "\n");
}
*/

roll()
{
	extern int die1,die2;
	die1=(rand()>>8)%6+1;
	die2=(rand()>>8)%6+1;
}

movegen(mover,movee)
int *mover,*movee;
{
	extern int i,j,l,m,count;
	extern int die1,die2;
	int k;
	for(i=0;i<=24;i++){
		count=0;
		if(mover[i]==0)continue;
		if((k=25-i-die1)>0&&movee[k]>=2)
		    if(mover[0]>0)break;
		    else continue;
		if(k<=0){
		    if(piececount(mover,0,18)!=0)break;
		    if((i+die1)!=25&&
		        piececount(mover,19,24-die1)!=0)break;
		}
		mover[i]--;
		mover[i+die1]++;
		count=1;
		for(j=0;j<=24;j++){
			if(mover[j]==0)continue;
			if((k=25-j-die2)>0&&movee[k]>=2)
			    if(mover[0]>0)break;
			    else continue;
			if(k<=0){
			    if(piececount(mover,0,18)!=0)break;
			    if((j+die2)!=25&&
			        piececount(mover,19,24-die2)!=0)break;
			}
			mover[j]--;
			mover[j+die2]++;
			count=2;
			if(die1!=die2){
			    moverecord(mover);
			    if(mover[0]>0)break;
			    else continue;
			}
			for(l=0;l<=24;l++){
			    if(mover[l]==0)continue;
			    if((k=25-l-die1)>0&&movee[k]>=2)
				if(mover[0]>0)break;
				else continue;
			    if(k<=0){
				if(piececount(mover,0,18)!=0)break;
				if((l+die2)!=25&&
				    piececount(mover,19,24-die1)!=0)break;
			    }
			    mover[l]--;
			    mover[l+die1]++;
			    count=3;
			    for(m=0;m<=24;m++){
				if(mover[m]==0)continue;
				if((k=25-m-die1)>=0&&movee[k]>=2)
				    if(mover[0]>0)break;
				    else continue;
				if(k<=0){
				    if(piececount(mover,0,18)!=0)break;
				    if((m+die2)!=25&&
				        piececount(mover,19,24-die1)!=0)break;
				}
				count=4;
				moverecord(mover);
				if(mover[0]>0)break;
			    }
			    if(count==3)moverecord(mover);
			    else{
				mover[l]++;
				mover[l+die1]--;
			    }
			    if(mover[0]>0)break;
			}
			if(count==2)moverecord(mover);
			else{
			    mover[j]++;
			    mover[j+die1]--;
			}
			if(mover[0]>0)break;
		}
		if(count==1)moverecord(mover);
		else{
		    mover[i]++;
		    mover[i+die1]--;
		}
		if(mover[0]>0)break;
	}
}
moverecord(mover)
int *mover;
{
	extern int i,j,l,m,imoves,count;
	int t;
	if(imoves>=MAXIMOVES)goto undo;;
	for(t=0;t<=3;t++)
	    moves[imoves].pos[t]= NIL;
	switch(count){
case 4:
	    moves[imoves].pos[3]=m;
	    moves[imoves].mov[3]=die1;
case 3:
	    moves[imoves].pos[2]=l;
	    moves[imoves].mov[2]=die1;
case 2:
	    moves[imoves].pos[1]=j;
	    moves[imoves].mov[1]=die2;
case 1:
	    moves[imoves].pos[0]=i;
	    moves[imoves].mov[0]=die1;
	    imoves++;
	}
undo:
	switch(count){
case 4:
	    break;
case 3:
	    mover[l]++;
	    mover[l+die1]--;
	    break;
case 2:
	    mover[j]++;
	    mover[j+die2]--;
	    break;
case 1:
	    mover[i]++;
	    mover[i+die1]--;
	}
}


strategy(player,playee)
int *player,*playee;
{
	extern char level;
	int k,n,nn,bestval,moveval,prob;
	n=0;
	if(imoves==0)return(NIL);
	goodmoves[0]=NIL;
	bestval= -32000;
	for(k=0;k<imoves;k++){
	    if((moveval=eval(player,playee,k,&prob))<bestval)continue;
	    if(moveval>bestval){
		bestval=moveval;
		n=0;
	    }
	    if(n<MAXGMOV){
		goodmoves[n]=k;
		probmoves[n++]=prob;
	    }
	}
	if(level=='e' && n>1){
	    nn=n;
	    n=0;
	    prob=32000;
	    for(k=0;k<nn;k++){
		if((moveval=probmoves[k])>prob)continue;
		if(moveval<prob){
		    prob=moveval;
		    n=0;
		}
		goodmoves[n]=goodmoves[k];
		probmoves[n++]=probmoves[k];
	    }
	}
	return(goodmoves[(rand()>>4)%n]);
}

eval(player,playee,k,prob)
int *player,*playee,k,*prob;
{
	extern char level;
	int newtry[31],newother[31],*r,*q,*p,n,sum,first;
	int ii,lastwhite,lastred;
	*prob=sum=0;
	r=player+25;
	p=newtry;
	q=newother;
	while(player<r){
	    *p++= *player++;
	    *q++= *playee++;
	}
	q=newtry+31;
	for(p=newtry+25;p<q;) *p++ = 0;	/*zero out spaces for hit pieces*/
	for(n=0;n<4;n++){
	    if(moves[k].pos[n]==NIL)break;
	    newtry[moves[k].pos[n]]--;
	    newtry[ii=moves[k].pos[n]+moves[k].mov[n]]++;
	    if(ii<25 && newother[25-ii]==1){
		newother[25-ii]=0;
		newother[0]++;
		if(ii<=15 && level=='e')sum++;	/*hit if near other's home*/
	    }
	}
	for(lastred=0;newother[lastred]==0;lastred++);
	for(lastwhite=0;newtry[lastwhite]==0;lastwhite++);
	lastwhite=25-lastwhite;
	if(lastwhite<=6 && lastwhite<lastred)sum=1000;
	if(lastwhite<lastred && level=='e'
	    && lastwhite>6){			/*expert's running game.
						  First priority to get all
						  pieces into white's home*/
	    for(sum=1000;lastwhite>6;lastwhite--)
		sum=sum-lastwhite*newtry[25-lastwhite];
	}
	for(first=0;first<25;first++)
	    if(newother[first]!=0)break;	/*find other's first piece*/
	q=newtry+25;
	for(p=newtry+1;p<q;)if(*p++ > 1)sum++;	/*blocked points are good*/
	if(first>5){	/*only stress removing pieces if homeboard
			  cannot be hit
			*/
	    q=newtry+31;
	    p=newtry+25;
	    for(n=6;p<q;n--)
		sum=+ *p++ * n;	/*remove pieces, but just barely*/
	}
	if(level!='b'){
	    r=newtry+25-first;	/*singles past this point can't be hit*/
	    for(p=newtry+7;p<r;)
		if(*p++ == 1)sum--;	/*singles are bad after 1st 6 points
					  if they can be hit*/
	    q=newtry+3;
	    for(p=newtry;p<q;)sum=- *p++;  /*bad to be on 1st three points*/
	}

	for(n=1;n<=4;n++)
	    *prob=+ n*getprob(newtry,newother,6*n-5,6*n);
	return(sum);
}
instructions()
{
	printf( "To play backgammon, type the numbers of the points\n");
	printf( "from which pieces are to be moved. Thus, if the\n");
	printf( "roll is '3,5', typing '2 6' will move a piece\n");
	printf( "from point 2 three spaces to point 5,\n");
	printf( "and a piece from point 6 forward to\n");
	printf( "point 11.  If the moves must be made in the\n");
	printf( "opposite order, the first character typed must\n");
	printf( "be a minus ('-'). Thus, typing\n");
	printf( "'-2 6' moves the piece on point 2\n");
	printf( "by 5, and the piece on point 6 by 3.\n");
	printf( "If you want to move a single piece several times,\n");
	printf( "the sequence of points from which it is to be\n");
	printf( "moved must be typed. Thus '14 17' will move\n");
	printf( "a piece from point 14 to point 17 and thence to\n");
	printf( "to point 22.\n");
	printf( "If a double is rolled, you should type four numbers.\n");
	printf( "Red pieces that have been removed from the board by\n");
	printf( "being hit by white are on point 0 and\n");
	printf( "must be brought in before any other move can be made.\n");
	printf( "White pieces that are hit are removed to point 25.\n");
	printf( "You will not be allowed to make an illegal move, or\n");
	printf( "to make too many moves. However, if you make too\n");
	printf( "few moves, the program does not care. In particular\n");
	printf( "you may skip your turn by typing a 'new-line'\n");
	printf( "all by itself.\n\n");
}

getprob(player,playee,start,finish)
int *player,*playee,start,finish;
{			/*returns the probability (times 102) that any
			  pieces belonging to 'player' and lying between
			  his points 'start' and 'finish' will be hit
			  by a piece belonging to playee
			*/
	int k,n,sum;
	sum=0;
	for(;start<=finish;start++){
	    if(player[start]==1){
		for(k=1;k<=12;k++){
		    if((n=25-start-k)<0)break;
		    if(playee[n]!=0)sum=+probability[k];
		}
	    }
	}
	return(sum);
}
prtbrd()
{
	int k;
	printf( "White's Home\n");
	for(k=1;k<=6;k++)
	    printf( "%4d",k);
	printf( "    ");
	for(k=7;k<=12;k++)printf( "%4d",k);
	putchar('\r' );
	for(k=1;k<=54;k++)putchar('_' );
	putchar('\n' );
	numline(red,white,1,6);
	printf( "    ");
	numline(red,white,7,12);
	putchar('\n' );
	colorline(red,'R',white,'W',1,6);
	printf( "    ");
	colorline(red,'R',white,'W',7,12);
	putchar('\n' );
	if(white[0]!=0)printf( "%28dW\n",white[0]);
	else putchar('\n' );
	if(red[0]!=0)printf( "%28dR\n",red[0]);
	else putchar('\n' );
	colorline(white,'W',red,'R',1,6);
	printf( "    ");
	colorline(white,'W',red,'R',7,12);
	putchar('\n' );
	numline(white,red,1,6);
	printf( "    ");
	numline(white,red,7,12);
	putchar('\r' );
	for(k=1;k<=54;k++)putchar('_' );
	putchar('\n' );
	for(k=24;k>=19;k--)printf( "%4d",k);
	printf( "    ");
	for(k=18;k>=13;k--)printf( "%4d",k);
	printf( "\nRed's Home\n\n\n\n\n");
}
numline(upcol,downcol,start,fin)
int *upcol,*downcol,start,fin;
{
	int k,n;
	for(k=start;k<=fin;k++){
	    if((n=upcol[k])!=0 || (n=downcol[25-k])!=0)printf( "%4d",n);
	    else printf( "    ");
	}
}
colorline(upcol,c1,downcol,c2,start,fin)
int *upcol,*downcol,start,fin;
char c1,c2;
{
	int k;
	char c;
	for(k=start;k<=fin;k++){
	    c=' ';
	    if(upcol[k]!=0)c=c1;
	    if(downcol[25-k]!=0)c=c2;
	printf( "   %c",c);
	}
}

int rrno 0;

srand(){
	rrno = _look( 0x40000 );
	_store( 0x40000, rrno+1 );
	}

rand(){
	rrno =* 0106273;
	rrno =+ 020202;
	return( rrno & 077777 );
	}

_look(p) int *p; {
	return( *p );
	}

_store( p, numb ) int *p; {
	*p = numb;
	}
