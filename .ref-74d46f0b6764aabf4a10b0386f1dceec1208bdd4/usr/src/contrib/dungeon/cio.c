/* these are c routines to handle the pdp dungeon i/o */
#include <stdio.h>

/*	send arguments to message printing process */
rspsb3_(arg1,arg2,arg3)

int *arg1,*arg2,*arg3;
{
	printf("n%d %d %d\n",*arg1,*arg2,*arg3);
	fflush(stdout);
	return;	
}

/* print for puzzle room */
puzout_(p)

char *p;
{

printf("       |%c%c %c%c %c%c|\n",p[0],p[0],p[1],p[1],p[2],p[2]);
printf("  WEST |%c%c .. %c%c| EAST\n",p[3],p[3],p[4],p[4]);
printf("       |%c%c %c%c %c%c|\n",p[5],p[5],p[6],p[6],p[7],p[7]);
fflush(stdout);
return;
}

/* output general string (echo room) */
outstr_(ptr,len)

int  *len;
char *ptr[];
{
	printf("%.*s\n",*len,ptr);
	fflush(stdout);
	return;
}


/* print a prompt */
prompt_()
{
	printf(" > ~");
	fflush(stdout);
	return;
}

/* terminate the game */
exit_()
{
	fprintf(stderr,"The game is over\n");
	exit(0);
}

/*	read a character */
rdchr_(cptr)

char *cptr;
{
static int ch;

	if((ch = getchar()) == EOF){
		fprintf(stderr,"EOF on input\n");
		exit_();
	}
	else 	*cptr = ch;
	return;
}

/* read a line from std input */
rdlin_(sptr,cntptr)

int *cntptr;
char *sptr;
{
static int chr;
	*cntptr = 0;
	while ((chr = getchar()) != EOF){
		if((chr >= 97) && (chr <= 122))	/* convert to uc */
			chr -= 32;
		if (chr == 10 ){ 		/* quit if newline */
			*sptr++ = '\0';
			return;
		}
		if ((chr == 32) && (*cntptr == 0))  /* rm lead blank */
			continue;
		if (*cntptr >= 78)
			continue;
		*sptr++ = chr;
		(*cntptr)++;
	}
	fprintf(stderr,"EOF on input\n");
	exit_();
}

/*	print cure time */
cured_(time)

int *time;
{
	printf(" You will be cured in %d moves\n",*time);
	fflush(stdout);
	return;
}

/* print the score */
pscore_(score,max,moves)

int *score, *max, *moves;
{
printf(" Your current score is %d out of %d points in %d moves.\n",*score, *max, *moves);
	fflush(stdout);
	return;
}

/* BUG-- REPORT FATAL SYSTEM ERROR 
C
C CALLED BY--
C
C	CALL BUG(NO,PAR)
C
C      note: return if DBGFLG set is not implemented
C*/
bug_(a,b)

int *a,*b;
{
	fprintf(stderr,"Program error %d ; Parameter %d\n",*a,*b);
	exit(0);
}


/* send restore message  */
restor_()
{
	printf(" Restore by starting with 'dungeon r'\n");
	return;
}

/* password output */
voice_(sp1, sp2)

char *sp1, *sp2;
{
	printf(" A Hollow voice replies: %6.6s %6.6s\n", sp1, sp2);
	return;
}

/* print version */
prvers_(v1, v2, v3)

int *v1, *v2;
char *v3;
{
	printf(" V%1d.%2d%c\n", *v1, *v2, *v3);
	return;
}

/* dummy stub for game debugger */
nogdt_()
{
	/* debugger deleted to save room */
	printf(" Sorry, no debugger available in this version.\n");
	return;
}
