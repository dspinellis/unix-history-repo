/*	Apush.c	1.1	86/07/20	*/

#include	"../tahoealign/align.h"

push (infop,longword)	process_info *infop;
int	longword;
/*
/*	Push the given datum on the current stack.
/*
/******************************************/
{

	struct oprnd temp;

	temp.mode = Add | W; 
	sp -= 4; 
	temp.address = sp; 
	temp.length = 4;
	write_back(infop,longword, &temp) ;
}
