#include <stdio.h>
#include "Time.h"

Time *localtime(), *gmtime();

Time
LocalTime()
{
	LongCardinal t;

	time(&t);
	return (*localtime(&t));
}

Time
GMTime()
{
	LongCardinal t;

	time(&t);
	return (*gmtime(&t));
}
