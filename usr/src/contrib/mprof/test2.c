#include	<stdio.h>

extern long random();

int
random_flip()
{
    int		r = random();
    
    return (r >> 10) & 0x1;
}

 
enum color { BLUE, RED};


typedef struct {
    enum color c;
    int	data[50];
} widgit;


widgit
*make_widgit()
{
    widgit	*w;

    w = (widgit *) malloc(sizeof(widgit));
    return w;
}

widgit
*make_blue_widgit()
{
    widgit	*w;
    
    w = make_widgit();
    w->c = BLUE;
    return w;
}

widgit
*make_red_widgit()
{
    widgit	*w;
    
    w = make_widgit();
    w->c = RED;
    return w;
}

void
consume_widgit(w)
widgit	*w;
{
    if (w->c == BLUE) {
	/* record blue widgit */
	free(w);
    } else {
	/* record red widgit */
    }
}


#define	NUM_WIDGITS	10000

int
main()
{
    int		i;
    widgit	*wqueue[NUM_WIDGITS];
/*
    extern int keeping_leaks;
    keeping_leaks = 0;
*/
    for (i = 0; i < NUM_WIDGITS; i++)
      if (random_flip())
	wqueue[i] = make_blue_widgit();
      else
	wqueue[i] = make_red_widgit();
    
    for (i = 0; i < NUM_WIDGITS; i++)
      consume_widgit(wqueue[i]);

    return 0;
}
