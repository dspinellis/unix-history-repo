#

#include "apl.h"

/*
 *	name line
 */

char	headline[] = {
	"a p l ? 1 1    `` 1 april 1980 ``\n"
};

int	cs_size = STKS;		/* Current stack size */

newstak()
{
register struct item **news, **olds;
register i;
struct	 item **oldstak;
unsignd	 diff;

#ifdef TRACE
	aprintf("\nnew stack allocated\n");
#endif

	diff = sp - stack;
	olds = stack;oldstak	= stack;
	stack	= alloc((cs_size+STKS)*sizeof(stack));news = stack;
	for(i=0; i<cs_size; ++i)
		*news++ = *olds++;
	cs_size += STKS;
	afree(oldstak);
	staktop = &stack[cs_size-1];
	sp = &stack[diff];
}
