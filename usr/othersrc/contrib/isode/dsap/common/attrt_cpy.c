#include "quipu/util.h"
#include "quipu/name.h"
#include "quipu/malloc.h"

/* Could turn this all into a macro ! */

#ifdef AttrT_cpy
#undef AttrT_cpy
#endif

AttributeType AttrT_cpy (x)
register AttributeType x;
{
	return x;
}


