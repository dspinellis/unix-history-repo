#include <sys/types.h>
#include <sys/stat.h>

access(name, mode)
{
	struct stat foo;

	return(stat(name, &foo));
}
