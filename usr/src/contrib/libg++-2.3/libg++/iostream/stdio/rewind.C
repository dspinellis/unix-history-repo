#include <stdioprivate.h>

extern "C" void rewind(FILE* fp)
{
    if (!__validfp(fp))
	return;
    (void)((streambuf*)fp)->seekoff(0, ios::beg);
}
