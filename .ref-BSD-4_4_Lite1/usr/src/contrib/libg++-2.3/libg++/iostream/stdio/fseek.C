#include <stdioprivate.h>

extern "C" int fseek(FILE* fp, long int offset, int whence)
{
    if (!__validfp(fp))
	return EOF;
    streampos pos = ((streambuf*)fp)->seekoff(offset, (_seek_dir)whence);
    return pos == (streampos)(EOF) ? EOF : 0;
}
