#include <stdioprivate.h>

extern "C" int setvbuf(FILE* stream, char* buf, int mode, size_t size)
{
    streambuf* sb = FILE_to_streambuf(stream);
    if (!sb)
	return EOF;
    switch (mode) {
      case _IOFBF:
	sb->linebuffered(0);
	return sb->setbuf(buf, size) == NULL ? EOF : 0;
      case _IOLBF:
	sb->linebuffered(1);
	return sb->setbuf(buf, size) == NULL ? EOF : 0;
      case _IONBF:
	return sb->setbuf(NULL, 0) == NULL ? EOF : 0;
      default:
	return EOF;
    }
}
