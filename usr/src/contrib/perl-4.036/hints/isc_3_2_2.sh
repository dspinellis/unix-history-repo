set `echo $libswanted | sed -e 's/ x / /' -e 's/ PW / /' -e 's/ malloc / /'`
libswanted="inet malloc $*"
doio_cflags='ccflags="$ccflags -DENOTSOCK=103"'
tdoio_cflags='ccflags="$ccflags -DENOTSOCK=103"'
echo "<net/errno.h> defines error numbers for network calls, but"
echo "the definitions for ENAMETOOLONG and ENOTEMPTY conflict with"
echo "those in <sys/errno.h>.  Instead just define ENOTSOCK here."
