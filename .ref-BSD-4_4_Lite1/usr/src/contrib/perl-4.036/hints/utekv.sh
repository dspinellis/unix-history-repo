# XD88/10 UTekV hints by Kaveh Ghazi (ghazi@caip.rutgers.edu)  2/11/92

# The -DUTekV is needed because the greenhills compiler does not have any
# UTekV specific definitions and we need one in perl.h
ccflags="$ccflags -X18 -DJMPCLOBBER -DUTekV"

usemymalloc='y'

# /usr/include/rpcsvc is for finding dbm.h
inclwanted="$inclwanted /usr/include/rpcsvc"

# dont use the wrapper, use the real thing.
cppstdin=/lib/cpp

echo " "
echo "NOTE: You may have to take out makefile dependencies on the files in"
echo "/usr/include (i.e. /usr/include/ctype.h) or the make will fail.  A"
echo "simple 'grep -v /usr/include/ makefile' should suffice."
