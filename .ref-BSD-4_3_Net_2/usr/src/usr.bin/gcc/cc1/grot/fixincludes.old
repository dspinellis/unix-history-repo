#! /bin/sh
# Install modified versions of certain ANSI-incompatible system header files
# which are fixed to work correctly with ANSI C
# and placed in a directory that GNU C will search.
# This works properly on a Sun in system version 3.4;
# for other versions, you had better check.

# Directory in which to store the results.
LIB=${LIB-/usr/local/lib/gcc-include}

# Make sure it exists.
if [ ! -d $LIB ]; then
  mkdir $LIB || exit 1
fi

# Determine whether this system has symbolic links.
if ln -s X $LIB/ShouldNotExist 2>/dev/null; then
  rm -f $LIB/ShouldNotExist
  LINKS=true
else
  LINKS=false
fi

echo 'Making directories:'
cd /usr/include
if $LINKS; then
  files=`ls -LR | sed -n s/:$//p`
else
  files=`find . -type d -print`
fi
for file in $files; do
  rm -rf $LIB/$file
  if [ ! -d $LIB/$file ]
  then mkdir $LIB/$file
  fi
done

# treetops gets an alternating list
# of old directories to copy
# and the new directories to copy to.
treetops="/usr/include ${LIB}"

if $LINKS; then
  echo 'Making internal symbolic directory links'
  for file in $files; do
    dest=`ls -ld $file | sed -n 's/.*-> //p'`
    if [ "$dest" ]; then    
      cwd=`pwd`
      cd $dest
      if [ $? = 0 ]; then
	dest=`pwd`
	cd $cwd
      fi
      if expr $dest : '[^/.].*' > /dev/null; then
	rmdir ${LIB}/$file > /dev/null 2>&1
	rm -f ${LIB}/$file > /dev/null 2>&1
	ln -s $dest ${LIB}/$file > /dev/null 2>&1
      else				# dont make links outside /usr/include
	treetops="$treetops $dest ${LIB}/$file"
      fi
    fi
  done
fi

set - $treetops
while [ $# != 0 ]; do
  # $1 is an old directory to copy, and $2 is the new directory to copy to.
  echo "Finding header files in $1:"
  cd /usr/include
  cd $1
  files=`find . -type f -print`
  echo 'Checking header files:'
  for file in $files; do
    if egrep '[ 	]_IO[A-Z]*\(|#define._IO|CTRL' $file > /dev/null; then
      echo Fixing $file
      if [ -r $file ]; then
	cp $file $2/$file >/dev/null 2>&1	\
	|| echo "Can't copy $file"
	chmod +w $2/$file
	sed -e '
				   :loop
	  /\\$/			N
	  /\\$/			b loop
	  /[ 	]_IO[A-Z]*(/	s/(\(.\),/('\''\1'\'',/
	  /#define._IO/		s/'\''x'\''/x/g
	  /[^A-Z]CTRL[ 	]*(/	s/\(.\))/'\''\1'\'')/
	  /#define.CTRL/		s/'\''c'\''/c/g
	' $2/$file > $2/$file.sed
	mv $2/$file.sed $2/$file
	if cmp $file $2/$file >/dev/null 2>&1; then
	   echo Deleting $2/$file\; no fixes were needed.
	   rm $2/$file
	fi
      fi
    fi
  done
  shift; shift
done

cd /usr/include

# Fix one other error in this file: a mismatched quote not inside a C comment.
file=sundev/vuid_event.h
if [ -r $file ]; then
  if [ ! -r ${LIB}/$file ]; then
    cp $file ${LIB}/$file >/dev/null 2>&1	\
    || echo "Can't copy $file"
    chmod +w ${LIB}/$file
  fi
fi

if [ -r ${LIB}/sundev/vuid_event.h ]; then
  echo Fixing sundev/vuid_event.h comment
  ex ${LIB}/sundev/vuid_event.h <<EOF
  g/doesn't/s/doesn't/does not/
  wq
EOF
fi

# Deal with yet another challenge, this in X11/Xmu.h
file=X11/Xmu.h
if [ -r $file ]; then
  if [ ! -r ${LIB}/$file ]; then
    mkdir ${LIB}/X11 2>&-
    cp $file ${LIB}/$file >/dev/null 2>&1	\
    || echo "Can't copy $file"
    chmod +w ${LIB}/$file
  fi
fi

if [ -r ${LIB}/$file ]; then
  echo Fixing $file sprintf declaration
  ex ${LIB}/$file <<EOF
  /^extern char \*	sprintf();$/c
#ifndef __STDC__
extern char *	sprintf();
#endif /* !defined __STDC__ */
.
  wq
EOF
fi

echo 'Removing unneeded directories:'
cd $LIB
files=`find . -type d -print | sort -r`
for file in $files; do
  rmdir $LIB/$file > /dev/null 2>&1
done

if $LINKS; then
  echo 'Making internal symbolic non-directory links'
  cd /usr/include
  files=`find . -type l -print`
  for file in $files; do
    dest=`ls -ld $file | sed -n 's/.*-> //p'`
    if expr "$dest" : '[^/].*' > /dev/null; then    
      target=${LIB}/`echo file | sed "s|[^/]*\$|$dest|"`
      if [ -f $target ]; then
        ln -s $dest ${LIB}/$file >/dev/null 2>&1
      fi
    fi
  done
fi

exit 0
