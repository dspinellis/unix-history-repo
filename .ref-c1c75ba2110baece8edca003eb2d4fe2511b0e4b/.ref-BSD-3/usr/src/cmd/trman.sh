sed -n -f /usr/man/man0/m1.sed $1\
|sed -n -f /usr/man/man0/m2.sed \
|sed -f /usr/man/man0/trref.sed
