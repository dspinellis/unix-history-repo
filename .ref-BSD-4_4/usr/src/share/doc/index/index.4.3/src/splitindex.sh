#! /bin/sh
# this should only be run on the dupheads version of the index
for i in a b c d e f g h i j k l m n o p q r s t u v w x y z
do
cat $* | grep -i "^$i" | sort -d -f >$i.sorted
echo $i
done
cat $* | egrep -v "^[a-zA-Z]" | sort -f >specials.sorted
cat  specials.sorted ?.sorted >/tmp/index.merged
