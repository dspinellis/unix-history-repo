: loop
if $1x = x exit
echo $1:
diff $1 /mntdv8/eric/trek/$1
shift
goto loop
