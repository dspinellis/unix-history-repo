# shell script to test some things
set echo
set prompt='% '
date
lisp << 'EOF'
(status ctime)
'EOF'
date
