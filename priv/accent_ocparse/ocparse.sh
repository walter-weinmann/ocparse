#!/bin/bash

>ocparse.log
exec >  >(tee -ia ocparse.log)
exec 2> >(tee -ia ocparse.log >&2)

# ------------------------------------------------------------------------------
# accent
# ------------------------------------------------------------------------------

ACCENT=../accent/accent
ENTIRE=../entire/entire.c
LEX=flex
CC=cc

$ACCENT ocparse.acc

$LEX ocparse.lex

$CC -o ocparse yygrammar.c lex.yy.c auxil.c $ENTIRE

# ------------------------------------------------------------------------------
# amber
# ------------------------------------------------------------------------------

ACCENTHOME=..
ACCENT=$ACCENTHOME/accent/accent
AMBER=$ACCENTHOME/amber/amber.c

set -e
set -x

$ACCENT ocparse.acc

cc -o amber -O3 yygrammar.c $AMBER

./amber each ellipsis examples 100000
