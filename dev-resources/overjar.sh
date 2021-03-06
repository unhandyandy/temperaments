#!/bin/sh
echo "overjar - packaging overtone projects into standalone uberjars"
echo

PROJECT=`basename $PWD`
if [ "$1" != "" ]; then
  MAIN="$1"
else
  MAIN="$PROJECT.core"
  echo "Assuming your :main to be $MAIN - if it is different please invoke:"
  echo "$ overjar <your.main.namespace>"
  echo
fi
echo "Uberjarring $PROJECT - this will only work if the :main is ^:skip-aot"
lein uberjar > /dev/null || exit $?
UBERJAR=`basename target/uberjar/*-standalone.jar`
echo "Created $UBERJAR"
mv "target/uberjar/$UBERJAR" "target/$UBERJAR"
# playing it safe viz. overwriting existing jars:
#cp -iv "target/$UBERJAR" "$UBERJAR"
#cmp --silent "$UBERJAR" "target/$UBERJAR"
#if [ $? -ne 0 ]; then
#  echo "Aborting uberjar build"
#  exit 1
#fi
echo "Compiling $MAIN"
lein compile "$MAIN" 2> /dev/null # we expect this to fail
echo "Re-packing $UBERJAR"
exit `jar uf target/$UBERJAR -C "target/base+system+user+dev/classes" "$PROJECT"`
