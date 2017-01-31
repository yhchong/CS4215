echo "compiling $1.epl using epli"
./epli $1.epl > out.$1

echo "Program: "
cat $1.epl

printf "\n\nOutput:"
cat out.$1
