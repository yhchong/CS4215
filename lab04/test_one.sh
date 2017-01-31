echo "compiling $1.epl using epli"
./epli $1.epl > out.$1

cat out.$1
printf "\n"
