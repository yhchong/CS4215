echo "compiling $1.epl using epl"
./$2eplc $1.epl > out.$1
echo "executin $1 using evm"
./$2evm $1 >> out.$1