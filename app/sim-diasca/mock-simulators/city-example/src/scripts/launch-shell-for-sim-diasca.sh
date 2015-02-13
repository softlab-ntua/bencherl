ROOT=$(pwd)/../../..

COMMON=$ROOT/common
WOOPER=$ROOT/wooper
TRACES=$ROOT/traces
SD=$ROOT/sim-diasca/src
MOCK=$ROOT/mock-simulators

DIRS="$MOCK/ssi-test/src $SD/core/src/plugins $SD/core/src $SD/core/src/data-management/data-exchange $SD/core/src/data-management/data-storage $SD/core/src/deployment $SD/core/src/data-management/monitoring/performance $SD/core/src/data-management/monitoring/probe $SD/core/src/data-management/result-management $SD/core/src/instance-creation $SD/core/src/random $SD/core/src/scenario $SD/core/src/scheduling $SD/core/src/resilience $SD/core/generic/src $SD/core/mesh/src $SD/core/space/src $SD/spatial-support $SD/models/electricity/src $SD/models/equipment/src $SD/models/metering/src $SD/models/telecom/src $SD/models/electricity/low-voltage-mesh/src $SD/models/metering/meter/src $SD/models/metering/concentrator/src $SD/models/reliability/failure/src $SD/models/reliability/reparation/src $SD/models/telecom/interferences/src $SD/models/telecom/network/src $SD/models/telecom/point-to-point/src $SD/models/telecom/power-line-communication/src $SD/models/telecom/wireless/src $TRACES/src $MOCK/city-example/src/. $WOOPER/src $WOOPER/examples $MOCK/city-example/src/. $COMMON/src/data-management $COMMON/src/maths $COMMON/src/user-interface $COMMON/src/utils"

SD_VERSION="2.2.5-for-kostis"
CASE_NAME="city_benchmarking_run-$USER"


echo "   Running a properly-set Erlang shell...."

echo "One can experiment with this shell at will (ex: 'spawn( etop, start, [] ).'), then enter: 'city_benchmarking_test:run( tiny, brief ).'. That's it!"

# Not used directly in this script but strictly necessary:
export ERL_EPMD_PORT=4506

# Auto-launch: add -eval 'city_benchmarking_test:run( tiny, brief )'


scheduler_count="$1"

if [ -n "${scheduler_count}" ] ; then

	scheduler_opt="+S $scheduler_count"

fi


erl +W w -pz $DIRS -smp ${scheduler_opt} +K true +A 128 +P 400000 -kernel inet_dist_listen_min 50000 inet_dist_listen_max 55000 -sname $CASE_NAME --sim-diasca-root ../../../sim-diasca --sim-diasca-version $SD_VERSION --batch
