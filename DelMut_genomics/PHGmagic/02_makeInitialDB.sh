#export PATH=/programs/sqlite-tools-linux-x86-3320100/bin:$PATH

WORKING_DIR=/workdir/hac89
DOCKER_CONFIG_FILE=/workdir/phg/config.txt

docker1 run --name create_initial_db --rm \
    -v ${WORKING_DIR}/:/phg/ \
    -t maizegenetics/phg:1.4 \
    /tassel-5-standalone/run_pipeline.pl -Xmx500G -debug -configParameters ${DOCKER_CONFIG_FILE} \
    -MakeInitialPHGDBPipelinePlugin -endPlugin

