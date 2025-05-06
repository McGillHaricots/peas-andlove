export PATH=/programs/sqlite-tools-linux-x86-3320100/bin:$PATH

WORKING_DIR=/workdir/hac89
DOCKER_CONFIG_FILE=/workdir/phg/config.txt

# The LoadHaplotypesFromGVCFPlugin parameters will be obtained via the config file
docker1 run --name load_haplotypes_container --rm \
    -v ${WORKING_DIR}/:/phg/ \
    -t maizegenetics/phg:1.4 \
    /tassel-5-standalone/run_pipeline.pl -Xmx500G -debug -configParameters ${DOCKER_CONFIG_FILE} \
    -LoadHaplotypesFromGVCFPlugin -endPlugin


