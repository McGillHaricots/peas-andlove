WORKING_DIR=/workdir/hac89
DOCKER_CONFIG_FILE=/workdir/phg/config.txt

docker1 run --name pipeline_container --rm \
    -v ${WORKING_DIR}/:/phg/ \
    -t maizegenetics/phg:1.3 \
    /tassel-5-standalone/run_pipeline.pl -Xmx500G -debug -configParameters ${DOCKER_CONFIG_FILE} \
    -ImputePipelinePlugin -imputeTarget pathToVCF -endPlugin
