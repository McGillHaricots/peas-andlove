#!java
WORKING_DIR=/workdir/hac89
DOCKER_CONFIG_FILE=/workdir/phg/config.txt

docker1 run --name test_assemblies --rm  \
    -v ${WORKING_DIR}/:/phg/ \
    -t maizegenetics/phg:1.3 \
    /tassel-5-standalone/run_pipeline.pl -Xmx500G -debug -configParameters ${DOCKER_CONFIG_FILE} \
    -CreateValidIntervalsFilePlugin -intervalsFile /workdir/phg/inputDir/reference/PvulgarisUI111_534_v1.1.CDS.forPHG.bed \
    -referenceFasta /workdir/phg/inputDir/reference/PvulgarisUI111_534_v1.0.fa \
    -mergeOverlaps true \
    -generatedFile /workdir/phg/valid_PvulgarisUI111_BedFile.bed -endPlugin


