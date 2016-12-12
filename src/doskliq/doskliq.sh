DOSKLIQ=$(realpath $1)
INPUT_PATH=$(realpath $2)
OUTPUT_PATH=$(realpath $3)
cd $OUTPUT_PATH
{ echo "\"$INPUT_PATH/test.cmd\"";
  echo "\"$INPUT_PATH/test.list\""; } | $DOSKLIQ

