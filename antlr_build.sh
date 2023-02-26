alias antlr='java -jar $ANTLR'

rm -rf dafny &&
antlr dafny.g4 -o dafny -no-listener -visitor -Werror &&
cd dafny &&
javac -cp $ANTLR:. *.java
