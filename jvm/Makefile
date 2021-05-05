.PHONY : clean

all : main.jar

%.jar : Main.class Runtime.class
	jar cfm $@ manifest.mf Main.class Runtime.class

%.class : %.j
	jasmin $<

%.j : %.java
	javac $<
	java -cp classfileanalyzer.jar ClassFileAnalyzer "$(basename $<).class" > $@

run: main.jar
	java -jar main.jar

clean:
	rm -f *.class *.j main.jar

# EOF
