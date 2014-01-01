
bin/Exp.class: *.scala bin
	scalac -d bin *.scala

bin:
	mkdir bin

run:
	@scala -cp bin AbstractCESKEvaluator 


clean:
	cleanup
	rm -rfv bin
