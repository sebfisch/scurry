COMPI = scalac
FLAGS = -deprecation -optimise

VPATH = scurry/rts scurry/lib scurry/lib/helpers scurry/test

%.class: %.scala
	$(COMPI) $(FLAGS) $<

all:
	find . -name "*.scala" | xargs $(COMPI) $(FLAGS)

clean:
	git clean -fX

examples: tests
	scala scurry.test.SmallExamples

tests: rts lib SmallExamples.class

rts: exp_and_task SequentialEvaluators.class

exp_and_task:
	$(COMPI) $(FLAGS) scurry/rts/Expression.scala scurry/rts/Task.scala

SequentialEvaluators.class: exp_and_task Normalise.class

lib: helpers Bool.class Lists.class

Bool.class: exp_and_task Match.class
Lists.class: exp_and_task Match.class Bool.class

helpers: Match.class Normalise.class

Match.class: exp_and_task
Normalise.class: exp_and_task

