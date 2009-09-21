COMPI = scalac
FLAGS = -deprecation # -optimise

VPATH = scurry/rts scurry/lib scurry/lib/helpers scurry/test

%.class: %.scala
	$(COMPI) $(FLAGS) $<

all:
	find . -name "*.scala" | xargs $(COMPI) $(FLAGS)

clean:
	git clean -fX

examples: all
	scala scurry.test.SmallExamples

rts: exp_and_task SequentialEvaluators.class

exp_and_task:
	$(COMPI) $(FLAGS) scurry/rts/Expression.scala scurry/rts/Task.scala

SequentialEvaluators.class: exp_and_task Normalise.class

lib: helpers Bool.class Integer.class Lists.class

Bool.class: exp_and_task Module.class
Integer.class: exp_and_task Module.class Bool.class
Lists.class: exp_and_task Module.class Bool.class

helpers: Module.class Normalise.class

Module.class: exp_and_task
Normalise.class: exp_and_task Module.class

