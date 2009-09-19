compile:
	find . -name "*.scala" | xargs scalac -deprecation -optimise

examples: compile
	scala scurry.test.SmallExamples
