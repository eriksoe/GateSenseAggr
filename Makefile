all:
	@${MAKE} compile || echo "Compilation failed.  Do you need to run 'make deps' first?"

deps:
	./rebar get-deps    </dev/null
	./rebar update-deps </dev/null
	./rebar compile     </dev/null

compile:
	./rebar compile skip_deps=true </dev/null

# Create a releaseable (but unpacked) node
rel: compile
	(cd rel && ../rebar generate) </dev/null

clean:
	./rebar clean skip_deps=true </dev/null

# Delete the generated runnable node
relclean:
	rm -r rel/aggrsense rel-windows/aggrsense

test:
	./rebar eunit skip_deps=true </dev/null

test.suite.%:
	./rebar eunit skip_deps=true suites=$* </dev/null

#
check: xref

xref:
	./rebar xref skip_deps=true </dev/null

#
doc:
	cd doc && for i in *.asciidoc ; do asciidoc "$$i" ; done

# Node admin targets
node-console:
	./rel/aggrsense/bin/aggrsense console
node-start:
	./rel/aggrsense/bin/aggrsense start
node-stop:
	./rel/aggrsense/bin/aggrsense stop

.PHONY: all deps compile rel rel-windows clean relclean test doc
.PHONY: check xref
.PHONY: node-console node-start node-stop
