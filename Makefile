all:
	@${MAKE} compile || echo "Compilation failed.  Do you need to run 'make deps' first?"

deps:
	./rebar get-deps
	./rebar update-deps
	./rebar compile

compile:
	./rebar compile skip_deps=true

# Create a releaseable (but unpacked) node
rel: compile
	(cd rel && ../rebar generate)

rel-windows: compile
	(cd rel-windows && ../rebar generate)

deployable: rel-windows
	rm sms_gateway.zip sms_gateway-win64.zip || true
	find rel-windows -name 'erts-*'
	cd rel-windows ; zip -r ../sms_gateway-win64.zip sms_gateway

clean:
	./rebar clean skip_deps=true

# Delete the generated runnable node
relclean:
	rm -r rel/sms_gateway rel-windows/sms_gateway

test:
	./rebar eunit skip_deps=true

test.suite.%:
	./rebar eunit skip_deps=true suites=$*

#
check: xref

xref:
	./rebar xref skip_deps=true

#
doc:
	cd doc && for i in *.asciidoc ; do asciidoc "$$i" ; done

# Node admin targets
node-console:
	./rel/sms_gateway/bin/sms_gateway console
node-start:
	./rel/sms_gateway/bin/sms_gateway start
node-stop:
	./rel/sms_gateway/bin/sms_gateway stop

.PHONY: all deps compile rel rel-windows clean relclean test doc
.PHONY: check xref
.PHONY: node-console node-start node-stop
