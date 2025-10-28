# If the first argument is good pass the rest of the line to the target
ifeq (chibicc,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "good"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(RUN_ARGS):;@:)
  .PHONY: $(RUN_ARGS)
endif

# make chibicc xn
chibicc:
	go build -o chibicc ./$(wordlist 2,2,$(MAKECMDGOALS))

test:
	@-./test.sh || true

clean:
	rm -f chibicc *.o *.s tmp*

.PHONY: chibicc clean test
