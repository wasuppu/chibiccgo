# If the first argument is good pass the rest of the line to the target
ifeq (rvcc,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "good"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(RUN_ARGS):;@:)
  .PHONY: $(RUN_ARGS)
endif

# make rvcc rn
rvcc:
	go build -o rvcc ./$(wordlist 2,2,$(MAKECMDGOALS))

test:
	@-./test.sh || true

clean:
	rm -f rvcc *.o *.s tmp*

.PHONY: rvcc clean test
