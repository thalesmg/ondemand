BUILD_DIR := $(CURDIR)/_build

##########################################################################################
# Concuerror
# Based on https://github.com/emqx/mria scripts
##########################################################################################

CONCUERROR_GIT_REF := 325d479ae8c156a9ea0533e3c6a3bd6b15bddb42

## Patch to work around current concuerror bug
CONCUERROR_PATCH := $(CURDIR)/test/replay_mismatch.patch

CONCUERROR := $(BUILD_DIR)/concuerror/bin/concuerror

$(CONCUERROR):
	mkdir -p _build/concuerror
	cd _build/concuerror && \
	  git init && \
	  git remote add origin https://github.com/parapluu/Concuerror.git && \
	  git fetch --depth=1 origin $(CONCUERROR_GIT_REF) && \
	  git reset --hard FETCH_HEAD && \
	  git apply $(CONCUERROR_PATCH)
	$(MAKE) -C _build/concuerror/

.PHONY: concuerror_tests
concuerror_tests: $(CONCUERROR)
	rebar3 as test eunit
	rebar3 as concuerror,test compile
	test/run_concuerror.sh

prepare_concuerror: $(CONCUERROR)
