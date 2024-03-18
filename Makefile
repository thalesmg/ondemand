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

CONCUERROR_RUN := $(CONCUERROR) \
	--treat_as_normal spindown --treat_as_normal normal --treat_as_normal shutdown \
	--treat_as_normal boom \
	-x logger -x error_handler \
	--pa $(BUILD_DIR)/test/lib/ondemand/ebin \
	--pa $(BUILD_DIR)/test/lib/ondemand/test/extra_src

concuerror = $(CONCUERROR_RUN) -f $(BUILD_DIR)/test/lib/ondemand/test/concuerror_tests.beam -t $(1) || \
	{ cat concuerror_report.txt; exit 1; }

.PHONY: concuerror_tests
concuerror_tests: $(CONCUERROR)
	rebar3 as test compile
	$(call concuerror,spinup_spindown_test)
	$(call concuerror,spindown_already_up_test)
	$(call concuerror,unknown_exit_test)
	$(call concuerror,two_workers_test)
	$(call concuerror,shutdown_spindown_test)

prepare_concuerror: $(CONCUERROR)
