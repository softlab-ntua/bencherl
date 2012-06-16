APP_NAME:=rfc4627_jsonrpc
DEPS:=mochiweb-wrapper

UPSTREAM_GIT:=http://github.com/tonyg/erlang-rfc4627.git
UPSTREAM_REVISION:=30c84984f86a1d67083f
WRAPPER_PATCHES:=fix-ets-leak.patch

ORIGINAL_APP_FILE=$(CLONE_DIR)/ebin/$(APP_NAME).app
DO_NOT_GENERATE_APP_FILE=true

## The path to httpd.hrl has changed in OTP R14A and newer. Detect the
## change, and supply a compile-time macro definition to allow
## rfc4627_jsonrpc_inets.erl to adapt to the new path.
ifeq ($(shell erl -noshell -eval 'io:format([list_to_integer(X) || X <- string:tokens(erlang:system_info(version), ".")] >= [5,8]), halt().'),true)
PACKAGE_ERLC_OPTS+=-Dnew_inets
endif
