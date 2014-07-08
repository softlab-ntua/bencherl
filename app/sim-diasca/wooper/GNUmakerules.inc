# Add all WOOPER-specific rules below.


# Outside prerequisites:
# $(WOOPER_SRC)/wooper.hrl wooper_class_manager and modules from 'common'.



class_%.beam: class_%.erl class_%.hrl class_%_dependencies
	@echo "		Compiling WOOPER class with header $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $<


class_%.beam: class_%.erl class_%_dependencies
	@echo "		Compiling WOOPER class $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $<


class_%.beam: class_%.erl class_%.hrl
	@echo "		Compiling WOOPER no-dependency class with header $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $<


# Most frequently-used rule:
class_%.beam: class_%.erl
	@echo "		Compiling WOOPER no-dependency class $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $<


# Allows for example 'make:files([class_XXX]).' or 'make:all().':
# (deactivated, otherwise could be the default rule applied)
#Emakefile:
#	@echo "     Generating Emakefile"
#	@echo "{'*',[debug_info,{i,\"$(WOOPER_INC)\"}]}." > Emakefile
