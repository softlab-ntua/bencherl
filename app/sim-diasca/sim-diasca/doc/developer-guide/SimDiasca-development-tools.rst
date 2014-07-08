
Development Tools For Sim-Diasca
================================


Standard tools: dialyzer, eunit, CommonTest, etc.

http://www.protest-project.eu/projects.html

erl_tidy
--------

The `erl_tidy <http://www.erlang.org/doc/man/erl_tidy.html>`_ module tidies and pretty-prints Erlang source code, removing unused functions, updating obsolete constructs and function calls, etc.

Applied to the Sim-Diasca code, we can see that on the negative side:

 - all empty lines in function bodies are removed
 - only one empty line between functions is kept
 - calls to macros are protected (enclosed in parentheses), but too many parenthesis levels are used (ex: ``?wooper_return_state_result(State, ((?getAttr(initial_tick)) + (?getAttr(stop_tick_offset)))).``)
 - export declarations ``f/n`` may become ``f / n``
 - arguments could be listed more compactly
 - newly indented comments may go past the 80th character
 - code that used to compile sometimes does not compile anymore (ex: deployment_agent.erl)


On a positive side:
 
 - some useful and safe transformations are done
 
So probably that this tool should not be used in our context.



WRANGLER
--------

Refactoring tool and help for development. Nicest features: detects duplicated code.



Tidier
------

Refactoring tool, not very well-known. Can be tested a bit on-line, commercial licenses are available.



QuickCheck
----------

Tool for test. Framework to test code properties, by generating random entries until the target crashes. Then tries to minimise the input that lead to this crash.

Commercial tool, two open-source tools exist: TRIK, Proper (sur github).



Mac Erlang
----------

Model-checker for Erlang. Works by changing the VM, checks concurrency.

