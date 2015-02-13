% Copyright (C) 2008-2014 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)




% The name under which the time manager process is to be registered:
%
% (this information must be shared with its clients)
%
-define(time_manager_name,sim_diasca_time_manager).


% Units of virtual seconds, i.e. seconds in simulation time.
%
% Note: these are strictly positive floating-point values; ex: 0.001 seconds
% (1ms).
%
-type virtual_seconds() :: float().



% Duration, in virtual time, of a fundamental tick of the simulation.
% It must be a strictly positive floating-point value.
%
% This duration is expressed in seconds; as it is a floating-point value, a tick
% may last (in simulation time, as opposed to wallclock time) for less than one
% virtual second.
%
% For example, if duration is 20ms (i.e. 0.02 virtual second), then the
% simulation frequency will be 50Hz.
%
% If the duration of a basic time step is 5 minutes (i.e. 300 virtual seconds),
% then the simulation frequency will be 1/300Hz.
%
% As a consequence, frequencies may be arbitrarily low or high. One should
% nevertheless ensure that rounding errors (ex: when dealing with very low
% frequencies) do not interfere. The conversion functions between virtual
% durations and tick counts acts as a safety net (they ensure that the relative
% error remains smaller than a threshold, either a default one or a user-defined
% one).
%
%-define(default_tick_duration,300).
-define(default_tick_duration,0.02).



% We can be in interactive mode without having to make the real and virtual
% units correspond one-to-one: virtual time can be scaled up or down compared to
% the time base (usually, real time).
%
% For example, 1 second in user (wall-clock) time may represent 1 simulated day
% (thus for a value of 3600*24), while still having a simulation running in
% interactive mode.
%
-define(scale_factor_for_interactive_time,1.0).


-type troubleshooting_mode() :: 'enabled' | 'disabled'.



% Allows to describe the evaluation mode of the simulation:
%
-type evaluation_mode() :: 'fastest' | 'reproducible' | 'ergodic'.


-type evaluation_requested_properties() :: evaluation_mode()
		| { 'reproducible', random_utils:seed() }.



% Allows to store most general simulation settings.
%
-record( simulation_settings, {


	% The name of that simulation:
	simulation_name = "Sim-Diasca Simulation" :: string(),


	% The duration (in floating-point virtual seconds) of a fundamental tick in
	% the simulation.
	%
	% Default frequency (50Hz, i.e. a duration of 0.02s) is fine for a large
	% number of uses.
	%
	tick_duration = ?default_tick_duration :: virtual_seconds(),


	% Whether the simulation should run in interactive or batch mode:
	interactivity_mode = batch :: class_TimeManager:interactivity_mode(),


	% Whether targeting reproducibility (with or without a seed) or ergodicity:
	evaluation_mode = reproducible :: evaluation_requested_properties(),


	% Lists the files (if any) that will be read in order to create at least at
	% part of the initial state of the simulation:
	%
	initialisation_files = [] :: [ file_utils:path() ],


	% The outputs that this simulation regards as results that are actually
	% needed are the ones that correspond to the result specification for that
	% simulation.
	%
	% It is:
	%
	% - either 'all_outputs', to retrieve results from all producers, with their
	% default settings (for larger simulations the volume of the results is
	% quickly overwhelming and a waste of resource)
	%
	% - or 'no_output' to retrieve no result from any producer (should be used
	% for debugging only, as a simulation exhibiting no result is of little
	% interest)
	%
	% - or 'all_basic_probes_only', to retrieve results from all plain probes
	% (with default basic probe settings), and only from them
	%
	% - or 'all_virtual_probes_only', to retrieve results from all virtual
	% probes (with default virtual proble settings) hosted by data-logger(s),
	% and only from these probes
	%
	% - or, in the general case, a list of options, among:
	%
	%  - { targeted_patterns, TargetPatterns } where TargetPatterns is a list of
	%  elements, each being:
	%
	%	 - either a standalone regular expression pattern, expressed as a plain
	%	 string, that allows to select which outputs are to be promoted to
	%	 results; no option is specified here, thus default ones will be used
	%
	%	 - or a pair, whose first element is such a regular expression pattern,
	%	 and second one is either a standalone option (specified as an atom,
	%	 among 'data_only', 'plot_only', and 'data_and_plot', depending on what
	%	 is requested by the user) or a list of such corresponding producer
	%	 options
	%
	%  - { blacklisted_patterns, BlacklistPatterns } where BlacklistPatterns is
	%  a list of regular expression patterns, expressed as plain strings,
	%  allowing to remove elements among the ones that the previous targeted
	%  patterns selected
	%
	% As a consequence, each output in turn will be matched against each
	% targeted pattern; as soon as the name of this output matches one of these
	% patterns, it will be selected, with the corresponding options (if any);
	% then it will be matched in turn against each of the blacklisted patterns;
	% as soon as the output name matches one of these patterns, it will be
	% removed.
	%
	% At the end, the selected results are the targeted non-blacklisted outputs,
	% and only them.
	%
	% Ex: result_specification = [
	%
	%   { targeted_patterns, [ {"*-case-A-*",plot_only}, "*-case-B-*",
	%						   {"my-test-probe",[data_and_plot]} ] },
	%
	%   { blacklisted_patterns, [ "*-emitter-(first|second)-*" ] }
	%
	%						   ]
	%
	% Note that if strings are not separated by commas (ex: [ "aaa" "bbb" ],
	% instead of [ "aaa", "bbb" ]), then they will be concatenated by the
	% preprocessor and be equivalent to "aaabbb" (which, in the general case,
	% leads to different selections than [ "aaa", "bbb" ]).
	%
	% Note also that the patterns are checked against the name of the output
	% (ex: "my interesting probe"), not against its translation to be a proper
	% filename (ex: "my_interesting_probe"). As a result, the corresponding
	% pattern should target the former, not the latter, as it would not match
	% otherwise.
	%
	% Patterns are to be expressed according to the “Perl Compatible Regular
	% Expressions” conventions, or PCRE for short.
	% For more information, see following cheat sheet:
	% www.bitcetera.com/page_attachments/0000/0030/regex_in_a_nutshell.pdf
	%
	% See also: http://erlang.org/doc/man/re.html
	%
	result_specification = all_outputs ::
	  class_ResultManager:result_specification(),


	% Tells whether the troubleshooting mode for models is to be enabled. If
	% yes, then for example a more exhaustive information table about model
	% instances will be kept, so that information about a faulty instance can be
	% retrieved despite its lack of cooperation.
	%
	troubleshooting_mode = enabled :: troubleshooting_mode()


}).


% For convenience:
-type simulation_settings() :: #simulation_settings{}.
