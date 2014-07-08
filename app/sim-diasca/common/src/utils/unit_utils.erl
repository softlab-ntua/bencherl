% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Wednesday, October 24, 2012


% Gathering of unit management facilities.
%
% All kinds of units are listed here, alongside the reference ones (ex: the
% meter is the unit of length in the International System of Units).
%
% One objective is to be able to specify, instead of mere values (ex: 1.14),
% values with units (ex: { meters, 1.14 }), and possibly to convert them into a
% canonical form transparently.
%
% See unit_utils_test.erl for the corresponding test.
%
-module(unit_utils).



% Time-related section.


% { year(), month(), day() }:
-type date() :: calendar:date().


% { hour(), minute(), second() }:
-type time() :: calendar:time().


-type megaseconds() :: integer().

-type years()        :: integer().
-type days()         :: integer().
-type hours()        :: integer().
-type minutes()      :: integer().
-type seconds()      :: integer().
-type milliseconds() :: integer().
-type microseconds() :: integer().


% Frequency:
-type hertz() :: float().

-type time_reference_unit() :: 'seconds'.

-type time_units() :: time_reference_unit() | 'years' | 'days' | 'hours'
					 | 'minutes' | 'milliseconds' | 'microseconds'.


-export_type([
			  date/0, time/0, megaseconds/0, years/0, days/0, hours/0,
			  minutes/0, seconds/0, milliseconds/0, microseconds/0,
			  hertz/0, time_reference_unit/0, time_units/0
			  ]).



% Length-related section.

-type meters() :: float().

-type millimeters() :: float().
-type int_millimeters() :: integer().

-type length_reference_unit() :: 'meters'.

-type length_units() :: length_reference_unit() | 'millimeters'
   | 'int_millimeters'.


-export_type([
			  meters/0, millimeters/0, int_millimeters/0,
			  length_reference_unit/0, length_units/0
			  ]).



% Speed related section.

-type km_per_hour() :: float().
-type meters_per_second() :: float().

-export_type([ km_per_hour/0, meters_per_second/0 ]).


% Volume-related section.

-type cubic_meters() :: float().
-type litre() :: float().

-type volume_reference_unit() :: cubic_meters().

-type volume_units() :: volume_reference_unit() | 'litre'.


-export_type([
			  cubic_meters/0, litre/0, volume_reference_unit/0, volume_units/0
			  ]).



% Mass-related section.

-type tons() :: float().
-type kilograms() :: float().
-type grams() :: float().

-type mass_reference_unit() :: 'kilograms'.

-type mass_units() :: mass_reference_unit() | 'tons' | 'grams'.


-export_type([
			  tons/0, kilograms/0, grams/0, mass_reference_unit/0, mass_units/0
			  ]).



% Energy-related section (energy, work, heat).

-type joules() :: float().

-type energy_reference_unit() :: 'joules'.

-type energy_units() :: energy_reference_unit().


-export_type([
			  joules/0, energy_reference_unit/0, energy_units/0
			  ]).



% Angle section.

-type radians() :: float().


% Angle in degrees.
%
% Preferably to be kept in [0.0,360.0[.
%
-type degrees() :: float().


% Angle in degrees.
%
% Strictly expected to be in [0,360[.
%
-type int_degrees() :: integer().


-type angle_reference_unit() :: 'radians'.

-type angle_units() :: angle_reference_unit() | 'degrees' | 'int_degrees'.


-export_type([
			  radians/0, degrees/0, int_degrees/0, angle_reference_unit/0,
			  angle_units/0
			  ]).



% All kinds of units:
-type units() :: time_units() | length_units() | volume_units() | mass_units()
				| energy_units() | angle_units().

-export_type([ units/0 ]).




% Conversion section.



% Converting speeds.

-export([ km_per_hour_to_meters_per_second/1,
		  meters_per_second_to_km_per_hour/1 ]).


-spec km_per_hour_to_meters_per_second( km_per_hour() ) -> meters_per_second().
km_per_hour_to_meters_per_second( K ) ->
	( K * 1000 ) / 3600.


-spec meters_per_second_to_km_per_hour( meters_per_second() ) -> km_per_hour().
meters_per_second_to_km_per_hour( M ) ->
	M * 3600 / 1000.
