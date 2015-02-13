% Copyright (C) 2014 EDF R&D

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


% This module centralises city descriptions.
%
-module(city_descriptions).


% For #city_description{}:
-include("class_CityGenerator.hrl").


-export([ get_description_for/1, to_string/1 ]).



-spec get_description_for( string() ) -> class_CityGenerator:city_description().
get_description_for( _CityName="Yzeure" ) ->

	% km²:
	Area = 1364.24,

	% Meters:
	SideLen = erlang:round( class_CityGenerator:area_to_side_length( Area ) ),

	MinAltitude = 217,
	MaxAltitude = 278,

	% This city happens to be just translated in terms of altitude from the
	% origin (for X and Y, it is centered):
	%
	#city_description{

			name="Yzeure",

			dimensions={ SideLen, SideLen, MaxAltitude - MinAltitude },

			center={ - SideLen / 2, - SideLen / 2,
					( MinAltitude + MaxAltitude ) / 2 },

			incinerator_count=1,
			landfill_count=2,
			residential_waste_source_count=5,
			industrial_waste_source_count=4,
			road_junction_count=15,
			waste_truck_count=1

										};



get_description_for( _CityName="Orleans" ) ->

	% Km²:
	Area = 23643.24,

	% Meters:
	SideLen = erlang:round( class_CityGenerator:area_to_side_length( Area ) ),

	MinAltitude = 217,
	MaxAltitude = 278,

	% This city happens to be just translated in terms of altitude from the
	% origin (for X and Y, it is centered):
	%
	#city_description{

			name="Orleans",

			dimensions={ SideLen, SideLen, MaxAltitude - MinAltitude },

			center={ - SideLen / 2, - SideLen / 2,
					( MinAltitude + MaxAltitude ) / 2 },

			incinerator_count=18,
			landfill_count=14,
			residential_waste_source_count=450,
			industrial_waste_source_count=130,
			road_junction_count=500,
			waste_truck_count=180

										};



get_description_for( _CityName="Rennes" ) ->

	% Km²:
	Area = 9782643.24,

	% Meters:
	SideLen = erlang:round( class_CityGenerator:area_to_side_length( Area ) ),

	MinAltitude = 217,
	MaxAltitude = 278,

	% This city happens to be just translated in terms of altitude from the
	% origin (for X and Y, it is centered):
	%
	#city_description{

			name="Rennes",

			dimensions={ SideLen, SideLen, MaxAltitude - MinAltitude },

			center={ - SideLen / 2, - SideLen / 2,
					( MinAltitude + MaxAltitude ) / 2 },

			incinerator_count=124,
			landfill_count=59,
			residential_waste_source_count=1700,
			industrial_waste_source_count=430,
			road_junction_count=1950,
			waste_truck_count=1007

										};



get_description_for( _CityName="Paris" ) ->

	% Km²:
	Area = 56172643.24,

	% Meters:
	SideLen = erlang:round( class_CityGenerator:area_to_side_length( Area ) ),

	MinAltitude = 217,
	MaxAltitude = 278,

	% This city happens to be just translated in terms of altitude from the
	% origin (for X and Y, it is centered):
	#city_description{

			name="Paris",

			dimensions={ SideLen, SideLen, MaxAltitude - MinAltitude },

			center={ - SideLen / 2, - SideLen / 2,
					( MinAltitude + MaxAltitude ) / 2 },

			incinerator_count=248,
			landfill_count=218,
			residential_waste_source_count=6500,
			industrial_waste_source_count=980,
			road_junction_count=8500,
			waste_truck_count=4912

						};



get_description_for( _CityName="Beijing" ) ->

	% Km²:
	Area = 218921485.0,

	% Meters:
	SideLen = erlang:round( class_CityGenerator:area_to_side_length( Area ) ),

	MinAltitude = 1937,
	MaxAltitude = 2602,

	% This city happens to be just translated in terms of altitude from the
	% origin (for X and Y, it is centered):
	#city_description{

			name="Beijing",

			dimensions={ SideLen, SideLen, MaxAltitude - MinAltitude },

			center={ - SideLen / 2, - SideLen / 2,
					( MinAltitude + MaxAltitude ) / 2 },

			incinerator_count=672,
			landfill_count=495,
			residential_waste_source_count=20000,
			industrial_waste_source_count=4000,
			road_junction_count=80000,
			waste_truck_count=95100

										}.



% Returns a textual description of specified city.
%
-spec to_string( class_CityGenerator:city_description() ) -> string().
to_string( #city_description{

			name=Name,

			dimensions={ Length, Width, _Height },

			center=_Position,

			incinerator_count=IncCount,
			landfill_count=LandCount,
			residential_waste_source_count=ResidCount,
			industrial_waste_source_count=IndusCount,
			road_junction_count=JuncCount,
			waste_truck_count=TruckCount } ) ->

	{ _CellsPerEdge, CellCount } = class_CityGenerator:get_cell_infos( Length,
																	   Width ),

	Elements = text_utils:strings_to_string( [
		text_utils:format( "~B incinerators", [ IncCount ] ),
		text_utils:format( "~B landfills", [ LandCount ] ),
		text_utils:format( "~B residential waste sources", [ ResidCount ] ),
		text_utils:format( "~B industrial waste sources", [ IndusCount ] ),
		text_utils:format( "~B road junctions", [ JuncCount ] ),
		text_utils:format( "~B waste trucks", [ TruckCount ] ),
		text_utils:format( "1 weather system and ~B weather cells",
						   [ CellCount ] )

											 ],
										   _Bullet="%   + " ),

	text_utils:format( "~s city has:~s~n", [ Name, Elements ] ).
