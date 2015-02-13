% Copyright (C) 2012-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Common type specifications to be used in the ecosystem-based WOOPER examples.


-type age() :: non_neg_integer().

-type gender() :: 'male' | 'female'.

-type color() :: atom().

-type fur_color() :: color().
-type whisker_color() :: color().
-type nozzle_color() :: color().

-type teat_count() :: non_neg_integer().

-type food() :: atom().

-type children_count() :: non_neg_integer().
-type egg_count() :: non_neg_integer().
