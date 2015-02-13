% Modular WOOPER header gathering all serialisation-related exports.



% The conventional atom to mark internal, local processes that must escape the
% serialisation/deserialisation processes:
%
-define( process_restoration_marker, 'wooper_restore_local_process' ).


% The conventional atom to mark internal, local open files (akin to file
% descriptor)s) that must escape the serialisation/deserialisation processes:
%
-define( file_restoration_marker, 'wooper_restore_local_file' ).


% The conventional atom to mark internal, local terms that must escape the
% serialisation process (ex: they may be recreated afterwards):
%
-define( term_restoration_marker, 'wooper_restore_local_term' ).



% Serialisation hooks and all:
%
-export([

		 serialise/3,

		 pre_serialise_hook/1, post_serialise_hook/3,
		 pre_deserialise_hook/2, post_deserialise_hook/1

		 ]).
