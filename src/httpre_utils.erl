-module(httpre_utils).
-compile(export_all).

lookup(Key, Proplist) ->
    lookup(Key, Proplist, undefined).

lookup(Key, Proplist, Default) ->
    case lists:keyfind(Key, 1, Proplist) of
        false -> Default;
        {_, Value} -> Value
    end.

maybe_atom_to_binary(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
maybe_atom_to_binary(Name) when is_binary(Name) ->
    Name.

option(Key, Opts, Default) ->
    case lookup(Key, Opts) of
        false -> Default;
        undefined -> Default;
        Value -> Value
    end.
