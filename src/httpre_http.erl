-module(httpre_http).

-export([
    parse/1,
    parse/2
]).

%% public
-record(state, {
    state = request,
    buffer = <<>>,
    method,
    uri,
    version,
    headers = [],
    body
}).

parse(Buffer) ->
    parse(Buffer, #state {}).

parse(Buffer, undefined) ->
    parse(Buffer, #state {});
parse(Buffer, #state {state = request} = State) ->
    case erlang:decode_packet(http_bin, Buffer, []) of
        {ok, {http_request, HttpMethod, HttpUri, HttpVersion}, NewBuffer} ->
            parse(NewBuffer, State#state {
                state = headers,
                buffer = NewBuffer,
                method = HttpMethod,
                uri = HttpUri,
                version = HttpVersion
            });
        {more, _} ->
            {more, State#state {
                buffer = Buffer
            }}
    end;
parse(Buffer, #state {
        state = headers,
        headers = Headers
    } = State) ->

    case erlang:decode_packet(httph_bin, Buffer, []) of
        {ok, {http_header, _, Name, _, Value}, NewBuffer} ->
            BinName = httpre_utils:maybe_atom_to_binary(Name),
            parse(NewBuffer, State#state{
                headers = [{BinName, Value} | Headers]
            });
        {ok, http_eoh, NewBuffer} ->
            parse(NewBuffer, State#state {
                state = body
            });
        {more, _} ->
            {more, State#state {
                buffer = Buffer
            }}
    end;
parse(Buffer, #state {
        state = body,
        method = Method,
        uri = Uri,
        headers = Headers
    } = State) ->

    ContentLengthBin = httpre_utils:lookup(<<"Content-Length">>, Headers, <<"0">>),
    ContentLength = binary_to_integer(ContentLengthBin),

    case byte_size(Buffer) == ContentLength of
        true ->
            {ok, {Method, Uri, Headers, Buffer}, State};
        false ->
            {more, State#state {
                buffer = <<"">>
            }}
    end.
