-module(httpre_http).

-export([
    parse_request/2
]).

%% public
-record(state, {
    state   = request :: request | headers | body,
    buffer  = <<>> :: binary(),
    method,
    uri,
    version,
    headers = [],
    body
}).

parse_request(Bin, undefined) ->
    parse_request(Bin, #state {});
parse_request(Bin, #state {
        state = request,
        buffer = Buffer
    } = State) ->

    Bin2 = <<Buffer/binary, Bin/binary>>,
    case erlang:decode_packet(http_bin, Bin2, []) of
        {ok, {http_request, HttpMethod, HttpUri, HttpVersion}, NewBuffer} ->
            parse_request(NewBuffer, State#state {
                state = headers,
                buffer = <<>>,
                method = HttpMethod,
                uri = HttpUri,
                version = HttpVersion
            });
        {more, _} ->
            {more, State#state {
                buffer = Buffer
            }}
    end;
parse_request(Bin, #state {
        state = headers,
        buffer = Buffer,
        headers = Headers
    } = State) ->

    Bin2 = <<Buffer/binary, Bin/binary>>,
    case erlang:decode_packet(httph_bin, Bin2, []) of
        {ok, {http_header, _, Name, _, Value}, NewBuffer} ->
            BinName = httpre_utils:maybe_atom_to_binary(Name),
            parse_request(NewBuffer, State#state {
                buffer = <<>>,
                headers = [{BinName, Value} | Headers]
            });
        {ok, http_eoh, NewBuffer} ->
            parse_request(NewBuffer, State#state {
                state = body,
                buffer = <<>>
            });
        {more, _} ->
            {more, State#state {
                buffer = Buffer
            }}
    end;
parse_request(Bin, #state {
        state = body,
        buffer = Buffer,
        method = Method,
        uri = Uri,
        headers = Headers
    } = State) ->

    Bin2 = <<Buffer/binary, Bin/binary>>,
    case httpre_utils:lookup(<<"Content-Length">>, Headers) of
        undefined ->
            {ok, {Method, Uri, Headers, Bin2}};
        ContentLengthBin ->
            ContentLength = binary_to_integer(ContentLengthBin),
            case byte_size(Bin2) == ContentLength of
                true ->
                    {ok, {Method, Uri, Headers, Bin2}};
                false ->
                    {more, State#state {
                        buffer = Bin2
                    }}
            end
    end.
