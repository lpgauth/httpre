-module(httpre_connection).
-include("httpre.hrl").

-export([
    lookup/2,
    new/1,
    setup_table/0
]).

-define(CONNECTIONS_TABLE, httpre_connections).
-define(RECONNECT_SLEEP, 500).

-record(state, {
    ip,
    port,
    socket,
    buffer = <<>>,
    parser_state
}).

%% public
connect(Ip, Port) ->
    case gen_tcp:connect(Ip, Port, [binary, {packet, 0}]) of
        {ok, Socket} ->
            Socket;
        {error, Reason} ->
            io:format("failed to open socket: ~p~n", [Reason]),
            undefined
    end.

lookup(Id, Opts) ->
    case ets:lookup(?CONNECTIONS_TABLE, Id) of
        [] ->
            Pid = spawn(fun () -> new(Opts) end),
            case ets:insert_new(?CONNECTIONS_TABLE, {Id, Pid}) of
                true -> Pid;
                false ->
                    lookup(Id, Opts)
            end;
        [{Id, Pid}] ->
            Pid
    end.

new(Opts) ->
    Ip = httpre_utils:option("--ip", Opts, ?DEFAULT_DEST_IP),
    Port = httpre_utils:option("--port", Opts, ?DEFAULT_DEST_PORT),
    Port2 = list_to_integer(Port),

    receive_loop(#state {
        ip = Ip,
        port = Port,
        socket = connect(Ip, Port2)
    }).

setup_table() ->
    TableOpts = [named_table, {read_concurrency, true}],
    ets:new(?CONNECTIONS_TABLE, TableOpts).

%% private
receive_loop(#state {
        ip = Ip,
        port = Port,
        socket = undefined
    } = State) ->

    case connect(Ip, Port) of
        undefined ->
            timer:sleep(?RECONNECT_SLEEP),
            receive_loop(State);
        Socket ->
            receive_loop(State#state {
                socket = Socket
            })
    end;
receive_loop(#state {
        socket = Socket,
        buffer = Buffer,
        parser_state = ParserState
    } = State) ->

    receive
        {tcp, _, _} ->
            receive_loop(State);
        {tcp_closed, _} ->
            receive_loop(State#state {
                socket = undefined
            });
        {tcp_payload, Payload} ->
            NewBuffer = <<Buffer/binary, Payload/binary>>,
            case httpre_http:parse(Payload, ParserState) of
                {ok, {Method, Uri, _Headers, _Body}, _} ->
                    case gen_tcp:send(Socket, NewBuffer) of
                        ok ->
                            receive_loop(State#state {
                                buffer = <<>>,
                                parser_state = undefined
                            });
                        {error, _} ->
                            receive_loop(State#state {
                                socket = undefined,
                                buffer = <<>>,
                                parser_state = undefined
                            })
                    end;
                {more, ParserState2} ->
                    receive_loop(State#state {
                        buffer = NewBuffer,
                        parser_state = ParserState2
                    })
            end;
        X ->
            io:format("unknown message: ~p~n", [X]),
            receive_loop(State)
    end.
