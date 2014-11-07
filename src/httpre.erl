-module(httpre).
-include("httpre.hrl").


-export([
    main/1
]).

%% public
main(Args) ->
    Args2 = [[Arg, " "] || Arg <- Args],
    Args3 = binary_to_list(iolist_to_binary(Args2)),
    try
        Opts = docopt:docopt(?DOCOPT_OPT, Args3),
        PcapFile = filename:absname(lookup("<pcap_file>", Opts)),
        recap(PcapFile, Opts)
    catch
        throw:_ ->
            io:format("~s~n", [?DOCOPT_OPT])
    end.

%% private
recap(File, Opts) ->
    case pcapfile:open(File) of
        {ok, _Headers, IODevice} ->
            TableOpts = [named_table, {read_concurrency, true}],
            ets:new(?CONNECTIONS_TABLE, TableOpts),
            read_pcap_file(IODevice, Opts);
        {error, Reason} ->
            io:format("failed to open pcap file: ~p~n", [Reason]),
            ok
    end.

read_pcap_file(IODevice, Opts) ->
    case pcapfile:next(IODevice) of
        {ok, #pcap_record {payload = Payload}} ->
            case pkt:decapsulate(Payload) of
                [#ether {}, #ipv4 {}, #tcp {sport = SrcPort}, TcpPayload] ->
                    send_tcp_payload(TcpPayload, SrcPort, Opts);
                _ ->
                    ok
            end,
            read_pcap_file(IODevice, Opts);
        eof ->
            ok
    end.

send_tcp_payload(<<>>, _, _) ->
    ok;
send_tcp_payload(<<0,0,0,0,0,0>>, _, _) ->
    ok;
send_tcp_payload(TcpPayload, SrcPort, Opts) ->
    Pid = connection_pid(SrcPort, Opts),
    Pid ! {tcp_payload, TcpPayload}.

connection_pid(SrcPort, Opts) ->
    case ets:lookup(?CONNECTIONS_TABLE, SrcPort) of
        [] ->
            Pid = spawn(fun () -> new_connection(Opts) end),
            true = ets:insert(?CONNECTIONS_TABLE, {SrcPort, Pid}),
            Pid;
        [{SrcPort, Pid}] ->
            Pid
    end.

new_connection(Opts) ->
    Ip = option("--ip", Opts, ?DEFAULT_DEST_IP),
    Port = option("--port", Opts, ?DEFAULT_DEST_PORT),
    Port2 = list_to_integer(Port),

    case gen_tcp:connect(Ip, Port2, [binary, {packet, 0}]) of
        {ok, Socket} ->
            receive_http_payloads(#connection {
                socket = Socket,
                ip = Ip,
                port = Port2
            });
        {error, Reason} ->
            io:format("failed to open socket: ~p~n", [Reason])
    end.

receive_http_payloads(#connection {
        socket = Socket,
        buffer = Buffer,
        size = Size
    } = Connection) ->

    receive
        {tcp, _, _} ->
            receive_http_payloads(Connection);
        {tcp_closed, _} ->
            receive_http_payloads(Connection);
        {tcp_payload, <<"GET ", _/binary>> = HttpPayload} ->
            ok = gen_tcp:send(Socket, HttpPayload),
            receive_http_payloads(Connection);
        {tcp_payload, <<"POST ", _/binary>> = Payload} ->
            [LineHeaders, Body] = binary:split(Payload, <<"\r\n\r\n">>),
            [Line, Headers] = binary:split(LineHeaders, <<"\r\n">>),
            receive_http_payloads(Connection);
        {tcp_payload, Payload} ->
            receive_http_payloads(Connection);
        X ->
            io:format("unknown message: ~p~n", [X]),
            receive_http_payloads(Connection)
    end.

%% utils
lookup(Key, Proplist) ->
    lookup(Key, Proplist, undefined).

lookup(Key, Proplist, Default) ->
    case lists:keyfind(Key, 1, Proplist) of
        false ->
            Default;
        {_, Value} ->
            Value
    end.

option(Key, Opts, Default) ->
    case lookup(Key, Opts) of
        false ->
            Default;
        undefined ->
            Default;
        Value ->
            Value
    end.
