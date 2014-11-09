-module(httpre).
-include("httpre.hrl").

-export([
    main/1
]).

%% public
main(Args) ->
    Args2 = [[Arg, " "] || Arg <- Args],
    Args3 = binary_to_list(iolist_to_binary(Args2)),
    try play(docopt:docopt(?DOCOPT_OPT, Args3))
    catch
        throw:_ ->
            io:format("~s~n", [?DOCOPT_OPT])
    end.

%% private
play(Opts) ->
    File = filename:absname(httpre_utils:lookup("<pcap_file>", Opts)),
    case pcapfile:open(File) of
        {ok, _Headers, IODevice} ->
            httpre_connection:setup_table(),
            read_file(IODevice, Opts);
        {error, Reason} ->
            io:format("failed to open pcap file: ~p~n", [Reason]),
            ok
    end.

read_file(IODevice, Opts) ->
    case pcapfile:next(IODevice) of
        {ok, #pcap_record {payload = Payload}} ->
            case pkt:decapsulate(Payload) of
                [#ether {}, #ipv4 {}, #tcp {sport = SrcPort}, TcpPayload] ->
                    tcp_payload(TcpPayload, SrcPort, Opts);
                _ ->
                    ok
            end,
            read_file(IODevice, Opts);
        eof ->
            ok
    end.

tcp_payload(<<>>, _, _) ->
    ok;
tcp_payload(<<0,0,0,0,0,0>>, _, _) ->
    ok;
tcp_payload(TcpPayload, SrcPort, Opts) ->
    Pid = httpre_connection:lookup(SrcPort, Opts),
    Pid ! {tcp_payload, TcpPayload}.
