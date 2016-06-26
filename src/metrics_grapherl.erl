

%% @doc metrics handler is https://github.com/processone/grapherl

%% Created by benoitc on 26/06/16.


-module(metrics_grapherl).
-author("Benoit Chesneau").

%% API
-export([init/0, new/3, update/3]).

-export([send_metrics/3]).

init() ->
  Peer = application:get_env(barrel_metrics, grapherl_peer, {127,0,0,1}),
  Port =  application:get_env(barrel_metrics, grapherl_port, 11111),
  Host =  application:get_env(barrel_metrics, host, undefined),
  #{ peer => parse_address(Peer), port => Port, host => Host}.

new(counter, _Name, _Config) -> ok;
new(gauge, _Name, _Config) -> ok;
new(_, _, _) -> {error, unsupported_type}.

update(Name, Probe, Config) ->
  spawn(?MODULE, send_metrics, [Name, Probe, Config]).


parse_address({_, _, _, _}=Addr) -> Addr;
parse_address({_, _, _, _, _, _, _, _}= Addr) -> Addr;
parse_address(S) when is_binary(S) ->
  parse_address(binary_to_list(S));
parse_address(S) ->
  {ok, Addr} = inet:parse_address(S),
  Addr.

send_metrics(Name, Probe, Config) ->
  #{ peer := Peer, port := Port} = Config,
  [_, NodeId] = binary:split(atom_to_binary(node(), latin1), <<"@">>),
  BaseId = case maps:get(host, Config, undefined) of
             undefined -> << NodeId/binary, "." >>;
             Host -> << Host/binary, NodeId/binary, "." >>
           end,
  DateTime = erlang:universaltime(),
  UnixTime = calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200,
  TS = integer_to_binary(UnixTime),

  case gen_udp:open(0) of
    {ok, Socket} ->
      Data = case Probe of
               {c, I} ->
                 << BaseId/binary, (list_to_binary(Name))/binary, ":c/", TS/binary, ":",
                   (integer_to_binary(I))/binary >>;
               Val ->
                 << BaseId/binary, (list_to_binary(Name))/binary, ":g/", TS/binary, ":",
                   (integer_to_binary(Val))/binary >>
             end,
      try gen_udp:send(Socket, Peer, Port, Data)
      after
        gen_udp:close(Socket)
      end;
    Error ->
      error_logger:error_msg("can not open udp socket to grapherl: ~p", [Error])
  end.


