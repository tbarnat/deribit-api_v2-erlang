-module(deribit_api_websocket).
-behaviour(gen_server).

-export([
  start/2,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  id = 0,
  state :: ready | not_configured | connecting | upgrading | up | broken,
  owner,
  host,
  port,
  parent,
  connection,
  pids_map = maps:new(),
  notifications_pid = null,
  last_pong
}).

-define(TIMEOUT, 5000).
-define(DERIBIT_PING_TIME,   10000).
-define(MAX_NO_PONG_TIME, 30000000).

start(Host, Port) ->
  case gen_server:start(?MODULE, [self(), Host, Port, self()], []) of
    {ok, Pid} ->
      receive
        {Pid, connection_up} ->
          {ok, Pid}
      after
        ?TIMEOUT ->
          Pid ! stop,
          {error, timeout}
      end;
    Error ->
      Error
  end.

init([Owner, Host, Port, Parent]) ->
  monitor(process, Owner),
  {ok, Connection} = gun:open(Host, Port),
  {ok, #state{
    owner = Owner,
    host = Host,
    port = Port,
    connection = Connection,
    state = connecting,
    parent = Parent}}.

handle_call({request, Method, Params, Pid}, _From, #state{ id = Id, pids_map = PidsMap, connection = Connection } = State) ->
  NewId = Id + 1,
  Request = #{
    id => NewId,
    jsonrpc => <<"2.0">>,
    method => list_to_binary(Method),
    params => deribit_api_utils:transform_map_keys_to_atom(Params)
  },
  JsonRequest = jiffy:encode(Request),
  gun:ws_send(Connection, {text, JsonRequest}),
  {reply, NewId, State#state{ pids_map = maps:put(NewId, Pid, PidsMap), id = NewId }};
handle_call(_Request, _From, State) ->
  {reply, no_action, State}.

handle_cast(ping, State) ->
  gun:ws_send(State#state.connection, {text, <<"{\"id\":0,\"action\":\"/api/v1/public/ping\"}">>}),
  {noreply, State};
handle_cast(print_state, State) ->
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(stop, State) ->
  {stop, stopped, State#state{ state = broken }};

handle_info({gun_ws, _Pid, _Ref, {text, Text}}, #state{ pids_map = PidsMap, notifications_pid = NotificationsPid } = State) ->
  Json = jiffy:decode(binary_to_list(Text), [return_maps]),
  Result = case Json of
    #{ <<"method">> := Notification } -> {notification, Notification};
    #{ <<"result">> := ResultData } -> {ok, ResultData};
    #{ <<"error">> := Error } -> {error, Error};
    _ -> {error, <<"unknown response">>}
  end,

  case Result of
    {notification, _} ->
      NotificationsPid ! {self(), Result},
      {noreply, State};
    _ ->
      Id = maps:get(<<"id">>, Json, undefined),
      case Id of
        undefined ->
          {noreply, State};
        _ ->
          Key = maps:get(Id, PidsMap, undefined),
          case Key of
            undefined ->
              ok;
            _ when is_pid(Key) ->
              Key ! {self(), erlang:insert_element(1, Result, Id)};
            _ when is_function(Key) ->
              erlang:apply(Key, [Result])
          end,
          {noreply, State#state{pids_map = maps:remove(Id, PidsMap)}}
      end
  end;
handle_info({gun_upgrade, _ConnPid, _Ref, _Protocols, _Headers}, #state{ parent = Parent } = State) ->
  %% todo it would be nice to set a heartbeat here, so it would reset with every 'gun_ws' msg received
  Parent ! {self(), connection_up},
  {noreply, State#state{state = up, last_pong = os:timestamp()}};
handle_info({gun_up, Connection, http}, State) ->
  gun:ws_upgrade(Connection, "/ws/api/v2/"), %%"/ws/api/v1/"
  {noreply, State#state{ connection = Connection, state = upgrading, last_pong = os:timestamp()}};
handle_info({gun_error, _Pid, _Ref, _Reason} = _Err, #state{} = State) ->
  {stop, connection_broken, State};
handle_info({gun_down, _Pid, _, _, _, _}, #state{} = State) ->
  {stop, connection_down, State#state{ state = broken }};
handle_info({'DOWN',_,process,Pid,_}, #state{ owner = Pid } = State) ->
  {stop, shutdown, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{ connection = undefined }) ->
  ok;
terminate(_Reason, #state{ connection = Connection }) ->
  gun:shutdown(Connection).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
