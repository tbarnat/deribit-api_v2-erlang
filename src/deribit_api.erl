-module(deribit_api).

-export([
%% only essentials
  open/0, open/1, open/2,
  close/1,
  buy/2, buy/3,
  sell/2, sell/3,
  edit_by_label/2, edit_by_label/3,
  edit/2, edit/3,
  cancel/2, cancel/3,
  cancel_all_by_currency/2, cancel_all_by_currency/3,
  cancel_all_by_instrument/2, cancel_all_by_instrument/3,
  cancel_all/1, cancel_all/2,
  get_account_summary/2, get_account_summary/3,
  get_open_orders_by_currency/2, get_open_orders_by_currency/3,
  get_open_orders_by_instrument/2, get_open_orders_by_instrument/3,
  get_position/2, get_position/3,
  get_positions/2, get_positions/3,
  get_currencies/1, get_currencies/2,
  get_instrument/2, get_instrument/3,
  get_instruments/2, get_instruments/3,
  get_index_price/2, get_index_price/3,
  request/3, request/4
]).

-type connection()            :: pid().
-type connection_options()    :: websocket | http | url().
-type access_key()            :: string().
-type access_secret()         :: string().
-type authorize_credentials() :: { access_key(), access_secret() }.
-type url()                   :: string().
-type currency ()             :: btc | eth | usdt.
-type instrument()            :: string() | binary().
-type instrument_kind()       :: future | option.
-type params()                :: any(). %% This is just a placeholder; The specific params definition is made for a preceding no-option function
-type result()                :: {ok, any()} | {error, any()} | reference() | ok.
-type option()                :: {async} | {async, fun( (result()) -> ok )}.
-type options()               :: list(option()).


-spec open() -> Result when
  Result     :: {ok, connection()} | {error, Reason},
  Reason     :: string() | binary().
open() ->
  open(websocket, {}).


-spec open(Credentials) -> Result when
  Credentials  :: authorize_credentials(),
  Result       :: {ok, connection()} | {error, Reason},
  Reason       :: string() | binary().
open({AccessKey, AccessSecret}) ->
  open(websocket, {AccessKey, AccessSecret}).


-spec open(connection_options(), Credentials) -> Result when
  Credentials  :: authorize_credentials(),
  Result       :: {ok, connection()} | {error, Reason},
  Reason       :: string() | binary().
open(http, Credentials) ->
  open("https://www.deribit.com", Credentials);
open(websocket, Credentials) ->
  open("wss://www.deribit.com/ws/api/v2", Credentials);
open(Url, Credentials) ->
  UriMap = uri_string:parse(Url),
  Scheme = maps:get(scheme, UriMap, "wss"),
  Port = maps:get(port, UriMap, undefined),
  case {Scheme, Port} of
    {"wss", undefined} ->
      open_websocket(UriMap, Credentials, 443);
    {"ws", undefined} ->
      open_websocket(UriMap, Credentials, 80);
    {WsScheme, Port} when (WsScheme =:= "ws" orelse WsScheme =:= "wss") ->
      open_websocket(UriMap, Credentials, Port);
    {Http, Port} when (Http =:= "http" orelse Http =:= "https") ->
      {error, rest_api_not_supported__use_websocket};
    _ ->
      {error, wrong_url}
  end.

open_websocket(UriMap, Credentials, Port) ->
  Host = maps:get(host, UriMap),
  ConnectionResult = deribit_api_websocket:start(Host, Port),
  case ConnectionResult of
    {ok, ConnectionPid} ->
      possibly_authorize_connection(ConnectionPid, Credentials);
    Error ->
      Error
  end.

%% private function
possibly_authorize_connection(ConnectionPid, {}) ->
  {ok, ConnectionPid};
possibly_authorize_connection(ConnectionPid, {AccessKey,AccessSecret}) ->
  AuthParams = #{
    grant_type => <<"client_credentials">>,
    client_id => list_to_binary(AccessKey),
    client_secret => list_to_binary(AccessSecret)
  },
  AuthResult = request(ConnectionPid, "public/auth", AuthParams),
  case AuthResult of
    {ok, _} ->
      {ok, ConnectionPid};
    AuthError ->
      {error, {could_not_auth_the_ws_connection, AuthError}}
  end.

-spec close(connection()) -> ok.
close(Connection) ->
  gen_server:stop(Connection).

%% ==============================================================

-spec buy(connection(), Params) -> result() when
  Params :: #{
    instrument_name   := instrument(),
    amount            := number(),
    type              => limit | stop_limit | take_limit | market | stop_market | take_market | market_limit,
    label             => string() | binary(),
    price             => number(),
    time_in_force     => good_til_cancelled | good_til_day | fill_or_kill | immediate_or_cancel,
    max_show          => number(),
    post_only         => boolean(),
    reject_post_only  => boolean(),
    reduce_only       => boolean(),
    trigger_price     => number(),
    trigger           => index_price | mark_price | last_price,
    advanced          => usd | implv,
    mmp               => boolean()
  }.
buy(Connection, Params) when is_map(Params) ->
  buy(Connection, Params, []).

-spec buy(connection(), params(), options()) -> result().
buy(Connection, Params, Options) when is_map(Params)  ->
  request(Connection, "private/buy", Params, Options).

%% ==============================================================

-spec sell(connection(), Params) -> result() when
  Params :: #{
    instrument_name   := instrument(),
    amount            := number(),
    type              => limit | stop_limit | take_limit | market | stop_market | take_market | market_limit,
    label             => string() | binary(),
    price             => number(),
    time_in_force     => good_til_cancelled | good_til_day | fill_or_kill | immediate_or_cancel,
    max_show          => number(),
    post_only         => boolean(),
    reject_post_only  => boolean(),
    reduce_only       => boolean(),
    trigger_price     => number(),
    trigger           => index_price | mark_price | last_price,
    advanced          => usd | implv,
    mmp               => boolean()
  }.
sell(Connection, Params) ->
  sell(Connection, Params, []).

-spec sell(connection(), params(), options()) -> result().
sell(Connection, Params, Options) ->
  request(Connection, "private/sell", Params, Options).

%% ==============================================================

-spec edit(connection(), Params) -> result() when
  Params :: #{
    order_id          := string(),
    amount            := number(),
    price             => number(),
    post_only         => boolean(),
    reduce_only       => boolean(),
    reject_post_only  => boolean(),
    advanced          => usd | implv,
    trigger_price     => number(),
    mmp               => boolean()
  }.
edit(Connection, Params) ->
  edit(Connection, Params, []).

-spec edit(connection(), params(), options()) -> result().
edit(Connection, Params, Options) ->
  request(Connection, "private/edit", Params, Options).

%% ==============================================================

-spec edit_by_label(connection(), Params) -> result() when
  Params :: #{
  label             := string(),
  instrument_name   := string(),
  amount            := number(),
  price             => number(),
  post_only         => boolean(),
  reduce_only       => boolean(),
  reject_post_only  => boolean(),
  advanced          => usd | implv,
  trigger_price     => number(),
  mmp               => boolean()
  }.
edit_by_label(Connection, Params) ->
  edit_by_label(Connection, Params, []).

-spec edit_by_label(connection(), params(), options()) -> result().
edit_by_label(Connection, Params, Options)  ->
  request(Connection, "private/edit_by_label", Params, Options).

%% ==============================================================

-spec cancel(connection(), Params) -> result() when
  Params :: #{
    order_id   => boolean()
  }.
cancel(Connection, Params) ->
  cancel(Connection, Params, []).

-spec cancel(connection(), params(), options()) -> result().
cancel(Connection, Params, Options) ->
  request(Connection, "private/cancel", Params, Options).

%% ==============================================================

-spec cancel_all_by_instrument(connection(), Params) -> result() when
  Params :: #{
    instrument_name   => string(),
    type              := all | limit | stop
  }.
cancel_all_by_instrument(Connection, Params) ->
  cancel_all_by_instrument(Connection, Params, []).

-spec cancel_all_by_instrument(connection(), params(), options()) -> result().
cancel_all_by_instrument(Connection, Params, Options) ->
  request(Connection, "private/cancel_all_by_instrument", Params, Options).

%% ==============================================================

-spec cancel_all_by_currency(connection(), Params) -> result() when
  Params :: #{
    currency   => currency(),
    kind       := instrument_kind(),
    type       := all | limit | stop
  }.
cancel_all_by_currency(Connection, Params) ->
  cancel_all_by_currency(Connection, Params, []).

-spec cancel_all_by_currency(connection(), params(), options()) -> result().
cancel_all_by_currency(Connection, Params, Options) ->
  request(Connection, "private/cancel_all_by_currency", Params, Options).

%% ==============================================================

-spec cancel_all(connection()) -> result().
cancel_all(Connection) ->
  cancel_all(Connection, []).

-spec cancel_all(connection(), options()) -> result().
cancel_all(Connection, Options) ->
  request(Connection, "private/cancel_all", Options).

%% ==============================================================

-spec get_account_summary(connection(), Params) -> result() when
  Params :: #{
    currency   := currency(),
    extended   => boolean()
  }.
get_account_summary(Connection, Params) ->
  get_account_summary(Connection, Params, []).

-spec get_account_summary(connection(), params(), options()) -> result().
get_account_summary(Connection, Params, Options) ->
  request(Connection, "private/get_account_summary", Params, Options).

%% ==============================================================

-spec get_open_orders_by_instrument(connection(), Params) -> result() when
  Params :: #{
    instrument_name   => string(),
    type              := all | limit | algo_all | stop_all | stop_limit | stop_market | take_all | take_limit | take_market
  }.
get_open_orders_by_instrument(Connection, Params) ->
  get_open_orders_by_instrument(Connection, Params, []).

get_open_orders_by_instrument(Connection, Params, Options) ->
  request(Connection, "private/get_open_orders_by_instrument", Params, Options).

%% ==============================================================

-spec get_open_orders_by_currency(connection(), Params) -> result() when
  Params :: #{
    currency   => currency(),
    kind       := instrument_kind(),
    type       := all | limit | algo_all | stop_all | stop_limit | stop_market | take_all | take_limit | take_market
  }.
get_open_orders_by_currency(Connection, Params) ->
  get_open_orders_by_currency(Connection, Params, []).

get_open_orders_by_currency(Connection, Params, Options) ->
  request(Connection, "private/get_open_orders_by_currency", Params, Options).

%% ==============================================================

-spec get_position(connection(), Params) -> result() when
  Params :: #{
    instrument_name   => string()
  }.
get_position(Connection, Params) ->
  get_position(Connection, Params, []).

-spec get_position(connection(), params(), options()) -> result().
get_position(Connection, Params, Options) ->
  request(Connection, "private/get_position", Params, Options).

%% ==============================================================

-spec get_positions(connection(), Params) -> result() when
  Params :: #{
    currency   => currency(),
    kind       := instrument_kind()
  }.
get_positions(Connection, Params) ->
  get_positions(Connection, Params, []).

-spec get_positions(connection(), params(), options()) -> result().
get_positions(Connection, Params, Options) ->
  request(Connection, "private/get_positions", Params, Options).

%% ==============================================================

-spec get_instrument(connection(), Params) -> result() when
  Params :: #{
    instrument_name := instrument()
  }.
get_instrument(Connection, Params) ->
  get_instrument(Connection, Params, []).

-spec get_instrument(connection(), params(), options()) -> result().
get_instrument(Connection, Params, Options) ->
  request(Connection, "public/get_instrument", Params, Options).

%% ==============================================================

-spec get_instruments(connection(), Params) -> result() when
  Params :: #{
    currency   := currency(),
    kind       => instrument_kind(),
    expired    => boolean()
  }.
get_instruments(Connection, Params) ->
  get_instruments(Connection, Params, []).

-spec get_instruments(connection(), params(), options()) -> result().
get_instruments(Connection, Params, Options) ->
  request(Connection, "public/get_instruments", Params, Options).

%% ==============================================================

-spec get_currencies(connection()) -> result().
get_currencies(Connection) ->
  get_currencies(Connection, []).

-spec get_currencies(connection(), options()) -> result().
get_currencies(Connection, Options) ->
  request(Connection, "public/get_currencies", #{}, Options).

%% ==============================================================

-spec get_index_price(connection(), Params) -> result() when
  Params :: #{
    index_name   => btc_usd | eth_usd | btc_usdt | eth_usdt
  }.
get_index_price(Connection, Params) ->
  get_index_price(Connection, Params, []).

get_index_price(Connection, Params, Options) ->
  request(Connection, "public/get_index_price", Params, Options).

%% ==============================================================

request(Pid, Action, Data) when is_map(Data) ->
  deribit_api_utils:request(Pid, Action, Data).
request(Pid, Action, Data, async) when is_map(Data) ->
  deribit_api_utils:request(Pid, Action, Data, [async]);
request(Pid, Action, Data, {async, Fun}) when is_map(Data) ->
  deribit_api_utils:request(Pid, Action, Data, [{async, Fun}]);
request(Pid, Action, Data, Options) when is_map(Data) and is_list(Options) ->
  deribit_api_utils:request(Pid, Action, Data, Options).
