-module(deribit_api).

-export([
%% only essentials
  open/0, open/1, open/2, open/3,
  close/1,
  get_account_summary/2, get_account_summary/3,
  getcurrencies/1, getcurrencies/2,
  getinstruments/1, getinstruments/2,
  get_index/2, get_index/3,
  getopenorders/2, getopenorders/3,
  positions/1, positions/2,
  buy/2, buy/3,
  sell/2, sell/3,
  edit/2, edit/3,
  cancel/2, cancel/3,
  cancelall/2, cancelall/3

%%  orderhistory/1, orderhistory/2, orderhistory/3,
%%  tradehistory/2, tradehistory/3,
%%  getorderbook/2, getorderbook/3,
%%  getsummary/2, getsummary/3,
%%  getlasttrades/2, getlasttrades/3,
%%  subscribe/2, subscribe/3,
%%  unsubscribe/1, unsubscribe/2
]).

-type connection()      :: pid().
-type connection_options() :: websocket | http | url().
-type url()             :: string().
-type currency ()       :: btc | eth | usdt.
-type instrument()      :: string() | binary().
-type params()          :: any(). %% It's just a placeholder; The specific params definition is made for no-option function
-type result()          :: {ok, any()} | {error, any()} | reference() | ok.
-type option()          :: {async} | {async, fun( (result()) -> ok )}.
-type options()         :: list(option()).

-spec open() -> Result when
  Result     :: {ok, connection()} | {error, Reason},
  Reason     :: string() | binary().
open() ->
  open("", "", websocket).

-spec open(connection_options()) -> Result when
  Result     :: {ok, connection()} | {error, Reason},
  Reason     :: string() | binary().
open(Mode) ->
  open("", "", Mode).

-spec open(AccessKey, AccessSecret) -> Result when
  AccessKey    :: string(),
  AccessSecret :: string(),
  Result       :: {ok, connection()} | {error, Reason},
  Reason       :: string() | binary().
open(AccessKey, AccessSecret) ->
  open(AccessKey, AccessSecret, websocket).

-spec open(AccessKey, AccessSecret, connection_options()) -> Result when
  AccessKey    :: string(),
  AccessSecret :: string(),
  Result       :: {ok, connection()} | {error, Reason},
  Reason       :: string() | binary().

%%deribit_api_http

open(AccessKey, AccessSecret, http) ->
  open(AccessKey, AccessSecret, "https://www.deribit.com");
open(AccessKey, AccessSecret, websocket) ->
  open(AccessKey, AccessSecret, "wss://www.deribit.com/ws/api/v2");
open(AccessKey, AccessSecret, Url) ->
  UriMap = uri_string:parse(Url),
  Scheme = maps:get(scheme, UriMap, "wss"),
  case Scheme of
    "wss" ->
      Host = maps:get(host, UriMap),
      ConnectionResult = deribit_api_websocket:start(Host, 443),
      case ConnectionResult of
        {ok, ConnectionPid} ->
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
          end;
        Error ->
          Error
      end;
    "ws" ->
      {error, use_secure_websocket_protocol};
    Http when (Http =:= "http" orelse Http =:= "https") ->
      {error, rest_api_not_supported__use_websocket};
    _ ->
      {error, wrong_url}
  end.

-spec close(connection()) -> ok.
close(Connection) ->
  gen_server:stop(Connection).

-spec buy(connection(), Params) -> result() when
    Params     :: #{
                    instrument := instrument(),
                    quantity   := number(),
                    type       => limit | stop_limit | take_limit | market | stop_market | take_market | market_limit,
                    price      := number(),
                    post_only  => boolean(),
                    label      => string() | binary()
                   }.
buy(Connection, Params) when is_map(Params) ->
  request(Connection, "private/buy", Params).
buy(Connection, Params, Options) when is_map(Params), is_list(Options)  ->
  request(Connection, "/api/v1/private/buy", Params, Options).

-spec sell(connection(), Params) -> result() when
  Params     :: #{
                  instrument := instrument(),
                  quantity   := number(),
                  price      := number(),
                  post_only  => boolean(),
                  label      => string() | binary()
                 }.
sell(Connection, Params) when is_map(Params)  ->
  request(Connection, "/api/v1/private/sell", Params).

-spec sell(connection(), Params, options()) -> result() when
  Params     :: #{ instrument := instrument(),
                   quantity   := number(),
                   post_only  => boolean(),
                   label      => string() | binary()
                 }.
sell(Connection, Params, Options) when is_map(Params), is_list(Options)  ->
  request(Connection, "/api/v1/private/sell", Params, Options).

-spec cancel(connection(), integer()) -> result().
cancel(Connection, OrderId) ->
  request(Connection, "/api/v1/private/cancel", #{ orderId => OrderId }).

-spec cancel(connection(), integer(), options()) -> result().
cancel(Connection, OrderId, Options) when is_list(Options) ->
  request(Connection, "/api/v1/private/cancel", #{ orderId => OrderId }, Options).


-spec cancelall(connection(), Type) -> result() when
  Type     :: all | futures | options.
cancelall(Connection, Type) ->
  request(Connection, "/api/v1/private/cancelall", #{ type => Type }).

-spec cancelall(connection(), Type, options()) -> result() when
  Type     :: all | futures | options.
cancelall(Connection, Type, Options) when is_list(Options) ->
  request(Connection, "/api/v1/private/cancelall", #{ type => Type }, Options).

%% ==============================================================

-spec get_account_summary(connection(), Params) -> result() when
  Params :: #{
    currency   := currency(),
    extended   => boolean()
  }.
get_account_summary(Connection, Params) when is_map(Params) ->
  get_account_summary(Connection, Params, []).

-spec get_account_summary(connection(), params(), options()) -> result().
get_account_summary(Connection, Params, Options) when is_list(Options) ->
  request(Connection, "private/get_account_summary", Params, Options).

%% ==============================================================

-spec edit(connection(), Params) -> result() when
  Params     :: #{
                  orderId  := integer(),
                  quantity := number(),
                  price    := number()
                }.
edit(Connection, Params) when is_map(Params)  ->
  request(Connection, "/api/v1/private/edit", Params).

-spec edit(connection(), Params, options()) -> result() when
  Params     :: #{
                  orderId  := integer(),
                  quantity := number(),
                  price    := number()
                }.
edit(Connection, Params, Options) when is_map(Params), is_list(Options)  ->
  request(Connection, "/api/v1/private/edit", Params, Options).

-spec getopenorders(connection(), Params) -> result() when
  Params     :: #{
                  instrument => instrument(),
                  orderId    => integer()
                }.
getopenorders(Connection, Params) when is_map(Params)  ->
  request(Connection, "/api/v1/private/getopenorders", Params).

-spec getopenorders(connection(), Params, options()) -> result() when
  Params     :: #{
                  instrument => instrument(),
                  orderId    => integer()
                }.
getopenorders(Connection, Params, Options) when is_map(Params), is_list(Options)  ->
  request(Connection, "/api/v1/private/getopenorders", Params, Options).

-spec positions(connection()) -> result().
positions(Connection) ->
  request(Connection, "/api/v1/private/positions", #{}).

-spec positions(connection(), options()) -> result().
positions(Connection, Options) ->
  request(Connection, "/api/v1/private/positions", #{}, Options).


-spec getinstruments(connection()) -> result().
getinstruments(Connection) ->
  request(Connection, "/api/v1/public/getinstruments", #{}).

-spec getinstruments(connection(), options()) -> result().
getinstruments(Connection, Options) when is_list(Options) ->
  request(Connection, "/api/v1/public/getinstruments", #{}, Options).

-spec getcurrencies(connection()) -> result().
getcurrencies(Connection) ->
  request(Connection, "/api/v1/public/getcurrencies", #{}).

-spec getcurrencies(connection(), options()) -> result().
getcurrencies(Connection, Options) when is_list(Options) ->
  request(Connection, "/api/v1/public/getcurrencies", #{}, Options).

%% ==============================================================

-spec get_index(connection(), Params) -> result() when
  Params :: #{
    currency   := currency()
  }.
get_index(Connection, Params) ->
  get_index(Connection, Params, []).

get_index(Connection, Params, Options) ->
  request(Connection, "public/get_index", Params, Options).

%% ==============================================================

request(Pid, Action, Data) ->
  deribit_api_utils:request(Pid, Action, Data).
request(Pid, Action, Data, []) ->
  deribit_api_utils:request(Pid, Action, Data);
request(Pid, Action, Data, Options) when is_list(Options) ->
  deribit_api_utils:request(Pid, Action, Data, Options).
