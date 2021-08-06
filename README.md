# Erlang library for [Deribit API v2](https://docs.deribit.com/)

## Description

This package is a basic [Deribit API](https://docs.deribit.com/) client.

This package contains module:
* `deribit_api` - [Deribit API](https://docs.deribit.com/) websocket client

### Compile 
```
rebar3 compile
```

### Example

```
{ok, Connection} = deribit_api:open("wss://test.deribit.com/ws/api/v2", {ApiKey, Secret}),
{ok, IndexResult} = deribit_api:get_index(Connection, #{currency => "ETH"}),
IndexPrice = maps:get(<<"ETH">>, IndexResult),
BuySpecs = #{
  instrument_name => "ETH-PERPETUAL",
  amount => 100,
  price => IndexPrice - 50
},
Ref = deribit_api:buy(Connection, BuySpecs, async),
receive
  {Connection, {Ref, Status, Result}} -> io:format("Status: ~p, Result: ~p~n", [Status, Result])
end,
CancelSpec = #{ instrument_name => <<"ETH-PERPETUAL">>},
deribit_api:cancel_all_by_instrument(Connection, CancelSpec, {async,
  fun (SellRes) ->
    io:format("cancel by instrument result: ~p~n" ,[SellRes])
  end}),
```

## API - Client `deribit_api`

### Initializer

  `open()`  
  `open({AccessKey, AccessSecret})`  
  `open(Mode, {AccessKey, AccessSecret})`

  Creates new `ConnectionPid` - connection identifier, prepares connection with server. Currently on `Mode` == websocket is supported

  **Params map values:**

  | Name           | Type               | Definition                                                 |
  |----------------|--------------------|------------------------------------------------------------|
  | `Credentials`  | `{string(), string()}` | Optional, { AccessKey, AccessSecret }, for public method use empty credentials {}   |
  | `Options`      | `websocket`  `https` `string()` | Optional, 'https' currently not supported, for testnet use full url: "wss://test.deribit.com/ws/api/v2" |                                                      |


### Methods

Parameters in all methods which requires `Params` field: 

| Key              | Definition                                                 |
|------------------|------------------------------------------------------------|
| `ConnectionPid`  | Required, connection identifier, from `deribit_api:open`   |
| `Params`         | Required map in some methods, see description of each      |
| `Options`        | Optional, see description below                            |

By analogy for methods which does not require `Params` field:

| Key              | Definition                                                 |
|------------------|------------------------------------------------------------|
| `ConnectionPid`  | Required, connection identifier, from `deribit_api:open`   |
| `Options`        | Optional, see description below                            |

#### Options

All methods accept optional `Options` list as last parameter. Allowed values:

* not provided - method returns:
  * `{error, Message}` - when error occurred, `Message` is error message
  * `{ok, Result}` - when result was received, `Result` is resonse
* `[async]` - result is sent to calling process and `Ref` - request identifier is returned. Sent message format: `{Pid, {Ref, ok | error, Data = any()}`
* `{async, fun:1}` - when result is received `fun` is called. 

### List of methods:

*  Supported method which requires `Params`
  
  `buy`
  `sell`
  `edit_by_label`
  `edit`
  `cancel`
  `cancel_all_by_currency`
  `cancel_all_by_instrument`
  `get_account_summary`
  `get_open_orders_by_currency`
  `get_open_orders_by_instrument`
  `get_position`
  `get_instruments`
  `get_index`
  
  `Params` types defined in source code. Types defined according to api version 2.1.0
    
*   Supported method which does not require `Params`:
  
  `get_currencies`
  `cancel_all`
  
  For more information about deribit's api v2 read [Doc](https://docs.deribit.com/)
