-module(geohash_int).

-export([define_world/5,
         fast_encode/4,
         encode/4]).

-on_load(init/0).

-define(MAX_LEVEL, 30).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).


%% Hashing works like this:
%%  Divide the world into 4 buckets. As we can see,
%%  there are 2 different modes to divide world: Z mode and N mode
%%  Label each one as such:
%%  +-------+-------+           +-------+-------+
%%  |       |       |           |       |       |
%%  |       |       |           |       |       |
%%  | 0,1   | 1,1   |           | 1,0   | 1,1   |
%%  |-------+-------|     OR    |-------+-------|
%%  |       |       |           |       |       |
%%  |       |       |           |       |       |
%%  | 0,0   | 1,0   |           | 0,0   | 0,1   |
%%  +-------+-------+           +-------+-------+
%%       N mode                       Z mode
%%
-spec define_world(float(),float(),float(),float(),atom()) -> {ok,map()}|{error,atom()}.
define_world(S,N,W,E,Mode) when N > S, E > W ->
    {ok, #{north => N,
           south => S,
           west  => W,
           east  => E,
           mode  => Mode}};
define_world(_,_,_,_,_) ->
    {error, insane_world}.

-spec fast_encode(map(),float(),float(),1..?MAX_LEVEL) -> {ok,map()} | {error, atom()|tuple()}. 
fast_encode(#{east:=E, north:=N, south:=S, west:=W, mode:=M}, Latitude, Longitude, Level) ->
    ?nif_stub.

-spec encode(map(),float(),float(),1..?MAX_LEVEL) -> {ok,map()} | {error, atom()|tuple()}. 
encode(_,_,_,Level) when Level > ?MAX_LEVEL ->
    {error, too_small_world};
encode(#{east:=E, north:=N, south:=S, west:=W, mode:=M}, Latitude, Longitude, Level) 
        when W =< Longitude, Longitude =< E, S =< Latitude, Latitude =< N ->
    case M of
    'N' ->
        H = do_encode(W, E, Longitude, {S,N,Latitude}, 2*Level, 0),
        {ok, #{bits=>H, level=>Level}};
    'Z' ->
        H = do_encode(S, N, Latitude, {W,E,Longitude}, 2*Level, 0),
        {ok, #{bits=>H, level=>Level}};
    Other ->
        {error, {unknow_world_mode, Other}}
    end;
encode(_,_,_,_) ->
    {error, out_of_world}.

% @private
do_encode(_Min, _Max, _L, _, 0, H) ->
    H;
do_encode(Min, Max, L, {WS,EN,LonLat}, Step, H) when (Max - L) >= (L - Min) ->
    do_encode(WS, EN, LonLat, {Min, (Min + Max) / 2.0, L}, Step - 1, H bsl 1);
do_encode(Min, Max, L, {WS,EN,LonLat}, Step, H)  ->
    do_encode(WS, EN, LonLat, {(Min + Max) / 2.0, Max, L}, Step - 1, (H bsl 1) + 1).


