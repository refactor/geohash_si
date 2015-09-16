-module(geohash_int).

-export([define_world/4,
         encode/4]).

%-on_load(init/0).

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

define_world(N,S,W,E) when N > S, E > W ->
    {ok, #{north => N,
           south => S,
           west  => W,
           east  => E}};
define_world(_,_,_,_) ->
    {error, insane_world}.

encode(#{east:=E, north:=N, south:=S, west:=W} = World, Latitude, Longitude, Level) 
    when W =< Longitude, Longitude =< E, S =< Latitude, Latitude =< N ->
    H = encode(W, E, Longitude, {S,N,Latitude}, 2*Level, 0),
    {ok, #{bits=>H, level=>Level}};
encode(_,_,_,_) ->
    {error, out_of_world}.


% @private
encode(Min, Max, L, _, 0, H) ->
    H;
encode(Min, Max, L, {WS,EN,LonLat}, Step, H) when (Max - L) >= (L - Min) ->
    encode(WS, EN, LonLat, {Min, (Min + Max) / 2.0, L}, Step - 1, H bsl 1);
encode(Min, Max, L, {WS,EN,LonLat}, Step, H)  ->
    encode(WS, EN, LonLat, {(Min + Max) / 2.0, Max, L}, Step - 1, (H bsl 1) + 1).



