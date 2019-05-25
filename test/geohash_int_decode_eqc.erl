-module(geohash_int_decode_eqc).
-include_lib("proper/include/proper.hrl").

-include("../src/geohash_int.hrl").

-define(PRECISION, 100000).

prop_geohash_decode_leftbottom() ->
    ?FORALL({Point,Level,World}, geohash_eqc_utils:generate_point_at_leftbottem_corner(),
            begin
                {Longitude, Latitude} = Point,
                {ok, Hash} = geohash_int:encode(World, Latitude, Longitude, Level),
                #{bits:=HashInt, level:=L} = Hash,
                {ok, PointArea} = geohash_int:fast_decode(World, Hash),
                #{north:=N,south:=S,west:=W,east:=E} = World,
                #{xmin:=XMin,xmax:=XMax,ymin:=YMin,ymax:=YMax} = PointArea,
                CellHeight = (N - S) / (1 bsl Level),
                CellWidth = (E - W) / (1 bsl Level),
                equals(lists:map(fun float_round/1, [W, S, W+CellWidth, S+CellHeight]),
                       lists:map(fun float_round/1, [XMin, YMin, XMax, YMax]))
            end).

prop_geohash_decode_righttop() ->
    ?FORALL({Point,Level,World}, geohash_eqc_utils:generate_point_at_righttop_corner(),
            begin
                {Longitude, Latitude} = Point,
                {ok, Hash} = geohash_int:encode(World, Latitude, Longitude, Level),
                #{bits:=HashInt, level:=L} = Hash,
                {ok, PointArea} = geohash_int:fast_decode(World, Hash),
                #{north:=N,south:=S,west:=W,east:=E} = World,
                #{xmin:=XMin,xmax:=XMax,ymin:=YMin,ymax:=YMax} = PointArea,
                CellHeight = (N - S) / (1 bsl Level),
                CellWidth = (E - W) / (1 bsl Level),
                equals(lists:map(fun float_round/1, [E-CellWidth, N-CellHeight, E, N]),
                       lists:map(fun float_round/1, [XMin, YMin, XMax, YMax]))
            end).

prop_geohash_decode_center_leftbottom() ->
    ?FORALL({Point,Level,World}, geohash_eqc_utils:generate_point_at_center_leftbottom(),
            begin
                {Longitude, Latitude} = Point,
                {ok, Hash} = geohash_int:encode(World, Latitude, Longitude, Level),
                #{bits:=HashInt, level:=L} = Hash,
                {ok, PointArea} = geohash_int:fast_decode(World, Hash),
                #{north:=N,south:=S,west:=W,east:=E} = World,
                #{xmin:=XMin,xmax:=XMax,ymin:=YMin,ymax:=YMax} = PointArea,
                CenterX = (E + W) / 2,
                CenterY = (N + S) / 2,
                CellHeight = (N - S) / (1 bsl Level),
                CellWidth = (E - W) / (1 bsl Level),
                equals(lists:map(fun float_round/1, [CenterX-CellWidth, CenterY-CellHeight, CenterX, CenterY]),
                       lists:map(fun float_round/1, [XMin, YMin, XMax, YMax]))
            end).

prop_geohash_decode_center_righttop() ->
    ?FORALL({Point,Level,World}, geohash_eqc_utils:generate_point_at_center_righttop(),
            begin
                {Longitude, Latitude} = Point,
                {ok, Hash} = geohash_int:encode(World, Latitude, Longitude, Level),
                #{bits:=HashInt, level:=L} = Hash,
                {ok, PointArea} = geohash_int:fast_decode(World, Hash),
                #{north:=N,south:=S,west:=W,east:=E} = World,
                #{xmin:=XMin,xmax:=XMax,ymin:=YMin,ymax:=YMax} = PointArea,
                CenterX = (E + W) / 2,
                CenterY = (N + S) / 2,
                CellHeight = (N - S) / (1 bsl Level),
                CellWidth = (E - W) / (1 bsl Level),
                equals(lists:map(fun float_round/1, [CenterX,CenterY,CenterX+CellWidth,CenterY+CellHeight]),
                       lists:map(fun float_round/1, [XMin, YMin, XMax, YMax]))
            end).

prop_geohash_decode_randomly() ->
    ?FORALL({Time, PointArea, Point, Level, World}, decode_randomly(fun tc_decode/2),
            begin
                #{north:=N,south:=S,west:=W,east:=E} = World,
                #{xmin:=XMin,xmax:=XMax,ymin:=YMin,ymax:=YMax} = PointArea,
                {Longitude, Latitude} = Point,
                CellHeight = (N - S) / (1 bsl Level),
                CellWidth = (E - W) / (1 bsl Level),
                AreaXmin = W + CellWidth * trunc((Longitude - W) / CellWidth),
                AreaYmin = S + CellHeight * trunc((Latitude - S) / CellHeight),
                equals(lists:map(fun float_round/1, [AreaXmin,AreaYmin,AreaXmin+CellWidth,AreaYmin+CellHeight]),
                       lists:map(fun float_round/1, [XMin, YMin, XMax, YMax]))
            end).

decode_randomly(Decode) ->
    ?LET({Point,Level,World}, geohash_eqc_utils:generate_point_randomly(),
            begin
                {Longitude, Latitude} = Point,
                {ok, Hash} = geohash_int:encode(World, Latitude, Longitude, Level),
                #{bits:=HashInt, level:=L} = Hash,
                {Time, PointArea} = Decode(World, Hash),
                {Time, PointArea, Point, L, World}
            end).


float_round(R) ->
    erlang:round(R * ?PRECISION) / ?PRECISION.

tc_decode(World, Hash) ->
    Start = erlang:system_time(nano_seconds),
    {ok, PointArea} = geohash_int:decode(World, Hash),
    Time = erlang:system_time(nano_seconds) - Start,
    {Time, PointArea}.

tc_fast_decode(World, Hash) ->
    Start = erlang:system_time(nano_seconds),
    {ok, PointArea} = geohash_int:fast_decode(World, Hash),
    Time = erlang:system_time(nano_seconds) - Start,
    {Time, PointArea}.
