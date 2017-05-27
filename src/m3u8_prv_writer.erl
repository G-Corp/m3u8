% @hidden
-module(m3u8_prv_writer).
-include("../include/m3u8.hrl").

-export([to_binary/1]).

to_binary(#{} = M3U8) ->
  bucbinary:join(
    remove_empty(
      [
       <<?EXTM3U>>,
       format(M3U8, playlist_type, ?EXT_X_PLAYLIST_TYPE),
       format(M3U8, target_duration, ?EXT_X_TARGETDURATION),
       format(M3U8, version, ?EXT_X_VERSION, [{undefined, 1}]),
       format(M3U8, media_sequence, ?EXT_X_MEDIA_SEQUENCE, [{undefined, 0}]),
       format(M3U8, program_date_time, ?EXT_X_PROGRAM_DATE_TIME),
       format(M3U8, allow_cache, ?EXT_X_ALLOW_CACHE),
       case M3U8 of
         #{i_frames_only := true} ->
           <<?EXT_X_I_FRAMES_ONLY>>;
         _ ->
           <<>>
       end
       |segments(M3U8)
      ]), <<"\n">>).

format(M3U8, Field, Name) ->
  format(M3U8, Field, Name, [{undefined, undefined}]).
format(M3U8, Field, Name, Defaults) ->
  case buclists:keyfind(maps:get(Field, M3U8, undefined),
                        1,
                        Defaults,
                        maps:get(Field, M3U8, undefined)) of
    undefined -> <<>>;
    Value -> <<(w_to_binary(Name))/binary, (w_to_binary(Value))/binary>>
  end.

segments(#{segments := Segments, keys := Keys}) ->
  segments(Segments, Keys).

segments([], _) ->
  [];
segments([endlist|Segments], Keys) ->
  [<<?EXT_X_ENDLIST>>|segments(Segments, Keys)];
segments([discontinuity|Segments], []) ->
  [<<?EXT_X_DISCONTINUITY>>|segments(Segments, [])];
segments([Segment|Segments], []) ->
  [segment(Segment)|segments(Segments, [])];
segments([discontinuity|Segments], [discontinuity|Keys]) ->
  [<<?EXT_X_DISCONTINUITY>>|segments(Segments, Keys)];
segments([Segment|Segments], [discontinuity|_] = Keys) ->
  [segment(Segment)|segments(Segments, Keys)];
segments([Segment|Segments], [Key|Keys]) ->
  [key(Key), segment(Segment)|segments(Segments, Keys)].

segment(#{sub_range_length := SubRangeLength,
          sub_range_start := SubRangeStart,
          duration := Duration,
          title := Title,
          uri := URI}) ->
  <<?EXT_X_BYTERANGE,
    (w_to_binary(SubRangeLength))/binary,
    "@",
    (w_to_binary(SubRangeStart))/binary,
    "\n",
    ?EXTINF,
    (w_to_binary(Duration))/binary,
    ",",
    (w_to_binary(Title))/binary,
    "\n",
    (w_to_binary(URI))/binary>>;
segment(#{sub_range_length := SubRangeLength,
          duration := Duration,
          title := Title,
          uri := URI}) ->
  <<?EXT_X_BYTERANGE,
    (w_to_binary(SubRangeLength))/binary,
    "\n",
    ?EXTINF,
    (w_to_binary(Duration))/binary,
    ",",
    (w_to_binary(Title))/binary,
    "\n",
    (w_to_binary(URI))/binary>>;
segment(#{duration := Duration, title := Title, uri := URI}) ->
  <<?EXTINF,
    (w_to_binary(Duration))/binary,
    ",",
    (w_to_binary(Title))/binary,
    "\n",
    (w_to_binary(URI))/binary>>.

key(#{method := <<"AES-128">>, iv := IV, uri := URI}) ->
  <<?EXT_X_KEY,
    "METHOD=AES-128,URI=\"",
    (w_to_binary(URI))/binary,
    "\",IV=",
    (w_to_binary(IV))/binary>>;
key(#{method := <<"NONE">>}) ->
  <<?EXT_X_KEY,
    "METHOD=NONE">>.


remove_empty([]) ->
  [];
remove_empty([<<>>|Rest]) ->
  remove_empty(Rest);
remove_empty([Data|Rest]) ->
  [Data|remove_empty(Rest)].

w_to_binary(true) -> <<"YES">>;
w_to_binary(false) -> <<"NO">>;
w_to_binary(Other) -> bucs:to_binary(Other).
