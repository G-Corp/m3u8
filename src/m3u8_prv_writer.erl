% @hidden
-module(m3u8_prv_writer).
-include("../include/m3u8.hrl").

-export([to_binary/1]).

to_binary(#{} = M3U8) ->
  <<(bucbinary:join(
       remove_empty(
         [
          <<?EXTM3U>>,
          format(M3U8, playlist_type, ?EXT_X_PLAYLIST_TYPE),
          format(M3U8, target_duration, ?EXT_X_TARGETDURATION),
          format(M3U8, version, ?EXT_X_VERSION),
          format(M3U8, media_sequence, ?EXT_X_MEDIA_SEQUENCE),
          format(M3U8, program_date_time, ?EXT_X_PROGRAM_DATE_TIME),
          format(M3U8, allow_cache, ?EXT_X_ALLOW_CACHE),
          case M3U8 of
            #{i_frames_only := true} ->
              <<?EXT_X_I_FRAMES_ONLY>>;
            _ ->
              <<>>
          end,
          case M3U8 of
            #{independent_segments := true} ->
              <<?EXT_X_INDEPENDENT_SEGMENTS>>;
            _ ->
              <<>>
          end
         ]
         ++ medias(M3U8)
         ++ playlists(M3U8)
         ++ segments(M3U8)
        ), <<"\n">>))/binary, "\n">>.

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

medias(#{medias := Medias}) ->
  medias(Medias);
medias([]) ->
  [];
medias([Media|Medias]) ->
  [<<
     ?EXT_X_MEDIA,
     "TYPE=", (maps:get(type, Media))/binary,
     (attribut(Media, uri, <<"URI">>, true))/binary,
     ",GROUP-ID=\"", (maps:get(group_id, Media))/binary, "\"",
     (attribut(Media, language, <<"LANGUAGE">>, true))/binary,
     (attribut(Media, assoc_language, <<"ASSOC-LANGUAGE">>, true))/binary,
     ",NAME=\"", (maps:get(name, Media))/binary, "\"",
     (attribut(Media, default, <<"DEFAULT">>, false))/binary,
     (attribut(Media, autoselect, <<"AUTOSELECT">>, false))/binary,
     (attribut(Media, forced, <<"FORCED">>, false))/binary,
     (attribut(Media, stream_id, <<"STREAM-ID">>, true))/binary,
     (attribut(Media, characteristics, <<"CHARACTERISTICS">>, true))/binary
   >>
   |medias(Medias)].

playlists(#{playlists := Playlists}) ->
  playlists(Playlists);
playlists([]) ->
  [];
playlists([Playlist|Playlists]) ->
  [<<
     (case maps:get(type, Playlist, undefined) of
        iframe -> <<?EXT_X_I_FRAME_STREAM_INF>>;
        _ -> <<?EXT_X_STREAM_INF>>
      end)/binary,
     "BANDWIDTH=", (w_to_binary(maps:get(bandwidth, Playlist)))/binary,
     (attribut(Playlist, average_bandwidth, <<"AVERAGE-BANDWIDTH">>, false))/binary,
     (attribut(Playlist, codecs, <<"CODECS">>, true,
               fun(C) ->
                   bucbinary:join([w_to_binary(E) || E <- C], <<",">>)
               end))/binary,
     (attribut(Playlist, resolution, <<"RESOLUTION">>, false,
               fun(#{width := W, height := H}) ->
                   <<(w_to_binary(W))/binary, "x", (w_to_binary(H))/binary>>
               end))/binary,
     (attribut(Playlist, frame_rate, <<"FRAME-RATE">>, false))/binary,
     (attribut(Playlist, hdcp_level, <<"HDCP-LEVEL">>, false))/binary,
     (attribut(Playlist, program_id, <<"PROGRAM-ID">>, false))/binary,
     (attribut(Playlist, audio, <<"AUDIO">>, true))/binary,
     (attribut(Playlist, video, <<"VIDEO">>, true))/binary,
     (attribut(Playlist, subtitles, <<"SUBTITLES">>, true))/binary,
     (attribut(Playlist, closed_captions, <<"CLOSED-CAPTIONS">>, true))/binary,
     (case maps:get(type, Playlist, undefined) of
        iframe ->
          attribut(Playlist, uri, <<"URI">>, true);
        _ ->
          case maps:get(uri, Playlist, undefined) of
            undefined -> <<>>;
            URI -> <<"\n", URI/binary>>
          end
      end)/binary
   >>
   | playlists(Playlists)].

attribut(Datas, Name, Attribut, Quote) ->
  attribut(Datas, Name, Attribut, Quote, fun(Value) ->
                                             <<(w_to_binary(Value))/binary>>
                                         end).
attribut(Datas, Name, Attribut, Quote, Fun) ->
  case maps:get(Name, Datas, undefined) of
     undefined ->
       <<>>;
     Value ->
       <<",",
         Attribut/binary,
         "=",
         (case Quote of
            true -> <<"\"">>;
            _ -> <<>>
          end)/binary,
         (Fun(Value))/binary,
         (case Quote of
            true -> <<"\"">>;
            _ -> <<>>
          end)/binary
       >>
   end.

w_to_binary(true) -> <<"YES">>;
w_to_binary(false) -> <<"NO">>;
w_to_binary(Other) -> bucs:to_binary(Other).
