% @hidden
-module(m3u8_prv_parser).
-include("../include/m3u8.hrl").

-export([parse/3]).

parse([], #{segments := Segments,
            medias := Medias,
            playlists := Playlists,
            keys := Keys} = M3U8,
      #{header := true,
        segment := false,
        playlist := false}) ->
  {ok, M3U8#{
         segments => lists:reverse(Segments),
         medias => lists:reverse(Medias),
         playlists => lists:reverse(Playlists),
         keys => lists:reverse(Keys)
        }};
parse([], _, _) ->
  {error, invalid};

parse([<<?EXTM3U>>|Lines], M3U8, #{header := false} = State) ->
  parse(Lines, M3U8, State#{header => true});

parse([<<?EXT_X_VERSION, Data/binary>>|Lines],
      M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  case bucbinary:is_integer(Data) of
    true ->
      parse(Lines,
            M3U8#{version => bucs:to_integer(Data)},
            State);
    false ->
      {error, invalid_ext_x_version, Data}
  end;

parse([<<?EXTINF, Data/binary>>|Lines],
      M3U8,
      #{header := true,
        playlist := false,
        playlist_end := false} = State) ->
  case parse_extinf(Data, State) of
    error ->
      {error, invalid_extinf};
    State0 ->
      parse(Lines, M3U8, State0)
  end;

parse([<<?EXT_X_BYTERANGE, Data/binary>>|Lines],
      M3U8,
      #{header := true,
        playlist := false,
        playlist_end := false} = State) ->
  case parse_ext_x_byterange(Data, State) of
    error ->
      {error, invalid_ext_x_byterange};
    State0 ->
      parse(Lines, M3U8, State0)
  end;

parse([<<?EXT_X_DISCONTINUITY>>|Lines],
      #{segments := Segments, keys := Keys} = M3U8,
      #{header := true,
        playlist := false,
        segment := false,
        playlist_end := false} = State) when length(Segments) > 0 ->
  parse(Lines, M3U8#{segments => [discontinuity|Segments],
                     keys => [discontinuity|Keys]}, State);

parse([<<?EXT_X_KEY, Data/binary>>|Lines],
      M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  case parse_ext_x_key(Data, M3U8) of
    error ->
      {error, invalid_ext_x_key};
    M3U80 ->
      parse(Lines, M3U80, State)
  end;

% TODO: EXT-X-MAP

parse([<<?EXT_X_PROGRAM_DATE_TIME, Data/binary>>|Lines],
      #{program_date_time := undefined} = M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  parse(Lines,
        M3U8#{program_date_time => Data},
        State);

% TODO: EXT-X-DATERANGE

parse([<<?EXT_X_TARGETDURATION, S/binary>>|Lines],
      #{target_duration := undefined} = M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  case bucbinary:is_integer(S) of
    true ->
      parse(Lines,
            M3U8#{target_duration => bucs:to_integer(S)},
            State);
    false ->
      {error, invalid_ext_x_targetduration}
  end;

parse([<<?EXT_X_MEDIA_SEQUENCE, S/binary>>|Lines],
      #{media_sequence := undefined} = M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  case bucbinary:is_integer(S) of
    true ->
      parse(Lines,
            M3U8#{media_sequence => bucs:to_integer(S)},
            State);
    false ->
      {error, invalid_ext_x_media_sequence}
  end;

% TODO: EXT-X-DISCONTINUITY-SEQUENCE

parse([<<?EXT_X_ENDLIST>>|Lines],
      #{segments := Segments} = M3U8,
      #{header := true,
        playlist := false,
        segment := false,
        playlist_end := false} = State) ->
  parse(Lines, M3U8#{segments => [endlist|Segments]}, State#{playlist_end => true});

parse([<<?EXT_X_PLAYLIST_TYPE, Data/binary>>|Lines],
      #{playlist_type := undefined} = M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  parse(Lines,
        M3U8#{playlist_type => Data},
        State);

parse([<<?EXT_X_I_FRAMES_ONLY>>|Lines],
      M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  parse(Lines, M3U8#{i_frames_only => true}, State);

parse([<<?EXT_X_MEDIA, Data/binary>>|Lines],
      M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  case parse_ext_x_media(Data, M3U8) of
    error ->
      {error, invalid_ext_x_media};
    M3U80 ->
      parse(Lines, M3U80, State)
  end;

parse([<<?EXT_X_STREAM_INF, Data/binary>>|Lines],
      M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  case parse_ext_x_stream_inf(Data, State) of
    error ->
      {error, invalid_ext_x_stream_inf};
    State0 ->
      parse(Lines, M3U8, State0)
  end;

parse([<<?EXT_X_I_FRAME_STREAM_INF, Data/binary>>|Lines],
      M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  case parse_ext_x_i_frame_stream_inf(Data, M3U8) of
    error ->
      {error, invalid_ext_x_i_frame_stream_inf};
    M3U80 ->
      parse(Lines, M3U80, State)
  end;

% TODO: EXT-X-SESSION-DATA

% TODO: EXT-X-SESSION-KEY

parse([<<?EXT_X_INDEPENDENT_SEGMENTS>>|Lines],
      M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  parse(Lines, M3U8#{independent_segments => true}, State);

% TODO: EXT-X-START

% -- Remove in V7 --

parse([<<?EXT_X_ALLOW_CACHE, Data/binary>>|Lines],
      #{allow_cache := undefined} = M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  parse(Lines,
        M3U8#{allow_cache => case Data of
                               <<"YES">> -> true;
                               _ -> false
                             end},
        State);

% Segment URI
parse([URI|Lines],
      #{segments := Segments} = M3U8,
      #{header := true,
        playlist := false,
        segment := Segment,
        playlist_end := false} = State) when is_map(Segment) ->
  parse(Lines,
        M3U8#{segments => [Segment#{uri => URI}|Segments]},
        State#{segment => false});

% Playlist URI
parse([URI|Lines],
      #{playlists := Playlists} = M3U8,
      #{header := true,
        playlist := Playlist,
        segment := false,
        playlist_end := false} = State) when is_map(Playlist) ->
  parse(Lines,
        M3U8#{playlists => [Playlist#{uri => URI}|Playlists]},
        State#{playlist => false});

parse([<<>>|Lines], M3U8, State) ->
  parse(Lines, M3U8, State);
parse([X|Lines], M3U8, State) ->
  parse(Lines, M3U8, State).

% -----------------------------------------------------------------------------

parse_ext_x_byterange(Data, #{segment := Segment} = State) ->
  Segment0 = case Segment of
               false ->
                 #{};
               Other ->
                 Other
             end,
  case binary:split(Data, [<<"@">>]) of
    [Length, Start] ->
      case bucbinary:are_integers([Length, Start]) of
        true ->
          State#{segment => Segment0#{sub_range_length => bucs:to_integer(Length),
                                      sub_range_start => bucs:to_integer(Start)}};
        false ->
          error
      end;
    [Length] ->
      case bucbinary:is_integer(Length) of
        true ->
          State#{segment => Segment0#{sub_range_length => bucs:to_integer(Length)}};
        false ->
          error
      end;
    _ ->
      error
  end.

parse_extinf(Data, #{segment := Segment} = State) ->
  Segment0 = case Segment of
               false ->
                 #{};
               Other ->
                 Other
             end,
  case binary:split(Data, [<<",">>]) of
    [Duration, Title] ->
      case bucbinary:is_integer(Duration) of
        true ->
          State#{segment => Segment0#{duration => bucs:to_integer(Duration),
                                      title => Title}};
        false ->
          case bucbinary:is_float(Duration) of
            true ->
              State#{segment => Segment0#{duration => bucs:to_float(Duration),
                                          title => Title}};
            false ->
              error
          end
      end;
    [Duration] ->
      case bucbinary:is_integer(Duration) of
        true ->
          State#{segment => Segment0#{duration => bucs:to_integer(Duration)}};
        false ->
          case bucbinary:is_float(Duration) of
            true ->
              State#{segment => Segment0#{duration => bucs:to_float(Duration)}};
            false ->
              error
          end
      end
  end.

parse_ext_x_key(Data, #{keys := Keys} = M3U8) ->
  Keys0 = get_attr(Data, <<"METHOD">>, #{}, method),
  Keys1 = get_quoted_attr(Data, <<"URI">>, Keys0, uri),
  Keys2 = get_attr(Data, <<"IV">>, Keys1, iv),
  Keys3 = get_quoted_attr(Data, <<"KEYFORMAT">>, Keys2, keyformat),
  Keys4 = get_quoted_attr(Data, <<"KEYFORMATVERSIONS">>, Keys3, keyformatversions),
  case Keys4 of
    #{method := <<"AES-128">>, uri := _} ->
      M3U8#{keys => [Keys4|Keys]};
    #{method := <<"NONE">>} ->
      M3U8#{keys => [Keys4|Keys]};
    _ ->
      error
  end.

parse_ext_x_media(Data, #{medias := Medias} = M3U8) ->
  Media0 = #{default => <<"NO">>},
  Media1 = get_quoted_attr(Data, <<"URI">>, Media0, uri),
  Media2 = get_attr(Data, <<"TYPE">>, Media1, type),
  Media3 = get_quoted_attr(Data, <<"GROUP-ID">>, Media2, group_id),
  Media4 = get_quoted_attr(Data, <<"LANGUAGE">>, Media3, language),
  Media5 = get_quoted_attr(Data, <<"NAME">>, Media4, name),
  Media6 = get_attr(Data,
                    <<"DEFAULT">>,
                    Media5,
                    default,
                    fun
                      (<<"YES">>) -> {ok, true};
                      (_) -> {ok, false}
                    end),
  Media7 = get_attr(Data,
                    <<"AUTOSELECT">>,
                    Media6,
                    autoselect,
                    fun
                      (<<"YES">>) -> {ok, true};
                      (_) -> {ok, false}
                    end),
  Media8 = get_attr(Data,
                    <<"FORCED">>,
                    Media7,
                    forced,
                    fun
                      (<<"YES">>) -> {ok, true};
                      (_) -> {ok, false}
                    end),
  Media9 = get_quoted_attr(Data, <<"CHARACTERISTICS">>, Media8, characteristics),
  Media10 = get_quoted_attr(Data, <<"ASSOC-LANGUAGE">>, Media9, assoc_language),
  Media11 = get_quoted_attr(Data, <<"INSTREAM-ID">>, Media10, stream_id),
  Media12 = get_quoted_attr(Data, <<"CHANNELS">>, Media11, channels),
  M3U8#{medias => [Media12|Medias]}.

parse_ext_x_stream_inf(Data, State) ->
  Playlist0 = get_attr(Data,
                       <<"BANDWIDTH">>,
                       #{},
                       bandwidth,
                       fun(BW) ->
                           case bucbinary:is_integer(BW) of
                             true ->
                               {ok, bucs:to_integer(BW)};
                             false ->
                               undefined
                           end
                       end),
  Playlist1 = get_attr(Data,
                       <<"AVERAGE-BANDWIDTH">>,
                       Playlist0,
                       average_bandwidth,
                       fun(BW) ->
                           case bucbinary:is_integer(BW) of
                             true ->
                               {ok, bucs:to_integer(BW)};
                             false ->
                               undefined
                           end
                       end),
  Playlist2 = get_attr(Data,
                       <<"FRAME-RATE">>,
                       Playlist1,
                       frame_rate,
                       fun(BW) ->
                           case bucbinary:is_float(BW) of
                             true ->
                               {ok, bucs:to_float(BW)};
                             false ->
                               undefined
                           end
                       end),
  Playlist3 = get_attr(Data, <<"HDCP-LEVEL">>, Playlist2, hdcp_level),
  Playlist4 = get_attr(Data, <<"PROGRAM-ID">>, Playlist3, program_id),
  % Playlist5 = get_quoted_attr(Data,
  %                             <<"CODECS">>,
  %                             Playlist4,
  %                             codecs,
  %                             fun(C) ->
  %                                 {ok, binary:split(C, [<<",">>], [global])}
  %                             end),
  Playlist5 = get_quoted_attr(Data,
                              <<"CODECS">>,
                              Playlist4,
                              codecs,
                              fun(C) ->
                                  {ok, binary:split(C, [<<",">>], [global])}
                              end),
  Playlist6 = get_attr(Data,
                       <<"RESOLUTION">>,
                       Playlist5,
                       resolution,
                       fun(R) ->
                           case binary:split(R, [<<"x">>]) of
                             [W, H] = R0 ->
                               case bucbinary:are_integers(R0) of
                                 true ->
                                   {ok, #{width => bucs:to_integer(W),
                                          height => bucs:to_integer(H)}};
                                 _ ->
                                   undefined
                               end;
                             _ ->
                               undefined
                           end
                       end),
  Playlist7 = get_quoted_attr(Data, <<"AUDIO">>, Playlist6, audio),
  Playlist8 = get_quoted_attr(Data, <<"VIDEO">>, Playlist7, video),
  Playlist9 = get_quoted_attr(Data, <<"SUBTITLES">>, Playlist8, subtitles),
  Playlist10 = get_quoted_attr(Data, <<"CLOSED-CAPTIONS">>, Playlist9, closed_captions),
  State#{playlist => Playlist10}.

parse_ext_x_i_frame_stream_inf(Data, #{playlists := Playlists} = M3U8) ->
  Playlist0 = get_attr(Data,
                       <<"BANDWIDTH">>,
                       #{type => iframe},
                       bandwidth,
                       fun(BW) ->
                           case bucbinary:is_integer(BW) of
                             true ->
                               {ok, bucs:to_integer(BW)};
                             false ->
                               undefined
                           end
                       end),
  Playlist1 = get_attr(Data,
                       <<"AVERAGE-BANDWIDTH">>,
                       Playlist0,
                       average_bandwidth,
                       fun(BW) ->
                           case bucbinary:is_integer(BW) of
                             true ->
                               {ok, bucs:to_integer(BW)};
                             false ->
                               undefined
                           end
                       end),
  Playlist2 = get_attr(Data, <<"HDCP-LEVEL">>, Playlist1, hdcp_level),
  Playlist3 = get_attr(Data, <<"PROGRAM-ID">>, Playlist2, program_id),
  Playlist4 = get_quoted_attr(Data,
                              <<"CODECS">>,
                              Playlist3,
                              codecs,
                              fun(C) ->
                                  {ok, binary:split(C, [<<",">>], [global])}
                              end),
  Playlist5 = get_attr(Data,
                       <<"RESOLUTION">>,
                       Playlist4,
                       resolution,
                       fun(R) ->
                           case binary:split(R, [<<"x">>]) of
                             [W, H] = R0 ->
                               case bucbinary:are_integers(R0) of
                                 true ->
                                   {ok, #{width => bucs:to_integer(W),
                                          height => bucs:to_integer(H)}};
                                 _ ->
                                   undefined
                               end;
                             _ ->
                               undefined
                           end
                       end),
  Playlist6 = get_quoted_attr(Data, <<"VIDEO">>, Playlist5, video),
  Playlist7 = get_quoted_attr(Data, <<"URI">>, Playlist6, uri),
  M3U8#{playlists => [Playlist7|Playlists]}.

get_attr(Data, Name, Map, Key, Fun) ->
  case re:run(Data, <<Name/binary, "=([^,]*)">>, [{capture, all_but_first, binary}]) of
    {match, [Attr]} ->
      case Fun(Attr) of
        {ok, Attr0} ->
          maps:put(Key, Attr0, Map);
        _ ->
          Map
      end;
    _ ->
      Map
  end.

get_quoted_attr(Data, Name, Map, Key, Fun) ->
  case re:run(Data, <<Name/binary, "=\"([^\"]*)\"">>, [{capture, all_but_first, binary}]) of
    {match, [Attr]} ->
      case Fun(Attr) of
        {ok, Attr0} ->
          maps:put(Key, Attr0, Map);
        _ ->
          Map
      end;
    _ ->
      Map
  end.

get_quoted_attr(Data, Name, Map, Key) ->
  get_quoted_attr(Data, Name, Map, Key, fun(X) -> {ok, X} end).
get_attr(Data, Name, Map, Key) ->
  get_attr(Data, Name, Map, Key, fun(X) -> {ok, X} end).
