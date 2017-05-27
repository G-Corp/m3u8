% @hidden
-module(m3u8_prv_parser).
-include("../include/m3u8.hrl").

-export([parse/3]).

parse([], #{segments := Segments,
            medias := Medias,
            playlists := Playlists,
            keys := Keys,
            i_frame_playlists := IFramePlaylists} = M3U8,
      #{header := true,
        segment := false,
        playlist := false}) ->
  {ok, M3U8#{
         segments => lists:reverse(Segments),
         medias => lists:reverse(Medias),
         playlists => lists:reverse(Playlists),
         keys => lists:reverse(Keys),
         i_frame_playlists => lists:reverse(IFramePlaylists)
        }};
parse([], _, _) ->
  {error, invalid};

% An Extended M3U file is distinguished from a basic M3U file by its
% first line, which MUST be the tag #EXTM3U.
parse([<<?EXTM3U>>|Lines], M3U8, #{header := false} = State) ->
  parse(Lines, M3U8, State#{header => true});

% The EXT-X-BYTERANGE tag indicates that a media segment is a sub-range
% of the resource identified by its media URI.  It applies only to the
% next media URI that follows it in the Playlist.  Its format is:
%
% #EXT-X-BYTERANGE:<n>[@o]
%
% where n is a decimal-integer indicating the length of the sub-range
% in bytes.  If present, o is a decimal-integer indicating the start of
% the sub-range, as a byte offset from the beginning of the resource.
% If o is not present, the sub-range begins at the next byte following
% the sub-range of the previous media segment.
%
% If o is not present, a previous media segment MUST appear in the
% Playlist file and MUST be a sub-range of the same media resource.
%
% A media URI with no EXT-X-BYTERANGE tag applied to it specifies a
% media segment that consists of the entire resource.
%
% The EXT-X-BYTERANGE tag appeared in version 4 of the protocol.
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

% The EXTINF tag specifies the duration of a media segment.  It applies
% only to the media URI that follows it.  Each media segment URI MUST
% be preceded by an EXTINF tag.  Its format is:
%
% #EXTINF:<duration>[,<title>]
%
% "duration" is an integer or floating-point number in decimal
% positional notation that specifies the duration of the media segment
% in seconds.  Durations that are reported as integers SHOULD be
% rounded to the nearest integer.  Durations MUST be integers if the
% protocol version of the Playlist file is less than 3.  The remainder
% of the line following the comma is an optional human-readable
% informative title of the media segment.
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

% The EXT-X-TARGETDURATION tag specifies the maximum media segment
% duration.  The EXTINF duration of each media segment in the Playlist
% file MUST be less than or equal to the target duration.  This tag
% MUST appear once in the Playlist file.  It applies to the entire
% Playlist file.  Its format is:
%
% #EXT-X-TARGETDURATION:<s>
%
% where s is an integer indicating the target duration in seconds.
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

% Each media URI in a Playlist has a unique integer sequence number.
% The sequence number of a URI is equal to the sequence number of the
% URI that preceded it plus one.  The EXT-X-MEDIA-SEQUENCE tag
% indicates the sequence number of the first URI that appears in a
% Playlist file.  Its format is:
%
% #EXT-X-MEDIA-SEQUENCE:<number>
%
% A Playlist file MUST NOT contain more than one EXT-X-MEDIA-SEQUENCE
% tag.  If the Playlist file does not contain an EXT-X-MEDIA-SEQUENCE
% tag then the sequence number of the first URI in the playlist SHALL
% be considered to be 0.
%
% A media URI is not required to contain its sequence number.
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

% Media segments MAY be encrypted.  The EXT-X-KEY tag specifies how to
% decrypt them.  It applies to every media URI that appears between it
% and the next EXT-X-KEY tag in the Playlist file (if any).  Its format
% is:
%
% #EXT-X-KEY:<attribute-list>
%
% The following attributes are defined:
%
% The METHOD attribute specifies the encryption method.  It is of type
% enumerated-string.  Each EXT-X-KEY tag MUST contain a METHOD
% attribute.
%
% Two methods are defined: NONE and AES-128.
%
% An encryption method of NONE means that media segments are not
% encrypted.  If the encryption method is NONE, the URI and the IV
% attributes MUST NOT be present.
%
% An encryption method of AES-128 means that media segments are
% encrypted using the Advanced Encryption Standard [AES_128] with a
% 128-bit key and PKCS7 padding [RFC5652].  If the encryption method is
% AES-128, the URI attribute MUST be present.  The IV attribute MAY be
% present; see Section 5.2.
%
% The URI attribute specifies how to obtain the key.  Its value is a
% quoted-string that contains a URI [RFC3986] for the key.
%
% The IV attribute, if present, specifies the Initialization Vector to
% be used with the key.  Its value is a hexadecimal-integer.  The IV
% attribute appeared in protocol version 2.
%
% If the Playlist file does not contain an EXT-X-KEY tag then media
% segments are not encrypted.
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

% The EXT-X-PROGRAM-DATE-TIME tag associates the first sample of a
% media segment with an absolute date and/or time.  It applies only to
% the next media URI.
%
% The date/time representation is ISO/IEC 8601:2004 [ISO_8601] and
% SHOULD indicate a time zone:
%
% #EXT-X-PROGRAM-DATE-TIME:<YYYY-MM-DDThh:mm:ssZ>
%
% For example:
%
% #EXT-X-PROGRAM-DATE-TIME:2010-02-19T14:54:23.031+08:00
parse([<<?EXT_X_PROGRAM_DATE_TIME, Data/binary>>|Lines],
      #{program_date_time := undefined} = M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  parse(Lines,
        M3U8#{program_date_time => Data},
        State);
% The EXT-X-ALLOW-CACHE tag indicates whether the client MAY or MUST
% NOT cache downloaded media segments for later replay.  It MAY occur
% anywhere in the Playlist file; it MUST NOT occur more than once.  The
% EXT-X-ALLOW-CACHE tag applies to all segments in the playlist.  Its
% format is:
%
% #EXT-X-ALLOW-CACHE:<YES|NO>
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

% The EXT-X-PLAYLIST-TYPE tag provides mutability information about the
% Playlist file.  It applies to the entire Playlist file.  It is
% optional.  Its format is:
%
% #EXT-X-PLAYLIST-TYPE:<EVENT|VOD>
parse([<<?EXT_X_PLAYLIST_TYPE, Data/binary>>|Lines],
      #{playlist_type := undefined} = M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  parse(Lines,
        M3U8#{playlist_type => Data},
        State);

% The EXT-X-ENDLIST tag indicates that no more media segments will be
% added to the Playlist file.  It MAY occur anywhere in the Playlist
% file; it MUST NOT occur more than once.  Its format is:
%
% #EXT-X-ENDLIST
parse([<<?EXT_X_ENDLIST>>|Lines],
      #{segments := Segments} = M3U8,
      #{header := true,
        playlist := false,
        segment := false,
        playlist_end := false} = State) ->
  parse(Lines, M3U8#{segments => [endlist|Segments]}, State#{playlist_end => true});

% The EXT-X-MEDIA tag is used to relate Playlists that contain
% alternative renditions of the same content.  For example, three EXT-
% X-MEDIA tags can be used to identify audio-only Playlists that
% contain English, French and Spanish renditions of the same
% presentation.  Or two EXT-X-MEDIA tags can be used to identify video-
% only Playlists that show two different camera angles.
%
% The EXT-X-MEDIA tag stands alone, in that it does not apply to a
% particular URI in the Playlist.  Its format is:
%
% #EXT-X-MEDIA:<attribute-list>
%
% The following attributes are defined:
%
% URI
%
% The value is a quoted-string containing a URI that identifies the
% Playlist file.  This attribute is optional; see Section 3.4.10.1.
%
% TYPE
%
% The value is enumerated-string; valid strings are AUDIO and VIDEO.
% If the value is AUDIO, the Playlist described by the tag MUST contain
% audio media.  If the value is VIDEO, the Playlist MUST contain video
% media.
%
% GROUP-ID
%
% The value is a quoted-string identifying a mutually-exclusive group
% of renditions.  The presence of this attribute signals membership in
% the group.  See Section 3.4.9.1.
%
% LANGUAGE
%
% The value is a quoted-string containing an RFC 5646 [RFC5646]
% language tag that identifies the primary language used in the
% rendition.  This attribute is optional.
%
% NAME
%
% The value is a quoted-string containing a human-readable description
% of the rendition.  If the LANGUAGE attribute is present then this
% description SHOULD be in that language.
%
% DEFAULT
%
% The value is enumerated-string; valid strings are YES and NO.  If the
% value is YES, then the client SHOULD play this rendition of the
% content in the absence of information from the user indicating a
% different choice.  This attribute is optional.  Its absence indicates
% an implicit value of NO.
%
% AUTOSELECT
%
% The value is enumerated-string; valid strings are YES and NO.  This
% attribute is optional.  Its absence indicates an implicit value of
% NO.  If the value is YES, then the client MAY choose to play this
% rendition in the absence of explicit user preference because it
% matches the current playback environment, such as chosen system
% language.
%
% The EXT-X-MEDIA tag appeared in version 4 of the protocol.
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

% The EXT-X-STREAM-INF tag identifies a media URI as a Playlist file
% containing a multimedia presentation and provides information about
% that presentation.  It applies only to the URI that follows it.  Its
% format is:
%
% #EXT-X-STREAM-INF:<attribute-list>
% <URI>
%
% The following attributes are defined:
%
% BANDWIDTH
%
% The value is a decimal-integer of bits per second.  It MUST be an
% upper bound of the overall bitrate of each media segment (calculated
% to include container overhead) that appears or will appear in the
% Playlist.
%
% Every EXT-X-STREAM-INF tag MUST include the BANDWIDTH attribute.
%
% PROGRAM-ID
%
% The value is a decimal-integer that uniquely identifies a particular
% presentation within the scope of the Playlist file.
%
% A Playlist file MAY contain multiple EXT-X-STREAM-INF tags with the
% same PROGRAM-ID to identify different encodings of the same
% presentation.  These variant playlists MAY contain additional EXT-X-
% STREAM-INF tags.
%
% CODECS
%
% The value is a quoted-string containing a comma-separated list of
% formats, where each format specifies a media sample type that is
% present in a media segment in the Playlist file.  Valid format
% identifiers are those in the ISO File Format Name Space defined by
% RFC 6381 [RFC6381].
%
% Every EXT-X-STREAM-INF tag SHOULD include a CODECS attribute.
%
% RESOLUTION
%
% The value is a decimal-resolution describing the approximate encoded
% horizontal and vertical resolution of video within the presentation.
%
% AUDIO
%
% The value is a quoted-string.  It MUST match the value of the
% GROUP-ID attribute of an EXT-X-MEDIA tag elsewhere in the Playlist
% whose TYPE attribute is AUDIO.  It indicates the set of audio
% renditions that MAY be used when playing the presentation.  See
% Section 3.4.10.1.
%
% VIDEO
%
% The value is a quoted-string.  It MUST match the value of the
% GROUP-ID attribute of an EXT-X-MEDIA tag elsewhere in the Playlist
% whose TYPE attribute is VIDEO.  It indicates the set of video
% renditions that MAY be used when playing the presentation.  See
% Section 3.4.10.1.
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

% The EXT-X-DISCONTINUITY tag indicates an encoding discontinuity
% between the media segment that follows it and the one that preceded
% it.  The set of characteristics that MAY change is:
%
% o  file format
%
% o  number and type of tracks
%
% o  encoding parameters
%
% o  encoding sequence
%
% o  timestamp sequence
%
% Its format is:
%
% #EXT-X-DISCONTINUITY
parse([<<?EXT_X_DISCONTINUITY>>|Lines],
      #{segments := Segments, keys := Keys} = M3U8,
      #{header := true,
        playlist := false,
        segment := false,
        playlist_end := false} = State) when length(Segments) > 0 ->
  parse(Lines, M3U8#{segments => [discontinuity|Segments],
                     keys => [discontinuity|Keys]}, State);

% The EXT-X-I-FRAMES-ONLY tag indicates that each media segment in the
% Playlist describes a single I-frame.  I-frames (or Intra frames) are
% encoded video frames whose encoding does not depend on any other
% frame.
%
% The EXT-X-I-FRAMES-ONLY tag applies to the entire Playlist.  Its
% format is:
%
% #EXT-X-I-FRAMES-ONLY
%
% In a Playlist with the EXT-X-I-FRAMES-ONLY tag, the media segment
% duration (EXTINF tag value) is the time between the presentation time
% of the I-frame in the media segment and the presentation time of the
% next I-frame in the Playlist, or the end of the presentation if it is
% the last I-frame in the Playlist.
%
% Media resources containing I-frame segments MUST begin with a
% Transport Stream PAT/PMT.  The byte range of an I-frame segment with
% an EXT-X-BYTERANGE tag applied to it (Section 3.4.1) MUST NOT include
% a PAT/PMT.
%
% The EXT-X-I-FRAMES-ONLY tag appeared in version 4 of the protocol.
parse([<<?EXT_X_I_FRAMES_ONLY>>|Lines],
      M3U8,
      #{header := true,
        playlist := false,
        segment := false} = State) ->
  parse(Lines, M3U8#{i_frames_only => true}, State);

% The EXT-X-I-FRAME-STREAM-INF tag identifies a Playlist file
% containing the I-frames of a multimedia presentation.  It stands
% alone, in that it does not apply to a particular URI in the Playlist.
% Its format is:
%
% #EXT-X-I-FRAME-STREAM-INF:<attribute-list>
%
% All attributes defined for the EXT-X-STREAM-INF tag (Section 3.4.10)
% are also defined for the EXT-X-I-FRAME-STREAM-INF tag, except for the
% AUDIO attribute.  In addition, the following attribute is defined:
%
% URI
%
% The value is a quoted-string containing a URI that identifies the
% I-frame Playlist file.
%
% Every EXT-X-I-FRAME-STREAM-INF tag MUST include a BANDWIDTH attribute
% and a URI attribute.
%
% The provisions in Section 3.4.10.1 also apply to EXT-X-I-FRAME-
% STREAM-INF tags with a VIDEO attribute.
%
% A Playlist that specifies alternative VIDEO renditions and I-frame
% Playlists SHOULD include an alternative I-frame VIDEO rendition for
% each regular VIDEO rendition, with the same NAME and LANGUAGE
% attributes.
%
% The EXT-X-I-FRAME-STREAM-INF tag appeared in version 4 of the
% protocol.  Clients that do not implement protocol version 4 or higher
% MUST ignore it.
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

% The EXT-X-VERSION tag indicates the compatibility version of the
% Playlist file.  The Playlist file, its associated media, and its
% server MUST comply with all provisions of the most-recent version of
% this document describing the protocol version indicated by the tag
% value.
%
% The EXT-X-VERSION tag applies to the entire Playlist file.  Its
% format is:
%
% #EXT-X-VERSION:<n>
%
% where n is an integer indicating the protocol version.
%
% A Playlist file MUST NOT contain more than one EXT-X-VERSION tag.  A
% Playlist file that does not contain an EXT-X-VERSION tag MUST comply
% with version 1 of this protocol.
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
parse([X|_], _, _) ->
  {error, invalid_entry, X}.

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
  case Keys2 of
    #{method := <<"AES-128">>, uri := _} ->
      M3U8#{keys => [Keys2|Keys]};
    #{method := <<"NONE">>} ->
      M3U8#{keys => [Keys2|Keys]};
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
  M3U8#{medias => [Media7|Medias]}.

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
  Playlist1 = get_attr(Data, <<"PROGRAM-ID">>, Playlist0, program_id),
  Playlist2 = get_quoted_attr(Data,
                              <<"CODECS">>,
                              Playlist1,
                              codecs,
                              fun(C) ->
                                  {ok, binary:split(C, [<<",">>], [global])}
                              end),
  Playlist3 = get_attr(Data,
                       <<"RESOLUTION">>,
                       Playlist2,
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
  Playlist4 = get_quoted_attr(Data, <<"AUDIO">>, Playlist3, audio),
  Playlist5 = get_quoted_attr(Data, <<"VIDEO">>, Playlist4, video),
  State#{playlist => Playlist5}.

parse_ext_x_i_frame_stream_inf(Data, #{i_frame_playlists := Playlists} = M3U8) ->
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
  Playlist1 = get_attr(Data, <<"PROGRAM-ID">>, Playlist0, program_id),
  Playlist2 = get_quoted_attr(Data,
                              <<"CODECS">>,
                              Playlist1,
                              codecs,
                              fun(C) ->
                                  {ok, binary:split(C, [<<",">>], [global])}
                              end),
  Playlist3 = get_attr(Data,
                       <<"RESOLUTION">>,
                       Playlist2,
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
  Playlist4 = get_quoted_attr(Data, <<"URI">>, Playlist3, uri),
  Playlist5 = get_quoted_attr(Data, <<"VIDEO">>, Playlist4, video),
  M3U8#{i_frame_playlists => [Playlist5|Playlists]}.

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
  case re:run(Data, <<Name/binary, "=\"(.*)\"[,|$]">>, [{capture, all_but_first, binary}]) of
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

