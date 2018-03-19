% @hidden
-module(m3u8_prv_downloader).

-export([run/2]).

-spec run(URI :: m3u8:uri(), DestDir :: file:filename_all()) -> {ok, file:filename_all()} | {error, term()}.
run(URI, DestDir) when (is_list(URI) orelse is_binary(URI)), (is_list(DestDir) orelse is_binary(DestDir)) ->
  run(#{uri => URI, alt_name => "adaptive.m3u8", dest => DestDir, parse => true}, #{base_uri => filename:dirname(URI), dest => DestDir});
run(#{uri := URI, dest := DestDir} = Manifest, State) ->
  case bucfile:make_dir(DestDir) of
    ok ->
      download(Manifest#{
        base_uri => filename:dirname(URI)
       }, State);
    Error ->
      Error
  end.

download(#{uri := URI, output := _Output} = Manifest, State) ->
  case download(URI) of
    {ok, Data} ->
      write_file(Manifest#{data => Data}, State);
    Error ->
      Error
  end;
download(#{uri := URI} = Manifest, State) ->
  case bucuri:type(URI) of
    {ok, Type} when Type == http; Type == https ->
      download(Manifest#{output => filename(Manifest)}, State);
    error ->
      {error, {invalid_uri, URI}}
  end.

download(URI) ->
  case httpc:request(get, {bucs:to_string(URI), []}, [{autoredirect, true}], []) of
    {ok, {{_, 200, _}, _, Body}} ->
      {ok, bucs:to_binary(Body)};
    _X ->
      {error, {cant_get, URI}}
  end.

filename(#{dest := DestDir, uri := URI, base_uri := BaseURI, alt_name := AltName}) ->
  Name = re:replace(
           filename:join(DestDir, bucfile:relative_from(URI, BaseURI)),
           "[\\?=\\&\"@\\-#~()\[\]]",
           "_",
           [global, {return, list}]),
  case filename:extension(Name) of
    [] -> filename:join(DestDir, AltName);
    _ -> Name
  end.

write_file(#{output := Output, data := Data, parse := true} = Manifest, State) ->
  case m3u8:parse(Data) of
    {ok, M3U8} ->
      case save_content(M3U8, Manifest, State) of
        {ok, FinalM3U8} ->
          case m3u8:to_file(FinalM3U8, Output) of
            ok ->
              {ok, Output};
            Error ->
              Error
          end;
        Error ->
          Error
      end;
    Error ->
      Error
  end;
write_file(#{output := Output, data := Data}, _State) ->
  case file:write_file(Output, bucs:to_binary(Data)) of
    ok ->
      {ok, Output};
    Error ->
      Error
  end.

save_content(M3U8, Manifest, State) ->
  get_enc_keys(
    get_segments(
      get_playlists(
        get_medias({ok, M3U8}, Manifest, State),
        Manifest, State),
      Manifest, State),
    Manifest, State).

get_medias({ok, #{medias := Medias} = M3U8}, #{dest := DestDir, base_uri := BaseURI}, State) ->
  case do_get_datas(Medias,
                    #{dest => filename:join(DestDir, "medias"),
                      base_uri => BaseURI,
                      alt_prefix => "media",
                      alt_ext => "m3u8",
                      parse => true},
                    State,
                    []) of
    {ok, FinalMedias} ->
      {ok, m3u8:medias(M3U8, lists:reverse(FinalMedias))};
    Error ->
      Error
  end;
get_medias(Other, _Manifest, _State) ->
  Other.

get_playlists({ok, #{playlists := Playlists} = M3U8}, #{dest:= DestDir, base_uri := BaseURI}, State) ->
  case do_get_datas(Playlists,
                    #{dest => filename:join(DestDir, "playlists"),
                      base_uri => BaseURI,
                      alt_prefix => "playlist",
                      alt_ext => "m3u8",
                      parse => true},
                    State,
                    []) of
    {ok, FinalPlaylists} ->
      {ok, m3u8:playlists(M3U8, lists:reverse(FinalPlaylists))};
    Error ->
      Error
  end;
get_playlists(Other, _Manifest, _State) ->
  Other.

get_segments({ok, #{segments := Segments} = M3U8}, #{dest:= DestDir, base_uri := BaseURI, output := Output}, State) ->
  Base = filename:basename(Output, filename:extension(Output)),
  case do_get_datas(Segments,
                    #{dest => filename:join(DestDir, Base),
                      base_uri => BaseURI,
                      alt_prefix => "segment",
                      alt_ext => "ts", parse => false},
                    State#{dest => DestDir},
                    []) of
    {ok, FinalSegments} ->
      {ok, m3u8:segments(M3U8, lists:reverse(FinalSegments))};
    Error ->
      Error
  end;
get_segments(Other, _Manifest, _State) ->
  Other.

get_enc_keys({ok, #{keys := Keys} = M3U8}, #{dest:= DestDir, base_uri := BaseURI, output := Output}, State) ->
  Base = filename:basename(Output, filename:extension(Output)),
  case do_get_datas(Keys,
                    #{dest => filename:join(DestDir, Base),
                      base_uri => BaseURI,
                      alt_prefix => "enc",
                      alt_ext => "key",
                      parse => false},
                    State#{dest => DestDir},
                    []) of
    {ok, FinalKeys} ->
      {ok, m3u8:keys(M3U8, lists:reverse(FinalKeys))};
    Error ->
      Error
  end;
get_enc_keys(Other, _Manifest, _State) ->
  Other.

do_get_datas([], _LocalState, _State, Medias) -> {ok, Medias};
do_get_datas([#{uri := URI} = Current|Rest],
             #{dest := DestDir,
               base_uri := BaseURI,
               alt_prefix := AltPrefix,
               alt_ext := AltExt,
               parse := Parse} = LocalState,
             #{dest := BaseDestDir} = State,
             Medias) ->
  case get_url(URI, BaseURI) of
    {ok, URL} ->
      case run(#{uri => URL, dest => DestDir, alt_name => alt_name(AltPrefix, length(Medias), AltExt), parse => Parse}, State) of
        {ok, MediaFile} ->
          do_get_datas(Rest, LocalState, State, [Current#{uri => bucs:to_binary(bucfile:relative_from(MediaFile, BaseDestDir))}|Medias]);
        Error ->
          Error
      end;
    Error ->
      Error
  end;
do_get_datas([Current|Rest], LocalState, State, Medias) ->
  do_get_datas(Rest, LocalState, State, [Current|Medias]).

get_url(URI, BaseURI) ->
  case bucuri:type(URI) of
    {ok, http} -> {ok, URI};
    {ok, https} -> {ok, URI};
    {ok, _} -> {error, {invalid_url, URI}};
    error -> {ok, bucuri:join(BaseURI, bucs:to_string(URI))}
  end.

alt_name(Name, N, Ext) ->
  lists:flatten(io_lib:format("~s_~w.~s", [Name, N, Ext])).
