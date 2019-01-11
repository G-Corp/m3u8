-module(m3u8_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").

m3u8_tests_test_() ->
  {setup,
   fun() ->
     ok
   end,
   fun(_) ->
     ok
   end,
   [
     fun() ->
         t("test/data/alternate-media-1.m3u8"),
         t("test/data/alternate-media-2.m3u8"),
         t("test/data/byterange.m3u8"),
         t("test/data/discontinuity-aes.m3u8"),
         t("test/data/discontinuity.m3u8"),
         t("test/data/event.m3u8"),
         t("test/data/master_720.m3u8"),
         t("test/data/variant.m3u8"),
         t("test/data/vod-local.m3u8"),
         t("test/data/vod.m3u8")
     end,
     fun() ->
         ?assertEqual("mp4a.40.2", m3u8:audio_codec_code("aac-lc")),
         ?assertEqual("mp4a.40.5", m3u8:audio_codec_code("he-aac")),
         ?assertEqual("mp4a.40.34", m3u8:audio_codec_code("mp3")),
         ?assertEqual("mp4a.40.2", m3u8:audio_codec_code("AAC-LC")),
         ?assertEqual("mp4a.40.5", m3u8:audio_codec_code("HE-AAC")),
         ?assertEqual("mp4a.40.34", m3u8:audio_codec_code("MP3")),

         ?assertEqual("avc1.66.30", m3u8:video_codec_code("baseline" , 3.0)),
         ?assertEqual("avc1.42001f", m3u8:video_codec_code("baseline" , 3.1)),
         ?assertEqual("avc1.77.30", m3u8:video_codec_code("main" , 3.0)),
         ?assertEqual("avc1.4d001f", m3u8:video_codec_code("main" , 3.1)),
         ?assertEqual("avc1.4d0028", m3u8:video_codec_code("main" , 4.0)),
         ?assertEqual("avc1.4d0029", m3u8:video_codec_code("main" , 4.1)),
         ?assertEqual("avc1.64001f", m3u8:video_codec_code("high" , 3.1)),
         ?assertEqual("avc1.640028", m3u8:video_codec_code("high" , 4.0)),
         ?assertEqual("avc1.640029", m3u8:video_codec_code("high" , 4.1))

     end,
     fun() ->
         M = m3u8:new(),
         {ok, M0} = m3u8:segment(M, #{duration => 200, uri => <<"hello1.ts">>}),
         {ok, M1} = m3u8:segment(M0, #{duration => 200, uri => <<"hello2.ts">>}),
         {ok, M2} = m3u8:key(M1, #{method => <<"AES-128">>, uri => <<"key.enc">>, iv => <<"azertqsdfgwxcvb">>}),
         ?assertMatch(
            #{allow_cache := undefined,
              i_frames_only := undefined,
              keys := [#{iv := <<"azertqsdfgwxcvb">>,
                         method := <<"AES-128">>,
                         uri := <<"key.enc">>}],
              media_sequence := undefined,
              medias := [],
              playlist_type := undefined,
              playlists := [],
              program_date_time := undefined,
              segments := [#{duration := 200,
                             title := <<>>,
                             uri := <<"hello1.ts">>},
                           #{duration := 200,
                             title := <<>>,
                             uri := <<"hello2.ts">>}],
              target_duration := undefined,
              version := undefined}, M2),
         ?assertEqual(<<"#EXTM3U\n#EXT-X-KEY:METHOD=AES-128,URI=\"key.enc\",IV=azertqsdfgwxcvb\n#EXTINF:200,\nhello1.ts\n#EXTINF:200,\nhello2.ts\n">>,
                      m3u8:to_binary(M2))
     end
   ]}.

t(File) ->
  ?assertContinueIfMatch(
     {ok, Parse0},
     m3u8:parse(File),
     Parse0,
     fun(P0) ->
         ?assertContinueIfMatch(
            {ok, Parse1},
            m3u8:parse(m3u8:to_binary(P0)),
            Parse1,
            fun(P1) ->
                ?assertEqual(
                   P0, P1)
            end
           )
     end).
