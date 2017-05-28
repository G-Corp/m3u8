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

