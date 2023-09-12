-module(v2_eel_renderer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

% render_test() ->
%     _Snapshot = #{
%         tokens => #{
%             statics => [
%                 <<"Hello ">>, <<"!">>
%             ],
%             dynamics = #{
%                 0 => #{
%                     expr => fun(Bindings) -> maps:get(world, Bindings) end,
%                     vars => [world],
%                     engine => v2_eel_smart_engine
%                 }
%             },
%             indexes => #{
%                 vars => #{
%                     world => [0]
%                 }
%             }
%         }
%     },
%     ok.

-endif.
