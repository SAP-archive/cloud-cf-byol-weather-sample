%% ---------------------------------------------------------------------------------------------------------------------
%% Since the NOW macro uses an anonymous function, its internal variables exist in the scope of the calling function;
%% therefore, the variable names used here are longer to reduce the risk of an invisible name clash which would then
%% lead to weird "no_match" exceptions at runtime...
%%
%% Switched from the use of erlang:now/0 to os:timestamp/0
%% erlang:now/0 returns a monotonically increasing timestamp guarunteed to be unique across the entire node.
%% The problem with this function however is that it requires the timer value to be locked/unlocked before/after each
%% call - and this simply doesn't scale!
%%
%% ---------------------------------------------------------------------------------------------------------------------
-define(NOW, (fun({_,_,Micro} = TS) ->
    {{YYYY_temp,MM_temp,DD_temp},{H_temp,M_temp,S_temp}} = calendar:now_to_local_time(TS),
    {{YYYY_temp,MM_temp,DD_temp},{H_temp,M_temp,S_temp,Micro}}
   end)
   (os:timestamp())
  ).

