
%% ---------------------------------------------------------------------------------------------------------------------
%% Record describing response to a server command
%% Records of this type are converted to JSON and sent to the client in response to receiving a command

-record(cmd_response, {from_server, cmd, status, reason, payload}).

