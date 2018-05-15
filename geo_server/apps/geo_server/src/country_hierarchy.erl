-module(country_hierarchy).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/02/16 14:15:02").
-created_by("chris.whealy@sap.com").

-export([
    init/2
  , start/1
]).

-include("../include/records/geoname.hrl").

%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Initialise the country hierarchy server
init(MyName, FCA) -> register(MyName, spawn_link(?MODULE, start, [FCA])).

%% ---------------------------------------------------------------------------------------------------------------------
%% Start the hierarchy server for this country
start(FCA) -> wait_for_msg(seperate_admin_codes(FCA)).

%% ---------------------------------------------------------------------------------------------------------------------
wait_for_msg(AdminLists) ->
  receive
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Return the admin[1,2,3,4] description of a given FCP record
    {name_lookup, FCPRec, CountryServerPid} ->
      CountryServerPid ! populate_admin_txt(FCPRec, AdminLists);

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Commands from country server
    {cmd, stop} ->
      exit(normal)

  end,

  wait_for_msg(AdminLists).

%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Fill in the admin[1,2,3,4]_txt fields of the given FCP record
populate_admin_txt(FCPRec, {Admin1List, Admin2List, Admin3List, Admin4List}) ->
  FCPRec#geoname_int{
      admin1_txt = get_text_for_admin_code(FCPRec#geoname_int.admin1, 1, Admin1List)
    , admin2_txt = get_text_for_admin_code(FCPRec#geoname_int.admin2, 2, Admin2List)
    , admin3_txt = get_text_for_admin_code(FCPRec#geoname_int.admin3, 3, Admin3List)
    , admin4_txt = get_text_for_admin_code(FCPRec#geoname_int.admin4, 4, Admin4List)
  }.

%% ---------------------------------------------------------------------------------------------------------------------
%% Divide up the FCA file into 4 lists of ADM1-4 values respectively
seperate_admin_codes(FCA) -> seperate_admin_codes(FCA, {[], [], [], []}).

seperate_admin_codes([], Acc) -> Acc;

seperate_admin_codes([FCARec | Rest], {A1, A2, A3, A4}) ->
  AdminLists = case FCARec#geoname_int.feature_code of
    <<"ADM1">> -> {A1 ++ [FCARec], A2, A3, A4};
    <<"ADM2">> -> {A1, A2 ++ [FCARec], A3, A4};
    <<"ADM3">> -> {A1, A2, A3 ++ [FCARec], A4};
    <<"ADM4">> -> {A1, A2, A3, A4 ++ [FCARec]};
    _          -> {A1, A2, A3, A4}
  end,

  seperate_admin_codes(Rest, AdminLists).

%% ---------------------------------------------------------------------------------------------------------------------
get_text_for_admin_code(AdminCode, AdminNo, AdminList) ->
  ElNo = #geoname_int.admin1 + AdminNo - 1,

  case lists:keyfind(AdminCode, ElNo, AdminList) of
    false  -> undefined;
    FCPRec -> FCPRec#geoname_int.name
  end.

