%% We're only interested in administrative areas and populations centres
-define(FEATURE_CLASS_A, <<"A">>).
-define(FEATURE_CLASS_P, <<"P">>).

%% The administrative centres in which we are interested
-define(FEATURE_CODE_ADM1, <<"ADM1">>).         %% 1st order administrative division
-define(FEATURE_CODE_ADM2, <<"ADM2">>).         %% 2nd order administrative division
-define(FEATURE_CODE_ADM3, <<"ADM3">>).         %% 3rd order administrative division
-define(FEATURE_CODE_ADM4, <<"ADM4">>).         %% 4th order administrative division
-define(FEATURE_CODE_ADM5, <<"ADM5">>).         %% 5th order administrative division
-define(FEATURE_CODE_ADMD, <<"ADMD">>).         %% Administrative division not differentiated by level
-define(FEATURE_CODE_PCL,  <<"PCL">>).          %% Political Entity
-define(FEATURE_CODE_PCLD, <<"PCLD">>).         %% Dependent political entity
-define(FEATURE_CODE_PCLF, <<"PCLF">>).         %% Freely associated state
-define(FEATURE_CODE_PCLI, <<"PCLI">>).         %% Independent political entity
-define(FEATURE_CODE_PCLS, <<"PCLS">>).         %% Semi-independent political entity

%% The population centres in which we are interested
-define(FEATURE_CODE_PPL,   <<"PPL">>).         %% Populated place
-define(FEATURE_CODE_PPLA,  <<"PPLA">>).        %% Seat of 1st order administrative division
-define(FEATURE_CODE_PPLA2, <<"PPLA2">>).       %% Seat of 2nd order administrative division
-define(FEATURE_CODE_PPLA3, <<"PPLA3">>).       %% Seat of 3rd order administrative division
-define(FEATURE_CODE_PPLA4, <<"PPLA4">>).       %% Seat of 4th order administrative division
-define(FEATURE_CODE_PPLC,  <<"PPLC">>).        %% Capital of political entity
-define(FEATURE_CODE_PPLG,  <<"PPLG">>).        %% Seat of government of political entity
-define(FEATURE_CODE_PPLS,  <<"PPLS">>).        %% Some agglomeration of buildings in which people live and work
-define(FEATURE_CODE_PPLX,  <<"PPLX">>).        %% Section of populated place

