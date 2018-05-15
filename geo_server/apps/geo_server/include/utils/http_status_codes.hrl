%% ---------------------------------------------------------------------------------------------------------------------
%% Transform all known HTTP status codes into a tuple of {Boolean, String}
%% 
%% Boolean = Is status code IANA standard?
%% String  = Text description
%% ---------------------------------------------------------------------------------------------------------------------


%% Information 100 -> 199
http_status_code(100) -> {true , <<"Continue">>};
http_status_code(101) -> {true , <<"Switching Protocols">>};
http_status_code(102) -> {true , <<"Processing">>};
http_status_code(103) -> {false, <<"Checkpoint">>};


%% Success 200 -> 299
http_status_code(200) -> {true , <<"OK">>};
http_status_code(201) -> {true , <<"Created">>};
http_status_code(202) -> {true , <<"Accepted">>};
http_status_code(203) -> {true , <<"Non-authoritative Information">>};
http_status_code(204) -> {true , <<"No Content">>};
http_status_code(205) -> {true , <<"Reset Content">>};
http_status_code(206) -> {true , <<"Partial Content">>};
http_status_code(207) -> {true , <<"Multi-Status">>};
http_status_code(208) -> {true , <<"Already Reported">>};
http_status_code(226) -> {true , <<"IM Used">>};


%% Redirection 300 -> 399
http_status_code(300) -> {true , <<"Multiple Choices">>};
http_status_code(301) -> {true , <<"Moved Permanently">>};
http_status_code(302) -> {true , <<"Found">>};
http_status_code(303) -> {true , <<"See Other">>};
http_status_code(304) -> {true , <<"Not Modified">>};
http_status_code(305) -> {true , <<"Use Proxy">>};
http_status_code(307) -> {true , <<"Temporary Redirect">>};
http_status_code(308) -> {true , <<"Permanent Redirect">>};


%% Client-side errors 400 -> 499
http_status_code(400) -> {true , <<"Bad Request">>};
http_status_code(401) -> {true , <<"Unauthorized">>};
http_status_code(402) -> {true , <<"Payment Required">>};
http_status_code(403) -> {true , <<"Forbidden">>};
http_status_code(404) -> {true , <<"Not Found">>};
http_status_code(405) -> {true , <<"Method Not Allowed">>};
http_status_code(406) -> {true , <<"Not Acceptable">>};
http_status_code(407) -> {true , <<"Proxy Authentication Required">>};
http_status_code(408) -> {true , <<"Request Timeout">>};
http_status_code(409) -> {true , <<"Conflict">>};
http_status_code(410) -> {true , <<"Gone">>};
http_status_code(411) -> {true , <<"Length Required">>};
http_status_code(412) -> {true , <<"Precondition Failed">>};
http_status_code(413) -> {true , <<"Payload Too Large">>};
http_status_code(414) -> {true , <<"Request-URI Too Long">>};
http_status_code(415) -> {true , <<"Unsupported Media Type">>};
http_status_code(416) -> {true , <<"Requested Range Not Satisfiable">>};
http_status_code(417) -> {true , <<"Expectation Failed">>};
http_status_code(418) -> {false, <<"I'm a teapot">>};
http_status_code(420) -> {false, <<"Method Failure">>};
http_status_code(421) -> {true , <<"Misdirected Request">>};
http_status_code(422) -> {true , <<"Unprocessable Entity">>};
http_status_code(423) -> {true , <<"Locked">>};
http_status_code(424) -> {true , <<"Failed Dependency">>};
http_status_code(426) -> {true , <<"Upgrade Required">>};
http_status_code(428) -> {true , <<"Precondition Required">>};
http_status_code(429) -> {true , <<"Too Many Requests">>};
http_status_code(431) -> {true , <<"Request Header Fields Too Large">>};
http_status_code(440) -> {false, <<"Login Time-out">>};
http_status_code(444) -> {false, <<"No Response">>};
http_status_code(449) -> {false, <<"Retry With">>};
http_status_code(450) -> {false, <<"Blocked by Windows Parental Control">>};
http_status_code(451) -> {false, <<"Unavailable For Legal Reasons">>};
http_status_code(495) -> {false, <<"SSL Certificate Error">>};
http_status_code(496) -> {false, <<"SSL Certificate Required">>};
http_status_code(497) -> {false, <<"HTTP Request Sent to HTTPS Port">>};
http_status_code(498) -> {false, <<"Invalid Token (Esri)">>};
http_status_code(499) -> {false, <<"Client Closed Request">>};


%% Server-side errors 500 -> 599
http_status_code(500) -> {true , <<"Internal Server Error">>};
http_status_code(501) -> {true , <<"Not Implemented">>};
http_status_code(502) -> {true , <<"Bad Gateway">>};
http_status_code(503) -> {true , <<"Service Unavailable">>};
http_status_code(504) -> {true , <<"Gateway Timeout">>};
http_status_code(505) -> {true , <<"HTTP Version Not Supported">>};
http_status_code(506) -> {true , <<"Variant Also Negotiates">>};
http_status_code(507) -> {true , <<"Insufficient Storage">>};
http_status_code(508) -> {true , <<"Loop Detected">>};
http_status_code(509) -> {false, <<"Bandwidth Limit Exceeded">>};
http_status_code(510) -> {true , <<"Not Extended">>};
http_status_code(511) -> {true , <<"Network Authentication Required">>};
http_status_code(520) -> {false, <<"Unknown Error">>};
http_status_code(521) -> {false, <<"Web Server Is Down">>};
http_status_code(522) -> {false, <<"Connection Timed Out">>};
http_status_code(523) -> {false, <<"Origin Is Unreachable">>};
http_status_code(524) -> {false, <<"A Timeout Occurred">>};
http_status_code(525) -> {false, <<"SSL Handshake Failed">>};
http_status_code(526) -> {false, <<"Invalid SSL Certificate">>};
http_status_code(527) -> {false, <<"Railgun Error">>};
http_status_code(530) -> {false, <<"Site is frozen">>};
http_status_code(598) -> {false, <<"(Informal convention) Network read/connect timeout error">>};


%% Unknown status code
http_status_code(SC) -> {false, list_to_binary([<<"Unknown status code ">>, integer_to_binary(SC)])}.
