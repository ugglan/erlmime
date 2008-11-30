-module(mimetools).
-include_lib("eunit/include/eunit.hrl").

-export([encode/2, create_mail/2]).


create_mail(Headers, Body) ->
    [headers(Headers), body(Body)].



headers(Headers) ->
    [ header( Header, Value )  || {Header, Value} <- Headers ].


header(Header, Value) ->
    [ Header, <<": ">>, Value, <<"\r\n">> ].


body({CharSet, Encoding, Body})->
    body({"text/plain", CharSet, Encoding, Body});

body({Type, CharSet, Encoding, Body}) ->
    EncodingStr = case Encoding of
		      quoted_printable -> <<"quoted-printable">>;
		      base64 -> <<"base64">>
				    end,
    [ headers( [ { <<"Content-Type">>, [Type, <<"; charset=\"">>, CharSet, <<"\"">>] },
		 { <<"Content-Transfer-Encoding">>, EncodingStr } ] ),
      <<"\r\n">>,
      encode(Encoding, Body) ].


%%
%% Encoding formats
%%

encode(quoted_printable, IoList) -> encode_qp(iolist_to_binary(IoList), [], 0);

encode(base64, IoList) -> encode_b64(iolist_to_binary(IoList)).

%%
%% Quoted printable encoding as specified in RFC 2045.
%%
%% This implementation changes unix newline (LF) to CRLF. A single CR
%% is converted to =0D unless directly followed by a LF. And CRLF is
%% kept as CRLF. This means that it is not suitable for non-text data.
%%

encode_qp(<<>>, Acc, Width) when Width<77 -> lists:reverse(Acc);

encode_qp(<<$\n,In/binary>>, Acc, _Width) ->  encode_qp(In, [<<"\r\n">> | Acc], 0);
encode_qp(<<"\r\n",In/binary>>, Acc, _Width) -> encode_qp(In, [<<"\r\n">> | Acc], 0);

encode_qp(In, [L|Acc], Width) when Width>75 ->
    encode_qp(In, [L,<<"=\r\n">> | Acc], 
	      case is_binary(L) of true -> 3; false -> 1 end);

encode_qp(<<C,In/binary>>, Acc, Width) when C>32, C<127, C=/=61 -> 
    encode_qp(In, [C | Acc], Width+1);

encode_qp(<<C,$\n,In/binary>>, Acc, Width) when C==9; C==32 -> 
    encode_qp(<<$\n,In/binary>>, [char_to_code(C) | Acc], Width+3);

encode_qp(<<C>>, Acc, Width) when C==9; C==32 -> 
    encode_qp(<<>>, [char_to_code(C) | Acc], Width+3);

encode_qp(<<C,In/binary>>, Acc, Width) when C==9; C==32 -> 
    encode_qp(In, [C | Acc], Width+1);

encode_qp(<<C,In/binary>>, Acc, Width) ->
    encode_qp(In, [char_to_code(C) | Acc], Width+3).


char_to_code(C) when is_integer(C) ->
    <<Hi:4,Lo:4>> = <<C>>,
    Hi8=hexdigit(Hi), Lo8=hexdigit(Lo),
    <<$=, Hi8, Lo8 >>.

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

%%
%% Base 64 encoding 
%%
%% Uses stdlib, then splits lines at 76 chars and inserts CRLF
%%

encode_b64(In) ->
    encode_b64(base64:encode(In),[]).

encode_b64(<<>>, Acc) -> lists:reverse(Acc);

encode_b64(<<Row:76/binary, Rest/binary>>, Acc) ->
    encode_b64(Rest, [<<"\r\n">>, Row | Acc]);

encode_b64(Rest, Acc) -> encode_b64(<<>>, [Rest | Acc]).



%%
%% Tests
%%

base64_test() ->
    BEncode=fun(X)->binary_to_list(iolist_to_binary(encode(base64, X))) end,
    [
     ?assertEqual(
	  "MTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTEx\r\n"++
	  "MTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTEx\r\n"++
	  "MTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTEx\r\n"++
	  "MTExMTExMTExMTExMTExMTExMTExMTExMQo=", BEncode([string:copies("1",196),$\n])),
     ?assertEqual("SGVsbG8gd29ybGQh", BEncode("Hello world!")),
     ?assertEqual("", BEncode(""))
    ].

quoted_printable_test() ->
    Txt = "1234567890",
    Blank70 = string:copies(" ",70),     
    Blank75 = Blank70++"     ",
    Txt6 = string:copies(Txt,6), 
    Txt7 = Txt6++Txt,
    FlatEncode=fun(X)->binary_to_list(iolist_to_binary(encode(quoted_printable, X))) end,
    [
     ?assertEqual("", FlatEncode("")),

     ?assertEqual(Blank75++"=\r\n"++Blank75++"=\r\n=20", FlatEncode(Blank75++Blank75++" ")),
     ?assertEqual(Blank75++"=\r\n=3D"++Blank70++"  =\r\n   =3D", FlatEncode(Blank75++"="++Blank75++"=")),
     
     ?assertEqual(Txt7++"123456", FlatEncode(Txt7++"123456")),
     ?assertEqual(Txt7++"123=3D", FlatEncode(Txt7++"123=")),
     ?assertEqual(Txt7++"1234=\r\n=3D", FlatEncode(Txt7++"1234=")),
     ?assertEqual(Txt7++"12=3D=\r\n456", FlatEncode(Txt7++"12=456")),

     ?assertEqual(Blank75++"=\r\n    =20", FlatEncode(Blank75++"     ")),
     ?assertEqual(Blank75++"=\r\n=3D  =20", FlatEncode(Blank75++"=   ")),
     ?assertEqual(Blank75++"=\r\n=3D", FlatEncode(Blank75++"=")),

     ?assertEqual("=0D=0D=0D=0D", FlatEncode("\r\r\r\r")),
     ?assertEqual("=0D=0D\r\n", FlatEncode("\r\r\r\n")),
     ?assertEqual("\r\n\r\n\r\n\r\n", FlatEncode("\n\n\n\n")),
     ?assertEqual("\r\n1\r\n\r\n123\r\n", FlatEncode("\n1\n\n123\n")),
     ?assertEqual("\r\n=20\r\n =09\r\n\t  =09\r\n", FlatEncode("\n \n \t\n\t  \t\n")),
     ?assertEqual("\r\n1\r\n\r\n123\r\n", FlatEncode("\r\n1\n\n123\r\n")),

     ?assertEqual(Txt7++"123456\r\n1234567890\r\n", FlatEncode(Txt7++"123456\n1234567890\n")),
     ?assertEqual(Txt7++"123=3D\r\n1234567890\r\n", FlatEncode(Txt7++"123=\n1234567890\n")),
     ?assertEqual(Txt7++"123456\r\n=3D234567890\r\n", FlatEncode(Txt7++"123456\n=234567890\n")),
     ?assertEqual(Txt7++"123=3D\r\n=3D234567890\r\n", FlatEncode(Txt7++"123=\n=234567890\n")),

     ?assertEqual(Txt7++"12345=\r\n6=3D890\r\n", FlatEncode(Txt7++"123456=890\n")),
     ?assertEqual(Txt7++"12345=\r\n=3D7890\r\n", FlatEncode(Txt7++"12345=7890\n")),
     ?assertEqual(Txt7++"1234=\r\n=3D67890\r\n", FlatEncode(Txt7++"1234=67890\n")),
     ?assertEqual(Txt7++"123=\r\n=3D567890\r\n", FlatEncode(Txt7++"123=567890\n")),
     ?assertEqual(Txt7++"12=3D=\r\n4567890\r\n", FlatEncode(Txt7++"12=4567890\n")),
     ?assertEqual(Txt7++"1=3D3=\r\n4567890\r\n", FlatEncode(Txt7++"1=34567890\n")),

     ?assertEqual(Txt7++"12345=\r\n6=3D=3D90\r\n", FlatEncode(Txt7++"123456==90\n")),
     ?assertEqual(Txt7++"12345=\r\n=3D=3D890\r\n", FlatEncode(Txt7++"12345==890\n")),
     ?assertEqual(Txt7++"1234=\r\n=3D=3D7890\r\n", FlatEncode(Txt7++"1234==7890\n")),
     ?assertEqual(Txt7++"123=\r\n=3D=3D67890\r\n", FlatEncode(Txt7++"123==67890\n")),
     ?assertEqual(Txt7++"12=3D=\r\n=3D567890\r\n", FlatEncode(Txt7++"12==567890\n")),
     ?assertEqual(Txt7++"1=3D=\r\n=3D4567890\r\n", FlatEncode(Txt7++"1==4567890\n")),
     ?assertEqual(Txt7++"=3D=\r\n=3D34567890\r\n", FlatEncode(Txt7++"==34567890\n")),
     ?assertEqual(Txt6++"123456789=3D=3D=\r\n234567890\r\n", FlatEncode(Txt6++"123456789==234567890\n")),
     ?assertEqual(Txt6++"12345678=3D=3D1=\r\n234567890\r\n", FlatEncode(Txt6++"12345678==1234567890\n")),

     ?assertEqual(Txt7++"12345=\r\n67890\r\n", FlatEncode(string:copies(Txt,8)++"\n")),
     ?assertEqual(Txt7++"123456\r\n", FlatEncode(Txt7++"123456\n")),

     ?assertEqual(
	Txt7++"12345=\r\n67890"++Txt7++"=\r\n"++Txt7++"12345=\r\n67890"++Txt7++"=\r\nTobias L=C3=B6fgren =C3=85=C3=84=C3=96 "++string:copies(Txt,3)++"123456=\r\n7890"++string:copies(Txt,3)++"1234=3D//=3D-=3D567890"++Txt++Txt++"\r\n", 
	FlatEncode(string:copies(Txt,30)++"Tobias Löfgren ÅÄÖ "++Txt7++"1234=//=-=567890"++Txt++Txt++"\n")),

     ?assertEqual("=3D=3D=3D=3D=3D", FlatEncode("=====")),
     ?assertEqual("Hej\r\nd=C3=A5!", FlatEncode("Hej\ndå!")),
     ?assertEqual("Test of \twhitespace,\t=20\r\nbefore newline.\r\n", FlatEncode("Test of \twhitespace,\t \nbefore newline.\n")),
     ?assertEqual("Tobias L=C3=B6fgren", FlatEncode("Tobias Löfgren")),
     ?assertEqual("Tobias Lofgren", FlatEncode("Tobias Lofgren"))
    ].


