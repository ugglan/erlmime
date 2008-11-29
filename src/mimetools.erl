-module(mimetools).
-include_lib("eunit/include/eunit.hrl").

-export([encode/2]).


encode(quoted_printable, IoList) -> encode_qp(lists:flatten(IoList), [], 0).


encode_qp([], Acc, Width) when Width<77 -> lists:reverse(Acc);

encode_qp([$\n|In], Acc, _Width) ->  encode_qp(In, [$\n | Acc], 0);
encode_qp([$\r,$\n|In], Acc, _Width) -> encode_qp(In, [$\n | Acc], 0);

encode_qp(In, [L|Acc], Width) when Width>75 ->
    encode_qp(In, [L,$\n,$= | Acc], 
	   case is_list(L) of true -> 3; false -> 1 end );

encode_qp([C|In], Acc, Width) when C>32, C<127, C=/=61 -> 
    encode_qp(In, [C | Acc], Width+1);

encode_qp([C,$\n|In], Acc, Width) when C==9; C==32 -> 
    encode_qp([$\n|In], [char_to_code(C) | Acc], Width+3);

encode_qp([C], Acc, Width) when C==9; C==32 -> 
    encode_qp([], [char_to_code(C) | Acc], Width+3);

encode_qp([C|In], Acc, Width) when C==9; C==32 -> 
    encode_qp(In, [C | Acc], Width+1);

encode_qp([C|In], Acc, Width) ->
    encode_qp(In, [char_to_code(C) | Acc], Width+3).


char_to_code(C) when is_integer(C) ->
    <<Hi:4,Lo:4>> = <<C>>,
    [$=, hexdigit(Hi), hexdigit(Lo)].

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).


all_test() ->
    Txt = "1234567890",
    Blank75 = string:copies(" ",75), 
    Txt6 = string:copies(Txt,6), 
    Txt7 = Txt6++Txt,
    FlatEncode=fun(X)->lists:flatten(encode(quoted_printable, X)) end,
    [
     ?assertEqual("", FlatEncode("")),
     
     ?assertEqual(Txt7++"123456", FlatEncode(Txt7++"123456")),
     ?assertEqual(Txt7++"123=3D", FlatEncode(Txt7++"123=")),
     ?assertEqual(Txt7++"1234=\n=3D", FlatEncode(Txt7++"1234=")),
     ?assertEqual(Txt7++"12=3D=\n456", FlatEncode(Txt7++"12=456")),

     ?assertEqual(Blank75++"=\n    =20", FlatEncode(Blank75++"     ")),
     ?assertEqual(Blank75++"=\n=3D  =20", FlatEncode(Blank75++"=   ")),
     ?assertEqual(Blank75++"=\n=3D", FlatEncode(Blank75++"=")),

     ?assertEqual("=0D=0D=0D=0D", FlatEncode("\r\r\r\r")),
     ?assertEqual("=0D=0D\n", FlatEncode("\r\r\r\n")),
     ?assertEqual("\n\n\n\n", FlatEncode("\n\n\n\n")),
     ?assertEqual("\n1\n\n123\n", FlatEncode("\n1\n\n123\n")),
     ?assertEqual("\n=20\n =09\n\t  =09\n", FlatEncode("\n \n \t\n\t  \t\n")),
     ?assertEqual("\n1\n\n123\n", FlatEncode("\r\n1\n\n123\r\n")),

     ?assertEqual(Txt7++"123456\n1234567890\n", FlatEncode(Txt7++"123456\n1234567890\n")),
     ?assertEqual(Txt7++"123=3D\n1234567890\n", FlatEncode(Txt7++"123=\n1234567890\n")),
     ?assertEqual(Txt7++"123456\n=3D234567890\n", FlatEncode(Txt7++"123456\n=234567890\n")),
     ?assertEqual(Txt7++"123=3D\n=3D234567890\n", FlatEncode(Txt7++"123=\n=234567890\n")),

     ?assertEqual(Txt7++"12345=\n6=3D890\n", FlatEncode(Txt7++"123456=890\n")),
     ?assertEqual(Txt7++"12345=\n=3D7890\n", FlatEncode(Txt7++"12345=7890\n")),
     ?assertEqual(Txt7++"1234=\n=3D67890\n", FlatEncode(Txt7++"1234=67890\n")),
     ?assertEqual(Txt7++"123=\n=3D567890\n", FlatEncode(Txt7++"123=567890\n")),
     ?assertEqual(Txt7++"12=3D=\n4567890\n", FlatEncode(Txt7++"12=4567890\n")),
     ?assertEqual(Txt7++"1=3D3=\n4567890\n", FlatEncode(Txt7++"1=34567890\n")),

     ?assertEqual(Txt7++"12345=\n6=3D=3D90\n", FlatEncode(Txt7++"123456==90\n")),
     ?assertEqual(Txt7++"12345=\n=3D=3D890\n", FlatEncode(Txt7++"12345==890\n")),
     ?assertEqual(Txt7++"1234=\n=3D=3D7890\n", FlatEncode(Txt7++"1234==7890\n")),
     ?assertEqual(Txt7++"123=\n=3D=3D67890\n", FlatEncode(Txt7++"123==67890\n")),
     ?assertEqual(Txt7++"12=3D=\n=3D567890\n", FlatEncode(Txt7++"12==567890\n")),
     ?assertEqual(Txt7++"1=3D=\n=3D4567890\n", FlatEncode(Txt7++"1==4567890\n")),
     ?assertEqual(Txt7++"=3D=\n=3D34567890\n", FlatEncode(Txt7++"==34567890\n")),
     ?assertEqual(Txt6++"123456789=3D=3D=\n234567890\n", FlatEncode(Txt6++"123456789==234567890\n")),
     ?assertEqual(Txt6++"12345678=3D=3D1=\n234567890\n", FlatEncode(Txt6++"12345678==1234567890\n")),

     ?assertEqual(Txt7++"12345=\n67890\n", FlatEncode(string:copies(Txt,8)++"\n")),
     ?assertEqual(Txt7++"123456\n", FlatEncode(Txt7++"123456\n")),

     ?assertEqual(
	Txt7++"12345=\n67890"++Txt7++"=\n"++Txt7++"12345=\n67890"++Txt7++"=\nTobias L=C3=B6fgren =C3=85=C3=84=C3=96 "++string:copies(Txt,3)++"123456=\n7890"++string:copies(Txt,3)++"1234=3D//=3D-=3D567890"++Txt++Txt++"\n", 
	FlatEncode(string:copies(Txt,30)++"Tobias Löfgren ÅÄÖ "++Txt7++"1234=//=-=567890"++Txt++Txt++"\n")),

     ?assertEqual("=3D=3D=3D=3D=3D", FlatEncode("=====")),
     ?assertEqual("Hej\nd=C3=A5!", FlatEncode("Hej\ndå!")),
     ?assertEqual("Test of \twhitespace,\t=20\nbefore newline.\n", FlatEncode("Test of \twhitespace,\t \nbefore newline.\n")),
     ?assertEqual("Tobias L=C3=B6fgren", FlatEncode("Tobias Löfgren")),
     ?assertEqual("Tobias Lofgren", FlatEncode("Tobias Lofgren"))
    ].


