%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(mailtools).

-include_lib("eunit/include/eunit.hrl").

-export([mail/4, clean_mail/1]).


mail(From, To, Subject, Body) ->
    {ok, Pid} = smtp_fsm:start(),
    smtp_fsm:ehlo(Pid),
    Mail=[

	  "From: ",From,"\n",
	  "To: ",To,"\n",
	  "Subject: ",Subject,"\n",
	  "Content-Type: text/plain; charset=\"utf-8\"\n",
	  "Content-Transfer-Encoding: quoted-printable\n"
	  "\n",
	  mimetools:encode(quoted_printable, Body)
	 ],
    smtp_fsm:sendemail(Pid, clean_mail(From), clean_mail(To), Mail),
    smtp_fsm:close(Pid).



clean_mail(Address) ->
   {match, Maillist} 
	= re:run(
	    Address,
	    "((^.*<\s*(?<bmail>.+@[^ ]+)\s*>\s*$)|(^\s*(?<mail>[^ <>]+@[^ <>]+)\s*$))",
	    [ {capture, [bmail, mail] ,list } ]),
    lists:flatten(Maillist).

clean_mail_test() ->
    [
     ?assertError({badmatch, nomatch} ,clean_mail("Foo foo@example.com")),
     ?assertError({badmatch, nomatch} ,clean_mail("Foo <fooexample.com>")),
     ?assertError({badmatch, nomatch} ,clean_mail("foo@example.com>")),
     ?assertError({badmatch, nomatch} ,clean_mail("fooexample.com")),
     ?assertError({badmatch, nomatch} ,clean_mail("<foo@example.com> Tobias")),
     ?assertMatch("foo@example.com",clean_mail("Foo <foo@example.com>")),
     ?assertMatch("foo@example.com",clean_mail("Foo <foo@example.com>    ")),
     ?assertMatch("apa@bepa.se",clean_mail("<apa@bepa.se>")),
     ?assertMatch("owl@pinkroom.biz",clean_mail("Tobias Löfgren <owl@pinkroom.biz>")),
     ?assertMatch("owl@pinkroom.biz",clean_mail("<owl@pinkroom.biz>")),
     ?assertMatch("owl@pinkroom.biz",clean_mail("<    owl@pinkroom.biz   >")),
     ?assertMatch("$@yo.biz",clean_mail("Me My Yo<$@yo.biz>")),
     ?assertMatch("a@b.no",clean_mail("   a@b.no     ")),
     ?assertMatch("a@b.no",clean_mail("a@b.no")),
     ?assertMatch("owl@pinkroom.biz",clean_mail("owl@pinkroom.biz"))
    ].

