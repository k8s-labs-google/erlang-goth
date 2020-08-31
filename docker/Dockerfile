FROM erlang:22.3

COPY . .

RUN rebar3 compile
RUN rebar3 dialyzer

CMD ["rebar3", "compile"]
