Prelude.null と Data.Map.null のように、既存の文字ジュールに似た関数を持つモジュールをつくる場合は、isMapNullのように名前がぶつからない配慮をするのではなく、あえて同じ名前の関数群を作り、ユーザーにqualifyして選んでもらうのがスマートです。

そこで

- main-todo.hs にあるように、importすると足し算と掛け算記号の意味が入れ替わるParupunteモジュールを作ってください。
- main-todo.hs のimport文を埋めてParupunteを使ってみてください。
- Parupunteを使っている同じモジュールで、Preludeの正規の足し算、掛け算も使ってみてください。
