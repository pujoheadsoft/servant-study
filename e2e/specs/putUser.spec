# ユーザー情報を登録することができる

## ユーザー情報を登録することができる(単純な実装)
* パス"/v1/simple/users/1"にJSON<file:fixtures/request.json>でPUTリクエストを送信する
* レスポンスのステータスコードが"200"である
* レスポンスボディが"OK"である
* "users"テーブルの内容が"fixtures/expected"にあるCSVファイルの内容と一致する