# ユーザー情報を登録することができる

## ユーザー情報を登録することができる(単純な実装)
* パス"/v1/users/1?architecture=simple"にJSON<file:fixtures/v1/users/[userId]/registration/request.json>でPUTリクエストを送信する
* レスポンスのステータスコードが"200"である
* レスポンスボディが"OK"である
* "users"テーブルの内容が"fixtures/v1/users/[userId]/registration/expected"にあるCSVファイルの内容と一致する
* "user_notifications"テーブルの内容が"fixtures/v1/users/[userId]/registration/expected"にあるCSVファイルの内容と一致する
