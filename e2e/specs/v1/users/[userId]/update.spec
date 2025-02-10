# ユーザー情報を更新することができる
* "fixtures/v1/users/[userId]/update/setup"にあるCSVファイルの内容がDBに登録されている

## 既存のユーザー情報を更新することができる(単純な実装)
* パス"/v1/users/1?architecture=simple"にJSON<file:fixtures/v1/users/[userId]/update/request.json>でPUTリクエストを送信する
* レスポンスのステータスコードが"200"である
* レスポンスボディが"OK"である
* "users"テーブルの内容が"fixtures/v1/users/[userId]/update/expected"にあるCSVファイルの内容と一致する
* "user_notifications"テーブルの内容が"fixtures/v1/users/[userId]/update/expected"にあるCSVファイルの内容と一致する