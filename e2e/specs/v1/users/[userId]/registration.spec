# ユーザー情報を登録することができる

|architecture|
|------------|
|simple      |
|taglessFinal|
|polysemy    |
|heftia      |
## ユーザー情報を登録することができる
* パス"/v1/users/1"(architecture=<architecture>)にJSON<file:fixtures/v1/users/[userId]/registration/request.json>でPUTリクエストを送信する
* レスポンスのステータスコードが"200"である
* レスポンスボディが"OK"である
* "users"テーブルの内容が"fixtures/v1/users/[userId]/registration/expected"にあるCSVファイルの内容と一致する
* "user_profiles"テーブルの内容が"fixtures/v1/users/[userId]/registration/expected"にあるCSVファイルの内容と一致する
