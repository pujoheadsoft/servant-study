# メールアドレスが妥当でない場合は登録できない

|architecture|
|------------|
|simple      |
|taglessFinal|
|polysemy    |
|heftia      |
## メールアドレスが妥当でない場合は登録できない
* パス"/v1/users/1"(architecture=<architecture>)にJSON<file:fixtures/v1/users/[userId]/failRegistrationDueToInvalidEmail/request.json>でPUTリクエストを送信する
* レスポンスのステータスコードが"400"である
* "users"テーブルの内容が"fixtures/v1/users/[userId]/failRegistrationDueToInvalidEmail/expected"にあるCSVファイルの内容と一致する
* "user_profiles"テーブルの内容が"fixtures/v1/users/[userId]/failRegistrationDueToInvalidEmail/expected"にあるCSVファイルの内容と一致する
