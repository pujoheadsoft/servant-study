# メールアドレスが妥当でない場合は登録できない(単純な実装)

## メールアドレスが妥当でない場合は登録できない(単純な実装)
* パス"/v1/users/1?architecture=simple"にJSON<file:fixtures/v1/users/[userId]/failRegistrationDueToInvalidEmail/request.json>でPUTリクエストを送信する
* レスポンスのステータスコードが"400"である
* "users"テーブルの内容が"fixtures/v1/users/[userId]/failRegistrationDueToInvalidEmail/expected"にあるCSVファイルの内容と一致する
* "user_notifications"テーブルの内容が"fixtures/v1/users/[userId]/failRegistrationDueToInvalidEmail/expected"にあるCSVファイルの内容と一致する