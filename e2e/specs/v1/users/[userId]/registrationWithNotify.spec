# ユーザー情報を登録しそれを通知することができる

|architecture|
|------------|
|simple      |
|taglessFinal|
|polysemy    |
|heftia      |

## ユーザー情報を登録しそれを通知することができる
* 通知先のAPIのパス"/notifications/userRegistered"にPOSTリクエストが送信されたら、ステータスコード"200"を返す
* パス"/v1/users/1"(architecture=<architecture>, withNotify="true")にJSON<file:fixtures/v1/users/[userId]/registrationWithNotify/request.json>でPUTリクエストを送信する
* レスポンスのステータスコードが"200"である
* レスポンスボディが"OK"である
* 通知先のAPIのパス"/notifications/userRegistered"にJSON<file:fixtures/v1/users/[userId]/registrationWithNotify/expected/notificationApi/request.json>でPOSTリクエストが送信された
* "users"テーブルの内容が"fixtures/v1/users/[userId]/registrationWithNotify/expected/db"にあるCSVファイルの内容と一致する
* "user_profiles"テーブルの内容が"fixtures/v1/users/[userId]/registrationWithNotify/expected/db"にあるCSVファイルの内容と一致する
