import static org.assertj.core.api.Assertions.assertThat;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublishers;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;

import com.thoughtworks.gauge.Step;
import com.thoughtworks.gauge.datastore.ScenarioDataStore;

public class StepImplementation {

  @Step("パス<path>にGETリクエストを送信する")
  public void sendGetRequest(String path) throws Exception {
    HttpClient client = HttpClient.newHttpClient();
    HttpRequest request = HttpRequest.newBuilder()
        .uri(new URI("http://localhost:8081" + path))
        .build();

    HttpResponse<String> response = client.send(request, BodyHandlers.ofString());

    ScenarioDataStore.put("STATUS_CODE", response.statusCode());
    ScenarioDataStore.put("RESPONSE_BODY", response.body());
  }

  @Step("パス<path>にJSON<json>でPUTリクエストを送信する")
  public void sendPutRequest(String path, String json) throws Exception {
    doSendPutRequest("http://localhost:8081" + path, json);
  }

  @Step("パス<path>(architecture=<architecture>)にJSON<json>でPUTリクエストを送信する")
  public void sendPutRequest(String path, String architecture, String json) throws Exception {
    doSendPutRequest("http://localhost:8081" + path + "?architecture=" + architecture, json);
  }  

  @Step("パス<path>(architecture=<architecture>, withNotify=<withNotify>)にJSON<json>でPUTリクエストを送信する")
  public void sendPutRequest(String path, String architecture, Boolean withNotify, String json) throws Exception {
    doSendPutRequest("http://localhost:8081" + path + "?architecture=" + architecture + "&withNotify=" + withNotify, json);
  }

  public void doSendPutRequest(String uri, String json) throws Exception {
    HttpClient client = HttpClient.newHttpClient();
    HttpRequest request = HttpRequest.newBuilder()
        .uri(new URI(uri))
        .header("Content-Type", "application/json")
        .PUT(BodyPublishers.ofString(json))
        .build();

    HttpResponse<String> response = client.send(request, BodyHandlers.ofString());

    ScenarioDataStore.put("STATUS_CODE", response.statusCode());
    ScenarioDataStore.put("RESPONSE_BODY", response.body());
  }

  @Step("レスポンスのステータスコードが<status_code>である")
  public void verifyStatusCode(int statusCode) {
    int actualStatusCode = (int) ScenarioDataStore.get("STATUS_CODE");
    assertThat(actualStatusCode).isEqualTo(statusCode);
  }

  @Step("レスポンスボディが<expected_body>である")
  public void verifyResponseBody(String expectedBody) {
    String actualBody = (String) ScenarioDataStore.get("RESPONSE_BODY");
    assertThat(actualBody).isEqualTo(expectedBody);
  }

  @Step("<tableName>テーブルの内容が<path>にあるCSVファイルの内容と一致する")
  public void verifyTableData(String tableName, String path) throws Exception {
    Database.getInstance().assertTable(tableName, path);
  }

  @Step("<path>にあるCSVファイルの内容がDBに登録されている")
  public void registerTableData(String path) throws Exception {
    Database.getInstance().registerTableData(path);
  }

  @Step("通知先のAPIのパス<path>にPOSTリクエストが送信されたら、ステータスコード<status>を返す")
  public void verifyPostRequest(String path, Integer status) {
    WireMockClient.getInstance().setupPost(path, status);
  }

  @Step("通知先のAPIのパス<path>にJSON<json>でPOSTリクエストが送信された")
  public void verifyPostRequest(String path, String json) {
    WireMockClient.getInstance().assertPost(path, json);
  }

}