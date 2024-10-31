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

  @Step("パス<path>にJSON<json>でPUTリクエストを送信する")
  public void sendPutRequest(String path, String json) throws Exception {
    HttpClient client = HttpClient.newHttpClient();
    HttpRequest request = HttpRequest.newBuilder()
        .uri(new URI("http://localhost:8080" + path))
        .header("Content-Type", "application/json")
        .PUT(BodyPublishers.ofString(json))
        .build();

    HttpResponse<String> response = client.send(request, BodyHandlers.ofString());

    ScenarioDataStore.put("STATUS_CODE", response.statusCode());
  }

  @Step("レスポンスのステータスコードが<status_code>である")
  public void verifyStatusCode(int statusCode) {
    int actualStatusCode = (int) ScenarioDataStore.get("STATUS_CODE");
    assertThat(actualStatusCode).isEqualTo(statusCode);
  }

  @Step("<tableName>テーブルの内容が<path>にあるCSVファイルの内容と一致する")
  public void verifyTableData(String tableName, String path) throws Exception {
    Database.getInstance().assertTable(tableName, path);
  }
}