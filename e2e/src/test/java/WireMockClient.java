
import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.equalToJson;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.postRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;

import com.github.tomakehurst.wiremock.client.WireMock;

public class WireMockClient {

  private WireMock wiremock = new WireMock("localhost", 8080);

  private static WireMockClient INSTANCE;
  static {
    try {
      INSTANCE = new WireMockClient();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  public static WireMockClient getInstance() {
    return INSTANCE;
  }

  @SuppressWarnings("static-access")
  public void setupPost(String path, Integer status) {
    // Mockサーバーの設定
    wiremock.stubFor(post(urlEqualTo(path))
        .willReturn(aResponse()
            .withStatus(status)
            .withBody("{\"message\":\"OK\"}")));
  }

  public void assertPost(String path, String json) {
    wiremock.verify(postRequestedFor(urlEqualTo(path))
        .withRequestBody(equalToJson(json)));
  }
}
