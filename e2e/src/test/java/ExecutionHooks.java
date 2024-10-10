import com.thoughtworks.gauge.BeforeScenario;
import com.thoughtworks.gauge.BeforeSuite;

public class ExecutionHooks {

  private Database database;

  @BeforeSuite
  public void setup() {
    database = new Database();
    try {
      database.init();
      database.truncateTables();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @BeforeScenario
  public void clearData() {
    try {
      database.truncateTables();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
  
}
