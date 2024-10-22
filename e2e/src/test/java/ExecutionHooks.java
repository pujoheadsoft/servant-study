import com.thoughtworks.gauge.BeforeScenario;
import com.thoughtworks.gauge.BeforeSuite;

public class ExecutionHooks {

  private Database database;

  @BeforeSuite
  public void setup() throws Exception {
    System.err.println("BeforeSuite");
    database = new Database();
    database.init();
  }

  @BeforeScenario
  public void clearData() throws Exception {
    System.err.println("BeforeScenario");
    System.err.println(database);
    database.truncateTables();
  }
  
}
